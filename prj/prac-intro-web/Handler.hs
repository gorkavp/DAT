

{-# LANGUAGE OverloadedStrings #-}

module Handler
    -- Exporta les seguents declaracions d'aquest modul
    ( Handler, dispatchHandler
    , HandlerResponse, respHtml, respRedirect, respError
    , getMethod, onMethod
    , getSession, setSession, deleteSession
    , postParams, lookupPostParams, lookupPostParam
    )
where
import qualified Network.Wai as W
import qualified Network.HTTP.Types as W
import qualified Web.Cookie as W

import qualified Text.Blaze.Html5 as H
import           Text.Blaze.Html.Renderer.Utf8

import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Encoding as T
import qualified Data.ByteString as B
import           Data.ByteString.Builder
import qualified Data.ByteString.Lazy as BL
import           Data.Maybe
import           Data.Monoid
import           Control.Monad
import           Control.Applicative
import           Control.Monad.IO.Class
import           Text.Read

-- ****************************************************************

-- Tipus correponent al monad 'Handler'.
-- El context d'un Handler compren:
--      L'argument Request que permet obtenir informacio sobre la peticio.
--      L'estat del Handler (argument i resultat de les operacions).
newtype Handler a =
    HandlerC { runHandler :: W.Request -> HandlerState -> IO (a, HandlerState) }

-- HandlerState compren:
--      'Cache' dels parametres de la peticio.
--      L'estat de la sessio que s'obte de les corresponents 'cookies'.
--        Aquest estat de sessio es una llista de parelles nom-valor.
data HandlerState =
    HandlerStateC { hsQuery :: Maybe [(Text, Text)], hsSession :: [(Text, Text)] }

-- Funcions auxiliars per modificar l'estat del handler
hsSetQuery :: Maybe [(Text, Text)] -> HandlerState -> HandlerState
hsSetQuery q (HandlerStateC _ s) = HandlerStateC q s
hsSetSession :: [(Text, Text)] -> HandlerState -> HandlerState
hsSetSession s (HandlerStateC q _) = HandlerStateC q s

instance Functor Handler where
    -- tipus en aquesta instancia:
    --      fmap :: (a -> b) -> Handler a -> Handler b
    fmap f (HandlerC h) = HandlerC $ \ req st0 -> do
        -- Monad IO:
        -- f :: a -> b
        (x, st1) <- h req st0 -- h :: W.Request -> HandlerState -> IO (x, st1) = IO (a, HandlerState) = Handler a
        pure (f x, st1) -- pure :: (f x, st1) -> IO (b, HandlerState) = Handler b

instance Applicative Handler where
    -- tipus en aquesta instancia:
    --      pure  :: a -> Handler a
    --      (<*>) :: Handler (a -> b) -> Handler a -> Handler b
    pure x =
        HandlerC $ \ _ st0 -> pure (x, st0) -- HandlerC :: (W.Request -> HandlerState -> IO (a, HandlerState)) -> Handler a
    HandlerC hf <*> HandlerC hx =
        HandlerC $ \ req st0 -> do
            -- Monad IO
            (f, st1) <- hf req st0 -- hf :: W.Request -> HandlerState -> IO (f, st1) = IO (a -> b, HandlerState) = Handler (a -> b) = Handler f
            (x, st2) <- hx req st1 -- hx :: W.Request -> HandlerState -> IO (x, st2) = IO (a, HandlerState) = Handler a
            pure (f x, st2) -- pure :: (f x, st2) -> IO (b, HandlerState) = Handler b

instance Monad Handler where
    -- tipus en aquesta instancia:
    --      (>>=) :: Handler a -> (a -> Handler b) -> Handler b
    HandlerC hx >>= f =
    -- f :: a -> Handler b
        HandlerC $ \ req st0 -> do -- apliquem f al resultat de hx
            -- Monad IO:
            (x, st1) <- hx req st0 -- hx :: W.Request -> HandlerState -> IO (a, HandlerState) = Handler a
            runHandler (f x) req st1 -- runHandler :: Handler (f x) -> W.Request -> HandlerState -> IO (b, HandlerState) = Handler b

-- class MonadIO: Monads in which IO computations may be embedded.
-- The method 'liftIO' lifts a computation from the IO monad.
instance MonadIO Handler where
    -- tipus en aquesta instancia:
    --      liftIO :: IO a -> Handler a
    liftIO io = HandlerC $ \ _ st0 -> do
        x <- io -- Monad IO: crea un IO action que executa la IO action io i retorna el resultat x
        pure (x, st0) -- Monad IO: crea un IO action que retorna el valor x i l'estat st0

-- ****************************************************************
-- Aquestes funcions no s'exporten pero son utils en les implementacions
-- de les funcions exportades.

-- Obte informaciÃ³ de la peticio
asksRequest :: (W.Request -> a) -> Handler a
asksRequest f = HandlerC $ \ req st0 ->
    pure (f req, st0)

-- Obte informaciÃ³ de l'estat del handler
getsHandlerState :: (HandlerState -> a) -> Handler a
-- f :: HandlerState -> a
getsHandlerState f =
    -- Monad IO:
    HandlerC $ \ _ st0 -> pure (f st0, st0) -- pure :: (f st0, st0) -> IO (a, HandlerState) = Handler a

-- Modifica l'estat del handler
modifyHandlerState :: (HandlerState -> HandlerState) -> Handler ()
-- f :: HandlerState -> HandlerState
modifyHandlerState f =
    -- Monad IO:
    HandlerC $ \ _ st0 -> pure ((), f st0) -- pure :: ((), f st0) -> IO ((), HandlerState) = Handler ()

-- ****************************************************************

-- Tipus que ha de tenir el resultat del handler que se li passa a 'dispatchHandler'.
data HandlerResponse =
        HRHtml H.Html           -- Resposta normal. Parametre: Contingut HTML.
      | HRRedirect Text         -- Redireccio. Parametre: URL.
      | HRError W.Status Text   -- Resposta anormal. Parametres: Codi d'estat HTTP i missatge.

-- 'dispatchHandler' converteix (adapta) un 'Handler' a una aplicacio WAI,
-- realitzant els passos seguents:
--      Obte l'estat inicial (st0) del handler amb una sessio inicial a partir
--        de les cookies rebudes en la peticio WAI.
--      Executa el handler passant-li la peticio i l'estat inicial.
--      Amb l'execucio del handler s'obte el parell format
--        pel resultat del handler (res) i l'estat final (st1).
--      Construeix la corresponent resposta WAI i l'envia.
--        La resposta WAI depen del nou estat de sessio en st1.
-- El tipus 'Application' esta definit en el modul 'Network.Wai' com:
--      type Application = Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
dispatchHandler :: Handler HandlerResponse -> W.Application
dispatchHandler handler req respond = do
    -- Monad IO:
    let st0 = HandlerStateC{ hsQuery = Nothing, hsSession = requestSession req }
    (res, st1) <- runHandler handler req st0
    let scValue = mkSetCookieValue $ hsSession st1
        wairesp = case res of
            HRHtml html ->
                let headers = [ ("Content-Type", mimeHtml)
                              , ("Set-Cookie", scValue) ]
                in W.responseBuilder W.ok200 headers (renderHtmlBuilder html)
            HRRedirect url ->
                let headers = [ ("Location", T.encodeUtf8 url)
                              , ("Content-Type", mimeText)
                              , ("Set-Cookie", scValue) ]
                in W.responseBuilder W.seeOther303 headers (T.encodeUtf8Builder "Redirect")
            HRError status msg ->
                let headers = [ ("Content-Type", mimeText) ]
                in W.responseBuilder status headers (T.encodeUtf8Builder msg)
    respond wairesp

-- Els constructors de HandlerResponse no s'exporten.
-- S'exporten en canvi les funcions seguents que obtenen simples handlers que retornen
-- els diferents tipus de resposta:

respHtml :: H.Html -> Handler HandlerResponse
respHtml html = pure $ HRHtml html

respRedirect :: Text -> Handler HandlerResponse
respRedirect url = pure $ HRRedirect url

respError :: W.Status -> Text -> Handler HandlerResponse
respError status msg = pure $ HRError status msg


-- ****************************************************************

-- Obte el metode HTTP de la peticio
getMethod :: Handler W.Method
getMethod = asksRequest W.requestMethod

-- Obte el metode HTTP de la peticio
onMethod :: [(W.Method, Handler HandlerResponse)] -> Handler HandlerResponse
onMethod alts = do
    -- Monad Handler:
    method <- getMethod
    case lookup method alts of
        Just h -> h
        Nothing -> respError W.methodNotAllowed405 "Invalid method"

-- Obte el valor de l'atribut de sessio amb el nom indicat.
-- Retorna Nothing si l'atribut indicat no existeix o no te la sintaxis adequada.
getSession :: Read a => Text -> Handler (Maybe a)
getSession name = do
    session <- getsHandlerState hsSession
    pure $ maybe Nothing (readMaybe . T.unpack) $ lookup name session

-- Fixa l'atribut de sessio amb el nom i valor indicats.
setSession :: Show a => Text -> a -> Handler ()
setSession name value = do
    session <- getsHandlerState hsSession
    let newsession = (name, T.pack $ show value) : filter ((name /=) . fst) session
    modifyHandlerState (hsSetSession newsession)

-- Elimina l'atribut de sessio amb el nom indicat.
deleteSession :: Text -> Handler ()
deleteSession name = do
    session <- getsHandlerState hsSession
    let newsession = filter ((name /=) . fst) session
    modifyHandlerState (hsSetSession newsession)

-- Obte els valor associat al parametre de la peticio amb el nom indicat.
lookupPostParam :: Text -> Handler (Maybe Text)
lookupPostParam name = do
    vals <- lookupPostParams name
    case vals of
        [] -> pure Nothing
        (v:_) -> pure (Just v)

-- Obte els valors associats al parametre de la peticio amb el nom indicat.
lookupPostParams :: Text -> Handler [Text]
lookupPostParams name = do
    -- Monad Handler:
    mbparams <- postParams
    case mbparams of
        Just params -> -- params es una llista de parelles de tipus (Text, Text)
            -- Caldra obtenir tots els valors (segon component) de les parelles que tenen el nom (primer component) igual al indicat.
            -- NOTA: Useu les funcions
            --   fst :: (a, b) -> a
            --   snd :: (a, b) -> b
            --   filter :: (a -> Bool) -> [a] -> [a]
            --   map :: (a -> b) -> [a] -> [b]
            pure $ map snd $ filter ((name ==) . fst) params
            -- la composició de funcions (.) és associativa per la dretam es a dir que f (g h) = (f g) h 
            -- la funció map aplica la funció snd a tots els elements de la llista que retorna la funció filter per tant retorna una llista amb els segons elements de les parelles (tupla) que tenen el nom indicat
            -- la funció filter elimina tots els elements de la llista on el primer element de la parella (tupla) no coincideixi amb el nom indicat
        Nothing ->
            -- El contingut de la peticio no es un formulari. No hi ha valors.
            pure []

-- Obte tots els parametres (parelles (nom,valor)) del contingut de la peticio.
-- Retorna Nothing si el contingut de la peticio no es un formulari.
postParams :: Handler (Maybe [(Text, Text)])
postParams = do
    -- Si previament ja s'havien obtingut els parametres (i guardats en l'estat del handler)
    -- aleshores es retornen aquests, evitant tornar a llegir el contingut de la peticio.
    cache <- getsHandlerState hsQuery
    case cache of
        Just query ->
            pure $ Just query
        Nothing -> do
            req <- asksRequest id
            if lookup W.hContentType (W.requestHeaders req) == Just "application/x-www-form-urlencoded" then do
                query <- liftIO $ parsePostQuery <$> getAllBody req
                modifyHandlerState (hsSetQuery $ Just query)
                pure $ Just query
            else
                pure Nothing

-- ****************************************************************
-- Funcions internes (utilitats no exportades)

mimeText :: B.ByteString
mimeText = "text/plain;charset=UTF-8"

mimeHtml :: B.ByteString
mimeHtml = "text/html;charset=UTF-8"

-- Obte l'estat de sessio a partir de la corresponent 'cookie' de la peticio.
requestSession :: W.Request -> [(Text, Text)]
requestSession req =
    let mbvalue = do -- Monad Maybe
            cookieHeader <- lookup "Cookie" (W.requestHeaders req)
            session <- lookup (T.encodeUtf8 "session") (W.parseCookies cookieHeader)
            readMaybe $ T.unpack $ T.decodeUtf8 session
    in maybe [] id mbvalue

-- Funcio auxiliar que obte el valor de la 'cookie' resultant a partir de l'estat de sessio.
mkSetCookieValue :: [(Text, Text)] -> B.ByteString
mkSetCookieValue session =
    let setCookie = W.defaultSetCookie { W.setCookieName = T.encodeUtf8 "session"
                                       , W.setCookieValue = T.encodeUtf8 $ T.pack $ show session
                                       }
    in BL.toStrict $ toLazyByteString $ W.renderSetCookie setCookie

parsePostQuery :: B.ByteString -> [(Text, Text)]
parsePostQuery content =
    decodepair <$> W.parseSimpleQuery content
    where
        decodepair (n, v) = (T.decodeUtf8 n, T.decodeUtf8 v)

getAllBody :: W.Request -> IO B.ByteString
getAllBody req = do
    b <- W.getRequestBodyChunk req
    if B.null b then pure B.empty
    else do
        bs <- getAllBody req
        pure $ b <> bs