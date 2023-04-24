

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
    -- \ req st0 -> do: The function takes a request and a state and returns an IO action that takes a state and returns a tuple (a, HandlerState). The first element of the tuple is the result of the computation and the second element is the new state.
    fmap f (HandlerC h) = HandlerC $ \ req st0 -> do
        -- Monad IO:
        (x, st1) <- h req st0
        pure (f x, st1)

instance Applicative Handler where
    -- tipus en aquesta instancia:
    --      pure  :: a -> Handler a
    --      (<*>) :: Handler (a -> b) -> Handler a -> Handler b
    -- The pure function takes a value of type a and wrap it into a Handler. The <*> function takes two Handlers and returns a Handler that applies the function in the first Handler to the value in the second Handler. The result is wrapped in a Handler using the pure function.
    pure x =
        HandlerC $ \ _ st0 -> pure (x, st0) -- Monad IO: crea un IO action que retorna el valor x i l'estat st0
    HandlerC hf <*> HandlerC hx =
        HandlerC $ \ req st0 -> do -- Monad IO: crea un IO action que executa el handler hf sobre la peticio i l'estat inicial de hx i despres el handler hx sobre la peticio i l'estat final de hf. Els arguments req i st0 son els que s'han passat a la funcio (<*>).
            -- Monad IO:
            (f, st1) <- hf req st0 -- es passa l'estat st0 al handler hf perque es pot haver modificat l'estat en el handler hx
            (x, st2) <- hx req st1 -- es passa l'estat st1 al handler hx perque es pot haver modificat l'estat en el handler hf
            pure (f x, st2) -- es retorna el resultat de aplicar la funcio f al valor x i l'estat final st2

instance Monad Handler where
    -- tipus en aquesta instancia:
    --      (>>=) :: Handler a -> (a -> Handler b) -> Handler b
    HandlerC hx >>= f = -- executa el handler hx i despres el handler f
        HandlerC $ \ req st0 -> do -- Monad IO: crea un IO action que executa el handler hx i despres el handler f. Els arguments req i st0 son els que s'han passat a la funcio (>>=).
            -- Monad IO:
            (x, st1) <- hx req st0 -- Executa el handler hx amb l'estat st0 i retorna el resultat x i l'estat st1 que es passa al seguent handler f. El seguent handler f es el que s'ha passat com a argument a la funcio (>>=).
            runHandler (f x) req st1 -- Executa el handler f amb l'estat st1 i retorna el resultat i l'estat final.

-- class MonadIO: Monads in which IO computations may be embedded.
-- The method 'liftIO' lifts a computation from the IO monad.
instance MonadIO Handler where
    -- tipus en aquesta instancia:
    --      liftIO :: IO a -> Handler a
    liftIO io = HandlerC $ \ _ st0 -> do
        x <- io
        pure (x, st0)

-- ****************************************************************
-- Aquestes funcions no s'exporten pero son utils en les implementacions
-- de les funcions exportades.

-- Obte informaciÃ³ de la peticio
asksRequest :: (W.Request -> a) -> Handler a
asksRequest f = HandlerC $ \ req st0 ->
    pure (f req, st0)

-- Obte informaciÃ³ de l'estat del handler
getsHandlerState :: (HandlerState -> a) -> Handler a
getsHandlerState f =
    -- We ignore the first argument (the request) because we don't need it and then we apply the function f to the second argument (HandlerState) which gives us a result of type a. Finally we wrap the result of type a in the Handler monad unsing pure function, which is defined in the Applicative instance of Handler, along with the HandlerState.
    HandlerC $ \ _ st0 -> pure (f st0, st0)

-- Modifica l'estat del handler
modifyHandlerState :: (HandlerState -> HandlerState) -> Handler ()
modifyHandlerState f =
    -- Creates a HandlerC action that takes a state st0 and returns a new state st1. The action returns a tuple ((), st1) which means that the handler returns a value of type () (unit) and modifies the state st0 to st1. The state is modified by applying the function f to the state st0.
    HandlerC $ \ _ st0 -> pure ((), f st0)

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