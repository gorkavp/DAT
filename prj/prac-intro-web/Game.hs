{-# LANGUAGE OverloadedStrings #-}

module Main
where
import           Network.Wai
import           Network.Wai.Handler.Warp (runEnv)

import           Handler

import           Data.Text (Text)
import qualified Data.Text as T

import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

-- ****************************************************************

main :: IO ()
main = do
    -- runEnv :: Port -> Application -> IO ()
    runEnv 4050 $ dispatchHandler gameApp -- routeHandler

-- ****************************************************************
-- Controller

gameApp :: Handler HandlerResponse
gameApp = onMethod
    [ ("GET", doGet)
    , ("POST", doPost)
    ]

-- funció que retorna l'estat de la sessió a la vista html o l'estat inicial si no hi ha cap valor a la sessió
doGet :: Handler HandlerResponse
doGet = do
    -- getSession :: Text -> Handler (Maybe Text)
    mbSessionState <- getSession "playState" -- retorna el valor de l'atribut "playState" de la sessió de l'usuari
    -- unpack :: Text -> String
    -- read :: Read a => String -> a
    let game = maybe startState (read . T.unpack) mbSessionState -- si mbSessionState és Nothing retorna startState, si no aplicarà la funció unpack que converteix el Text a String i la funció read que converteix el String a l'estat del joc (game)
    -- htmlView :: GameState -> H.Html
    -- respHtml :: H.Html -> Handler HandlerResponse
    respHtml $ htmlView game -- retorna la vista amb l'estat de la sessió

-- funció que actualitza l'estat de la sessió obtenint la cadena de caràcters del formulari introduïda per l'usuari usant la funció lookupPostParam amb el nom del paràmetre "playText" i manté l'estat de la sessió usant les funcions setSession i getSession amb el nom de l'atribut "playState"
doPost :: Handler HandlerResponse
doPost = do
    -- lookupPostParam :: Text -> Handler (Maybe Text)
    mbvalue <- lookupPostParam "playText" -- retorna el valor del paràmetre "playText" del formulari de l'usuari
    case mbvalue of
        Nothing -> -- si no hi ha cap valor
            -- respRedirect :: Text -> Handler HandlerResponse
            respRedirect "#" -- redirigeix a la mateixa pàgina
        Just value -> do -- si hi ha un valor
            -- getSession :: Text -> Handler (Maybe Text)
            mbSessionState <- getSession "playState" -- retorna el valor de l'atribut "playState" de la sessió de l'usuari
            -- unpack :: Text -> String
            -- read :: Read a => String -> a
            let game = maybe startState (read . T.unpack) mbSessionState -- si mbSessionState és Nothing retorna startState, si no aplicarà la funció unpack que converteix el Text a String i la funció read que converteix el String a l'estat del joc (game)
            -- setSession :: Text -> Text -> Handler ()
            -- pack :: String -> Text
            -- show :: Show a => a -> String
            -- playText :: Text -> GameState -> GameState
            setSession "playState" (T.pack $ show $ playText value game) -- actualitza l'estat de la sessió mitjançant la funció playText que retorna l'estat final del joc a partir del text introduït per l'usuari i l'estat inicial del joc, la funció show que converteix l'estat del joc a String, la funció pack que converteix el String a Text i la funció setSession que actualitza el valor de l'atribut "playState" de la sessió de l'usuari
            -- respRedirect :: Text -> Handler HandlerResponse
            respRedirect "#" -- redirigeix a la mateixa pàgina

-- ****************************************************************
-- View

htmlView :: GameState -> H.Html
htmlView game =
    H.docTypeHtml $ do
        H.head $
            H.title "A simple game ..."
        H.body $ do
            H.h1 $ H.text $ "Game state: " <> T.pack (show game)
            H.hr
            H.form H.! A.method "POST" H.! A.action "#" $ do
                H.p $ do
                    H.span "String to play: "
                    H.input H.! A.name "playText"
                H.input H.! A.type_ "submit" H.! A.name "ok" H.! A.value "Play"

-- ****************************************************************
-- Model

-- Tipus de l'estat
type GameState = (Bool, Int)

-- inicialització de l'estat inicial
startState :: GameState
startState = (False, 0)

-- funció que a partir d'un caràcter i un estat inicial retorna l'estat corresponent
playChar :: Char -> GameState -> GameState
playChar x (on, score) = do -- on és un booleà que indica si el joc està actiu o no i score és un enter que indica la puntuació
    case x of
        '+' -> if on then (on, score + 1) else (on, score) -- si està a true i es troba un '+', incrementa el score en 1, si no, no fa res
        '-' -> if on then (on, score - 1) else (on, score) -- si està a true i es troba un '-', decrementa el score en 1, si no, no fa res
        '*' -> (not on, score) -- si es troba un '*', canvia el valor de on
        _ -> (on, score) -- si es troba qualsevol altre caràcter, no fa res

-- funció que aplicar playChar a cada un dels caràcters del String
playString :: String -> GameState -> GameState
-- playString [] game = game
-- playString (x:xs) = do
    -- playChar x game
    -- playString xs
-- foldl :: (b -> a -> b) -> b -> [a] -> b
playString xs game = foldl (flip playChar) game xs

-- funció que a partir d'un text i un estat inicial retorna l'estat final
playText :: Text -> GameState -> GameState
-- unpack :: Text -> String
-- playString :: String -> GameState -> GameState
playText t = playString (T.unpack t) -- aplica playString al String obtingut a partir del Text t que es passa com a paràmetre