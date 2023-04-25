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
    mbSessionState <- getSession "playState" -- retorna el valor de l'atribut "playState" de la sessió
    let game = maybe startState (read . T.unpack) mbSessionState -- si no hi ha cap valor, retorna l'estat inicial
    respHtml $ htmlView game -- retorna la vista amb l'estat de la sessió

-- funció que actualitza l'estat de la sessió amb el nou valor retornat per playText i retorna l'estat de la sessió actualitzat a la vista html
doPost :: Handler HandlerResponse
doPost = do
    mbvalue <- lookupPostParam "playText"
    let egame = do -- Monad (Either T.Text)
            value <- maybe (Left "Text obligatori") Right mbvalue
            if T.null value then Left "Text obligatori"
            else Right value
    case egame of
        Left err ->
            respHtml $ htmlView startState
        Right game -> do
            setSession "playState" (playState $ playText game startState) -- actualitza l'estat de la sessió amb el nou valor retornat per playText

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
playChar x (on, score) = do
    case x of
        '+' -> if on then (on, score + 1) else (on, score)
        '-' -> if on then (on, score - 1) else (on, score)
        '*' -> (not on, score)
        _ -> (on, score)

-- funció que aplicar playChar a cada un dels caràcters del String
playString :: String -> GameState -> GameState
-- playString [] game = game
-- playString (x:xs) = do
    -- playChar x game
    -- playString xs
--playString xs game = foldl (flip playChar) game xs
playString xs game = foldr playChar game xs

-- funció que a partir d'un text i un estat inicial retorna l'estat final
playText :: Text -> GameState -> GameState
playText t = playString (T.unpack t)

playState :: GameState -> Text
playState (on, score) = T.pack (show (on, score))