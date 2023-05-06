
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module AuthHandler
where
import           Found
import           Model
import           Db

import           Develop.DatFw
import           Develop.DatFw.Widget
import           Develop.DatFw.Template
import           Develop.DatFw.Form
import           Develop.DatFw.Form.Fields

import           Data.Text (Text)
import           Control.Monad            -- imports when, ...
import           Control.Monad.Trans.Maybe
import           Text.Blaze


-- ****************************************************************
-- Sistema d'autenticacio.

-- Handlers del sitema d'autenticació.

loginForm :: MonadHandler m => AForm m (Text, Text)
loginForm =
    (,) <$> freq textField "Nom d'usuari" Nothing
        <*> freq passwordField "Clau d'accés" Nothing

getLoginR :: HandlerFor ForumsApp Html
getLoginR = do
    setUltDestReferer
    -- Return HTML page
    (_, formw) <- runAFormPost loginForm
    defaultLayout $ loginView formw

postLoginR :: HandlerFor ForumsApp Html
postLoginR = do
    toMaster <- getRouteToMaster
    (formr, formw) <- runAFormPost loginForm
    case formr of
        FormSuccess (name, password) -> do
            ok <- validatePassword name password
            if ok then do
                -- Good credentials
                Just uid <- runDbAction $ loginUser name
                setSession authId_SESSION_KEY $ toPathPiece uid
                redirectUltDest HomeR
            else do
                -- Login error
                setMessage "Error d'autenticaciò"
                redirect LoginR
        _ ->
            defaultLayout (loginView formw)
    where
        validatePassword :: Text -> Text -> HandlerFor ForumsApp Bool
        validatePassword name password = do
            mbuser <- runDbAction $ getUserByName name
            case mbuser of
                Nothing -> pure False
                Just (_, user) -> pure $ pHashValidate password $ udPassword user

handleLogoutR :: HandlerFor ForumsApp ()
handleLogoutR = do
    -- | After logout (from the browser), redirect to the referring page.
    setUltDestReferer
    deleteSession authId_SESSION_KEY
    redirectUltDest HomeR


-- ---------------------------------------------------------------
-- Vistes del sistema d'autenticacio.

loginView :: Widget ForumsApp -> Widget ForumsApp
loginView formw = [widgetTempl|
<div class="row">
  <div class="col-sm-offset-2 col-sm-10">
    <form role="form" method="POST" action="@{LoginR}">
      ^{formw}
      <div class="form-group"><button type="submit" class="btn btn-success">Entra</button></div>
    </form>
  </div>
</div>
|]

