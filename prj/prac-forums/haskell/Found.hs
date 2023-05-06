
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}

module Found
where
import           Config
import           Model

import           Develop.DatFw
import           Develop.DatFw.Handler
import           Develop.DatFw.Widget
import           Develop.DatFw.Template

import           Data.Text (Text)
import qualified Data.Text.Encoding as T
import           Data.ByteString.Builder
import           Control.Monad.IO.Class   -- imports liftIO
import           Control.Monad.Trans.Maybe

-- ---------------------------------------------------------------
-- Definició dels tipus del site ForumsApp i de les corresponents rutes.

data ForumsApp = ForumsApp { forumsDb :: Connection }


instance RenderRoute ForumsApp where
    data Route ForumsApp =
                  HomeR | ForumR ForumId | TopicR TopicId
                | LoginR | LogoutR

    renderRoute HomeR   = ([], [])
    renderRoute (ForumR tid) = (["forums",toPathPiece tid], [])
    renderRoute (TopicR qid) = (["topics",toPathPiece qid], [])
    renderRoute LoginR  = (["login"], [])
    renderRoute LogoutR = (["logout"], [])

-- Nota: Els tipus ForumId, TopicId i PostId són alias de 'Key ...' (veieu el model)
instance PathPiece (Key a) where
    toPathPiece (Key k) = showToPathPiece k
    fromPathPiece p = Key <$> readFromPathPiece p


runDbAction :: (MonadHandler m, HandlerSite m ~ ForumsApp) => DbM a -> m a
runDbAction f = do
    conn <- getsSite forumsDb
    runDbTran conn f

-- ---------------------------------------------------------------
-- Instancia de WebApp (configuracio del lloc) per a ForumsApp.

instance WebApp ForumsApp where
    defaultLayout wdgt = do
        page <- widgetToPageContent wdgt
        mbmsg <- getMessage
        mbuser <- fmap (fmap snd) maybeAuth
        applyUrlRenderTo $(htmlTemplFile $ templatesDir <> "/default-layout.html")

-- ****************************************************************
-- Sistema d'autenticacio.

-- Utilitats a ser usades des dels handlers

authId_SESSION_KEY :: Text
authId_SESSION_KEY = "__AUTHID"

maybeAuthId :: (MonadHandler m) => m (Maybe UserId)
maybeAuthId = do
    mbsid <- lookupSession authId_SESSION_KEY
    return $ mbsid >>= fromPathPiece

requireAuthId :: (MonadHandler m, ForumsApp ~ HandlerSite m) => m UserId
requireAuthId = do
    mbaid <- maybeAuthId
    maybe handleNoAuthId pure mbaid

handleNoAuthId :: (MonadHandler m, ForumsApp ~ HandlerSite m) => m a
handleNoAuthId = do
    setUltDestCurrent
    redirect LoginR


maybeAuth :: (MonadHandler m, HandlerSite m ~ ForumsApp) => m (Maybe (UserId, UserD))
maybeAuth = runMaybeT $ do
    aid <- MaybeT maybeAuthId
    ae <- MaybeT $ runDbAction $ getUser aid
    pure (aid, ae)

requireAuth :: (MonadHandler m, HandlerSite m ~ ForumsApp) => m (UserId, UserD)
requireAuth = do
    mbp <- maybeAuth
    maybe handleNoAuthId pure mbp

