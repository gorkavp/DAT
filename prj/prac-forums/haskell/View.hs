
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module View
where
import           Config
import           Found
import           Model

import           Develop.DatFw
import           Develop.DatFw.Widget
import           Develop.DatFw.Template
import           Text.Blaze

import           Control.Monad.IO.Class   -- imports liftIO
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Maybe
import           Data.Time
import           Data.Semigroup
import           Language.Haskell.TH.Syntax

-- ---------------------------------------------------------------
-- Utilities for the view components
-- ---------------------------------------------------------------

uidNameWidget :: UserId -> Widget ForumsApp
uidNameWidget uid = do
    uname <- maybe "???" udName <$> runDbAction (getUser uid)
    toWidget $ toMarkup uname

dateWidget :: UTCTime -> Widget ForumsApp
dateWidget time = do
    zt <- liftIO $ utcToLocalZonedTime time
    let locale = defaultTimeLocale
    toWidget $ toMarkup $ T.pack $ formatTime locale "%e %b %Y, %H:%M" zt

pidPostedWidget :: PostId -> Widget ForumsApp
pidPostedWidget pid = do
    mbpost <- runDbAction $ getPost pid
    maybe "???" (dateWidget . pdPosted) mbpost

-- ---------------------------------------------------------------
-- Views
-- ---------------------------------------------------------------

homeView :: Maybe (UserId, UserD) -> Widget ForumsApp -> Widget ForumsApp
homeView mbuser fformw = do
    forums <- runDbAction getForumList
    $(widgetTemplFile $ templatesDir <> "/home.html")

forumView :: Maybe (UserId, UserD) -> (ForumId, ForumD) -> WidgetFor ForumsApp () -> Widget ForumsApp -> Widget ForumsApp
forumView mbuser (fid, forum) tformw fformw = do
    topics <- runDbAction $ getTopicList fid
    $(widgetTemplFile $ templatesDir <> "/forum.html")

topicView :: Maybe (UserId, UserD) -> (ForumId, ForumD) -> (TopicId, TopicD) -> WidgetFor ForumsApp () -> Widget ForumsApp -> Widget ForumsApp
topicView mbuser (fid, forum) (tid, topic) pformw tformw = do
    posts <- runDbAction $ getPostList tid
    $(widgetTemplFile $ templatesDir <> "/topic.html")

postView :: Maybe (UserId, UserD) -> (ForumId, ForumD) -> (TopicId, TopicD) -> (PostId, PostD) -> Widget ForumsApp -> Widget ForumsApp
postView mbuser (fid, forum) (tid, topic) (pid, post) pformw = do
    $(widgetTemplFile $ templatesDir <> "/post.html")
    

