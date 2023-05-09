
{-# LANGUAGE OverloadedStrings #-}

module Handler
where
import View
import Found
import Model

import Develop.DatFw
import Develop.DatFw.Handler
import Develop.DatFw.Template
import Develop.DatFw.Widget
import Develop.DatFw.Form
import Develop.DatFw.Form.Fields
import Text.Blaze

import Data.Text as T

-- ---------------------------------------------------------------

markdownField :: Field (HandlerFor ForumsApp) Markdown
markdownField = checkMap
        (\ t -> if T.length t < minPostLen then Left "Text massa curt"
                else if T.length t > maxPostLen then Left "Text massa llarg"
                else Right (Markdown t))
        getMdText
        textareaField

---------------------------------------------------------------------

newForumForm :: AForm (HandlerFor ForumsApp) NewForum
newForumForm =
    NewForum <$> freq textField (withPlaceholder "Introduiu el títol del fòrum" "Titol") Nothing
             <*> freq markdownField (withPlaceholder "Introduiu la descripció del fòrum" "Descripció") Nothing

newTopicForm :: AForm (HandlerFor ForumsApp) NewTopic
newTopicForm =
    NewTopic <$> freq textField (withPlaceholder "Introduiu el títol del tema" "Titol") Nothing
             <*> freq markdownField (withPlaceholder "Introduiu el text del tema" "Text") Nothing

-- newPostForm :: AForm (HandlerFor ForumsApp) NewPost
-- newPostForm =
--     NewPost <$> freq markdownField (withPlaceholder "Introduiu el text del missatge" "Text") Nothing

getHomeR :: HandlerFor ForumsApp Html
getHomeR = do
    -- Get authenticated user
    mbuser <- maybeAuth
    -- Get a fresh form
    fformw <- generateAFormPost newForumForm
    -- Return HTML content
    defaultLayout $ homeView mbuser fformw

postHomeR :: HandlerFor ForumsApp Html
postHomeR = do
    user <- requireAuth
    (fformr, fformw) <- runAFormPost newForumForm
    case fformr of
        FormSuccess newtheme -> do
            runDbAction $ addForum (fst user) newtheme
            redirect HomeR
        _ ->
            defaultLayout $ homeView (Just user) fformw


getForumR :: ForumId -> HandlerFor ForumsApp Html
getForumR fid = do
    -- Get authenticated user
    -- maybeAuth :: HandlerFor ForumsApp (Maybe (UserId, UserD))
    -- mbuser :: Maybe (UserId, UserD)
    mbuser <- maybeAuth
    -- Get requested forum from data-base.
    -- Short-circuit (responds immediately) with a 'Not found' status if forum don't exist
    -- runDbAction :: SqlPersistT (HandlerFor ForumsApp) a -> HandlerFor ForumsApp a
    -- getForum :: ForumId -> SqlPersistT (HandlerFor ForumsApp) (Maybe ForumD)
    -- maybe notFound pure :: Maybe a -> HandlerFor ForumsApp a
    -- forum :: ForumD
    forum <- runDbAction (getForum fid) >>= maybe notFound pure
    -- Other processing (forms, ...)
    -- generateAFormPost :: AForm (HandlerFor ForumsApp) a -> HandlerFor ForumsApp (FormResult a, WidgetFor ForumsApp ())
    -- newTopicForm :: AForm (HandlerFor ForumsApp) NewTopic
    -- tformw :: WidgetFor ForumsApp ()
    tformw <- generateAFormPost newTopicForm
    -- Return HTML content
    -- defaultLayout :: WidgetFor ForumsApp () -> HandlerFor ForumsApp Html
    -- forumView :: Maybe (UserId, UserD) -> (ForumId, ForumD) -> WidgetFor ForumsApp ()
    defaultLayout $ forumView mbuser (fid, forum)

-- funció per crear un nou fòrum sempre i quan l'usuari estigui autenticat
postForumR :: ForumId -> HandlerFor ForumsApp Html
postForumR fid = do
    -- requireAuth :: HandlerFor ForumsApp (UserId, UserD)
    -- user :: (UserId, UserD)
    user <- requireAuth
    -- runAFormPost :: AForm (HandlerFor ForumsApp) a -> HandlerFor ForumsApp (FormResult a, WidgetFor ForumsApp ())
    -- newTopicForm :: AForm (HandlerFor ForumsApp) NewTopic
    -- (tformr, tformw) :: (FormResult NewTopic, WidgetFor ForumsApp ())
    (tformr, tformw) <- runAFormPost newTopicForm
    case tformr of
        -- FormSuccess :: a -> FormResult a
        -- newtopic :: NewTopic
        FormSuccess newtopic -> do
            -- runDbAction :: SqlPersistT (HandlerFor ForumsApp) a -> HandlerFor ForumsApp a
            -- addTopic :: ForumId -> UserId -> NewTopic -> SqlPersistT (HandlerFor ForumsApp) (TopicId, PostId)
            runDbAction $ addTopic fid (fst user) newtopic
            -- redirect :: Route ForumsApp -> HandlerFor ForumsApp a
            -- ForumR :: ForumId -> Route ForumsApp
            redirect (ForumR fid)    
        _ -> do
            -- runDbAction :: SqlPersistT (HandlerFor ForumsApp) a -> HandlerFor ForumsApp a
            -- getForum :: ForumId -> SqlPersistT (HandlerFor ForumsApp) (Maybe ForumD)
            -- maybe notFound pure :: Maybe a -> HandlerFor ForumsApp a
            -- forum :: ForumD
            forum <- runDbAction (getForum fid) >>= maybe notFound pure
            -- defaultLayout :: WidgetFor ForumsApp () -> HandlerFor ForumsApp Html
            -- forumView :: Maybe (UserId, UserD) -> (ForumId, ForumD) -> WidgetFor ForumsApp ()
            defaultLayout $ forumView (Just user) (fid, forum)
        

-- funció per poder obetnir el tema dins d'un fòrum
getTopicR :: TopicId -> HandlerFor ForumsApp Html
getTopicR tid = do
    -- maybeAuth :: HandlerFor ForumsApp (Maybe (UserId, UserD))
    -- mbuser :: Maybe (UserId, UserD)
    mbuser <- maybeAuth
    -- runDbAction :: SqlPersistT (HandlerFor ForumsApp) a -> HandlerFor ForumsApp a
    -- getTopic :: TopicId -> SqlPersistT (HandlerFor ForumsApp) (Maybe TopicD)
    -- maybe notFound pure :: Maybe a -> HandlerFor ForumsApp a
    -- topic :: TopicD
    topic <- runDbAction (getTopic tid) >>= maybe notFound pure
    -- runDbAction :: SqlPersistT (HandlerFor ForumsApp) a -> HandlerFor ForumsApp a
    -- newPostForm :: SqlPersistT (HandlerFor ForumsApp) (FormResult NewPost, WidgetFor ForumsApp ())
    -- post :: (FormResult NewPost, WidgetFor ForumsApp ())
    -- post <- generateAFormPost newPostForm
    -- defaultLayout :: WidgetFor ForumsApp () -> HandlerFor ForumsApp Html
    -- topicView :: Maybe (UserId, UserD) -> (ForumId, ForumD) -> (TopicId, TopicD) -> WidgetFor ForumsApp ()
    defaultLayout $ topicView mbuser  (tid, topic)

-- funció per crear un nou tema dins d'un fòrum
postTopicR :: TopicId -> HandlerFor ForumsApp Html
postTopicR tid = do
    -- requireAuth :: HandlerFor ForumsApp (UserId, UserD)
    -- user :: (UserId, UserD)
    user <- requireAuth
    -- runAFormPost :: AForm (HandlerFor ForumsApp) a -> HandlerFor ForumsApp (FormResult a, WidgetFor ForumsApp ())
    -- newTopicForm :: AForm (HandlerFor ForumsApp) NewTopic
    -- (tformr, tformw) :: (FormResult NewTopic, WidgetFor ForumsApp ())
    (tformr, tformw) <- runAFormPost newTopicForm
    case tformr of
        -- FormSuccess :: a -> FormResult a
        -- newtopic :: NewTopic
        FormSuccess newtopic -> do
            -- runDbAction :: SqlPersistT (HandlerFor ForumsApp) a -> HandlerFor ForumsApp a
            -- addTopic :: ForumId -> UserId -> NewTopic -> SqlPersistT (HandlerFor ForumsApp) (TopicId, PostId)
            -- redirect :: Route ForumsApp -> HandlerFor ForumsApp a
            -- TopicR :: TopicId -> Route ForumsApp
            runDbAction $ addTopic tid (fst user) newtopic
            redirect (TopicR tid)
        _ -> do
            -- runDbAction :: SqlPersistT (HandlerFor ForumsApp) a -> HandlerFor ForumsApp a
            -- getTopic :: TopicId -> SqlPersistT (HandlerFor ForumsApp) (Maybe TopicD)
            -- maybe notFound pure :: Maybe a -> HandlerFor ForumsApp a
            -- topic :: TopicD
            topic <- runDbAction (getTopic tid) >>= maybe notFound pure
            -- defaultLayout :: WidgetFor ForumsApp () -> HandlerFor ForumsApp Html
            -- topicView :: Maybe (UserId, UserD) -> (TopicId, TopicD) -> [PostD] -> WidgetFor ForumsApp ()
            defaultLayout $ topicView (Just user) (tid, topic) 

-- -- funció per poder obtenir un post dins d'un tema
-- getPostR :: PostId -> HandlerFor ForumsApp Html
-- getPostR pid = do
--     -- maybeAuth :: HandlerFor ForumsApp (Maybe (UserId, UserD))
--     -- mbuser :: Maybe (UserId, UserD)
--     mbuser <- maybeAuth
--     -- runDbAction :: SqlPersistT (HandlerFor ForumsApp) a -> HandlerFor ForumsApp a
--     -- getPost :: PostId -> SqlPersistT (HandlerFor ForumsApp) (Maybe PostD)
--     -- maybe notFound pure :: Maybe a -> HandlerFor ForumsApp a
--     -- post :: PostD
--     post <- runDbAction (getPost pid) >>= maybe notFound pure
--     -- defaultLayout :: WidgetFor ForumsApp () -> HandlerFor ForumsApp Html
--     -- postView :: Maybe (UserId, UserD) -> (PostId, PostD) -> WidgetFor ForumsApp ()
--     defaultLayout $ postView mbuser (pid, post)

-- -- funció per poder editar un post dins d'un tema
-- postEditPostR :: PostId -> HandlerFor ForumsApp Html
-- postEditPostR pid = do
--     user <- requireAuth
--     (pformr, pformw) <- runAFormPost editPostForm
--     case pformr of
--         FormSuccess newpost -> do
--             runDbAction $ editPost (fst user) pid newpost
--             redirect (PostR pid)
--         _ ->
--             defaultLayou

-- -- funció per poder editar un tema dins d'un fòrum
-- postEditTopicR :: TopicId -> HandlerFor ForumsApp Html
-- postEditTopicR tid = do
--     user <- requireAuth
--     (tformr, tformw) <- runAFormPost editTopicForm
--     case tformr of
--         FormSuccess newtopic -> do
--             runDbAction $ editTopic (fst user) tid newtopic
--             redirect (TopicR tid)
--         _ ->
--             defaultLayout

-- -- funció per poder editar un fòrum
-- postEditForumR :: ForumId -> HandlerFor ForumsApp Html
-- postEditForumR fid = do
--     user <- requireAuth
--     (fformr, fformw) <- runAFormPost editForumForm
--     case fformr of
--         FormSuccess newforum -> do
--             runDbAction $ editForum (fst user) fid newforum
--             redirect (ForumR fid)
--         _ ->
--             defaultLayout

-- -- funció per poder eliminar un post dins d'un tema
-- postDeletePostR :: PostId -> HandlerFor ForumsApp Html
-- postDeletePostR pid = do
--     user <- requireAuth
--     runDbAction $ deletePost (fst user) pid
--     redirect HomeR

-- -- funció per poder eliminar un tema dins d'un fòrum
-- postDeleteTopicR :: TopicId -> HandlerFor ForumsApp Html
-- postDeleteTopicR tid = do
--     user <- requireAuth
--     runDbAction $ deleteTopic (fst user) tid
--     redirect HomeR

-- -- funció per poder eliminar un fòrum
-- postDeleteForumR :: ForumId -> HandlerFor ForumsApp Html
-- postDeleteForumR fid = do
--     user <- requireAuth
--     runDbAction $ deleteForum (fst user) fid
--     redirect HomeR

-- -- funció per poder eliminar un usuari
-- postDeleteUserR :: UserId -> HandlerFor ForumsApp Html
-- postDeleteUserR uid = do
--     user <- requireAuth
--     runDbAction $ deleteUser (fst user) uid
--     redirect HomeR

-- -- funció per poder editar un usuari
-- postEditUserR :: UserId -> HandlerFor ForumsApp Html
-- postEditUserR uid = do
--     user <- requireAuth
--     (uformr, uformw) <- runAFormPost editUserForm
--     case uformr of
--         FormSuccess newuser -> do
--             runDbAction $ editUser (fst user) uid newuser
--             redirect (UserR uid)
--         _ ->
--             defaultLayout

-- -- funció per poder obtenir un usuari
-- getUserR :: UserId -> HandlerFor ForumsApp Html
-- getUserR uid = do
--     user <- runDbAction (getUser uid) >>= maybe notFound pure
--     mbuser <- maybeAuth
--     defaultLayout $ userView mbuser (uid, user)

-- -- funció per poder obtenir tots els usuaris
-- getUsersR :: HandlerFor ForumsApp Html
-- getUsersR = do
--     mbuser <- maybeAuth
--     users <- runDbAction getUsers
--     defaultLayout $ usersView mbuser users

    

