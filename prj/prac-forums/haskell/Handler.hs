
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
    -- NewForum :: Text -> Markdown -> NewForum
    NewForum <$> freq textField (withPlaceholder "Introduïu el títol del fòrum" "Titol") Nothing
             <*> freq markdownField (withPlaceholder "Introduïu la descripció del fòrum" "Descripció") Nothing

newTopicForm :: AForm (HandlerFor ForumsApp) NewTopic
newTopicForm =
    -- NewTopic :: Text -> Markdown -> NewTopic
    NewTopic <$> freq textField (withPlaceholder "Introduïu el títol de la qüestió" "Titol") Nothing
             <*> freq markdownField (withPlaceholder "Introduïu la descripció de la qüestió" "Text") Nothing

newPostForm :: AForm (HandlerFor ForumsApp) NewPost
newPostForm =
    -- NewPost :: Markdown -> NewPost
    NewPost <$> freq markdownField (withPlaceholder "Introduïu el text de la resposta" "Text") Nothing

editForumForm :: ForumD -> AForm (HandlerFor ForumsApp) NewForum
editForumForm forumD =
    -- NewForum :: Text -> Markdown -> NewForum
    NewForum <$> freq textField (withPlaceholder "Introduïu el títol del fòrum" "Titol") (Just (fdTitle forumD))
             <*> freq markdownField (withPlaceholder "Introduïu la descripció del fòrum" "Descripció") (Just (fdDescription forumD))

editTopicForm :: TopicD -> PostD -> AForm (HandlerFor ForumsApp) NewTopic
editTopicForm topicD postD =
    -- NewTopic :: Text -> Markdown -> NewTopic
    NewTopic <$> freq textField (withPlaceholder "Introduïu el títol de la qüestió" "Titol") (Just (tdSubject topicD))
                <*> freq markdownField (withPlaceholder "Introduïu la descripció de la qüestió" "Text") (Just (pdMessage postD))

editTopicForm2 :: TopicD -> AForm (HandlerFor ForumsApp) NewTopic
editTopicForm2 topicD =
    -- NewTopic :: Text -> Markdown -> NewTopic
    NewTopic <$> freq textField (withPlaceholder "Introduïu el títol de la qüestió" "Titol") (Just (tdSubject topicD))
                <*> freq markdownField (withPlaceholder "Introduïu la descripció de la qüestió" "Text") Nothing

editPostForm :: PostD -> AForm (HandlerFor ForumsApp) NewPost
editPostForm postD =
    -- NewPost :: Markdown -> NewPost
    NewPost <$> freq markdownField (withPlaceholder "Introduïu el text de la resposta" "Text") (Just (pdMessage postD))


-- funció que retorna un widget amb el contingut de la pàgina principal (amb els fòrums) sempre i quan l'usuari estigui autenticat
getHomeR :: HandlerFor ForumsApp Html
getHomeR = do
    -- Get authenticated user
    mbuser <- maybeAuth
    -- Get a fresh form
    fformw <- generateAFormPost newForumForm
    -- Return HTML content
    defaultLayout $ homeView mbuser fformw

-- funció per crear un nou fòrum sempre i quan l'usuari estigui autenticat i el formulari sigui vàlid
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

-- funció que retorna un widget amb el contingut d'un fòrum concret (amb els seus temes) sempre i quan l'usuari estigui autenticat
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
    -- forumD :: ForumD
    forumD <- runDbAction (getForum fid) >>= maybe notFound pure
    -- generateAFormPost :: AForm (HandlerFor ForumsApp) a -> HandlerFor ForumsApp (FormResult a, WidgetFor ForumsApp ())
    -- newTopicForm :: AForm (HandlerFor ForumsApp) NewTopic
    -- tformw :: WidgetFor ForumsApp ()
    tformw <- generateAFormPost newTopicForm
    -- generateAFormPost :: AForm (HandlerFor ForumsApp) a -> HandlerFor ForumsApp (FormResult a, WidgetFor ForumsApp ())
    -- editForumForm :: ForumD -> AForm (HandlerFor ForumsApp) NewForum
    -- fformw :: WidgetFor ForumsApp ()
    fformw <- generateAFormPost (editForumForm forumD)
    -- Return HTML content
    -- defaultLayout :: WidgetFor ForumsApp () -> HandlerFor ForumsApp Html
    -- forumView :: Maybe (UserId, UserD) -> (ForumId, ForumD) -> WidgetFor ForumsApp () -> WidgetFor ForumsApp -> WidgetFor ForumsApp
    defaultLayout $ forumView mbuser (fid, forumD) tformw fformw

-- funció que crea un nou tema dins d'un fòrum sempre i quan l'usuari estigui autenticat i el tema no existeixi
postForumR :: ForumId -> HandlerFor ForumsApp Html
postForumR fid = do
    -- Get authenticated user
    -- requireAuth :: HandlerFor ForumsApp (UserId, UserD)
    -- user :: (UserId, UserD)
    user <- requireAuth
    -- runAFormPost :: AForm (HandlerFor ForumsApp) a -> HandlerFor ForumsApp (FormResult a, WidgetFor ForumsApp ())
    -- newTopicForm :: AForm (HandlerFor ForumsApp) NewTopic
    -- (tformr, tformw) :: (FormResult NewTopic, WidgetFor ForumsApp ())
    (tformr, tformw) <- runAFormPost newTopicForm 
    case tformr of
        -- FormSuccess :: a -> FormResult a
        -- newTopic :: NewTopic
        FormSuccess newTopic -> do
            -- runDbAction :: SqlPersistT (HandlerFor ForumsApp) a -> HandlerFor ForumsApp a
            -- addTopic :: ForumId -> UserId -> NewTopic -> SqlPersistT (HandlerFor ForumsApp) TopicId
            runDbAction $ addTopic fid (fst user) newTopic
            -- redirect :: Route ForumsApp -> HandlerFor ForumsApp a
            -- ForumR :: ForumId -> Route ForumsApp
            redirect (ForumR fid)    
        _ -> do
            -- redirect :: Route ForumsApp -> HandlerFor ForumsApp a
            -- ForumR :: ForumId -> Route ForumsApp
            redirect (ForumR fid) 

deleteForumR :: ForumId -> HandlerFor ForumsApp Html
deleteForumR fid = do
    -- Get authenticated user
    -- requireAuth :: HandlerFor ForumsApp (UserId, UserD)
    -- user :: (UserId, UserD)
    user <- requireAuth
    -- runDbAction :: SqlPersistT (HandlerFor ForumsApp) a -> HandlerFor ForumsApp a
    -- deleteForum :: ForumId -> SqlPersistT (HandlerFor ForumsApp) ()
    runDbAction $ deleteForum fid
    -- redirect :: Route ForumsApp -> HandlerFor ForumsApp a
    -- HomeR :: Route ForumsApp
    redirect HomeR

editForumR :: ForumId -> HandlerFor ForumsApp Html
editForumR fid = do
    -- Get authenticated user
    -- requireAuth :: HandlerFor ForumsApp (UserId, UserD)
    -- user :: (UserId, UserD)
    user <- requireAuth
    -- runDbAction :: SqlPersistT (HandlerFor ForumsApp) a -> HandlerFor ForumsApp a
    -- getForum :: ForumId -> SqlPersistT (HandlerFor ForumsApp) (Maybe ForumD)
    -- maybe notFound pure :: Maybe a -> HandlerFor ForumsApp a
    -- forumD :: ForumD
    forumD <- runDbAction (getForum fid) >>= maybe notFound pure
    -- runAFormPost :: AForm (HandlerFor ForumsApp) a -> HandlerFor ForumsApp (FormResult a, WidgetFor ForumsApp ())
    -- editForumForm :: AForm (HandlerFor ForumsApp) EditForum
    -- (fformr, fformw) :: (FormResult EditForum, WidgetFor ForumsApp ())
    (fformr, fformw) <- runAFormPost (editForumForm forumD)
    case fformr of
        -- FormSuccess :: a -> FormResult a
        -- editForum :: EditForum
        FormSuccess newForum -> do
            -- runDbAction :: SqlPersistT (HandlerFor ForumsApp) a -> HandlerFor ForumsApp a
            -- editForum :: ForumId -> Text -> Markdown -> SqlPersistT (HandlerFor ForumsApp) ()
            runDbAction $ editForum fid (nfTitle newForum) (nfDescription newForum)
            -- redirect :: Route ForumsApp -> HandlerFor ForumsApp a
            -- ForumR :: ForumId -> Route ForumsApp
            redirect (ForumR fid)
        _ ->
            -- redirect :: Route ForumsApp -> HandlerFor ForumsApp a
            -- ForumR :: ForumId -> Route ForumsApp
            redirect (ForumR fid)
        

-- funció per poder obetnir el tema dins d'un fòrum sempre i quan l'usuari estigui autenticat
getTopicR :: TopicId -> HandlerFor ForumsApp Html
getTopicR tid = do
    -- Get authenticated user
    -- maybeAuth :: HandlerFor ForumsApp (Maybe (UserId, UserD))
    -- mbuser :: Maybe (UserId, UserD)
    mbuser <- maybeAuth
    -- runDbAction :: SqlPersistT (HandlerFor ForumsApp) a -> HandlerFor ForumsApp a
    -- getTopic :: TopicId -> SqlPersistT (HandlerFor ForumsApp) (Maybe TopicD)
    -- maybe notFound pure :: Maybe a -> HandlerFor ForumsApp a
    -- topicD :: TopicD
    topicD <- runDbAction (getTopic tid) >>= maybe notFound pure
    -- tdForumId :: TopicD -> ForumId
    let fid = tdForumId topicD
    -- runDbAction :: SqlPersistT (HandlerFor ForumsApp) a -> HandlerFor ForumsApp a
    -- getForum :: ForumId -> SqlPersistT (HandlerFor ForumsApp) (Maybe ForumD)
    -- maybe notFound pure :: Maybe a -> HandlerFor ForumsApp a
    -- forumD :: ForumD
    forumD <- runDbAction (getForum fid) >>= maybe notFound pure
    -- generateAFormPost :: AForm (HandlerFor ForumsApp) a -> HandlerFor ForumsApp (FormResult a, WidgetFor ForumsApp ())
    -- newPostForm :: AForm (HandlerFor ForumsApp) NewPost
    pformw <- generateAFormPost newPostForm
    -- runDbAction :: SqlPersistT (HandlerFor ForumsApp) a -> HandlerFor ForumsApp a
    -- getPostList :: TopicId -> SqlPersistT (HandlerFor ForumsApp) [PostD]
    -- posts :: [(PostId, PostD)]
    posts <- runDbAction $ getPostList tid
    case posts of
            [] -> do
                tformw <- generateAFormPost (editTopicForm2 topicD)
                -- defaultLayout :: WidgetFor ForumsApp () -> HandlerFor ForumsApp Html
                -- topicView :: Maybe (UserId, UserD) -> (ForumId, ForumD) -> (TopicId, TopicD) -> WidgetFor ForumsApp () -> WidgetFor ForumsApp -> WidgetFor ForumsApp
                defaultLayout $ topicView mbuser (fid, forumD) (tid, topicD) pformw tformw
            (p:_) -> do
                -- generateAFormPost :: AForm (HandlerFor ForumsApp) a -> HandlerFor ForumsApp (FormResult a, WidgetFor ForumsApp ())
                -- editTopicForm :: AForm (HandlerFor ForumsApp) EditTopic
                -- tformw :: WidgetFor ForumsApp ()
                tformw <- generateAFormPost (editTopicForm topicD $ snd p)
                -- defaultLayout :: WidgetFor ForumsApp () -> HandlerFor ForumsApp Html
                -- topicView :: Maybe (UserId, UserD) -> (ForumId, ForumD) -> (TopicId, TopicD) -> WidgetFor ForumsApp () -> WidgetFor ForumsApp -> WidgetFor ForumsApp
                defaultLayout $ topicView mbuser (fid, forumD) (tid, topicD) pformw tformw

-- funció per crear un nou post dins d'un tema sempre i quan l'usuari estigui autenticat i el formulari sigui vàlid
postTopicR :: TopicId -> HandlerFor ForumsApp Html
postTopicR tid = do
    -- Get authenticated user
    -- requireAuth :: HandlerFor ForumsApp (UserId, UserD)
    -- user :: (UserId, UserD)
    user <- requireAuth
    -- runDbAction :: SqlPersistT (HandlerFor ForumsApp) a -> HandlerFor ForumsApp a
    -- getTopic :: TopicId -> SqlPersistT (HandlerFor ForumsApp) (Maybe TopicD)
    -- maybe notFound pure :: Maybe a -> HandlerFor ForumsApp a
    -- topicD :: TopicD
    topicD <- runDbAction (getTopic tid) >>= maybe notFound pure
    -- tdForumId :: TopicD -> ForumId
    let fid = tdForumId topicD
    -- runAFormPost :: AForm (HandlerFor ForumsApp) a -> HandlerFor ForumsApp (FormResult a, WidgetFor ForumsApp ())
    -- newPostForm :: AForm (HandlerFor ForumsApp) NewPost
    -- (pformr, pformw) :: (FormResult NewPost, WidgetFor ForumsApp ())
    (pformr, pformw) <- runAFormPost newPostForm
    case pformr of
        -- FormSuccess :: a -> FormResult a
        -- newPost :: NewPost
        FormSuccess newPost -> do
            -- runDbAction :: SqlPersistT (HandlerFor ForumsApp) a -> HandlerFor ForumsApp a
            -- addReply :: ForumId -> TopicId -> UserId -> Markdown -> SqlPersistT (HandlerFor ForumsApp) PostId
            runDbAction $ addReply fid tid (fst user) (npMessage newPost)
            -- redirect :: Route ForumsApp -> HandlerFor ForumsApp a
            -- TopicR :: TopicId -> Route ForumsApp
            redirect (TopicR tid)
        _ -> do
            -- redirect :: Route ForumsApp -> HandlerFor ForumsApp a
            -- TopicR :: TopicId -> Route ForumsApp
            redirect (TopicR tid)

deleteTopicR :: TopicId -> HandlerFor ForumsApp Html
deleteTopicR tid = do
    -- Get authenticated user
    -- requireAuth :: HandlerFor ForumsApp (UserId, UserD)
    -- user :: (UserId, UserD)
    user <- requireAuth
     -- runDbAction :: SqlPersistT (HandlerFor ForumsApp) a -> HandlerFor ForumsApp a
    -- getTopic :: TopicId -> SqlPersistT (HandlerFor ForumsApp) (Maybe TopicD)
    -- maybe notFound pure :: Maybe a -> HandlerFor ForumsApp a
    -- topicD :: TopicD
    topicD <- runDbAction (getTopic tid) >>= maybe notFound pure
    -- tdForumId :: TopicD -> ForumId
    let fid = tdForumId topicD
    -- runDbAction :: SqlPersistT (HandlerFor ForumsApp) a -> HandlerFor ForumsApp a
    -- deleteTopic :: ForumId -> TopicId -> SqlPersistT (HandlerFor ForumsApp) ()
    runDbAction $ deleteTopic fid tid
    -- redirect :: Route ForumsApp -> HandlerFor ForumsApp a
    -- ForumR :: ForumId -> Route ForumsApp
    redirect (ForumR fid)

editTopicR :: TopicId -> HandlerFor ForumsApp Html
editTopicR tid = do
    -- Get authenticated user
    -- requireAuth :: HandlerFor ForumsApp (UserId, UserD)
    -- user :: (UserId, UserD)
    user <- requireAuth
    -- runDbAction :: SqlPersistT (HandlerFor ForumsApp) a -> HandlerFor ForumsApp a
    -- getTopic :: TopicId -> SqlPersistT (HandlerFor ForumsApp) (Maybe TopicD)
    -- maybe notFound pure :: Maybe a -> HandlerFor ForumsApp a
    -- topicD :: TopicD
    topicD <- runDbAction (getTopic tid) >>= maybe notFound pure
    -- runDbAction :: SqlPersistT (HandlerFor ForumsApp) a -> HandlerFor ForumsApp a
    -- getPostList :: TopicId -> SqlPersistT (HandlerFor ForumsApp) [PostD]
    -- posts :: [(PostId, PostD)]
    posts <- runDbAction $ getPostList tid
    let firstPostD = case posts of
                  (p:_) -> snd p -- Extract the second element (PostD) from the tuple
    -- runAFormPost :: AForm (HandlerFor ForumsApp) a -> HandlerFor ForumsApp (FormResult a, WidgetFor ForumsApp ())
    -- editTopicForm :: AForm (HandlerFor ForumsApp) EditTopic
    -- (tformr, tformw) :: (FormResult EditTopic, WidgetFor ForumsApp ())
    (tformr, tformw) <- runAFormPost (editTopicForm topicD firstPostD)
    case tformr of
        -- FormSuccess :: a -> FormResult a
        -- editTopic :: EditTopic
        FormSuccess newTopic -> do
            -- runDbAction :: SqlPersistT (HandlerFor ForumsApp) a -> HandlerFor ForumsApp a
            -- editTopic :: TopicId -> Text -> SqlPersistT (HandlerFor ForumsApp) ()
            runDbAction $ editTopic tid (ntSubject newTopic) (ntMessage newTopic)
            -- redirect :: Route ForumsApp -> HandlerFor ForumsApp a
            -- TopicR :: TopicId -> Route ForumsApp
            redirect (TopicR tid)
        _ -> do
            -- redirect :: Route ForumsApp -> HandlerFor ForumsApp a
            -- TopicR :: TopicId -> Route ForumsApp
            redirect (TopicR tid)

getPostR :: PostId -> HandlerFor ForumsApp Html
getPostR pid = do
    -- Get authenticated user
    -- maybeAuth :: HandlerFor ForumsApp (Maybe (UserId, UserD))
    -- mbuser :: Maybe (UserId, UserD)
    mbuser <- maybeAuth
    -- runDbAction :: SqlPersistT (HandlerFor ForumsApp) a -> HandlerFor ForumsApp a
    -- getPost :: PostId -> SqlPersistT (HandlerFor ForumsApp) (Maybe PostD)
    -- maybe notFound pure :: Maybe a -> HandlerFor ForumsApp a
    -- postD :: PostD
    postD <- runDbAction (getPost pid) >>= maybe notFound pure
    -- tdTopicId :: PostD -> TopicId
    let tid = pdTopicId postD
    -- runDbAction :: SqlPersistT (HandlerFor ForumsApp) a -> HandlerFor ForumsApp a
    -- getTopic :: TopicId -> SqlPersistT (HandlerFor ForumsApp) (Maybe TopicD)
    -- maybe notFound pure :: Maybe a -> HandlerFor ForumsApp a
    -- topicD :: TopicD
    topicD <- runDbAction (getTopic tid) >>= maybe notFound pure
    -- tdForumId :: TopicD -> ForumId
    let fid = tdForumId topicD
    -- runDbAction :: SqlPersistT (HandlerFor ForumsApp) a -> HandlerFor ForumsApp a
    -- getForum :: ForumId -> SqlPersistT (HandlerFor ForumsApp) (Maybe ForumD)
    -- maybe notFound pure :: Maybe a -> HandlerFor ForumsApp a
    -- forumD :: ForumD
    forumD <- runDbAction (getForum fid) >>= maybe notFound pure
    -- generateAFormPost :: AForm (HandlerFor ForumsApp) a -> HandlerFor ForumsApp (FormResult a, WidgetFor ForumsApp ())
    -- editPostForm :: AForm (HandlerFor ForumsApp) EditPost
    -- pformw :: WidgetFor ForumsApp ()
    pformw <- generateAFormPost (editPostForm postD)
    -- defaultLayout :: WidgetFor ForumsApp () -> HandlerFor ForumsApp Html
    -- postView :: Maybe (UserId, UserD) -> (ForumId, ForumD) -> (TopicId, TopicD) -> (PostId, PostD) -> WidgetFor ForumsApp
    defaultLayout $ postView mbuser (fid, forumD) (tid, topicD) (pid, postD) pformw

deletePostR :: PostId -> HandlerFor ForumsApp Html
deletePostR pid = do
    -- Get authenticated user
    -- requireAuth :: HandlerFor ForumsApp (UserId, UserD)
    -- user :: (UserId, UserD)
    user <- requireAuth
    -- runDbAction :: SqlPersistT (HandlerFor ForumsApp) a -> HandlerFor ForumsApp a
    -- getPost :: PostId -> SqlPersistT (HandlerFor ForumsApp) (Maybe PostD)
    -- maybe notFound pure :: Maybe a -> HandlerFor ForumsApp a
    -- postD :: PostD
    postD <- runDbAction (getPost pid) >>= maybe notFound pure
    -- pdTopicId :: PostD -> TopicId
    let tid = pdTopicId postD
    -- runDbAction :: SqlPersistT (HandlerFor ForumsApp) a -> HandlerFor ForumsApp a
    -- getTopic :: TopicId -> SqlPersistT (HandlerFor ForumsApp) (Maybe TopicD)
    -- maybe notFound pure :: Maybe a -> HandlerFor ForumsApp a
    -- topicD :: TopicD
    topicD <- runDbAction (getTopic tid) >>= maybe notFound pure
    -- tdForumId :: TopicD -> ForumId
    let fid = tdForumId topicD
    -- runDbAction :: SqlPersistT (HandlerFor ForumsApp) a -> HandlerFor ForumsApp a
    -- deletePost :: ForumId -> TopicId -> PostId -> SqlPersistT (HandlerFor ForumsApp) ()
    runDbAction $ deletePost fid tid pid
    -- redirect :: Route ForumsApp -> HandlerFor ForumsApp a
    -- TopicR :: TopicId -> Route ForumsApp
    redirect (TopicR tid)