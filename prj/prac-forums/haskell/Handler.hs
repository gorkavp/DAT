
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

-- Formulari per a crear un nou fòrum
newForumForm :: AForm (HandlerFor ForumsApp) NewForum
newForumForm =
    -- NewForum :: Text -> Markdown -> NewForum
    NewForum <$> freq textField (withPlaceholder "Introduïu el títol del fòrum" "Titol") Nothing
             <*> freq markdownField (withPlaceholder "Introduïu la descripció del fòrum" "Descripció") Nothing

-- Formulari per a crear una nova qüestió
newTopicForm :: AForm (HandlerFor ForumsApp) NewTopic
newTopicForm =
    -- NewTopic :: Text -> Markdown -> NewTopic
    NewTopic <$> freq textField (withPlaceholder "Introduïu el títol de la qüestió" "Titol") Nothing
             <*> freq markdownField (withPlaceholder "Introduïu la descripció de la qüestió" "Text") Nothing

-- Formulari per a crear una nova resposta
newPostForm :: AForm (HandlerFor ForumsApp) NewPost
newPostForm =
    -- NewPost :: Markdown -> NewPost
    NewPost <$> freq markdownField (withPlaceholder "Introduïu el text de la resposta" "Text") Nothing

-- Formulari per canviar el nom d'usuari i la contrasenya
newUserForm :: UserD -> AForm (HandlerFor ForumsApp) NewUser
newUserForm userD =
    -- NewUser :: Text -> Text -> Text -> NewUser
    NewUser <$> freq textField (withPlaceholder "Introduïu el nom d'usuari" "Nom d'usuari") (Just (udName userD))
            <*> freq passwordField (withPlaceholder "Introduïu la contrasenya" "Contrasenya") Nothing
            <*> freq passwordField (withPlaceholder "Introduïu la contrasenya (repetició)" "Confirmar contrasenya") Nothing

-- Formulari per canviar el títol i la descripció d'un fòrum
editForumForm :: ForumD -> AForm (HandlerFor ForumsApp) NewForum
editForumForm forumD =
    -- NewForum :: Text -> Markdown -> NewForum
    NewForum <$> freq textField (withPlaceholder "Introduïu el títol del fòrum" "Titol") (Just (fdTitle forumD))
             <*> freq markdownField (withPlaceholder "Introduïu la descripció del fòrum" "Descripció") (Just (fdDescription forumD))

-- Formulari per canviar el títol i la descripció d'una qüestió (primer post)
editTopicForm :: TopicD -> PostD -> AForm (HandlerFor ForumsApp) NewTopic
editTopicForm topicD postD =
    -- NewTopic :: Text -> Markdown -> NewTopic
    NewTopic <$> freq textField (withPlaceholder "Introduïu el títol de la qüestió" "Titol") (Just (tdSubject topicD))
                <*> freq markdownField (withPlaceholder "Introduïu la descripció de la qüestió" "Text") (Just (pdMessage postD))

-- Formulari per canviar el títol i afegir una descripció a una qüestió (primer post)
editTopicForm2 :: TopicD -> AForm (HandlerFor ForumsApp) NewTopic
editTopicForm2 topicD =
    -- NewTopic :: Text -> Markdown -> NewTopic
    NewTopic <$> freq textField (withPlaceholder "Introduïu el títol de la qüestió" "Titol") (Just (tdSubject topicD))
                <*> freq markdownField (withPlaceholder "Introduïu la descripció de la qüestió" "Text") Nothing

-- Formulari per canviar el text d'una resposta (post)
editPostForm :: PostD -> AForm (HandlerFor ForumsApp) NewPost
editPostForm postD =
    -- NewPost :: Markdown -> NewPost
    NewPost <$> freq markdownField (withPlaceholder "Introduïu el text de la resposta" "Text") (Just (pdMessage postD))


-- funció que gestiona els HTTP GET de la pàgina principal (home) de l'aplicació
getHomeR :: HandlerFor ForumsApp Html
getHomeR = do
    -- Get authenticated user
    mbuser <- maybeAuth
    -- Get a fresh form
    fformw <- generateAFormPost newForumForm
    -- Return HTML content
    defaultLayout $ homeView mbuser fformw

-- funció que gestiona els HTTP POST de la pàgina principal (home) de l'aplicació per a crear un nou fòrum
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

-- funció que gestiona els HTTP GET de la pàgina d'un fòrum
getForumR :: ForumId -> HandlerFor ForumsApp Html
getForumR fid = do
    -- Get authenticated user
    -- maybeAuth :: HandlerFor ForumsApp (Maybe (UserId, UserD))
    -- mbuser :: Maybe (UserId, UserD)
    mbuser <- maybeAuth
    -- Get requested forum from data-base.
    -- Short-circuit (responds immediately) with a 'Not found' status if forum don't exist
    -- runDbAction :: DbM a -> HandlerFor ForumsApp a
    -- getForum fid :: DbM (Maybe ForumD)
    -- forumD :: ForumD
    forumD <- runDbAction (getForum fid) >>= maybe notFound pure -- obtenim la dada forumD fent una comana a la base de dades
    -- generateAFormPost NewTopicForm :: HandlerFor ForumsApp (FormResult NewTopic, WidgetFor ForumsApp ())
    -- tformw :: WidgetFor ForumsApp ()
    tformw <- generateAFormPost newTopicForm -- generem el formulari per crear una nova qüestió (topic)
    -- generateAFormPost (editForumForm forumD) :: HandlerFor ForumsApp (FormResult NewForum, WidgetFor ForumsApp ())
    -- fformw :: WidgetFor ForumsApp ()
    fformw <- generateAFormPost (editForumForm forumD) -- generem el formulari per editar el fòrum
    -- Return HTML content
    -- defaultLayout :: WidgetFor ForumsApp () -> HandlerFor ForumsApp Html
    -- forumView :: Maybe (UserId, UserD) -> (ForumId, ForumD) -> WidgetFor ForumsApp () -> WidgetFor ForumsApp -> WidgetFor ForumsApp
    defaultLayout $ forumView mbuser (fid, forumD) tformw fformw -- generem la vista de la pàgina del fòrum

-- funció que gestiona els HTTP POST de la pàgina d'un fòrum per a crear una nova qüestió (topic)
postForumR :: ForumId -> HandlerFor ForumsApp Html
postForumR fid = do
    -- Get authenticated user
    -- requireAuth :: HandlerFor ForumsApp (UserId, UserD)
    -- user :: (UserId, UserD)
    user <- requireAuth -- obtenim l'usuari autenticat
    -- runAFormPost :: AForm (HandlerFor ForumsApp) a -> HandlerFor ForumsApp (FormResult a, WidgetFor ForumsApp ())
    -- newTopicForm :: AForm (HandlerFor ForumsApp) NewTopic
    -- (tformr, tformw) :: (FormResult NewTopic, WidgetFor ForumsApp ())
    (tformr, tformw) <- runAFormPost newTopicForm -- obtenim el formulari per crear una nova qüestió (topic)
    case tformr of -- analitzem el resultat del formulari
        -- FormSuccess :: a -> FormResult a
        -- newTopic :: NewTopic
        FormSuccess newTopic -> do -- si el formulari és correcte
            -- runDbAction :: DbM a -> HandlerFor ForumsApp a
            -- addTopic :: ForumId -> UserId -> NewTopic -> DbM (TopicId, TopicD)
            runDbAction $ addTopic fid (fst user) newTopic -- afegim la nova qüestió (topic) a la base de dades
            -- redirect :: Route ForumsApp -> HandlerFor ForumsApp a
            -- ForumR :: ForumId -> Route ForumsApp
            redirect (ForumR fid) -- redirigim a la pàgina del fòrum    
        _ -> do -- si el formulari és incorrecte
            -- runDbAction :: DbM a -> HandlerFor ForumsApp a
            -- getForum fid :: DbM (Maybe ForumD)
            -- forumD :: ForumD
            forumD <- runDbAction (getForum fid) >>= maybe notFound pure -- obtenim la dada forumD fent una comana a la base de dades
            -- generateAFormPost (editForumForm forumD) :: HandlerFor ForumsApp (FormResult NewForum, WidgetFor ForumsApp ())
            -- fformw :: WidgetFor ForumsApp ()
            fformw <- generateAFormPost (editForumForm forumD) -- generem el formulari per editar el fòrum
            -- foruView :: Maybe (UserId, UserD) -> (ForumId, ForumD) -> WidgetFor ForumsApp () -> WidgetFor ForumsApp -> WidgetFor ForumsApp
            defaultLayout $ forumView (Just user) (fid, forumD) tformw fformw -- generem la vista de la pàgina del fòrum

-- funció que gestiona els HTTP POST de la pàgina d'un fòrum per a eliminar un fòrum
deleteForumR :: ForumId -> HandlerFor ForumsApp Html
deleteForumR fid = do
    -- requireAuth :: HandlerFor ForumsApp (UserId, UserD)
    -- user :: (UserId, UserD)
    user <- requireAuth -- obtenim l'usuari autenticat
    -- runDbAction :: DbM a -> HandlerFor ForumsApp a
    -- deleteForum :: ForumId -> DbM ()
    runDbAction $ deleteForum fid -- eliminem el fòrum de la base de dades
    -- redirect :: Route ForumsApp -> HandlerFor ForumsApp a
    -- HomeR :: Route ForumsApp
    redirect HomeR -- redirigim a la pàgina principal (home)

-- funció que gestiona els HTTP GET de la pàgina d'un fòrum per a editar un fòrum
editForumR :: ForumId -> HandlerFor ForumsApp Html
editForumR fid = do
    -- requireAuth :: HandlerFor ForumsApp (UserId, UserD)
    -- user :: (UserId, UserD)
    user <- requireAuth -- obtenim l'usuari autenticat
    -- runDbAction :: DbM a -> HandlerFor ForumsApp a
    -- getForum :: ForumId -> DbM (Maybe ForumD)
    -- forumD :: ForumD
    forumD <- runDbAction (getForum fid) >>= maybe notFound pure -- obtenim la dada forumD fent una comana a la base de dades
    -- runAFormPost :: AForm (HandlerFor ForumsApp) a -> HandlerFor ForumsApp (FormResult a, WidgetFor ForumsApp ())
    -- editForumForm :: AForm (HandlerFor ForumsApp) EditForum
    -- (fformr, fformw) :: (FormResult EditForum, WidgetFor ForumsApp ())
    (fformr, fformw) <- runAFormPost (editForumForm forumD) -- obtenim el formulari per editar el fòrum
    case fformr of -- analitzem el resultat del formulari
        -- FormSuccess :: a -> FormResult a
        -- editForum :: EditForum
        FormSuccess newForum -> do -- si el formulari és correcte
            -- runDbAction :: DbM a -> HandlerFor ForumsApp a
            -- editForum :: ForumId -> Text -> Markdown -> DbM ()
            runDbAction $ editForum fid (nfTitle newForum) (nfDescription newForum) -- editem el fòrum a la base de dades
            -- redirect :: Route ForumsApp -> HandlerFor ForumsApp a
            -- ForumR :: ForumId -> Route ForumsApp
            redirect (ForumR fid) -- redirigim a la pàgina del fòrum
        _ -> do -- si el formulari és incorrecte
            -- generateAFormPost NewTopicForm :: HandlerFor ForumsApp (FormResult NewTopic, WidgetFor ForumsApp ())
            -- tformw :: WidgetFor ForumsApp ()
            tformw <- generateAFormPost newTopicForm -- generem el formulari per crear una nova qüestió (topic)
            -- foruView :: Maybe (UserId, UserD) -> (ForumId, ForumD) -> WidgetFor ForumsApp () -> WidgetFor ForumsApp -> WidgetFor ForumsApp
            defaultLayout $ forumView (Just user) (fid, forumD) tformw fformw -- generem la vista de la pàgina del fòrum
        

-- funció que gestiona els HTTP GET de la pagina d'una qüestió (topic) d'un fòrum 
getTopicR :: TopicId -> HandlerFor ForumsApp Html
getTopicR tid = do
    -- maybeAuth :: HandlerFor ForumsApp (Maybe (UserId, UserD))
    -- mbuser :: Maybe (UserId, UserD)
    mbuser <- maybeAuth
    -- runDbAction :: DbM a -> HandlerFor ForumsApp a
    -- getTopic :: TopicId -> SqlPersistT (HandlerFor ForumsApp) (Maybe TopicD)
    -- topicD :: TopicD
    topicD <- runDbAction (getTopic tid) >>= maybe notFound pure -- obtenim la dada topicD fent una comana a la base de dades
    -- tdForumId :: TopicD -> ForumId
    let fid = tdForumId topicD -- obtenim l'identificador del fòrum
    -- runDbAction :: DbM a -> HandlerFor ForumsApp a
    -- getForum :: ForumId -> DbM (Maybe ForumD)
    -- forumD :: ForumD
    forumD <- runDbAction (getForum fid) >>= maybe notFound pure -- obtenim la dada forumD fent una comana a la base de dades
    -- generateAFormPost :: AForm (HandlerFor ForumsApp) a -> HandlerFor ForumsApp (FormResult a, WidgetFor ForumsApp ())
    -- newPostForm :: AForm (HandlerFor ForumsApp) NewPost
    pformw <- generateAFormPost newPostForm -- generem el formulari per crear una nova resposta (post)
    -- runDbAction :: DbM a -> HandlerFor ForumsApp a
    -- getPostList :: TopicId -> DbM [(PostId, PostD)]
    -- posts :: [(PostId, PostD)]
    posts <- runDbAction $ getPostList tid -- obtenim la llista de respostes (posts) fent una comana a la base de dades
    case posts of -- analitzem la llista de respostes (posts)
            [] -> do -- si la llista de respostes (posts) és buida
                -- generateAFormPost :: AForm (HandlerFor ForumsApp) a -> HandlerFor ForumsApp (FormResult a, WidgetFor ForumsApp ())
                -- editTopicForm2 :: TopicD -> AForm (HandlerFor ForumsApp) EditTopic
                -- tformw :: WidgetFor ForumsApp ()
                tformw <- generateAFormPost (editTopicForm2 topicD) -- generem el formulari per editar la qüestió (topic) amb la descripció buida
                -- defaultLayout :: WidgetFor ForumsApp () -> HandlerFor ForumsApp Html
                -- topicView :: Maybe (UserId, UserD) -> (ForumId, ForumD) -> (TopicId, TopicD) -> WidgetFor ForumsApp () -> WidgetFor ForumsApp -> WidgetFor ForumsApp
                defaultLayout $ topicView mbuser (fid, forumD) (tid, topicD) pformw tformw -- generem la vista de la pàgina de la qüestió (topic)
            (p:_) -> do -- si la llista de respostes (posts) no és buida
                -- generateAFormPost :: AForm (HandlerFor ForumsApp) a -> HandlerFor ForumsApp (FormResult a, WidgetFor ForumsApp ())
                -- editTopicForm :: AForm (HandlerFor ForumsApp) EditTopic
                -- tformw :: WidgetFor ForumsApp ()
                tformw <- generateAFormPost (editTopicForm topicD $ snd p) -- generem el formulari per editar la qüestió (topic) amb la descripció de la primera resposta (post)
                -- defaultLayout :: WidgetFor ForumsApp () -> HandlerFor ForumsApp Html
                -- topicView :: Maybe (UserId, UserD) -> (ForumId, ForumD) -> (TopicId, TopicD) -> WidgetFor ForumsApp () -> WidgetFor ForumsApp -> WidgetFor ForumsApp
                defaultLayout $ topicView mbuser (fid, forumD) (tid, topicD) pformw tformw -- generem la vista de la pàgina de la qüestió (topic)

-- funció per gestionar els HTTP POST de la pàgina d'una qüestió (topic) d'un fòrum per crear una nova resposta (post)
postTopicR :: TopicId -> HandlerFor ForumsApp Html
postTopicR tid = do
    -- requireAuth :: HandlerFor ForumsApp (UserId, UserD)
    -- user :: (UserId, UserD)
    user <- requireAuth -- obtenim l'usuari autenticat
    -- runDbAction :: DbM a -> HandlerFor ForumsApp a
    -- getTopic :: TopicId -> DbM (Maybe TopicD)
    -- maybe notFound pure :: Maybe a -> HandlerFor ForumsApp a
    -- topicD :: TopicD
    topicD <- runDbAction (getTopic tid) >>= maybe notFound pure -- obtenim la dada topicD fent una comana a la base de dades
    -- tdForumId :: TopicD -> ForumId
    let fid = tdForumId topicD -- obtenim l'identificador del fòrum
    -- runAFormPost :: AForm (HandlerFor ForumsApp) a -> HandlerFor ForumsApp (FormResult a, WidgetFor ForumsApp ())
    -- newPostForm :: AForm (HandlerFor ForumsApp) NewPost
    -- (pformr, pformw) :: (FormResult NewPost, WidgetFor ForumsApp ())
    (pformr, pformw) <- runAFormPost newPostForm -- obtenim el formulari per crear una nova resposta (post)
    case pformr of -- analitzem el resultat del formulari per crear una nova resposta (post)
        -- FormSuccess :: a -> FormResult a
        -- newPost :: NewPost
        FormSuccess newPost -> do -- si el resultat del formulari per crear una nova resposta (post) és correcte
            -- runDbAction :: DbM a -> HandlerFor ForumsApp a
            -- addReply :: ForumId -> TopicId -> UserId -> Markdown -> SqlPersistT (HandlerFor ForumsApp) PostId
            runDbAction $ addReply fid tid (fst user) (npMessage newPost) -- afegim la nova resposta (post) a la base de dades
            -- redirect :: Route ForumsApp -> HandlerFor ForumsApp a
            -- TopicR :: TopicId -> Route ForumsApp
            redirect (TopicR tid) -- redirigim a la pàgina de la qüestió (topic)
        _ -> do
            -- redirect :: Route ForumsApp -> HandlerFor ForumsApp a
            -- TopicR :: TopicId -> Route ForumsApp
            redirect (TopicR tid) -- redirigim a la pàgina de la qüestió (topic)

-- funció per gestionar els HTTP POST de la pàgina d'una qüestió (topic) d'un fòrum per eliminar la qüestió (topic)
deleteTopicR :: TopicId -> HandlerFor ForumsApp Html
deleteTopicR tid = do
    -- requireAuth :: HandlerFor ForumsApp (UserId, UserD)
    -- user :: (UserId, UserD)
    user <- requireAuth -- obtenim l'usuari autenticat
    -- runDbAction :: DbM a -> HandlerFor ForumsApp a
    -- getTopic :: TopicId -> DbM (Maybe TopicD)
    -- maybe notFound pure :: Maybe a -> HandlerFor ForumsApp a
    -- topicD :: TopicD
    topicD <- runDbAction (getTopic tid) >>= maybe notFound pure -- obtenim la dada topicD fent una comana a la base de dades
    -- tdForumId :: TopicD -> ForumId
    let fid = tdForumId topicD -- obtenim l'identificador del fòrum
    -- runDbAction :: DbM a -> HandlerFor ForumsApp a
    -- deleteTopic :: ForumId -> TopicId -> DbM ()
    runDbAction $ deleteTopic fid tid -- eliminem la qüestió (topic) de la base de dades
    -- redirect :: Route ForumsApp -> HandlerFor ForumsApp a
    -- ForumR :: ForumId -> Route ForumsApp
    redirect (ForumR fid) -- redirigim a la pàgina del fòrum

-- funció per gestionar els HTTP POST de la pàgina d'una qüestió (topic) d'un fòrum per editar la qüestió (topic)
editTopicR :: TopicId -> HandlerFor ForumsApp Html
editTopicR tid = do
    -- requireAuth :: HandlerFor ForumsApp (UserId, UserD)
    -- user :: (UserId, UserD)
    user <- requireAuth -- obtenim l'usuari autenticat
    -- runDbAction :: DbM a -> HandlerFor ForumsApp a
    -- getTopic :: TopicId -> DbM (Maybe TopicD)
    -- topicD :: TopicD
    topicD <- runDbAction (getTopic tid) >>= maybe notFound pure -- obtenim la dada topicD fent una comana a la base de dades
    -- runDbAction :: DbM a -> HandlerFor ForumsApp a
    -- getPostList :: TopicId -> DbM [(PostId, PostD)]
    -- posts :: [(PostId, PostD)]
    posts <- runDbAction $ getPostList tid -- obtenim la llista de respostes (posts) de la qüestió (topic)
    case posts of -- analitzem la llista de respostes (posts) de la qüestió (topic)
        [] -> do -- si la llista de respostes (posts) de la qüestió (topic) és buida
            -- runAFormPost :: AForm (HandlerFor ForumsApp) a -> HandlerFor ForumsApp (FormResult a, WidgetFor ForumsApp ())
            -- editTopicForm2 :: AForm (HandlerFor ForumsApp) EditTopic
            -- (tformr, tformw) :: (FormResult EditTopic, WidgetFor ForumsApp ())
            (tformr, tformw) <- runAFormPost (editTopicForm2 topicD) -- obtenim el formulari per editar la qüestió (topic)
            case tformr of -- analitzem el resultat del formulari per editar la qüestió (topic)
                -- FormSuccess :: a -> FormResult a
                -- editTopic :: EditTopic
                FormSuccess newTopic -> do -- si el resultat del formulari per editar la qüestió (topic) és correcte
                    -- runDbAction :: DbM a -> HandlerFor ForumsApp a
                    -- addReply :: ForumId -> TopicId -> UserId -> Markdown -> DbM PostId
                    runDbAction $ addReply (tdForumId topicD) tid (fst user) (ntMessage newTopic) -- afegim la nova resposta (post) a la base de dades
                    -- redirect :: Route ForumsApp -> HandlerFor ForumsApp a
                    -- TopicR :: TopicId -> Route ForumsApp
                    redirect (TopicR tid) -- redirigim a la pàgina de la qüestió (topic)
                _ -> do
                    -- runDbAction :: DbM a -> HandlerFor ForumsApp a
                    -- getTopic :: TopicId -> SqlPersistT (HandlerFor ForumsApp) (Maybe TopicD)
                    -- topicD :: TopicD
                    topicD <- runDbAction (getTopic tid) >>= maybe notFound pure -- obtenim la dada topicD fent una comana a la base de dades
                    -- tdForumId :: TopicD -> ForumId
                    let fid = tdForumId topicD -- obtenim l'identificador del fòrum
                    -- runDbAction :: DbM a -> HandlerFor ForumsApp a
                    -- getForum :: ForumId -> DbM (Maybe ForumD)
                    -- forumD :: ForumD
                    forumD <- runDbAction (getForum fid) >>= maybe notFound pure -- obtenim la dada forumD fent una comana a la base de dades
                    -- generateAFormPost :: AForm (HandlerFor ForumsApp) a -> HandlerFor ForumsApp (FormResult a, WidgetFor ForumsApp ())
                    -- newPostForm :: AForm (HandlerFor ForumsApp) NewPost
                    pformw <- generateAFormPost newPostForm -- generem el formulari per crear una nova resposta (post)
                    -- defaultLayout :: WidgetFor ForumsApp () -> HandlerFor ForumsApp Html
                    -- topicView :: Maybe (UserId, UserD) -> (ForumId, ForumD) -> (TopicId, TopicD) -> WidgetFor ForumsApp () -> WidgetFor ForumsApp -> WidgetFor ForumsApp
                    defaultLayout $ topicView (Just user) (fid, forumD) (tid, topicD) pformw tformw -- generem la vista de la pàgina de la qüestió (topic)
        (p:_) -> do
            -- runAFormPost :: AForm (HandlerFor ForumsApp) a -> HandlerFor ForumsApp (FormResult a, WidgetFor ForumsApp ())
            -- editTopicForm :: AForm (HandlerFor ForumsApp) EditTopic
            -- (tformr, tformw) :: (FormResult EditTopic, WidgetFor ForumsApp ())
            (tformr, tformw) <- runAFormPost (editTopicForm topicD $ snd p) -- obtenim el formulari per editar la qüestió (topic)
            case tformr of -- analitzem el resultat del formulari per editar la qüestió (topic)
                -- FormSuccess :: a -> FormResult a
                -- editTopic :: EditTopic
                FormSuccess newTopic -> do -- si el resultat del formulari per editar la qüestió (topic) és correcte
                    -- runDbAction :: DbM a -> HandlerFor ForumsApp a
                    -- editTopic :: TopicId -> Text -> DbM ()
                    runDbAction $ editTopic tid (ntSubject newTopic) (ntMessage newTopic) -- editem la qüestió (topic) a la base de dades
                    -- redirect :: Route ForumsApp -> HandlerFor ForumsApp a
                    -- TopicR :: TopicId -> Route ForumsApp
                    redirect (TopicR tid) -- redirigim a la pàgina de la qüestió (topic)
                _ -> do
                    -- runDbAction :: DbM a -> HandlerFor ForumsApp a
                    -- getTopic :: TopicId -> SqlPersistT (HandlerFor ForumsApp) (Maybe TopicD)
                    -- topicD :: TopicD
                    topicD <- runDbAction (getTopic tid) >>= maybe notFound pure -- obtenim la dada topicD fent una comana a la base de dades
                    -- tdForumId :: TopicD -> ForumId
                    let fid = tdForumId topicD -- obtenim l'identificador del fòrum
                    -- runDbAction :: DbM a -> HandlerFor ForumsApp a
                    -- getForum :: ForumId -> DbM (Maybe ForumD)
                    -- forumD :: ForumD
                    forumD <- runDbAction (getForum fid) >>= maybe notFound pure -- obtenim la dada forumD fent una comana a la base de dades
                    -- generateAFormPost :: AForm (HandlerFor ForumsApp) a -> HandlerFor ForumsApp (FormResult a, WidgetFor ForumsApp ())
                    -- newPostForm :: AForm (HandlerFor ForumsApp) NewPost
                    pformw <- generateAFormPost newPostForm -- generem el formulari per crear una nova resposta (post)
                    -- defaultLayout :: WidgetFor ForumsApp () -> HandlerFor ForumsApp Html
                    -- topicView :: Maybe (UserId, UserD) -> (ForumId, ForumD) -> (TopicId, TopicD) -> WidgetFor ForumsApp () -> WidgetFor ForumsApp -> WidgetFor ForumsApp
                    defaultLayout $ topicView (Just user) (fid, forumD) (tid, topicD) pformw tformw -- generem la vista de la pàgina de la qüestió (topic)

-- funció per gestionar els HTTP GET de la pàgina d'una resposta (post) d'una qüestió (topic) d'un fòrum
getPostR :: PostId -> HandlerFor ForumsApp Html
getPostR pid = do
    -- maybeAuth :: HandlerFor ForumsApp (Maybe (UserId, UserD))
    -- mbuser :: Maybe (UserId, UserD)
    mbuser <- maybeAuth
    -- runDbAction :: DbM a -> HandlerFor ForumsApp a
    -- getPost :: PostId -> DbM (Maybe PostD)
    -- postD :: PostD
    postD <- runDbAction (getPost pid) >>= maybe notFound pure -- obtenim la dada postD fent una comana a la base de dades
    -- tdTopicId :: PostD -> TopicId
    let tid = pdTopicId postD -- obtenim l'identificador de la qüestió (topic)
    -- runDbAction :: DbM a -> HandlerFor ForumsApp a
    -- getTopic :: TopicId -> DbM (Maybe TopicD)
    -- topicD :: TopicD
    topicD <- runDbAction (getTopic tid) >>= maybe notFound pure -- obtenim la dada topicD fent una comana a la base de dades
    -- tdForumId :: TopicD -> ForumId
    let fid = tdForumId topicD -- obtenim l'identificador del fòrum
    -- runDbAction :: DbM a -> HandlerFor ForumsApp a
    -- getForum :: ForumId -> DbM (Maybe ForumD)
    -- maybe notFound pure :: Maybe a -> HandlerFor ForumsApp a
    -- forumD :: ForumD
    forumD <- runDbAction (getForum fid) >>= maybe notFound pure -- obtenim la dada forumD fent una comana a la base de dades
    -- generateAFormPost :: AForm (HandlerFor ForumsApp) a -> HandlerFor ForumsApp (FormResult a, WidgetFor ForumsApp ())
    -- editPostForm :: AForm (HandlerFor ForumsApp) EditPost
    -- pformw :: WidgetFor ForumsApp ()
    pformw <- generateAFormPost (editPostForm postD) -- generem el formulari per editar la resposta (post)
    -- defaultLayout :: WidgetFor ForumsApp () -> HandlerFor ForumsApp Html
    -- postView :: Maybe (UserId, UserD) -> (ForumId, ForumD) -> (TopicId, TopicD) -> (PostId, PostD) -> WidgetFor ForumsApp
    defaultLayout $ postView mbuser (fid, forumD) (tid, topicD) (pid, postD) pformw -- generem la vista de la pàgina de la resposta (post)

-- funció per gestionar els HTTP POST de la pàgina d'una resposta (post) d'una qüestió (topic) d'un fòrum per eliminar la resposta (post)
deletePostR :: PostId -> HandlerFor ForumsApp Html
deletePostR pid = do
    -- requireAuth :: HandlerFor ForumsApp (UserId, UserD)
    -- user :: (UserId, UserD)
    user <- requireAuth -- obtenim l'usuari autenticat
    -- runDbAction :: DbM a -> HandlerFor ForumsApp a
    -- getPost :: PostId -> DbM (Maybe PostD)
    -- maybe notFound pure :: Maybe a -> HandlerFor ForumsApp a
    -- postD :: PostD
    postD <- runDbAction (getPost pid) >>= maybe notFound pure -- obtenim la dada postD fent una comana a la base de dades
    -- pdTopicId :: PostD -> TopicId
    let tid = pdTopicId postD -- obtenim l'identificador de la qüestió (topic)
    -- runDbAction :: DbM a -> HandlerFor ForumsApp a
    -- getTopic :: TopicId -> DbM (Maybe TopicD)
    -- maybe notFound pure :: Maybe a -> HandlerFor ForumsApp a
    -- topicD :: TopicD
    topicD <- runDbAction (getTopic tid) >>= maybe notFound pure -- obtenim la dada topicD fent una comana a la base de dades
    -- tdForumId :: TopicD -> ForumId
    let fid = tdForumId topicD -- obtenim l'identificador del fòrum
    -- runDbAction :: DbM a -> HandlerFor ForumsApp a
    -- deletePost :: ForumId -> TopicId -> PostId -> SqlPersistT (HandlerFor ForumsApp) ()
    runDbAction $ deletePost fid tid pid -- eliminem la resposta (post) de la base de dades
    -- redirect :: Route ForumsApp -> HandlerFor ForumsApp a
    -- TopicR :: TopicId -> Route ForumsApp
    redirect (TopicR tid) -- redirigim a la pàgina de la qüestió (topic)

-- funció per gestionar els HTTP POST de la pàgina d'una resposta (post) d'una qüestió (topic) d'un fòrum per editar la resposta (post)
editPostR :: PostId -> HandlerFor ForumsApp Html
editPostR pid = do
    -- requireAuth :: HandlerFor ForumsApp (UserId, UserD)
    -- user :: (UserId, UserD)
    user <- requireAuth -- obtenim l'usuari autenticat
    -- runDbAction :: DbM a -> HandlerFor ForumsApp a
    -- getPost :: PostId -> DbM (Maybe PostD)
    -- maybe notFound pure :: Maybe a -> HandlerFor ForumsApp a
    -- postD :: PostD
    postD <- runDbAction (getPost pid) >>= maybe notFound pure -- obtenim la dada postD fent una comana a la base de dades
    -- runAFormPost :: AForm (HandlerFor ForumsApp) a -> HandlerFor ForumsApp (FormResult a, WidgetFor ForumsApp ())
    -- editPostForm2 :: AForm (HandlerFor ForumsApp) EditPost
    -- (pformr, pformw) :: (FormResult EditPost, WidgetFor ForumsApp ())
    (pformr, pformw) <- runAFormPost (editPostForm postD) -- obtenim el formulari per editar la resposta (post)
    case pformr of -- analitzem el resultat del formulari
        -- FormSuccess :: a -> FormResult a
        -- editPost :: EditPost
        FormSuccess newPost -> do -- si el formulari és correcte
            -- runDbAction :: DbM a -> HandlerFor ForumsApp a
            -- editPost :: PostId -> Markdown -> DbM ()
            runDbAction $ editPost pid (npMessage newPost) -- editem la resposta (post) a la base de dades
            -- redirect :: Route ForumsApp -> HandlerFor ForumsApp a
            -- PostR :: PostId -> Route ForumsApp
            redirect (PostR pid) -- redirigim a la pàgina de la resposta (post)
        _ -> do -- si el formulari és incorrecte
            -- runDbAction :: DbM a -> HandlerFor ForumsApp a
            -- getPost :: PostId -> DbM (Maybe PostD)
            -- postD :: PostD
            postD <- runDbAction (getPost pid) >>= maybe notFound pure -- obtenim la dada postD fent una comana a la base de dades
            -- tdTopicId :: PostD -> TopicId
            let tid = pdTopicId postD -- obtenim l'identificador de la qüestió (topic)
            -- runDbAction :: DbM a -> HandlerFor ForumsApp a
            -- getTopic :: TopicId -> DbM (Maybe TopicD)
            -- topicD :: TopicD
            topicD <- runDbAction (getTopic tid) >>= maybe notFound pure -- obtenim la dada topicD fent una comana a la base de dades
            -- tdForumId :: TopicD -> ForumId
            let fid = tdForumId topicD -- obtenim l'identificador del fòrum
            -- runDbAction :: DbM a -> HandlerFor ForumsApp a
            -- getForum :: ForumId -> DbM (Maybe ForumD)
            -- maybe notFound pure :: Maybe a -> HandlerFor ForumsApp a
            -- forumD :: ForumD
            forumD <- runDbAction (getForum fid) >>= maybe notFound pure -- obtenim la dada forumD fent una comana a la base de dades
            -- defaultLayout :: WidgetFor ForumsApp () -> HandlerFor ForumsApp Html
            -- postView :: Maybe (UserId, UserD) -> (ForumId, ForumD) -> (TopicId, TopicD) -> (PostId, PostD) -> WidgetFor ForumsApp
            defaultLayout $ postView (Just user) (fid, forumD) (tid, topicD) (pid, postD) pformw -- generem la vista de la pàgina de la resposta (post)

-- funció per gestionar els HTTP GET de la pàgina del perfil d'un usuari
getUserR :: HandlerFor ForumsApp Html
getUserR = do
    -- requireAuth :: HandlerFor ForumsApp (UserId, UserD)
    -- user :: (UserId, UserD)
    user <- requireAuth -- obtenim l'usuari autenticat
    -- generateAFormPost :: AForm (HandlerFor ForumsApp) a -> HandlerFor ForumsApp (FormResult a, WidgetFor ForumsApp ())
    -- newUserForm :: AForm (HandlerFor ForumsApp) NewUser
    -- (uformr, uformw) :: (FormResult NewUser, WidgetFor ForumsApp ())
    uformw <- generateAFormPost (newUserForm $ snd user) -- generem el formulari per canviar el el nom d'usuari i la contrasenya
    -- defaultLayout :: WidgetFor ForumsApp () -> HandlerFor ForumsApp Html
    -- userView :: Maybe (UserId, UserD) -> (UserId, UserD) -> WidgetFor ForumsApp -> WidgetFor ForumsApp
    defaultLayout $ userView (Just user) uformw -- generem la vista de la pàgina del perfil d'un usuari

-- funció per gestionar els HTTP POST de la pàgina del perfil d'un usuari
postUserR :: HandlerFor ForumsApp Html
postUserR = do
    -- requireAuth :: HandlerFor ForumsApp (UserId, UserD)
    -- user :: (UserId, UserD)
    user <- requireAuth -- obtenim l'usuari autenticat
    -- runDbAction :: DbM a -> HandlerFor ForumsApp a
    -- getUser :: UserId -> DbM (Maybe UserD)
    -- maybe notFound pure :: Maybe a -> HandlerFor ForumsApp a
    -- userD :: UserD
    userD <- runDbAction (getUser $ fst user) >>= maybe notFound pure -- obtenim la dada userD fent una comana a la base de dades
    -- runAFormPost :: AForm (HandlerFor ForumsApp) a -> HandlerFor ForumsApp (FormResult a, WidgetFor ForumsApp ())
    -- newUserForm :: AForm (HandlerFor ForumsApp) NewUser
    -- (uformr, uformw) :: (FormResult NewUser, WidgetFor ForumsApp ())
    (uformr, uformw) <- runAFormPost (newUserForm userD) -- obtenim el formulari per canviar el el nom d'usuari i la contrasenya
    case uformr of -- analitzem el resultat del formulari
        -- FormSuccess :: a -> FormResult a
        -- newUser :: NewUser
        FormSuccess newUser -> do -- si el formulari és correcte
            -- runDbAction :: DbM a -> HandlerFor ForumsApp a
            -- changeUserName :: UserId -> Text -> DbM ()
            -- changeUserPassword :: UserId -> Text -> DbM ()
            runDbAction $ changeUserName (fst user) (nuName newUser) -- canviem el nom d'usuari a la base de dades
            if nuPassword newUser == nuConfirm newUser -- si la contrasenya i la confirmació de la contrasenya coincideixen
                then do
                runDbAction $ changeUserPassword (fst user) (nuPassword newUser) -- canviem la contrasenya a la base de dades
                -- redirect :: Route ForumsApp -> HandlerFor ForumsApp a
                -- TopicR :: TopicId -> Route ForumsApp
                redirect HomeR -- redirigim a la pàgina principal
            else do
                -- defaultLayout :: WidgetFor ForumsApp () -> HandlerFor ForumsApp Html
                -- userView :: Maybe (UserId, UserD) -> (UserId, UserD) -> WidgetFor ForumsApp -> WidgetFor ForumsApp
                defaultLayout $ userView (Just user) uformw -- generem la vista de la pàgina del perfil d'un usuari
        _ -> do -- si el formulari és incorrecte
            -- defaultLayout :: WidgetFor ForumsApp () -> HandlerFor ForumsApp Html
            -- userView :: Maybe (UserId, UserD) -> (UserId, UserD) -> WidgetFor ForumsApp -> WidgetFor ForumsApp
            defaultLayout $ userView (Just user) uformw -- generem la vista de la pàgina del perfil d'un usuari