
{-# LANGUAGE OverloadedStrings #-}

module App
where
import Config
import Found
import Model
import Handler
import AuthHandler

import Develop.DatFw
import Develop.DatFw.Dispatch

import Network.Wai

-- ---------------------------------------------------------------
-- Application initialization

makeApp :: IO Application
makeApp = do
    -- Open the database (the model state)
    db <- openDb forumsDbName
    toApp ForumsApp{ forumsDb = db }

-- ---------------------------------------------------------------
-- Main controller

instance Dispatch ForumsApp where
    dispatch = routing
            $ route ( onStatic [] ) HomeR
                [ onMethod "GET" getHomeR
                , onMethod "POST" postHomeR
                ]
            <||> route ( onStatic ["forums"] <&&> onDynamic ) ForumR
                [ onMethod1 "GET" getForumR
                , onMethod1 "POST" postForumR
                , onMethod1 "DELETE" deleteForumR
                ]
            <||> route ( onStatic ["topics"] <&&> onDynamic ) TopicR
                [ onMethod1 "GET" getTopicR
                , onMethod1 "POST" postTopicR
                , onMethod1 "DELETE" deleteTopicR
                ]
            -- <||> route ( onStatic ["posts"] <&&> onDynamic ) PostR
            --     [ onMethod1 "GET" getPostR
            --     , onMethod1 "DELETE" deletePostR
            --     ]
            <||> route ( onStatic ["login"] ) LoginR
                [ onMethod "GET" getLoginR
                , onMethod "POST" postLoginR
                ]
            <||> route ( onStatic ["logout"] ) LogoutR
                (onAnyMethod handleLogoutR)
            

