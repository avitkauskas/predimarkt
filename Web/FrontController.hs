module Web.FrontController where

import IHP.RouterPrelude
import Web.Controller.Prelude
import IHP.LoginSupport.Middleware
import Web.View.Layout (defaultLayout)
import Web.Controller.Sessions

-- Controller Imports
import Web.Controller.Users
import Web.Controller.Static

instance FrontController WebApplication where
    controllers = 
        [ startPage WelcomeAction
        , parseRoute @SessionsController
        -- Generator Marker
        , parseRoute @UsersController
        ]

instance InitControllerContext WebApplication where
    initContext = do
        setLayout defaultLayout
        initAutoRefresh
        initAuthentication @User
