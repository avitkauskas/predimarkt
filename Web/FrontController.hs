module Web.FrontController where

import IHP.LoginSupport.Middleware
import IHP.RouterPrelude
import Web.Controller.Prelude
import Web.Controller.Sessions
import Web.View.Layout (defaultLayout)

-- Controller Imports
import Admin.Controller.Admins
import Web.Controller.Assets
import Web.Controller.Markets
import Web.Controller.Static
import Web.Controller.Users

instance FrontController WebApplication where
    controllers =
        [ startPage MarketsAction
        , parseRoute @SessionsController
        -- Generator Marker
        , parseRoute @AssetsController
        , parseRoute @MarketsController
        , parseRoute @UsersController
        ]

instance InitControllerContext WebApplication where
    initContext = do
        setLayout defaultLayout
        initAutoRefresh
        initAuthentication @User
