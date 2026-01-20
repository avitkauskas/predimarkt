module Admin.FrontController where

import Admin.Controller.Prelude
import Admin.View.Layout (defaultLayout)
import IHP.LoginSupport.Middleware
import IHP.RouterPrelude

-- Controller Imports
import Admin.Controller.Admins
import Admin.Controller.Categories
import Admin.Controller.Sessions
import Admin.Controller.Static

instance FrontController AdminApplication where
    controllers =
        [ startPage DashboardAction
        , parseRoute @AdminsController
        , parseRoute @SessionsController
        , parseRoute @CategoriesController
        -- Generator Marker
        ]

instance InitControllerContext AdminApplication where
    initContext = do
        setLayout defaultLayout
        initAutoRefresh
        initAuthentication @Admin
