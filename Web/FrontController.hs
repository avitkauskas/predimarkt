module Web.FrontController where

import IHP.AutoRefresh
import IHP.LoginSupport.Middleware
import IHP.RouterPrelude
import Web.Controller.Prelude
import Web.Controller.Sessions
import Web.Routes (webRoutes)
import Web.View.Layout (withoutFooterLayout)

-- Controller Imports
import Web.Controller.Assets
import Web.Controller.Auth
import Web.Controller.Dashboard
import Web.Controller.Leaderboard
import Web.Controller.Markets
import Web.Controller.Passkeys
import Web.Controller.Static
import Web.Controller.Trades
import Web.Controller.Users

-- Generator Marker

instance FrontController WebApplication where
    controllers = startPage MarketsAction : webRoutes

instance InitControllerContext WebApplication where
    initContext = do
        setLayout withoutFooterLayout
