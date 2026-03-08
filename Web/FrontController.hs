module Web.FrontController where

import IHP.AutoRefresh
import IHP.LoginSupport.Middleware
import IHP.RouterPrelude
import Web.Controller.Prelude
import Web.Controller.Sessions
import Web.View.Layout (defaultLayout)

-- Controller Imports
import Web.Controller.Assets
import Web.Controller.Auth
import Web.Controller.Dashboard
import Web.Controller.Leaderboard
import Web.Controller.Markets
import Web.Controller.Trades
import Web.Controller.Users

instance FrontController WebApplication where
    controllers =
        [ startPage MarketsAction
        , parseRoute @SessionsController
        , parseRoute @AuthController
        -- Generator Marker
        , parseRoute @AssetsController
        , parseRoute @DashboardController
        , parseRoute @LeaderboardController
        , post "/CreateMarketChatMessage" (CreateMarketChatMessageAction def)
        , parseRoute @MarketsController
        , parseRoute @TradesController
        , parseRoute @UsersController
        ]

instance InitControllerContext WebApplication where
    initContext = do
        setLayout defaultLayout
        initAuthentication @User
