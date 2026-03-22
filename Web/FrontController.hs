module Web.FrontController where

import IHP.AutoRefresh
import IHP.LoginSupport.Middleware
import IHP.RouterPrelude
import Web.Controller.Prelude
import Web.Controller.Sessions
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
    controllers =
        [ startPage MarketsAction
        , parseRoute @SessionsController
        , parseRoute @AuthController
        , parseRoute @StaticController
        -- Generator Marker
        , parseRoute @AssetsController
        , parseRoute @DashboardController
        , parseRoute @LeaderboardController
        , post "/CreateMarketChatMessage" (CreateMarketChatMessageAction def)
        , post "/DeleteMarketChatMessage" (DeleteMarketChatMessageAction def def Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing)
        , parseRoute @MarketsController
        , parseRoute @TradesController
        , parseRoute @UsersController
        , parseRoute @PasskeysController
        ]

instance InitControllerContext WebApplication where
    initContext = do
        setLayout withoutFooterLayout
        initAuthentication @User
