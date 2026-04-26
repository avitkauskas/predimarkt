{-# LANGUAGE LambdaCase  #-}
{-# LANGUAGE QuasiQuotes #-}

module Web.Routes where

import Generated.Types (MarketStatus (..))
import IHP.Router.Capture
import IHP.Router.DSL (routes)
import IHP.RouterPrelude
import Web.Types

instance UrlCapture MarketStatus where
    parseCapture bs = case bs of
        "draft"    -> Just MarketStatusDraft
        "open"     -> Just MarketStatusOpen
        "closed"   -> Just MarketStatusClosed
        "resolved" -> Just MarketStatusResolved
        "refunded" -> Just MarketStatusRefunded
        _          -> Nothing
    renderCapture = \case
        MarketStatusDraft    -> "draft"
        MarketStatusOpen     -> "open"
        MarketStatusClosed   -> "closed"
        MarketStatusResolved -> "resolved"
        MarketStatusRefunded -> "refunded"

[routes|webRoutes

-- Sessions
POST /DeleteSession DeleteSessionAction

-- Leaderboard
GET /Leaderboard LeaderboardAction

-- Assets
GET /NewAsset NewAssetAction

-- Trades
POST /ExecuteTrade?assetId ExecuteTradeAction
POST /ClosePosition?assetId ClosePositionAction
POST /ResolveMarket?marketId ResolveMarketAction
POST /RefundMarket?marketId RefundMarketAction

-- Categories
GET /Categories CategoriesAction
GET /NewCategory NewCategoryAction
GET /ShowCategory?categoryId ShowCategoryAction
POST /CreateCategory CreateCategoryAction
GET /EditCategory?categoryId EditCategoryAction
POST /UpdateCategory?categoryId UpdateCategoryAction
POST /DeleteCategory?categoryId DeleteCategoryAction

-- Users
GET /EditUser?userId EditUserAction
POST /UpdateUser?userId UpdateUserAction
GET /ConfirmDeletePasskey?passkeyId ConfirmDeletePasskeyAction
GET /ConfirmDeleteUser?userId ConfirmDeleteUserAction
POST /DeleteUser?userId DeleteUserAction

-- Passkeys
POST /UpdatePasskeyName?passkeyId UpdatePasskeyNameAction
POST /DeletePasskey?passkeyId DeletePasskeyAction

-- Auth
GET /NewSession LoginAction
POST /passkeys/registration/begin BeginPasskeyRegistrationAction
POST /passkeys/registration/finish FinishPasskeyRegistrationAction
POST /passkeys/authentication/begin BeginPasskeyAuthenticationAction
POST /passkeys/authentication/finish FinishPasskeyAuthenticationAction

-- Static
GET /about AboutAction
GET /how-it-works HowItWorksAction
GET /community-rules CommunityRulesAction
GET /terms-of-service TermsAction
GET /privacy-policy PrivacyPolicyAction
GET /cookie-policy CookiePolicyAction
GET /moderation-policy ModerationPolicyAction
GET /legal-notice LegalNoticeAction

-- Markets
GET /Markets MarketsAction
GET /NewMarket NewMarketAction
POST /CreateMarket CreateMarketAction
GET /ShowMarket?marketId&tradingAssetId&tradingAction&showChart&showDescription&showAllAssets&showTradeHistory&activityPage&chatPage&chatComposerRev&tradeQuantity&backTo ShowMarketAction
POST /CreateMarketChatMessage?marketId CreateMarketChatMessageAction
POST /DeleteMarketChatMessage?marketChatMessageId&marketId&tradingAssetId&tradingAction&showChart&showDescription&showAllAssets&showTradeHistory&activityPage&chatPage&chatComposerRev&tradeQuantity&backTo DeleteMarketChatMessageAction
GET /EditMarket?marketId&page&search EditMarketAction { searchFilter = #search }
POST /UpdateMarket?marketId UpdateMarketAction
POST /DeleteMarket?marketId&page&search DeleteMarketAction { searchFilter = #search }
POST /SetResolveAsset?marketId SetResolveAssetAction
POST /ConfirmRefundMarket?marketId ConfirmRefundMarketAction

-- Dashboard
GET /DashboardPositions?page&search&positionStatusFilter DashboardPositionsAction { searchFilter = #search }
GET /DashboardMarkets?page&search&statusFilter DashboardMarketsAction { searchFilter = #search }
GET /DashboardTransactions?page&search&typeFilter DashboardTransactionsAction { searchFilter = #search, typeFilter = #typeFilter }
GET /ConfirmDeleteMarket?marketId&page&search ConfirmDeleteMarketAction { confirmDeleteMarketId = #marketId, searchFilter = #search }
POST /ChangeMarketStatus?marketId&status&page&search ChangeMarketStatusAction { searchFilter = #search }
|]
