module Web.Routes where

import Generated.Types (MarketStatus (..))
import IHP.Router.DSL (routes)
import IHP.RouterPrelude
import Web.Types

instance UrlCapture MarketStatus where
    parseCapture = \case
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

-- Leaderboard
GET  /leaderboard LeaderboardAction

-- Assets
GET  /assets/new NewAssetAction
POST /assets/{assetId}/execute-trade ExecuteTradeAction
POST /assets/{assetId}/close-position ClosePositionAction

-- Users
POST /users/{userId} UpdateUserAction
GET  /users/{userId}/edit EditUserAction
POST /users/{userId}/delete DeleteUserAction
GET  /users/{userId}/confirm-delete ConfirmDeleteUserAction

-- Auth
GET  /login LoginAction
POST /logout DeleteSessionAction

-- Passkeys
POST /passkeys/registration/begin BeginPasskeyRegistrationAction
POST /passkeys/registration/finish FinishPasskeyRegistrationAction
POST /passkeys/authentication/begin BeginPasskeyAuthenticationAction
POST /passkeys/authentication/finish FinishPasskeyAuthenticationAction

GET  /passkeys/{passkeyId}/confirm-delete ConfirmDeletePasskeyAction
POST /passkeys/{passkeyId}/update-name UpdatePasskeyNameAction
POST /passkeys/{passkeyId}/delete DeletePasskeyAction

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
GET  /markets MarketsAction
POST /markets CreateMarketAction
GET  /markets/new NewMarketAction
GET  /markets/{marketId}?tradingAssetId&tradingAction&showChart&showDescription&showAllAssets&showTradeHistory&activityPage&chatPage&chatComposerRev&tradeQuantity&backTo ShowMarketAction
POST /markets/{marketId} UpdateMarketAction
GET  /markets/{marketId}/edit?page&search EditMarketAction { searchFilter = #search }
GET  /markets/{marketId}/confirm-delete?page&search ConfirmDeleteMarketAction { confirmDeleteMarketId = #marketId, searchFilter = #search }
POST /markets/{marketId}/delete?page&search DeleteMarketAction { searchFilter = #search }
POST /markets/{marketId}/change-status?status&page&search ChangeMarketStatusAction { searchFilter = #search }
POST /markets/{marketId}/set-resolve-asset SetResolveAssetAction
POST /markets/{marketId}/resolve ResolveMarketAction
POST /markets/{marketId}/confirm-refund ConfirmRefundMarketAction
POST /markets/{marketId}/refund RefundMarketAction
POST /markets/{marketId}/chat-messages CreateMarketChatMessageAction
POST /markets/{marketId}/chat-messages/{marketChatMessageId}/delete?tradingAssetId&tradingAction&showChart&showDescription&showAllAssets&showTradeHistory&activityPage&chatPage&chatComposerRev&tradeQuantity&backTo DeleteMarketChatMessageAction

-- Dashboard
GET  /dashboard/positions?page&search&positionStatusFilter DashboardPositionsAction { searchFilter = #search }
GET  /dashboard/transactions?page&search&typeFilter DashboardTransactionsAction { searchFilter = #search }
GET  /dashboard/markets?page&search&statusFilter DashboardMarketsAction { searchFilter = #search }
|]
