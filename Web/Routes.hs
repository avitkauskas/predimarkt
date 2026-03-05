module Web.Routes where

import IHP.ModelSupport
import IHP.Prelude
import IHP.RouterPrelude
import Web.Types

-- Generator Marker
instance AutoRoute SessionsController
instance AutoRoute UsersController
instance AutoRoute CategoriesController
instance AutoRoute AssetsController
instance AutoRoute TradesController
instance AutoRoute LeaderboardController

instance HasPath AuthController where
    pathTo LoginAction          = "/NewSession"
    pathTo WorkOSLoginAction    = "/workos-login"
    pathTo WorkOSCallbackAction = "/auth/callback"

instance CanRoute AuthController where
    parseRoute' =
        (string "/NewSession" >> pure LoginAction)
        <|> (string "/workos-login" >> pure WorkOSLoginAction)
        <|> (string "/auth/callback" >> pure WorkOSCallbackAction)

instance HasPath MarketsController where
    pathTo MarketsAction            = "/Markets"
    pathTo NewMarketAction          = "/NewMarket"
    pathTo CreateMarketAction       = "/CreateMarket"
    pathTo ShowMarketAction { marketId, tradingAssetId, tradingAction } =
        "/ShowMarket?marketId=" <> inputValue marketId
        <> maybe "" (\aid -> "&tradingAssetId=" <> inputValue aid) tradingAssetId
        <> maybe "" (\a -> "&tradingAction=" <> a) tradingAction
    pathTo EditMarketAction { marketId }   = "/EditMarket?marketId=" <> inputValue marketId
    pathTo UpdateMarketAction { marketId } = "/UpdateMarket?marketId=" <> inputValue marketId
    pathTo DeleteMarketAction { marketId } = "/DeleteMarket?marketId=" <> inputValue marketId
    pathTo SetResolveAssetAction { marketId } = "/SetResolveAsset?marketId=" <> inputValue marketId
    pathTo ConfirmRefundMarketAction { marketId } = "/ConfirmRefundMarket?marketId=" <> inputValue marketId

instance CanRoute MarketsController where
    parseRoute' =
        (string "/Markets" >> pure MarketsAction)
        <|> (string "/NewMarket" >> pure NewMarketAction)
        <|> (string "/CreateMarket" >> pure CreateMarketAction)
        <|> (string "/ShowMarket" >> pure (ShowMarketAction def Nothing Nothing))
        <|> (string "/EditMarket" >> pure (EditMarketAction def))
        <|> (string "/UpdateMarket" >> pure (UpdateMarketAction def))
        <|> (string "/DeleteMarket" >> pure (DeleteMarketAction def))
        <|> (string "/SetResolveAsset" >> pure (SetResolveAssetAction def))
        <|> (string "/ConfirmRefundMarket" >> pure (ConfirmRefundMarketAction def))

instance HasPath DashboardController where
    pathTo DashboardPositionsAction { page, searchFilter } =
        buildPaginatedSearchPath "/DashboardPositions" page searchFilter
    pathTo DashboardMarketsAction { statusFilter } =
        "/DashboardMarkets" <> maybe "" (\s -> "?statusFilter=" <> inputValue s) statusFilter
    pathTo DashboardTransactionsAction { page, searchFilter } =
        buildPaginatedSearchPath "/DashboardTransactions" page searchFilter
    pathTo ChangeMarketStatusAction { marketId, status } =
        "/ChangeMarketStatus"
        <> maybe "" (\mid -> "?marketId=" <> inputValue mid) marketId
        <> maybe "" (\s -> "&status=" <> inputValue s) status
    pathTo OpenMarketAction { marketId } =
        "/OpenMarket" <> maybe "" (\mid -> "?marketId=" <> inputValue mid) marketId

instance CanRoute DashboardController where
    parseRoute' =
        (string "/DashboardPositions" >> pure (DashboardPositionsAction Nothing Nothing))
        <|> (string "/DashboardMarkets" >> pure (DashboardMarketsAction Nothing))
        <|> (string "/DashboardTransactions" >> pure (DashboardTransactionsAction Nothing Nothing))
        <|> (string "/ChangeMarketStatus" >> pure (ChangeMarketStatusAction Nothing Nothing))
        <|> (string "/OpenMarket" >> pure (OpenMarketAction Nothing))

buildPaginatedSearchPath :: Text -> Maybe Int -> Maybe Text -> Text
buildPaginatedSearchPath basePath page searchFilter =
    let pageParam = maybe "" (\p -> "?page=" <> inputValue p) page
        searchParam = case (page, searchFilter) of
            (_, Just search) | search /= "" -> (if isNothing page then "?" else "&") <> "search=" <> inputValue search
            _ -> ""
    in basePath <> pageParam <> searchParam
