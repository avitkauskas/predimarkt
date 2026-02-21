module Web.Routes where
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import Generated.Types
import IHP.ModelSupport
import IHP.Prelude
import IHP.RouterPrelude
import Web.Types

-- Generator Marker
instance AutoRoute StaticController
instance AutoRoute SessionsController
instance AutoRoute UsersController
instance AutoRoute CategoriesController
instance AutoRoute AssetsController
instance AutoRoute TradesController

instance HasPath AuthController where
    pathTo LoginAction          = "/login"
    pathTo WorkOSCallbackAction = "/auth/callback"

instance CanRoute AuthController where
    parseRoute' =
        (string "/login" >> pure LoginAction)
        <|> (string "/auth/callback" >> pure WorkOSCallbackAction)

instance HasPath MarketsController where
    pathTo MarketsAction = "/Markets"
    pathTo NewMarketAction = "/NewMarket"
    pathTo CreateMarketAction = "/CreateMarket"
    pathTo ShowMarketAction { marketId, tradingAssetId, tradingAction } =
        "/ShowMarket"
        <> "?marketId=" <> inputValue marketId
        <> (case tradingAssetId of Just id -> "&tradingAssetId=" <> inputValue id; Nothing -> "")
        <> (case tradingAction of Just a -> "&tradingAction=" <> a; Nothing -> "")
    pathTo EditMarketAction { marketId } = "/EditMarket?marketId=" <> inputValue marketId
    pathTo UpdateMarketAction { marketId } = "/UpdateMarket?marketId=" <> inputValue marketId
    pathTo DeleteMarketAction { marketId } = "/DeleteMarket?marketId=" <> inputValue marketId
    pathTo SetResolveAssetAction { marketId } = "/SetResolveAsset?marketId=" <> inputValue marketId
    pathTo ConfirmRefundMarketAction { marketId } = "/ConfirmRefundMarket?marketId=" <> inputValue marketId

instance CanRoute MarketsController where
    parseRoute' =
        (string "/Markets" >> pure MarketsAction)
        <|> (string "/NewMarket" >> pure NewMarketAction)
        <|> (string "/CreateMarket" >> pure CreateMarketAction)
        <|> (string "/ShowMarket" >> pure (ShowMarketAction { marketId = def, tradingAssetId = Nothing, tradingAction = Nothing }))
        <|> (string "/EditMarket" >> pure (EditMarketAction { marketId = def }))
        <|> (string "/UpdateMarket" >> pure (UpdateMarketAction { marketId = def }))
        <|> (string "/DeleteMarket" >> pure (DeleteMarketAction { marketId = def }))
        <|> (string "/SetResolveAsset" >> pure (SetResolveAssetAction { marketId = def }))
        <|> (string "/ConfirmRefundMarket" >> pure (ConfirmRefundMarketAction { marketId = def }))

instance HasPath DashboardController where
    pathTo DashboardPositionsAction { page } =
        case page of
            Just p  -> "/DashboardPositions?page=" <> inputValue p
            Nothing -> "/DashboardPositions"
    pathTo DashboardMarketsAction { statusFilter } =
        case statusFilter of
            Just s  -> "/DashboardMarkets?statusFilter=" <> inputValue s
            Nothing -> "/DashboardMarkets"
    pathTo ChangeMarketStatusAction { marketId, status } =
        "/ChangeMarketStatus?marketId=" <> inputValue marketId <> "&status=" <> inputValue status
    pathTo DashboardTransactionsAction { page } =
        case page of
            Just p  -> "/DashboardTransactions?page=" <> inputValue p
            Nothing -> "/DashboardTransactions"
    pathTo OpenMarketAction { marketId } =
        "/OpenMarket"
        <> (case marketId of Just id -> "?marketId=" <> inputValue id; Nothing -> "")

instance CanRoute DashboardController where
    parseRoute' =
        (string "/DashboardPositions" >> pure (DashboardPositionsAction { page = Nothing }))
        <|> (string "/DashboardMarkets" >> pure (DashboardMarketsAction { statusFilter = Nothing }))
        <|> (string "/DashboardTransactions" >> pure (DashboardTransactionsAction { page = Nothing }))
        <|> (string "/ChangeMarketStatus" >> pure (ChangeMarketStatusAction { marketId = Nothing, status = Nothing }))
        <|> (string "/OpenMarket" >> pure (OpenMarketAction { marketId = Nothing }))
