module Web.Types where

import Generated.Types
import IHP.LoginSupport.Types
import IHP.ModelSupport
import IHP.Prelude

data WebApplication
    = WebApplication
    deriving (Eq, Show)

data StaticController
    = WelcomeAction
    deriving (Eq, Show, Data)

data SessionsController
    = NewSessionAction
    | CreateSessionAction
    | DeleteSessionAction
    deriving (Eq, Show, Data)

instance HasNewSessionUrl User where
    newSessionUrl _ = "/NewSession"

type instance CurrentUserRecord = User

data UsersController
    = NewUserAction
    | CreateUserAction
    | EditUserAction {userId :: !(Id User)}
    | UpdateUserAction {userId :: !(Id User)}
    | DeleteUserAction {userId :: !(Id User)}
    deriving (Eq, Show, Data)

data CategoriesController
    = CategoriesAction
    | NewCategoryAction
    | ShowCategoryAction { categoryId :: !(Id Category) }
    | CreateCategoryAction
    | EditCategoryAction { categoryId :: !(Id Category) }
    | UpdateCategoryAction { categoryId :: !(Id Category) }
    | DeleteCategoryAction { categoryId :: !(Id Category) }
    deriving (Eq, Show, Data)

data MarketsController
    = MarketsAction
    | NewMarketAction
    | ShowMarketAction { marketId :: !(Id Market), tradingAssetId :: !(Maybe (Id Asset)), tradingAction :: !(Maybe Text) }
    | CreateMarketAction
    | EditMarketAction { marketId :: !(Id Market) }
    | UpdateMarketAction { marketId :: !(Id Market) }
    | DeleteMarketAction { marketId :: !(Id Market) }
    | SetResolveAssetAction { marketId :: !(Id Market) }
    | ResolveMarketAction { marketId :: !(Id Market) }
    | ConfirmRefundMarketAction { marketId :: !(Id Market) }
    | RefundMarketAction { marketId :: !(Id Market) }
    deriving (Eq, Show, Data)

data AssetsController
    = DeleteAssetAction { assetId :: !(Id Asset) }
    | TradeAssetAction { assetId :: !(Id Asset) }
    | ClosePositionAction { assetId :: !(Id Asset) }
    | NewAssetAction
    deriving (Eq, Show, Data)

data DashboardController
    = DashboardPositionsAction { page :: Maybe Int }
    | DashboardWalletsAction
    | DashboardMarketsAction { statusFilter :: Maybe MarketStatus }
    | DashboardTransactionsAction { page :: Maybe Int }
    | ChangeMarketStatusAction { marketId :: !(Maybe (Id Market)), status :: !(Maybe MarketStatus) }
    deriving (Eq, Show, Data)

deriving instance Data MarketStatus

-- | OHLC data point for candlestick charts
-- Time is stored as Unix timestamp in seconds for JavaScript compatibility
data OhlcPoint = OhlcPoint
    { ohlcTime  :: Int
    , ohlcOpen  :: Double
    , ohlcHigh  :: Double
    , ohlcLow   :: Double
    , ohlcClose :: Double
    } deriving (Eq, Show)

-- | Chart data for a single asset
data AssetChartData = AssetChartData
    { chartAssetId    :: Id Asset
    , chartAssetName  :: Text
    , chartAssetColor :: Text
    , chartOhlcData   :: [OhlcPoint]
    } deriving (Eq, Show)
