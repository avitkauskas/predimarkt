module Web.Types
    ( module Web.Types
    , AssetChartData (..)
    , PricePoint (..)
    ) where

import Application.Domain.ChartData (AssetChartData (..), PricePoint (..))
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
    = DeleteSessionAction
    deriving (Eq, Show, Data)

data AuthController
    = LoginAction
    | WorkOSLoginAction
    | WorkOSCallbackAction
    deriving (Eq, Show, Data)

instance HasNewSessionUrl User where
    newSessionUrl _ = "/NewSession"

type instance CurrentUserRecord = User

data UsersController
    = EditUserAction {userId :: !(Id User)}
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
    | ConfirmRefundMarketAction { marketId :: !(Id Market) }
    deriving (Eq, Show, Data)

data AssetsController
    = DeleteAssetAction { assetId :: !(Id Asset) }
    | NewAssetAction
    deriving (Eq, Show, Data)

data TradesController
    = ExecuteTradeAction { assetId :: !(Id Asset) }
    | ClosePositionAction { assetId :: !(Id Asset) }
    | ResolveMarketAction { marketId :: !(Id Market) }
    | RefundMarketAction { marketId :: !(Id Market) }
    deriving (Eq, Show, Data)

data DashboardController
    = DashboardPositionsAction { page :: Maybe Int }
    | DashboardMarketsAction { statusFilter :: Maybe MarketStatus }
    | DashboardTransactionsAction { page :: Maybe Int }
    | ChangeMarketStatusAction { marketId :: !(Maybe (Id Market)), status :: !(Maybe MarketStatus) }
    | OpenMarketAction { marketId :: !(Maybe (Id Market)) }
    deriving (Eq, Show, Data)

data LeaderboardController
    = LeaderboardAction
    deriving (Eq, Show, Data)

deriving instance Data MarketStatus
