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
    | ShowMarketAction { marketId :: !(Id Market) }
    | CreateMarketAction
    | EditMarketAction { marketId :: !(Id Market) }
    | UpdateMarketAction { marketId :: !(Id Market) }
    | DeleteMarketAction { marketId :: !(Id Market) }

    deriving (Eq, Show, Data)

data AssetsController
    = DeleteAssetAction { assetId :: !(Id Asset) }
    | NewAssetAction
    deriving (Eq, Show, Data)

data DashboardController
    = DashboardHoldingsAction
    | DashboardMarketsAction { statusFilter :: Maybe MarketStatus }
    | ChangeMarketStatusAction { marketId :: !(Maybe (Id Market)), status :: !(Maybe MarketStatus) }
    deriving (Eq, Show, Data)
    
deriving instance Data MarketStatus
