module Web.Routes where
import Generated.Types
import IHP.RouterPrelude
import IHP.ModelSupport
import Web.Types

-- Generator Marker
instance AutoRoute StaticController
instance AutoRoute SessionsController
instance AutoRoute UsersController
instance AutoRoute CategoriesController
instance AutoRoute MarketsController
instance AutoRoute AssetsController

instance HasPath DashboardController where
    pathTo DashboardHoldingsAction = "/DashboardHoldings"
    pathTo DashboardMarketsAction { statusFilter } = 
        case statusFilter of
            Just s -> "/DashboardMarkets?statusFilter=" <> inputValue s
            Nothing -> "/DashboardMarkets"

instance CanRoute DashboardController where
    parseRoute' = 
        (string "/DashboardHoldings" >> pure DashboardHoldingsAction)
        <|> (string "/DashboardMarkets" >> pure (DashboardMarketsAction { statusFilter = Nothing }))
