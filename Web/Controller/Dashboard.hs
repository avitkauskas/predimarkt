module Web.Controller.Dashboard where

import Web.Controller.Prelude
import Web.View.Dashboard.Holdings
import Web.View.Dashboard.Markets

instance Controller DashboardController where
    beforeAction = ensureIsUser

    action DashboardHoldingsAction = render HoldingsView

    action DashboardMarketsAction = do
        markets <- query @Market
            |> filterWhere (#userId, Just currentUserId)
            |> orderByDesc #createdAt
            |> fetch
        render MarketsView { .. }
