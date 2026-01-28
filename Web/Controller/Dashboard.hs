module Web.Controller.Dashboard where

import Web.Controller.Prelude
import Web.View.Dashboard.Holdings
import Web.View.Dashboard.Markets

instance Controller DashboardController where
    beforeAction = ensureIsUser

    action DashboardHoldingsAction = render HoldingsView

    action DashboardMarketsAction { statusFilter } = do
        let activeStatus = fromMaybe MarketStatusDraft $ statusFilter <|> paramOrNothing @MarketStatus "statusFilter"
        markets <- query @Market
            |> filterWhere (#userId, Just currentUserId)
            |> filterWhere (#status, activeStatus)
            |> orderByDesc #createdAt
            |> fetch
        render MarketsView { .. }
