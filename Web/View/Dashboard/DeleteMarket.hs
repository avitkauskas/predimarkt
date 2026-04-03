module Web.View.Dashboard.DeleteMarket where

import Web.View.Prelude

data DeleteMarketView = DeleteMarketView
    { market       :: Market
    , page         :: Maybe Int
    , searchFilter :: Maybe Text
    }

instance View DeleteMarketView where
    html DeleteMarketView { .. } =
        renderConfirmationModal
            "Delete Draft Market"
            (pathTo DashboardMarketsAction { statusFilter = Just MarketStatusDraft, page, searchFilter })
            (pathTo DeleteMarketAction { marketId = market.id, page, searchFilter })
            "Delete Market"
            "btn btn-danger"
            [hsx|
                <p class="mb-0">
                    Delete draft market <strong>{market.title}</strong>? This cannot be undone.
                </p>
            |]
