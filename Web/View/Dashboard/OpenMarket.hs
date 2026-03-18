module Web.View.Dashboard.OpenMarket where

import Web.View.Prelude

data OpenMarketView = OpenMarketView
    { market :: Market
    }

instance View OpenMarketView where
    html OpenMarketView { .. } = renderModal Modal
        { modalTitle = "Update Closing Time"
        , modalCloseUrl = pathTo $ DashboardMarketsAction (Just MarketStatusClosed) Nothing
        , modalFooter = Nothing
        , modalContent = [hsx|
            <p>The closing time for this market is in the past.<br/>
            Please update it to a future time to open the market.</p>
            {renderForm}
        |]
        }
      where
        renderForm = formFor' market (pathTo $ OpenMarketAction (Just market.id)) [hsx|
            {(dateTimeField #closedAt) {
                fieldLabel = "Closing time",
                additionalAttributes =
                    [ ("data-alt-format", "Y-m-d H:i")
                    , ("data-month-selector-type", "static")
                    , ("data-allow-input", "true")
                    ]
            }}
            <div class="mt-3">
                <button type="submit" class="btn btn-primary">Open Market</button>
                <a href={DashboardMarketsAction (Just MarketStatusClosed) Nothing}
                   class="btn btn-outline-secondary ms-2">
                   Cancel
                </a>
            </div>
        |]
