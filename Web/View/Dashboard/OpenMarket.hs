module Web.View.Dashboard.OpenMarket where

import Web.View.Prelude

data OpenMarketView = OpenMarketView
    { market       :: Market
    , page         :: Maybe Int
    , searchFilter :: Maybe Text
    }

instance View OpenMarketView where
    html OpenMarketView { .. } = renderModal Modal
        { modalTitle = "Update Closing Time"
        , modalCloseUrl = closeUrl
        , modalFooter = Nothing
        , modalContent = [hsx|
            <p>The closing time for this market is in the past.<br/>
            Please update it to a future time to open the market.</p>
            {renderForm}
        |]
        }
      where
        renderForm = formFor' market (pathTo $ OpenMarketAction (Just market.id) page searchFilter) [hsx|
            {(dateTimeField #closedAt) {
                fieldLabel = "Closing time",
                fieldValue = utcDateTimeInputValue (get #closedAt market),
                additionalAttributes =
                    [ ("data-alt-format", "Y-m-d H:i")
                    , ("data-month-selector-type", "static")
                    , ("data-allow-input", "true")
                    , ("autocomplete", "off")
                    ]
            }}
            <div class="mt-3">
                <button type="submit" class="btn btn-primary">Open Market</button>
                <a href={closeUrl}
                   class="btn btn-outline-secondary ms-2"
                   onclick="document.getElementById('modal-backdrop').click(); return false">
                   Cancel
                </a>
            </div>
        |]
        closeUrl = pathTo $ DashboardMarketsAction (Just market.status) page searchFilter
