module Web.View.Dashboard.OpenMarket where

import Web.View.Prelude

data OpenMarketView = OpenMarketView
    { market       :: Market
    , page         :: Maybe Int
    , searchFilter :: Maybe Text
    }

instance View OpenMarketView where
    -- Restore this once IHP modal header close uses the same navigation path
    -- as the backdrop close under Bootstrap 5:
    --
    -- html OpenMarketView { .. } = renderModal Modal
    --     { modalTitle = "Update Closing Time"
    --     , modalCloseUrl = closeUrl
    --     , modalFooter = Nothing
    --     , modalContent = [hsx|
    --         <p>The closing time for this market is in the past.<br/>
    --         Please update it to a future time to open the market.</p>
    --         {renderForm}
    --     |]
    --     }
    html OpenMarketView { .. } = [hsx|
        <div class="modal fade overflow-auto show"
             id="modal"
             tabindex="-1"
             role="dialog"
             aria-labelledby="modal-title"
             aria-hidden="true"
             style="display: block"
             onclick="if (event.target.id === 'modal') document.getElementById('modal-backdrop').click()">
            <div class="modal-dialog" role="document" id="modal-inner">
                <div class="modal-content">
                    <div class="modal-header">
                        <h5 class="modal-title" id="modal-title">Update Closing Time</h5>
                        <a href={closeUrl}
                           class="btn-close"
                           aria-label="Close"
                           onclick="document.getElementById('modal-backdrop').click(); return false"></a>
                    </div>
                    <div class="modal-body">
                        <p>The closing time for this market is in the past.<br/>
                        Please update it to a future time to open the market.</p>
                        {renderForm}
                    </div>
                </div>
            </div>
        </div>
        <a id="modal-backdrop" href={closeUrl} class="modal-backdrop fade show" style="display: block"></a>
    |]
      where
        renderForm = formFor' market (pathTo $ OpenMarketAction (Just market.id) page searchFilter) [hsx|
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
                <a href={closeUrl}
                   class="btn btn-outline-secondary ms-2"
                   onclick="document.getElementById('modal-backdrop').click(); return false">
                   Cancel
                </a>
            </div>
        |]
        closeUrl = pathTo $ DashboardMarketsAction (Just market.status) page searchFilter
