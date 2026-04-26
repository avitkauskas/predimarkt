module Web.View.Dashboard.Markets where

import Web.View.Prelude

data MarketsView = MarketsView
    { markets      :: [Market]
    , activeStatus :: MarketStatus
    , currentPage  :: Int
    , totalPages   :: Int
    , searchFilter :: Maybe Text
    }

instance View MarketsView where
    html MarketsView { .. } = dashboardLayout [hsx|
        <div>
            <div class="d-flex justify-content-between align-items-center mb-2 ms-2">
                <h5>My Markets</h5>
                <a href={NewMarketAction} class="btn btn-primary">
                    <i class="bi bi-plus-lg"></i> New Market
                </a>
            </div>
            {renderTabs activeStatus searchFilter}
            <div class="mb-2">
                {renderSearchForm activeStatus searchFilter}
            </div>
            <table class="table table-hover ms-0">
                <tbody>
                    {forEach markets (\m -> renderMarket currentBackToPath currentPage searchFilter m)}
                </tbody>
            </table>
            {renderMarketsPagination currentPage totalPages activeStatus searchFilter}
        </div>
    |]
        where
            currentBackToPath = pathTo (DashboardMarketsAction (Just currentPage) searchFilter (Just activeStatus))

renderMarket :: (?context :: ControllerContext) => Text -> Int -> Maybe Text -> Market -> Html
renderMarket backToPath currentPage searchFilter market = [hsx|
    <tr class="d-none d-sm-table-row market-row">
        <td class="align-middle">
            <a class="text-decoration-none text-reset market-link-hover"
               href={ShowMarketAction market.id Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing (Just backToPath)}>
                {market.title}
            </a>
        </td>
        <td class="text-end">
            {renderActions market currentPage searchFilter backToPath}
        </td>
    </tr>
    <tr class="d-table-row d-sm-none">
        <td class="align-middle market-row">
            <div class="d-flex flex-column gap-2 py-1">
                <a class="text-decoration-none text-reset market-link-hover"
                   href={ShowMarketAction market.id Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing (Just backToPath)}>
                    {market.title}
                </a>
                <div class="overflow-auto">
                    {renderActions market currentPage searchFilter backToPath}
                </div>
            </div>
        </td>
    </tr>
|]

renderTabs :: MarketStatus -> Maybe Text -> Html
renderTabs activeStatus searchFilter = [hsx|
    <div class="overflow-x-auto scroll-no-bar mb-2">
        <ul class="nav nav-tabs dashboard-tabs flex-nowrap text-nowrap">
            {forEach statuses (renderTab searchFilter)}
        </ul>
    </div>
|]
    where
        statuses = [MarketStatusDraft, MarketStatusOpen, MarketStatusClosed, MarketStatusResolved, MarketStatusRefunded]
        renderTab search status = [hsx|
            <li class="nav-item">
                <a class={classes [("nav-link", True), ("active", status == activeStatus)]}
                   href={DashboardMarketsAction Nothing search (Just status)}>
                    {statusLabel status}
                </a>
            </li>
        |]

        statusLabel :: MarketStatus -> Text
        statusLabel MarketStatusDraft    = "Draft"
        statusLabel MarketStatusOpen     = "Open"
        statusLabel MarketStatusClosed   = "Closed"
        statusLabel MarketStatusResolved = "Resolved"
        statusLabel MarketStatusRefunded = "Refunded"

renderActions :: (?context :: ControllerContext) => Market -> Int -> Maybe Text -> Text -> Html
renderActions market currentPage searchFilter backToPath =
    let pageValue = if currentPage > 1 then Just currentPage else Nothing
        actionClasses :: Text
        actionClasses = "btn btn-sm text-nowrap"
        openDraftForm = renderPostForm (pathTo (ChangeMarketStatusAction (Just market.id) (Just MarketStatusOpen) pageValue searchFilter)) [("class", "d-inline-flex flex-shrink-0")] [hsx|
            <button type="submit" class={actionClasses <> " btn-outline-primary"}>Open</button>
        |]
        closeOpenForm = renderPostForm (pathTo (ChangeMarketStatusAction (Just market.id) (Just MarketStatusClosed) pageValue searchFilter)) [("class", "d-inline-flex flex-shrink-0")] [hsx|
            <button type="submit" class={actionClasses <> " btn-outline-primary"}>Close</button>
        |]
        reopenClosedForm = renderPostForm (pathTo (ChangeMarketStatusAction (Just market.id) (Just MarketStatusOpen) pageValue searchFilter)) [("class", "d-inline-flex flex-shrink-0")] [hsx|
            <button type="submit" class={actionClasses <> " btn-outline-primary"}>Open</button>
        |]
        resolveForm = renderPostForm (pathTo (SetResolveAssetAction market.id)) [("class", "d-inline")] [hsx|
            <button type="submit" class={actionClasses <> " btn-outline-success"}>Resolve</button>
        |]
        refundForm = renderPostForm (pathTo (ConfirmRefundMarketAction market.id)) [("class", "d-inline")] [hsx|
            <button type="submit" class={actionClasses <> " btn-outline-danger"}>Refund</button>
        |]
    in case market.status of
        MarketStatusDraft -> [hsx|
            <div class="d-inline-flex flex-nowrap align-items-center gap-2">
                <a href={EditMarketAction market.id (Just currentPage) searchFilter} class={actionClasses <> " btn-outline-secondary"}>Edit</a>
                {openDraftForm}
                <a href={ConfirmDeleteMarketAction market.id pageValue searchFilter} class={actionClasses <> " btn-outline-danger"}>Delete</a>
            </div>
        |]
        MarketStatusOpen -> [hsx|
            <div class="d-inline-flex flex-nowrap align-items-center gap-2">
                <a href={EditMarketAction market.id (Just currentPage) searchFilter} class={actionClasses <> " btn-outline-secondary"}>Edit</a>
                {closeOpenForm}
            </div>
        |]
        MarketStatusClosed -> [hsx|
            <div class="d-inline-flex flex-nowrap align-items-center gap-2">
                <a href={EditMarketAction market.id (Just currentPage) searchFilter} class={actionClasses <> " btn-outline-secondary"}>Edit</a>
                {reopenClosedForm}
                {resolveForm}
                {refundForm}
            </div>
        |]
        MarketStatusRefunded -> mempty
        MarketStatusResolved -> mempty

renderMarketsPagination :: Int -> Int -> MarketStatus -> Maybe Text -> Html
renderMarketsPagination currentPage totalPages statusFilter searchFilter =
    renderSmartPagination currentPage totalPages "Markets pagination"
        (\pageNum -> pathTo (DashboardMarketsAction (Just pageNum) searchFilter (Just statusFilter)))

renderSearchForm :: MarketStatus -> Maybe Text -> Html
renderSearchForm activeStatus searchFilter = [hsx|
    <div class="d-flex" id="markets-search-form-container">
        <form class="w-100 position-relative"
              action={DashboardMarketsAction Nothing Nothing (Just activeStatus)}
              method="GET"
              data-auto-submit-delay="800">
            <i class="bi bi-search text-muted position-absolute"
               style="left: 12px; top: 50%; transform: translateY(-50%); z-index: 3;">
            </i>
            <input type="search"
                   id="markets-search-input"
                   class="form-control"
                   name="search"
                   value={fromMaybe "" searchFilter}
                   placeholder="Search markets..."
                   aria-label="Search markets"
                   style="padding-left: 36px;">
        </form>
    </div>
|]
