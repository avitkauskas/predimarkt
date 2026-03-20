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
            currentBackToPath = pathTo (DashboardMarketsAction (Just activeStatus) (Just currentPage) searchFilter)

renderMarket :: (?context :: ControllerContext) => Text -> Int -> Maybe Text -> Market -> Html
renderMarket backToPath currentPage searchFilter market = [hsx|
    <tr>
        <td class="align-middle">
            <a class="text-decoration-none"
               href={ShowMarketAction market.id Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing (Just backToPath)}>
                {market.title}
            </a>
        </td>
        <td class="text-end">
            {renderActions market currentPage searchFilter backToPath}
        </td>
    </tr>
|]

renderTabs :: MarketStatus -> Maybe Text -> Html
renderTabs activeStatus searchFilter = [hsx|
    <ul class="nav nav-tabs mb-2 dashboard-tabs">
        {forEach statuses (renderTab searchFilter)}
    </ul>
|]
    where
        statuses = [MarketStatusDraft, MarketStatusOpen, MarketStatusClosed, MarketStatusResolved, MarketStatusRefunded]
        renderTab search status = [hsx|
            <li class="nav-item">
                <a class={classes [("nav-link", True), ("active", status == activeStatus)]}
                   href={DashboardMarketsAction (Just status) Nothing search}>
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
    in case market.status of
        MarketStatusDraft -> [hsx|
            <a href={EditMarketAction market.id (Just currentPage) searchFilter} class="btn btn-sm btn-outline-secondary me-2">Edit</a>
            <form method="POST" action={ChangeMarketStatusAction (Just market.id) (Just MarketStatusOpen) pageValue searchFilter} class="d-inline">
                <button type="submit" class="btn btn-sm btn-outline-primary me-2">Open</button>
            </form>
            <a href={DeleteMarketAction market.id pageValue searchFilter} class="btn btn-sm btn-outline-danger js-delete" data-confirm="Are you sure?">Delete</a>
        |]
        MarketStatusOpen -> [hsx|
            <a href={EditMarketAction market.id (Just currentPage) searchFilter} class="btn btn-sm btn-outline-secondary me-2">Edit</a>
            <form method="POST" action={ChangeMarketStatusAction (Just market.id) (Just MarketStatusClosed) pageValue searchFilter} class="d-inline">
                <button type="submit" class="btn btn-sm btn-outline-primary me-2">Close</button>
            </form>
        |]
        MarketStatusClosed -> [hsx|
            <a href={EditMarketAction market.id (Just currentPage) searchFilter} class="btn btn-sm btn-outline-secondary me-2">Edit</a>
            <form method="POST" action={ChangeMarketStatusAction (Just market.id) (Just MarketStatusOpen) pageValue searchFilter} class="d-inline">
                <button type="submit" class="btn btn-sm btn-outline-primary me-2">Open</button>
            </form>
            <a href={SetResolveAssetAction market.id} class="btn btn-sm btn-outline-success me-2">Resolve</a>
            <a href={ConfirmRefundMarketAction market.id} class="btn btn-sm btn-outline-danger">Refund</a>
        |]
        MarketStatusRefunded -> mempty
        MarketStatusResolved -> mempty

renderMarketsPagination :: Int -> Int -> MarketStatus -> Maybe Text -> Html
renderMarketsPagination currentPage totalPages statusFilter searchFilter =
    renderSmartPagination currentPage totalPages "Markets pagination"
        (\pageNum -> pathTo (DashboardMarketsAction (Just statusFilter) (Just pageNum) searchFilter))

renderSearchForm :: MarketStatus -> Maybe Text -> Html
renderSearchForm activeStatus searchFilter = [hsx|
    <div class="d-flex" id="markets-search-form-container">
        <form class="w-100 position-relative"
              action={DashboardMarketsAction (Just activeStatus) Nothing Nothing}
              method="GET"
              data-auto-submit-delay="300">
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
