module Web.View.Dashboard.Markets where

import Web.View.Prelude

data MarketsView = MarketsView
    { markets      :: [Market]
    , activeStatus :: MarketStatus
    }

instance View MarketsView where
    html MarketsView { .. } = dashboardLayout [hsx|
        <div class="container-fluid ps-2">
            <div class="d-flex justify-content-between align-items-center mb-2 ms-2">
                <h5>My Markets</h5>
                <a href={NewMarketAction} class="btn btn-primary">
                    <i class="bi bi-plus-lg"></i> New Market
                </a>
            </div>
            {renderFlashMessages}
            {renderTabs activeStatus}
            <table class="table table-hover ms-0">
                <tbody>
                    {forEach markets renderMarket}
                </tbody>
            </table>
        </div>
    |]

renderMarket :: (?context :: ControllerContext) => Market -> Html
renderMarket market = [hsx|
    <tr>
        <td class="align-middle">
            <a class="text-decoration-none"
               href={ShowMarketAction market.id Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing}>
                {market.title}
            </a>
        </td>
        <td class="text-end">
            {renderActions market}
        </td>
    </tr>
|]

renderTabs :: MarketStatus -> Html
renderTabs activeStatus = [hsx|
    <ul class="nav nav-tabs mb-2 dashboard-tabs">
        {forEach statuses renderTab}
    </ul>
|]
    where
        statuses = [MarketStatusDraft, MarketStatusOpen, MarketStatusClosed, MarketStatusRefunded, MarketStatusResolved]
        renderTab status = [hsx|
            <li class="nav-item">
                <a class={classes [("nav-link", True), ("active", status == activeStatus)]}
                   href={DashboardMarketsAction (Just status)}>
                    {statusLabel status}
                </a>
            </li>
        |]

        statusLabel :: MarketStatus -> Text
        statusLabel MarketStatusDraft    = "Draft"
        statusLabel MarketStatusOpen     = "Open"
        statusLabel MarketStatusClosed   = "Closed"
        statusLabel MarketStatusRefunded = "Refunded"
        statusLabel MarketStatusResolved = "Resolved"

renderActions :: (?context :: ControllerContext) => Market -> Html
renderActions market =
    case market.status of
        MarketStatusDraft -> [hsx|
            <a href={EditMarketAction market.id} class="btn btn-sm btn-outline-secondary me-2">Edit</a>
            <form method="POST" action={ChangeMarketStatusAction (Just market.id) (Just MarketStatusOpen)} class="d-inline">
                <button type="submit" class="btn btn-sm btn-outline-primary me-2">Open</button>
            </form>
            <a href={DeleteMarketAction market.id} class="btn btn-sm btn-outline-danger js-delete" data-confirm="Are you sure?">Delete</a>
        |]
        MarketStatusOpen -> [hsx|
            <form method="POST" action={ChangeMarketStatusAction (Just market.id) (Just MarketStatusClosed)} class="d-inline">
                <button type="submit" class="btn btn-sm btn-outline-primary me-2">Close</button>
            </form>
        |]
        MarketStatusClosed -> [hsx|
            <form method="POST" action={ChangeMarketStatusAction (Just market.id) (Just MarketStatusOpen)} class="d-inline">
                <button type="submit" class="btn btn-sm btn-outline-primary me-2">Open</button>
            </form>
            <a href={SetResolveAssetAction market.id} class="btn btn-sm btn-outline-success me-2">Resolve</a>
            <a href={ConfirmRefundMarketAction market.id} class="btn btn-sm btn-outline-danger">Refund</a>
        |]
        MarketStatusRefunded -> mempty
        MarketStatusResolved -> mempty
