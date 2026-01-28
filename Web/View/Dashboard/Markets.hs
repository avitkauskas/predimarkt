module Web.View.Dashboard.Markets where

import Web.View.Prelude

data MarketsView = MarketsView
    { markets :: [Market]
    , activeStatus :: MarketStatus
    }

instance View MarketsView where
    html MarketsView { .. } = dashboardLayout [hsx|
        <div class="h-100">
            <div class="d-flex justify-content-between align-items-center mb-3">
                <h3>My Markets</h3>
                <a href={NewMarketAction} class="btn btn-primary">+ New Market</a>
            </div>
            <div class="table-responsive">
                {renderTabs activeStatus}
                <table class="table table-hover">
                    <tbody>
                        {forEach markets renderMarket}
                    </tbody>
                </table>
            </div>
        </div>
    |]

renderMarket :: (?context :: ControllerContext) => Market -> Html
renderMarket market = [hsx|
    <tr>
        <td>{market.title}</td>
        <td class="text-end">
            {renderActions market}
        </td>
    </tr>
|]

renderTabs :: MarketStatus -> Html
renderTabs activeStatus = [hsx|
    <ul class="nav nav-tabs mb-4">
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
        statusLabel MarketStatusDraft = "Draft"
        statusLabel MarketStatusOpen = "Open"
        statusLabel MarketStatusClosed = "Closed"
        statusLabel MarketStatusRefunded = "Refunded"
        statusLabel MarketStatusResolved = "Resolved"

renderActions :: (?context :: ControllerContext) => Market -> Html
renderActions market =
    case market.status of
        MarketStatusDraft -> [hsx|
            <a href={EditMarketAction market.id} class="btn btn-sm btn-outline-secondary me-2">Edit</a>
            <button class="btn btn-sm btn-outline-primary" disabled>Publish</button>
        |]
        MarketStatusOpen -> [hsx|
            <button class="btn btn-sm btn-outline-secondary me-2" disabled>Close</button>
            <button class="btn btn-sm btn-outline-secondary me-2" disabled>Refund</button>
            <button class="btn btn-sm btn-outline-secondary" disabled>Resolve</button>
        |]
        MarketStatusClosed -> [hsx|
            <button class="btn btn-sm btn-outline-secondary me-2" disabled>Refund</button>
            <button class="btn btn-sm btn-outline-secondary" disabled>Resolve</button>
        |]
        _ -> mempty
