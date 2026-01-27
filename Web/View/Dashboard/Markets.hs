module Web.View.Dashboard.Markets where

import Web.View.Prelude

data MarketsView = MarketsView { markets :: [Market] }

instance View MarketsView where
    html MarketsView { .. } = dashboardLayout [hsx|
        <div class="h-100">
            <div class="d-flex justify-content-between align-items-center mb-3">
                <h3>My Markets</h3>
                <a href={NewMarketAction} class="btn btn-primary">+ New Market</a>
            </div>
            <div class="table-responsive">
                <table class="table table-hover">
                    <thead>
                        <tr>
                            <th>Title</th>
                            <th>Status</th>
                            <th>Created At</th>
                            <th></th>
                            <th></th>
                        </tr>
                    </thead>
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
        <td>{market.status}</td>
        <td>{market.createdAt |> timeAgo}</td>
        <td class="text-end">
            <a href={ShowMarketAction market.id} 
               class="btn btn-outline-secondary btn-sm me-2">
                View
            </a>
        </td>
        <td class="text-end">
            <a href={EditMarketAction market.id}
               class="btn btn-outline-secondary btn-sm me-2">
                Edit
            </a>
        </td>
    </tr>
|]
