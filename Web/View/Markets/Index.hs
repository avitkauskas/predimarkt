module Web.View.Markets.Index where
import Web.View.Prelude

data IndexView = IndexView { markets :: [Market], pagination :: Pagination }

instance View IndexView where
    html IndexView { .. } = [hsx|
        {breadcrumb}

        <h1>Index<a href={pathTo NewMarketAction} class="btn btn-primary ms-4">+ New</a></h1>
        <div class="table-responsive">
            <table class="table">
                <thead>
                    <tr>
                        <th>Market</th>
                        <th></th>
                        <th></th>
                        <th></th>
                    </tr>
                </thead>
                <tbody>{forEach markets renderMarket}</tbody>
            </table>
            {renderPagination pagination}
        </div>
    |]
        where
            breadcrumb = renderBreadcrumb
                [ breadcrumbLink "Markets" MarketsAction
                ]

renderMarket :: Market -> Html
renderMarket market = [hsx|
    <tr>
        <td>{market}</td>
        <td><a href={ShowMarketAction market.id}>Show</a></td>
        <td><a href={EditMarketAction market.id} class="text-muted">Edit</a></td>
        <td><a href={DeleteMarketAction market.id} class="js-delete text-muted">Delete</a></td>
    </tr>
|]