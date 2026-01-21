module Web.View.Markets.Index where
import Web.View.Prelude

data IndexView = IndexView { markets :: [Include "categoryId" Market], pagination :: Pagination }

instance View IndexView where
    html IndexView { .. } = [hsx|
        {breadcrumb}

        <h1>Index<a href={pathTo NewMarketAction} class="btn btn-primary ms-4">+ New</a></h1>
        <div class="table-responsive">
            <table class="table">
                <thead>
                    <tr>
                        <th>Title</th>
                        <th>Slug</th>
                        <th>Category</th>
                        <th>Closes at</th>
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

renderMarket :: (?context :: ControllerContext) => Include "categoryId" Market -> Html
renderMarket market = [hsx|
    <tr>
        <td>{market.title}</td>
        <td>{market.slug}</td>
        <td>{category.name}</td>
        <td>{market.closedAt}</td>
        <td><a href={ShowMarketAction market.id}>Show</a></td>
        <td>{editButton}</td>
    </tr>
|]
    where
        category = market.categoryId
        editButton = if market.userId == currentUserId
            then [hsx|<a href={EditMarketAction market.id} class="text-muted">Edit</a>|]
            else mempty

