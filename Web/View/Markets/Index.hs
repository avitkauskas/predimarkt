module Web.View.Markets.Index where
import Web.View.Prelude

data IndexView = IndexView { markets :: [Include "categoryId" Market], pagination :: Pagination }

instance View IndexView where
    html IndexView { .. } = [hsx|
        <div class="d-flex justify-content-between align-items-center mb-4">
            <div>Category selection and tag filtering will go here...</div>
            <a href={pathTo NewMarketAction} class="btn btn-primary">+ New Market</a>
        </div>
        <div class="row">
            {forEach markets renderMarket}
        </div>
        {renderPagination pagination}
    |]

renderMarket :: (?context :: ControllerContext) => Include "categoryId" Market -> Html
renderMarket market = [hsx|
    <div class="col-12 col-sm-6 col-md-4 col-lg-3 mb-4">
        <div class="card h-100">
            <div class="card-header">
                {category.name}
            </div>
            <div class="card-body">
                <h5 class="card-title scroll-no-bar">
                    <a href={ShowMarketAction market.id}
                       class="text-decoration-none text-reset stretched-link">
                        {market.title}
                    </a>
                </h5>
            </div>
        </div>
    </div>
|]
    where
        category = market.categoryId


