module Web.View.Markets.Index where
import Web.View.Prelude

data IndexView = IndexView { markets :: [Include "categoryId" Market], pagination :: Pagination }

instance View IndexView where
    html IndexView { .. } = [hsx|
        <div class="d-flex justify-content-between align-items-center mb-4">
            <div>Category selection and tag filtering will go here...</div>
            <a href={pathTo NewMarketAction} class="btn btn-primary">+ New Market</a>
        </div>
        <div class="row g-3">
            {forEach markets renderMarket}
        </div>
        {renderPagination pagination}
    |]

renderMarket :: (?context :: ControllerContext) => Include "categoryId" Market -> Html
renderMarket market = [hsx|
    <div class="col-12 col-sm-6 col-md-4 col-lg-3">
        <div class="card h-100">
            <a href={ShowMarketAction market.id} class="stretched-link" aria-hidden="true"></a>
            <div class="card-header text-muted small">
                {category.name}
            </div>
            <div class="card-body position-relative">
                <h6 class="card-title scroll-no-bar fs-6 position-relative" style="z-index: 2;">
                    <a href={ShowMarketAction market.id}
                       class="text-decoration-none text-reset stretched-link">
                        {market.title}
                    </a>
                </h6>
            </div>
        </div>
    </div>
|]
    where
        category = market.categoryId


