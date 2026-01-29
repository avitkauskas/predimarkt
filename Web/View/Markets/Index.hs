module Web.View.Markets.Index where
import Web.View.Prelude

data IndexView = IndexView
    { markets        :: [Include' ["categoryId", "assets"] Market]
    , categories     :: [Category]
    , categoryFilter :: Maybe (Id Category)
    }

instance View IndexView where
    html IndexView { .. } = [hsx|
        <div class="d-flex justify-content-between align-items-center mb-4">
            <ul class="nav nav-underline scroll-no-bar flex-nowrap mb-0 ms-2">
                <li class="nav-item">
                    <a class={classes ["nav-link text-reset", ("active", isNothing categoryFilter)]}
                       href={MarketsAction}>
                        All
                    </a>
                </li>
                {forEach categories (renderCategoryTab categoryFilter)}
            </ul>
            <a href={NewMarketAction} class="btn btn-primary ms-3 text-nowrap">+ New Market</a>
        </div>
        <div class="row g-3">
            {forEach markets renderMarket}
        </div>
    |]

renderCategoryTab :: (?context :: ControllerContext) => Maybe (Id Category) -> Category -> Html
renderCategoryTab categoryFilter category = [hsx|
    <li class="nav-item">
        <a class={classes ["nav-link text-reset", ("active", categoryFilter == Just category.id)]}
           href={pathTo MarketsAction <> "?category=" <> show category.id}>
            {category.name}
        </a>
    </li>
|]

renderMarket :: (?context :: ControllerContext) => Include' ["categoryId", "assets"] Market -> Html
renderMarket market = [hsx|
    <div class="col-12 col-sm-6 col-md-4 col-lg-3">
        <div class="card h-100">
            <a href={ShowMarketAction market.id} class="stretched-link" aria-hidden="true"></a>
            <div class={classes [("card-header", True), ("text-muted", True), ("small", True), ("d-flex", True), ("justify-content-between", True), ("align-items-center", True), ("py-1", True), (headerClass, True)]}>
                <span>{category.name}</span>
                {statusBadge}
            </div>
            <div class={classes [("card-body", True), ("position-relative", True), ("d-flex", True), ("flex-column", True), (bodyClass, True)]}>
                <h6 class="card-title scroll-no-bar fs-6 position-relative mb-2" style="z-index: 2;">
                    <a href={ShowMarketAction market.id}
                       class="text-decoration-none text-reset stretched-link">
                        {market.title}
                    </a>
                </h6>
                <div class="scroll-no-bar position-relative pt-1" style="overflow-y: auto; max-height: 54px; z-index: 3;">
                    {forEach market.assets renderAsset}
                </div>
            </div>
        </div>
    </div>
|]
    where
        category = market.categoryId

        statusBadge =
            when (market.status /= MarketStatusOpen)
                [hsx|<span>{marketStatusLabel market.status}</span>|]
        
        bodyClass = marketStatusClasses market.status
        headerClass = marketStatusHeaderClasses market.status

        lmsrState = precompute market.beta market.assets

        renderAsset asset =
            let
                assetPrice :: Int
                assetPrice = round (price asset.id lmsrState * 100)

                buttons = if market.status == MarketStatusOpen
                    then [hsx|
                        <div class="btn-group shadow-sm" style="width: 80px">
                            <button class="btn btn-soft-success p-0 rounded-start-1" style="font-size: 0.75rem; line-height: 1.5; width: 50%;">Buy</button>
                            <button class="btn btn-soft-danger p-0 rounded-end-1" style="font-size: 0.75rem; line-height: 1.5; width: 50%;">Sell</button>
                        </div>
                    |]
                    else mempty
            in [hsx|
            <div class="d-flex justify-content-between align-items-center mb-1 small">
                <div class="text-nowrap overflow-auto scroll-no-bar me-2" style="flex: 1;">
                   {asset.name}
                </div>
                <div class="d-flex align-items-center gap-1 ps-1 flex-shrink-0">
                    <span class="me-1" style="font-size: 0.75rem;">{assetPrice}%</span>
                    {buttons}
                </div>
            </div>
        |]
