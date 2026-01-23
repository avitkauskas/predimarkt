module Web.View.Markets.Index where
import Web.View.Prelude

data IndexView = IndexView
    { markets        :: [Include "categoryId" Market]
    , categories     :: [Category]
    , categoryFilter :: Maybe (Id Category)
    }

instance View IndexView where
    html IndexView { .. } = [hsx|
        <div class="d-flex justify-content-between align-items-center mb-4">
            <ul class="nav nav-underline scroll-no-bar flex-nowrap mb-0">
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

renderMarket :: (?context :: ControllerContext) => Include "categoryId" Market -> Html
renderMarket market = [hsx|
    <div class="col-12 col-sm-6 col-md-4 col-lg-3">
        <div class="card h-100">
            <a href={ShowMarketAction market.id} class="stretched-link" aria-hidden="true"></a>
            <div class={classes ["card-header", "text-muted", "small", "d-flex", "justify-content-between", "align-items-center", headerClass]}>
                <span>{category.name}</span>
                {statusBadge}
            </div>
            <div class={classes ["card-body", "position-relative", bodyClass]}>
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

        statusBadge =
            when (inputValue market.status /= "open")
                [hsx|<span>{inputValue market.status}</span>|]

        (headerClass, bodyClass) = case inputValue market.status of
            "closed"   -> ("market-status-closed-header",   "market-status-closed-body")
            "resolved" -> ("market-status-resolved-header", "market-status-resolved-body")
            "refunded" -> ("market-status-refunded-header", "market-status-refunded-body")
            _          -> ("", "")
