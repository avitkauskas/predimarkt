module Web.View.Markets.Index where
import Application.Helper.View (formatMoney)
import qualified Domain.LMSR as LMSR
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
    <div class="col-12 col-sm-6 col-lg-4">
        <div class="card h-100">

            <!-- Clickable header -->
            <div class="position-relative">
                <a href={ShowMarketAction market.id Nothing Nothing}
                    class="stretched-link" aria-hidden="true">
                </a>
                <div class={classes [
                        "card-header position-relative text-muted small d-flex",
                        "justify-content-between align-items-center py-1 border-bottom-0",
                        (headerClass, True)]}>
                    <span>{category.name}</span>
                    {statusBadge}
                </div>
            </div>

            <!-- Card body -->
            <div class={classes ["card-body d-flex flex-column position-relative", (bodyClass, True)]}>

                <!-- Scrollable, clickable title -->
                <div class="position-relative scroll-no-bar mb-2">
                    <a href={ShowMarketAction market.id Nothing Nothing}
                       class="stretched-link" aria-hidden="true">
                    </a>
                    <h6 class="card-title fs-6 mb-0 d-inline-block">
                        {market.title}
                    </h6>
                </div>

                <!-- Assets container -->
                <div class="scroll-no-bar position-relative pt-1" style="overflow-y: auto; max-height: 54px;">
                    <div class="pe-auto">
                        {forEach market.assets renderAsset}
                    </div>
                </div>
            </div>

            <!-- Footer (not clickable) -->
            <div class={classes ["card-footer text-muted small",
                                 "d-flex align-items-center py-1 border-top-0",
                                 (footerClass, True)]}>
                <span class="me-3" title="Number of trades">
                    <i class="bi bi-arrow-left-right"></i>
                    {formatWithSep market.trades}
                </span>
                <span class="me-3" title="Total shares traded">
                    <i class="bi bi-layers"></i>
                    {formatWithSep market.volume}
                </span>
                <span class="me-3" title="Total money turnover">
                    <i class="bi bi-cash-stack"></i>
                    {formatMoney market.turnover}
                </span>
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
        footerClass = marketStatusFooterClasses market.status

        lmsrState = LMSR.precompute market.beta [(a.symbol, a.quantity) | a <- market.assets]

        renderAsset asset =
            let
                assetPrice :: Int
                assetPrice = round (LMSR.price asset.symbol lmsrState * 100)

                buttons = if market.status == MarketStatusOpen
                    then [hsx|
                        <div class="btn-group shadow-sm" style="width: 80px">
                            <a href={ShowMarketAction market.id (Just asset.id) (Just "buy")}
                                    class="btn btn-soft-success p-0 rounded-start-1 d-flex align-items-center justify-content-center"
                                    style="font-size: 0.75rem; line-height: 1.5; width: 50%;">
                                Buy
                            </a>
                            <a href={ShowMarketAction market.id (Just asset.id) (Just "sell")}
                                    class="btn btn-soft-danger p-0 rounded-end-1 d-flex align-items-center justify-content-center"
                                    style="font-size: 0.75rem; line-height: 1.5; width: 50%;">
                                Sell
                            </a>
                        </div>
                    |]
                    else mempty
            in [hsx|
            <div class="d-flex justify-content-between align-items-center mb-1 small">
                <div class="text-nowrap overflow-auto scroll-no-bar me-2" style="flex: 1;">
                   {asset.name}
                </div>
                <div class="d-flex align-items-center gap-1 ps-1 flex-shrink-0">
                    <span class="me-1">
                        {assetPrice}%
                    </span>
                    {buttons}
                </div>
            </div>
        |]
