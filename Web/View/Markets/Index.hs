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
            <div class="position-relative overflow-hidden rounded-top">
                <a href={ShowMarketAction market.id Nothing Nothing}
                    class="stretched-link" aria-hidden="true">
                </a>
                <div class={classes [
                        "card-header border-0 position-relative text-muted small d-flex",
                        "justify-content-between align-items-center py-1",
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
                <div class="scroll-no-bar position-relative"
                     style="overflow-y: auto; max-height: 54px; margin-left: -0.5rem; margin-right: -0.5rem;">
                    <div class="pe-auto">
                        {forEach market.assets renderAsset}
                    </div>
                </div>
            </div>

            <!-- Footer (not clickable) -->
            <div class={classes ["card-footer text-muted small py-1 border-top-0",
                                 "d-flex align-items-center justify-content-between gap-3",
                                 "text-nowrap overflow-auto scroll-no-bar",
                                 (footerClass, True)]}>
                <div class="d-flex align-items-center gap-3">
                    <span title="Number of trades">
                        <i class="bi bi-arrow-left-right"></i>
                        {formatWithSep market.trades}
                    </span>
                    <span title="Total shares traded">
                        <i class="bi bi-layers"></i>
                        {formatWithSep market.volume}
                    </span>
                    <span title="Total money turnover">
                        <i class="bi bi-cash-stack"></i>
                        {formatMoney market.turnover}
                    </span>
                </div>
                <span title="Market closing time" style="font-size: 0.7rem;">
                    <i class="bi bi-alarm"></i>
                    {renderTime market.closedAt}
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

        lmsrState = LMSR.precompute market.beta [(a.id, a.quantity) | a <- market.assets]

        renderAsset asset =
            let
                assetPrice :: Int
                assetPrice = round (LMSR.price asset.id lmsrState * 100)

                buttons = if market.status == MarketStatusOpen
                    then [hsx|
                        <div class="d-flex gap-1 ms-1" style="width: 80px;">
                            <a href={ShowMarketAction market.id (Just asset.id) (Just "buy")}
                                    class="btn btn-outline-success p-0 rounded-1 fw-medium"
                                    style="font-size: 0.65rem; width: calc(50% - 2px);">
                                BUY
                            </a>
                            <a href={ShowMarketAction market.id (Just asset.id) (Just "sell")}
                                    class="btn btn-outline-danger p-0 rounded-1 fw-medium"
                                    style="font-size: 0.65rem; width: calc(50% - 2px);">
                                SELL
                            </a>
                        </div>
                    |]
                    else mempty

                isResolvedWinner = market.status == MarketStatusResolved
                    && market.outcomeAssetId == Just asset.id

            in [hsx|
            <div class={classes ["d-flex justify-content-between align-items-center mb-1 small rounded-1",
                                 ("market-status-resolved-asset", isResolvedWinner)]}
                 style="padding: 0.0rem 0.75rem;">
                <div class="text-nowrap overflow-auto scroll-no-bar me-2" style="flex: 1;">
                   {asset.name}
                </div>
                <div class="d-flex align-items-center gap-1 ps-1 flex-shrink-0">
                    {assetPrice}%
                    {buttons}
                </div>
            </div>
        |]
