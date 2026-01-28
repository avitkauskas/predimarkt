module Web.View.Markets.Index where
import Web.View.Prelude

import qualified Data.Map as M
import Text.Printf (printf)

data IndexView = IndexView
    { markets        :: [Include' ["categoryId", "assets"] Market]
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

renderMarket :: (?context :: ControllerContext) => Include' ["categoryId", "assets"] Market -> Html
renderMarket market = [hsx|
    <div class="col-12 col-sm-6 col-md-4 col-lg-3">
        <div class="card h-100">
            <a href={ShowMarketAction market.id} class="stretched-link" aria-hidden="true"></a>
            <div class={classes ["card-header", "text-muted", "small", "d-flex", "justify-content-between", "align-items-center", headerClass]}>
                <span>{category.name}</span>
                {statusBadge}
            </div>
            <div class={classes ["card-body", "position-relative", "d-flex", "flex-column", bodyClass]}>
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

        (headerClass, bodyClass) = case market.status of
            MarketStatusClosed   -> ("market-status-closed-header",   "market-status-closed-body")
            MarketStatusResolved -> ("market-status-resolved-header", "market-status-resolved-body")
            MarketStatusRefunded -> ("market-status-refunded-header", "market-status-refunded-body")
            _                    -> ("", "")

        lmsrState = precompute market.beta market.assets

        renderAsset asset = [hsx|
            <div class="d-flex justify-content-between align-items-center mb-1 small">
                <div class="text-nowrap overflow-auto scroll-no-bar me-2" style="flex: 1;">
                   {asset.name}
                </div>
                <div class="d-flex align-items-center gap-1 ps-1 flex-shrink-0">
                    <span class="me-1">{show (round (price asset.id lmsrState * 100) :: Int) <> "%"}</span>
                    <button class="btn btn-success p-1 border-0" style="font-size: 0.7rem; line-height: 1;">Buy</button>
                    <button class="btn btn-danger p-1 border-0" style="font-size: 0.7rem; line-height: 1;">Sell</button>
                </div>
            </div>
        |]

data LMSRState = LMSRState
    { sMap :: M.Map (Id Asset) Double
    , sSum :: !Double
    }

precompute :: Double -> [Asset] -> LMSRState
precompute beta assets =
    let quantities = map (.quantity) assets
        m = if null quantities then 0 else maximum quantities
        sMap = M.fromList
            [ (asset.id, exp ((asset.quantity - m) / beta))
            | asset <- assets
            ]
        sSum = sum (M.elems sMap)
    in LMSRState sMap sSum

price :: Id Asset -> LMSRState -> Double
price aid st =
    case M.lookup aid (sMap st) of
        Just v -> v / sSum st
        Nothing -> 0.0
