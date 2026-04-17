module Web.View.Markets.Index where
import Application.Domain.LMSR
import Application.Domain.Types
import qualified Data.Map.Strict as M
import Data.Text (intercalate)
import Web.View.Prelude

data IndexView = IndexView
    { markets        :: [Include' ["categoryId", "assets"] Market]
    , categories     :: [Category]
    , categoryFilter :: Maybe (Id Category)
    , statusFilter   :: MarketIndexStatusFilter
    , searchFilter   :: Maybe Text
    , currentPage    :: Int
    , totalMarkets   :: Int
    , hasMoreMarkets :: Bool
    }

data MarketIndexStatusFilter
    = MarketIndexStatusPopular
    | MarketIndexStatusNewest
    | MarketIndexStatusEnding
    | MarketIndexStatusClosed
    | MarketIndexStatusResolved
    | MarketIndexStatusRefunded
    deriving (Eq, Show)

parseMarketIndexStatusFilter :: Maybe Text -> MarketIndexStatusFilter
parseMarketIndexStatusFilter = \case
    Just "newest"   -> MarketIndexStatusNewest
    Just "ending"   -> MarketIndexStatusEnding
    Just "closed"   -> MarketIndexStatusClosed
    Just "resolved" -> MarketIndexStatusResolved
    Just "refunded" -> MarketIndexStatusRefunded
    _               -> MarketIndexStatusPopular

marketIndexStatusFilterParamValue :: MarketIndexStatusFilter -> Maybe Text
marketIndexStatusFilterParamValue = \case
    MarketIndexStatusPopular  -> Nothing
    MarketIndexStatusNewest   -> Just "newest"
    MarketIndexStatusEnding   -> Just "ending"
    MarketIndexStatusClosed   -> Just "closed"
    MarketIndexStatusResolved -> Just "resolved"
    MarketIndexStatusRefunded -> Just "refunded"

marketIndexStatusFilterFormValue :: MarketIndexStatusFilter -> Text
marketIndexStatusFilterFormValue = fromMaybe "" . marketIndexStatusFilterParamValue

marketIndexStatusFilterLabel :: MarketIndexStatusFilter -> Text
marketIndexStatusFilterLabel = \case
    MarketIndexStatusPopular  -> "Popular"
    MarketIndexStatusNewest   -> "Newest"
    MarketIndexStatusEnding   -> "Ending"
    MarketIndexStatusClosed   -> "Closed"
    MarketIndexStatusResolved -> "Resolved"
    MarketIndexStatusRefunded -> "Refunded"

marketIndexStatusFilterOptions :: [MarketIndexStatusFilter]
marketIndexStatusFilterOptions =
    [ MarketIndexStatusPopular
    , MarketIndexStatusNewest
    , MarketIndexStatusEnding
    , MarketIndexStatusClosed
    , MarketIndexStatusResolved
    , MarketIndexStatusRefunded
    ]

instance View IndexView where
    html IndexView { .. } = [hsx|
        <div id="markets-content">
            {renderMarketsResults markets categories categoryFilter statusFilter searchFilter currentPage totalMarkets hasMoreMarkets}
        </div>
    |]

renderSearchForm :: Maybe (Id Category) -> MarketIndexStatusFilter -> Maybe Text -> Html
renderSearchForm categoryFilter statusFilter searchFilter = [hsx|
    <div class="d-flex" id="markets-search-form-container">
        <form class="w-100 position-relative"
              action={pathTo MarketsAction}
              method="GET"
              data-auto-submit-delay="800">
            {forEach (maybeToList categoryFilter) renderCategoryInput}
            {renderStatusInput statusFilter}
            <i class="bi bi-search text-muted position-absolute"
               style="left: 12px; top: 50%; transform: translateY(-50%); z-index: 3;">
            </i>
            <input type="search"
                       id="markets-search-input"
                       class="form-control"
                       name="search"
                       value={fromMaybe "" searchFilter}
                       placeholder="Search markets or assets..."
                       aria-label="Search markets"
                       style="padding-left: 36px;">
        </form>
    </div>
|]

renderCategoryInput :: Id Category -> Html
renderCategoryInput categoryId = [hsx|
    <input type="hidden" name="category" value={inputValue categoryId} />
|]

renderSearchInput :: Text -> Html
renderSearchInput searchQuery = [hsx|
    <input type="hidden" name="search" value={searchQuery} />
|]

renderStatusInput :: MarketIndexStatusFilter -> Html
renderStatusInput statusFilter = case marketIndexStatusFilterParamValue statusFilter of
    Just statusValue -> [hsx|
        <input type="hidden" name="status" value={statusValue} />
    |]
    Nothing -> mempty

renderStatusForm :: Maybe (Id Category) -> MarketIndexStatusFilter -> Maybe Text -> Html
renderStatusForm categoryFilter statusFilter searchFilter = [hsx|
    <form class="d-flex align-items-center gap-2 flex-shrink-0"
          action={pathTo MarketsAction}
          method="GET">
        {forEach (maybeToList categoryFilter) renderCategoryInput}
        {forEach (maybeToList searchFilter) renderSearchInput}
        <div class="d-inline-flex align-items-center gap-2 rounded border border-secondary-subtle ps-2 pe-0 text-body-secondary bg-transparent"
             style="min-width: 133px; padding-top: 0.36rem; padding-bottom: 0.36rem;">
            <i class="bi bi-filter-right"></i>
            <select id="markets-status-filter"
                    name="status"
                    class="form-select form-select-sm flex-grow-1 border-0 bg-transparent text-body-secondary shadow-none py-0 ps-0 pe-4"
                    aria-label="Filter markets by status"
                    onchange="window.visitGetFormWithTurbolinks(this.form)">
                {forEach marketIndexStatusFilterOptions (renderStatusOption statusFilter)}
            </select>
        </div>
    </form>
|]

renderStatusOption :: MarketIndexStatusFilter -> MarketIndexStatusFilter -> Html
renderStatusOption activeStatus optionStatus = [hsx|
    <option value={marketIndexStatusFilterFormValue optionStatus}
            selected={activeStatus == optionStatus}>
        {marketIndexStatusFilterLabel optionStatus}
    </option>
|]

renderMarketsResults
    :: (?context :: ControllerContext)
    => [Include' ["categoryId", "assets"] Market]
    -> [Category]
    -> Maybe (Id Category)
    -> MarketIndexStatusFilter
    -> Maybe Text
    -> Int
    -> Int
    -> Bool
    -> Html
renderMarketsResults markets categories categoryFilter statusFilter searchFilter currentPage totalMarkets hasMoreMarkets = [hsx|
    <div id="markets-results">
        <div class="d-flex justify-content-between align-items-center gap-3 mb-3">
            <div class="d-flex align-items-center flex-nowrap flex-grow-1 scroll-no-bar">
                <ul class="nav nav-underline flex-nowrap mb-0 ms-2">
                    <li class="nav-item">
                        <a class={classes ["nav-link text-reset", ("active", isNothing categoryFilter)]}
                           href={buildMarketsPath Nothing statusFilter searchFilter Nothing}>
                            All
                        </a>
                    </li>
                    {forEach categories (renderCategoryTab categoryFilter statusFilter searchFilter)}
                </ul>
            </div>
            <a href={NewMarketAction} class="btn btn-primary text-nowrap flex-shrink-0"><i class="bi bi-plus-lg"></i> New Market</a>
        </div>
        <div class="d-flex align-items-center gap-3 mb-3">
            <div class="flex-grow-1" style="min-width: 0;">
                {renderSearchForm categoryFilter statusFilter searchFilter}
            </div>
            {renderStatusForm categoryFilter statusFilter searchFilter}
        </div>
        {renderMarketsList markets currentMarketsPath}
        {renderLoadMoreButton categoryFilter statusFilter searchFilter currentPage (length markets) totalMarkets hasMoreMarkets}
    </div>
|]
    where
        currentMarketsPath = buildCurrentMarketsPath categoryFilter statusFilter searchFilter currentPage

renderMarketsList :: (?context :: ControllerContext) => [Include' ["categoryId", "assets"] Market] -> Text -> Html
renderMarketsList [] _ = [hsx|
    <div class="alert alert-info mb-5">
        No markets match the current filters.
    </div>
|]
renderMarketsList markets backToPath = [hsx|
    <div class="row g-3 mb-4">
        {forEach markets (renderMarket backToPath)}
    </div>
|]

renderLoadMoreButton
    :: (?context :: ControllerContext)
    => Maybe (Id Category)
    -> MarketIndexStatusFilter
    -> Maybe Text
    -> Int
    -> Int
    -> Int
    -> Bool
    -> Html
renderLoadMoreButton categoryFilter statusFilter searchFilter currentPage shownMarkets totalMarkets hasMoreMarkets =
    when hasMoreMarkets [hsx|
        <div class="d-flex justify-content-center mb-4">
            <a href={buildMarketsPath categoryFilter statusFilter searchFilter (Just (currentPage + 1))}
               class="btn btn-sm btn-outline-secondary text-nowrap">
                Showing {shownMarkets} of {totalMarkets} markets · Load more
            </a>
        </div>
    |]

renderCategoryTab :: (?context :: ControllerContext) => Maybe (Id Category) -> MarketIndexStatusFilter -> Maybe Text -> Category -> Html
renderCategoryTab categoryFilter statusFilter searchFilter category = [hsx|
    <li class="nav-item">
        <a class={classes ["nav-link text-reset", ("active", categoryFilter == Just category.id)]}
           href={categoryLink}>
            {category.name}
        </a>
    </li>
|]
    where
        categoryLink :: Text
        categoryLink = buildMarketsPath (Just category.id) statusFilter searchFilter Nothing

buildMarketsPath :: Maybe (Id Category) -> MarketIndexStatusFilter -> Maybe Text -> Maybe Int -> Text
buildMarketsPath categoryFilter statusFilter searchFilter page =
    let queryParams = catMaybes
            [ fmap (\categoryId -> "category=" <> inputValue categoryId) categoryFilter
            , fmap ("status=" <>) (marketIndexStatusFilterParamValue statusFilter)
            , fmap (\query -> "search=" <> inputValue query) searchFilter
            , fmap (\pageNumber -> "page=" <> inputValue pageNumber) page
            ]
        queryString = case queryParams of
            [] -> ""
            _  -> "?" <> intercalate "&" queryParams
    in pathTo MarketsAction <> queryString

buildCurrentMarketsPath :: Maybe (Id Category) -> MarketIndexStatusFilter -> Maybe Text -> Int -> Text
buildCurrentMarketsPath categoryFilter statusFilter searchFilter currentPage =
    buildMarketsPath categoryFilter statusFilter searchFilter (if currentPage > 1 then Just currentPage else Nothing)

-- Helper to construct ShowMarketAction with default Nothing values, reducing positional noise
showMarketWithBackTo :: Id Market -> Maybe Text -> MarketsController
showMarketWithBackTo marketId backTo = ShowMarketAction
    { marketId = marketId
    , tradingAssetId = Nothing
    , tradingAction = Nothing
    , showChart = Nothing
    , showDescription = Nothing
    , showAllAssets = Nothing
    , showTradeHistory = Nothing
    , activityPage = Nothing
    , chatPage = Nothing
    , chatComposerRev = Nothing
    , tradeQuantity = Nothing
    , backTo = backTo
    }

-- Helper to construct ShowMarketAction for trading operations with asset and action specified
showMarketTradeAction :: Id Market -> Id Asset -> Text -> Maybe Text -> MarketsController
showMarketTradeAction marketId assetId action backTo = ShowMarketAction
    { marketId = marketId
    , tradingAssetId = Just assetId
    , tradingAction = Just action
    , showChart = Nothing
    , showDescription = Nothing
    , showAllAssets = Nothing
    , showTradeHistory = Nothing
    , activityPage = Nothing
    , chatPage = Nothing
    , chatComposerRev = Nothing
    , tradeQuantity = Nothing
    , backTo = backTo
    }

renderMarket :: (?context :: ControllerContext) => Text -> Include' ["categoryId", "assets"] Market -> Html
renderMarket backToPath market = [hsx|
    <div class="col-12 col-sm-6 col-lg-4">
        <div class="card h-100">

            <!-- Clickable header -->
            <a href={showMarketLink}
               data-start-market-page-at-top="true"
               class={classes [
                    "card-header border-0 position-relative text-muted small d-flex",
                    "justify-content-between align-items-center py-1",
                    "text-reset text-decoration-none overflow-hidden rounded-top",
                    (headerClass, True)]}>
                <span>{category.name}</span>
                {statusBadge}
            </a>

            <!-- Card body -->
            <div class={classes ["card-body d-flex flex-column position-relative pb-2",
                        (bodyClass, True)]}>

                <!-- Wrapping, vertically-scrollable title (always 2 lines tall) -->
                <div class="position-relative market-title-container mb-2">
                    <h6 class="card-title fs-6 mb-0 market-card-title">
                        <a href={showMarketLink}
                           data-start-market-page-at-top="true"
                           class="stretched-link text-reset text-decoration-none">
                            {market.title}
                        </a>
                    </h6>
                </div>

                <!-- Assets container -->
                <div class="scroll-no-bar position-relative"
                     style="overflow-y: auto; max-height: 50px; margin-left: -0.5rem; margin-right: -0.5rem;">
                    {forEach market.assets renderAsset}
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
                <span title={marketFooterTimeTitle} style="font-size: 0.74rem;">
                    <i class="bi bi-alarm"></i>
                    {renderTime marketFooterTime}
                </span>
            </div>

        </div>
    </div>
|]
    where
        showMarketLink :: MarketsController
        showMarketLink = showMarketWithBackTo market.id (Just backToPath)

        category = market.categoryId

        statusBadge =
            when (market.status /= MarketStatusOpen)
                [hsx|<span>{marketStatusLabel market.status}</span>|]

        bodyClass = marketStatusClasses market.status
        headerClass = marketStatusHeaderClasses market.status
        footerClass = marketStatusFooterClasses market.status
        marketFooterTimeTitle :: Text
        marketFooterTimeTitle = case market.status of
            MarketStatusResolved -> "Market resolution time"
            MarketStatusRefunded -> "Market refund time"
            _                    -> "Market closing time"
        marketFooterTime :: UTCTime
        marketFooterTime = case market.status of
            MarketStatusResolved -> fromMaybe market.closedAt market.resolvedAt
            MarketStatusRefunded -> fromMaybe market.closedAt market.refundedAt
            _                    -> market.closedAt

        qtyMap = M.fromList [(a.id, Quantity a.quantity) | a <- market.assets]
        beta = Beta market.beta

        renderAsset asset =
            let
                isResolvedWinner = market.status == MarketStatusResolved
                    && market.outcomeAssetId == Just asset.id

                assetPriceVal :: Text
                assetPriceVal = case market.status of
                    MarketStatusResolved ->
                        if isResolvedWinner
                            then tshow 100 <> "%"
                            else tshow 0 <> "%"
                    MarketStatusRefunded -> "--"
                    _ -> tshow (round (assetPrice asset.id beta qtyMap * 100)) <> "%"

                buttons = if market.status == MarketStatusOpen
                    then [hsx|
                        <div class="d-flex gap-1 ms-1" style="width: 80px;">
                            <a href={showMarketTradeAction market.id asset.id "buy" (Just backToPath)}
                                    data-start-market-page-at-top="true"
                                    class="btn btn-outline-success p-0 rounded-1 fw-medium"
                                    style="font-size: 0.65rem; width: calc(50% - 2px);">
                                BUY
                            </a>
                            <a href={showMarketTradeAction market.id asset.id "sell" (Just backToPath)}
                                    data-start-market-page-at-top="true"
                                    class="btn btn-outline-danger p-0 rounded-1 fw-medium"
                                    style="font-size: 0.65rem; width: calc(50% - 2px);">
                                SELL
                            </a>
                        </div>
                    |]
                    else mempty

            in [hsx|
            <div class={classes ["d-flex justify-content-between mb-1 small rounded-1",
                                 ("market-status-resolved-asset", isResolvedWinner)]}
                 style="padding: 0.0rem 0.6rem;">
                <div class="text-nowrap overflow-auto scroll-no-bar me-2" style="flex: 1;">
                   {asset.name}
                </div>
                <div class="d-flex align-items-center gap-1 ps-1 flex-shrink-0">
                    {assetPriceVal}
                    {buttons}
                </div>
            </div>
        |]
