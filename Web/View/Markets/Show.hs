{-# LANGUAGE DataKinds #-}

module Web.View.Markets.Show where

import Application.Domain.LMSR
import Application.Domain.Types
import Application.Helper.QueryParams (normalizePageParam)
import qualified Data.List as List
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as Text
import qualified IHP.QueryBuilder as QueryBuilder
import Text.Printf (printf)
import Web.Types (AssetChartData (..), PricePoint (..))
import Web.View.Prelude

type MarketActivityTransaction = Include' ["assetId", "userId"] Transaction

data MarketChatEntry = MarketChatEntry
    { message :: MarketChatMessage' (Id' "markets") User
    , author  :: User
    }

data ShowView = ShowView
    { market               :: Market
    , owner                :: Maybe User
    , category             :: Category
    , assets               :: [Asset]
    , tradingAssetId       :: Maybe (Id Asset)
    , tradingAction        :: Maybe Text
    , showChart            :: Bool
    , showDescription      :: Bool
    , showAllAssets        :: Bool
    , showTradeHistory     :: Bool
    , activityTransactions :: [MarketActivityTransaction]
    , activityCurrentPage  :: Int
    , activityTotalPages   :: Int
    , chatMessages         :: [MarketChatEntry]
    , chatCurrentPage      :: Int
    , hasOlderChatMessages :: Bool
    , chatComposerRev      :: Maybe Text
    , tradeQuantity        :: Maybe Int
    , backTo               :: Maybe Text
    , chartData            :: [AssetChartData]
    }

instance View ShowView where
    html ShowView { .. } = [hsx|
        <div class="py-3">
            <div class="row g-4">
                <div class="col-12 col-lg-8 order-1 order-lg-1">
                    <div class="card shadow-sm">
                        <div class={classes ["card-header text-muted py-2 overflow-x-auto scroll-no-bar", (headerClass, True)]}>
                            <div class="d-flex justify-content-between align-items-center flex-nowrap gap-3"
                                 style="min-width: max-content;">
                                <span class="ms-2 text-nowrap">{category.name}</span>
                                <div class="me-2 d-inline-flex align-items-center flex-nowrap gap-2">
                                    {statusBadge}
                                    {renderManageMarketButton}
                                </div>
                            </div>
                        </div>
                        <div class="card-body p-4">
                            <header class="mb-4">
                                <div class="d-flex align-items-start gap-2">
                                    <a href={backLink}
                                       class="btn btn-outline-secondary back-button flex-shrink-0">
                                        <i class="bi bi-chevron-left"></i>
                                    </a>
                                    <div class="flex-grow-1 ms-1" style="padding-top: 0.29rem;">
                                        <span class="h4 fw-bold">
                                            {market.title}
                                        </span>
                                    </div>
                                </div>
                            </header>

                            <div class="assets-list border-top mt-4">
                                {forEach assets renderAsset}
                            </div>

                            {toggleAssetsButton}

                            <div class="mt-3">
                                {renderSectionToggle "Price Chart" showChart chartToggleAction}
                                <div id="price-chart"
                                     class={classes [("d-none", not showChart)]}
                                     style="height: 300px; margin-bottom: 1.5rem;">
                                </div>
                                {chartDataScript}

                                {renderSectionToggle "Rules & Description" showDescription descriptionToggleAction}
                                <div id="market-description" class={classes [("d-none", not showDescription)]}>
                                    {renderOwnerAndDates}
                                    {renderTextParagraphs market.description}
                                </div>

                                {renderTradeHistorySection}
                            </div>
                            {chartScript}
                            {assetLayoutScript}
                        </div>
                    </div>
                </div>

                <div class="col-12 col-lg-4 order-2 order-lg-2">
                    {renderChatCard}
                </div>
            </div>
        </div>
    |]
        where
            headerClass = marketStatusHeaderClasses market.status
            statusBadge =
                if (market.status /= MarketStatusOpen)
                    then [hsx|
                        <span class="text-nowrap">{marketStatusLabel market.status}</span>
                    |]
                    else [hsx|
                        <span class="text-nowrap" data-bs-toggle="tooltip" data-bs-title="market closing time" style="font-size: 0.85rem;">
                            <i class="bi bi-alarm"></i>
                            {renderTime market.closedAt}
                        </span>
                    |]

            renderManageMarketButton :: Html
            renderManageMarketButton = case currentUserOrNothing :: Maybe User of
                Just currentUser
                    | shouldShowManageMarketButton currentUser -> [hsx|
                        <a href={manageMarketAction}
                           class="btn btn-sm btn-outline-primary text-nowrap"
                           style="--bs-btn-padding-y: 0.10rem; --bs-btn-padding-x: 0.4rem; --bs-btn-font-size: 0.8rem;">
                            Manage Market
                        </a>
                    |]
                _ -> mempty

            shouldShowManageMarketButton :: User -> Bool
            shouldShowManageMarketButton currentUser =
                market.userId == Just currentUser.id
                    && market.status `elem` [MarketStatusOpen, MarketStatusClosed]

            manageMarketAction :: DashboardController
            manageMarketAction = DashboardMarketsAction
                { statusFilter = Just market.status
                , page = Nothing
                , searchFilter = Just market.title
                }

            lmsrState = let qtyMap = M.fromList [(a.id, Quantity a.quantity) | a <- assets]
                         in (qtyMap, Beta market.beta)

            renderOwnerAndDates :: Html
            renderOwnerAndDates = [hsx|
                <div class="small text-muted my-3">
                    <span class="text-nowrap me-4 fs-6" data-bs-toggle="tooltip" data-bs-title="market creator">
                        <i class="bi bi-person-gear fs-6"></i> {fromMaybe "admin" $ fmap (.nickname) owner}
                    </span>
                    <span class="text-nowrap me-4" data-bs-toggle="tooltip" data-bs-title="market opened at">
                        <i class="bi bi-clock"></i> {renderTime $ fromMaybe market.createdAt market.openedAt}
                    </span>
                    <span class="text-nowrap" data-bs-toggle="tooltip" data-bs-title="market closing time">
                        <i class="bi bi-alarm"></i> {renderTime market.closedAt}
                    </span>
                </div>
            |]

            currentPrices :: M.Map (Id Asset) Double
            currentPrices = M.map (\a -> assetPrice a.id (snd lmsrState) (fst lmsrState))
                               (M.fromList [(a.id, a) | a <- assets])

            leadingAssetIds :: S.Set (Id Asset)
            leadingAssetIds = S.fromList
                [ a.id
                | a <- assets
                , let p = fromMaybe 0.0 (M.lookup a.id currentPrices)
                , p >= 0.05 || Just a.id == tradingAssetId
                ]

            hasLeadingAssets :: Bool
            hasLeadingAssets = length assets > 6 && length (filter (\a -> a.id `S.member` leadingAssetIds) assets) /= length assets

            showAssetSymbols :: Bool
            showAssetSymbols = any (\asset -> asset.symbol /= asset.name) assets

            marketShowAction :: Maybe (Id Asset) -> Maybe Text -> Bool -> Bool -> Bool -> Bool -> Int -> Int -> MarketsController
            marketShowAction tradingAssetId' tradingAction' showChart' showDescription' showAllAssets' showTradeHistory' activityPage' chatPage' =
                ShowMarketAction
                    { marketId = market.id
                    , tradingAssetId = tradingAssetId'
                    , tradingAction = tradingAction'
                    , showChart = Just showChart'
                    , showDescription = Just showDescription'
                    , showAllAssets = Just showAllAssets'
                    , showTradeHistory = Just showTradeHistory'
                    , activityPage = normalizePageParam activityPage'
                    , chatPage = normalizePageParam chatPage'
                    , chatComposerRev = chatComposerRev
                    , tradeQuantity = tradeQuantity
                    , backTo = backTo
                    }

            backLink :: Text
            backLink = fromMaybe (pathTo MarketsAction) backTo

            renderSectionToggle :: Text -> Bool -> MarketsController -> Html
            renderSectionToggle label isOpen action = [hsx|
                <h6 class="mb-2 mt-3">
                    <a href={action}
                       class="info-label text-reset text-decoration-none d-inline-flex align-items-center gap-2"
                       aria-expanded={boolText isOpen}>
                        <span>{label}</span>
                        {renderChevronIcon isOpen}
                    </a>
                </h6>
            |]

            renderChevronIcon :: Bool -> Html
            renderChevronIcon True  = [hsx|<i class="bi bi-chevron-down"></i>|]
            renderChevronIcon False = [hsx|<i class="bi bi-chevron-right"></i>|]

            chartToggleAction :: MarketsController
            chartToggleAction =
                marketShowAction tradingAssetId tradingAction (not showChart) showDescription showAllAssets showTradeHistory activityCurrentPage chatCurrentPage

            descriptionToggleAction :: MarketsController
            descriptionToggleAction =
                marketShowAction tradingAssetId tradingAction showChart (not showDescription) showAllAssets showTradeHistory activityCurrentPage chatCurrentPage

            assetsToggleAction :: MarketsController
            assetsToggleAction =
                marketShowAction tradingAssetId tradingAction showChart showDescription (not showAllAssets) showTradeHistory activityCurrentPage chatCurrentPage

            tradeHistoryToggleAction :: MarketsController
            tradeHistoryToggleAction =
                marketShowAction tradingAssetId tradingAction showChart showDescription showAllAssets (not showTradeHistory) activityCurrentPage chatCurrentPage

            activityPageAction :: Int -> MarketsController
            activityPageAction pageNum =
                marketShowAction tradingAssetId tradingAction showChart showDescription showAllAssets showTradeHistory pageNum chatCurrentPage

            chatPageAction :: Int -> MarketsController
            chatPageAction pageNum =
                marketShowAction tradingAssetId tradingAction showChart showDescription showAllAssets showTradeHistory activityCurrentPage pageNum

            deleteChatMessageAction :: Id MarketChatMessage -> MarketsController
            deleteChatMessageAction messageId = DeleteMarketChatMessageAction
                { marketChatMessageId = messageId
                , marketId = market.id
                , tradingAssetId = tradingAssetId
                , tradingAction = tradingAction
                , showChart = Just showChart
                , showDescription = Just showDescription
                , showAllAssets = Just showAllAssets
                , showTradeHistory = Just showTradeHistory
                , activityPage = Just activityCurrentPage
                , chatPage = Just chatCurrentPage
                , chatComposerRev = chatComposerRev
                , tradeQuantity = tradeQuantity
                , backTo = backTo
                }

            toggleAssetsButton :: Html
            toggleAssetsButton = if hasLeadingAssets
                then [hsx|
                    <div class="text-end mt-3">
                        <a href={assetsToggleAction} class="text-decoration-none me-2">
                            {if showAllAssets then "Show Only Leading Assets" else "Show All Assets" :: Text}
                        </a>
                    </div>
                |]
                else [hsx||]

            assetLayoutScript :: Html
            assetLayoutScript = [hsx|
                <script>
                    function parsePx(value) {
                        return Number.parseFloat(value) || 0;
                    }

                    function outerScrollWidth(element) {
                        var style = window.getComputedStyle(element);
                        return element.scrollWidth
                            + parsePx(style.marginLeft)
                            + parsePx(style.marginRight);
                    }

                    function updateMarketAssetRows() {
                        var rows = Array.from(document.querySelectorAll('.market-asset-row'));
                        var shouldWrap = false;

                        rows.forEach(function(row) {
                            row.classList.remove('is-wrapped');
                            if (row.offsetParent === null) return;

                            var main = row.querySelector('.market-asset-main');
                            var side = row.querySelector('.market-asset-side');
                            if (!main || !side) return;

                            var style = window.getComputedStyle(row);
                            var gap = parsePx(style.columnGap || style.gap);
                            var requiredWidth = outerScrollWidth(main) + outerScrollWidth(side) + gap;

                            if (requiredWidth > row.clientWidth + 1) {
                                shouldWrap = true;
                            }
                        });

                        rows.forEach(function(row) {
                            if (row.offsetParent === null) {
                                row.classList.remove('is-wrapped');
                                return;
                            }
                            row.classList.toggle('is-wrapped', shouldWrap);
                        });
                    }

                    document.addEventListener('turbolinks:load', function() {
                        window.updateMarketAssetRows = updateMarketAssetRows;
                        if (!window.marketAssetRowsResizeBound) {
                            window.addEventListener('resize', function() {
                                if (window.updateMarketAssetRows) window.updateMarketAssetRows();
                            });
                            window.marketAssetRowsResizeBound = true;
                        }
                        requestAnimationFrame(updateMarketAssetRows);
                    });
                </script>
            |]

            isLeading :: Asset -> Text
            isLeading a = if a.id `S.member` leadingAssetIds then "true" else "false"

            shouldHideAsset :: Asset -> Bool
            shouldHideAsset asset =
                hasLeadingAssets && not showAllAssets && asset.id `S.notMember` leadingAssetIds

            tradeToggleAction :: Id Asset -> Text -> MarketsController
            tradeToggleAction assetId action =
                let isOpen = tradingAssetId == Just assetId && tradingAction == Just action
                    nextAssetId = if isOpen then Nothing else Just assetId
                    nextAction = if isOpen then Nothing else Just action
                in marketShowAction nextAssetId nextAction showChart showDescription showAllAssets showTradeHistory activityCurrentPage chatCurrentPage

            boolText :: Bool -> Text
            boolText value = if value then "true" else "false"

            chatTradeQuantityValue :: Text
            chatTradeQuantityValue = inputValue (fromMaybe 10 tradeQuantity)

            renderBackToInput :: Html
            renderBackToInput = case backTo of
                Just backToPath -> [hsx|<input type="hidden" name="backTo" value={backToPath} />|]
                Nothing -> [hsx||]

            renderActivityPageInput :: Html
            renderActivityPageInput = [hsx|<input type="hidden" name="activityPage" value={inputValue activityCurrentPage} />|]

            renderChatPageInput :: Html
            renderChatPageInput = [hsx|<input type="hidden" name="chatPage" value={inputValue chatCurrentPage} />|]

            renderChatComposerRevInput :: Html
            renderChatComposerRevInput = case chatComposerRev of
                Just revision -> [hsx|<input type="hidden" name="chatComposerRev" value={revision} />|]
                Nothing -> [hsx||]

            renderTradeQuantityInput :: Html
            renderTradeQuantityInput = [hsx|
                <input id="market-chat-trade-quantity" type="hidden" name="tradeQuantity" value={chatTradeQuantityValue} />
            |]

            renderTradingAssetIdInput :: Html
            renderTradingAssetIdInput = case tradingAssetId of
                Just assetId -> [hsx|<input type="hidden" name="tradingAssetId" value={inputValue assetId} />|]
                Nothing -> [hsx||]

            renderTradingActionInput :: Html
            renderTradingActionInput = case tradingAction of
                Just action -> [hsx|<input type="hidden" name="tradingAction" value={action} />|]
                Nothing -> [hsx||]

            renderTradeHistoryToggleInput :: Html
            renderTradeHistoryToggleInput = [hsx|<input type="hidden" name="showTradeHistory" value={boolText showTradeHistory} />|]

            renderChatCard :: Html
            renderChatCard = [hsx|
                <div id="market-chat-card" class="card shadow-sm h-100 d-flex flex-column">
                    <div class="card-header py-2">
                        <span class="fw-semibold">Users Chat</span>
                    </div>
                    <div class="card-body p-3 d-flex flex-column flex-grow-1" style="min-height: 420px;">
                        <div id={"market-chat-composer-" <> fromMaybe "stable" chatComposerRev} class="mb-3">
                            <form id="market-chat-form" action={CreateMarketChatMessageAction market.id} method="POST">
                                {renderTradingAssetIdInput}
                                {renderTradingActionInput}
                                <input type="hidden" name="showChart" value={boolText showChart} />
                                <input type="hidden" name="showDescription" value={boolText showDescription} />
                                <input type="hidden" name="showAllAssets" value={boolText showAllAssets} />
                                {renderTradeHistoryToggleInput}
                                {renderActivityPageInput}
                                {renderChatPageInput}
                                {renderChatComposerRevInput}
                                {renderTradeQuantityInput}
                                {renderBackToInput}
                                <div class="input-group mt-2">
                                    <input id={"market-chat-input-" <> fromMaybe "stable" chatComposerRev}
                                           type="text"
                                           name="body"
                                           form="market-chat-form"
                                           class="form-control"
                                           maxlength="280"
                                           placeholder="Type a message... (280 chars max)"
                                           autocomplete="off" />
                                    <button id="market-chat-submit"
                                            class="btn btn-primary"
                                            type="submit"
                                            form="market-chat-form"
                                            formmethod="post"
                                            formaction={CreateMarketChatMessageAction market.id}>
                                        Send
                                    </button>
                                </div>
                            </form>
                        </div>

                        <div id="market-chat-messages"
                             class="flex-grow-1 rounded border p-3"
                             style="overflow-y: auto; min-height: 0; flex-basis: 0;"
                             data-next-url={nextChatPageUrl}
                             data-scroll-key={chatScrollStorageKey}
                             data-composer-rev={fromMaybe "" chatComposerRev}>
                            {renderChatMessages}
                            {renderChatLoadMore}
                        </div>
                    </div>
                </div>
            |]
                where
                    nextChatPageUrl = if hasOlderChatMessages
                        then pathTo (chatPageAction (chatCurrentPage + 1))
                        else ""

                    chatScrollStorageKey = "market-chat-scroll-" <> inputValue market.id

            renderChatMessages :: Html
            renderChatMessages = case chatMessages of
                [] -> [hsx|
                    <div class="text-muted small">
                        No messages yet. Be the first to write something.
                    </div>
                |]
                messages -> [hsx|
                    <div class="d-flex flex-column gap-2">
                        {forEach messages renderChatMessage}
                    </div>
                |]

            renderChatMessage :: MarketChatEntry -> Html
            renderChatMessage chatEntry = [hsx|
                <div class="chat-message" style="position: relative;">
                    <div class="d-flex justify-content-between align-items-baseline gap-2 mb-0 text-secondary small">
                        <span class="text-nowrap d-flex align-items-center gap-1">
                            {author.nickname}
                            {renderDeleteButton message.id author.id}
                        </span>
                        <span class="text-nowrap" style="font-size: 0.8em;">
                            {timeAgo message.createdAt}
                        </span>
                    </div>
                    <div style="overflow-wrap: anywhere;">
                        {message.body}
                    </div>
                </div>
            |]
                where
                    message = chatEntry.message
                    author = chatEntry.author
                    renderDeleteButton msgId authorId =
                        case currentUserOrNothing :: Maybe User of
                            Just currentUser ->
                                if authorId == get #id currentUser
                                    then [hsx|
                                        <form method="POST" action={pathTo (deleteChatMessageAction msgId)} class="d-inline">
                                            <button type="submit" class="btn btn-link btn-sm p-0 text-decoration-none" data-bs-toggle="tooltip" data-bs-title="delete">
                                                <i class="bi bi-x-lg text-danger"></i>
                                            </button>
                                        </form>
                                    |]
                                    else mempty
                            Nothing -> mempty

            renderChatLoadMore :: Html
            renderChatLoadMore =
                when hasOlderChatMessages [hsx|
                    <div class="text-center mt-3">
                        <a id="market-chat-load-more"
                           href={pathTo (chatPageAction (chatCurrentPage + 1))}
                           data-chat-load-more-url={pathTo (chatPageAction (chatCurrentPage + 1))}
                           class="btn btn-sm btn-outline-secondary">
                            Load older messages
                        </a>
                    </div>
                |]

            renderTradeHistorySection :: Html
            renderTradeHistorySection = [hsx|
                <div class="mt-4">
                    {renderSectionToggle "Trade History" showTradeHistory tradeHistoryToggleAction}
                    <div class={classes [("d-none", not showTradeHistory)]}>
                        {renderActivityContent}
                        {renderActivityPagination}
                    </div>
                </div>
            |]

            renderActivityContent :: Html
            renderActivityContent = case activityTransactions of
                [] -> [hsx|
                    <div class="text-muted small py-2">
                        No trades yet for this market.
                    </div>
                |]
                txns -> [hsx|
                    <div class="overflow-x-auto scroll-no-bar">
                        <table class="table table-sm table-borderless table-hover align-middle mb-0 small text-nowrap"
                               style="--bs-table-bg: transparent; --bs-table-color: var(--bs-body-color); --bs-table-hover-bg: rgba(var(--bs-body-color-rgb), 0.04); --bs-table-hover-color: var(--bs-body-color);">
                            <thead>
                                <tr>
                                    <th scope="col" class="info-label">User</th>
                                    <th scope="col" class="info-label">Action</th>
                                    <th scope="col" class="info-label text-end">Shares</th>
                                    <th scope="col" class="info-label">Asset</th>
                                    <th scope="col" class="info-label text-end">Amount</th>
                                    <th scope="col" class="info-label text-center">Probability</th>
                                    <th scope="col" class="info-label text-end" style="min-width: 110px;">Time</th>
                                </tr>
                            </thead>
                            <tbody>
                                {forEach txns renderActivityTransaction}
                            </tbody>
                        </table>
                    </div>
                |]

            renderActivityTransaction :: MarketActivityTransaction -> Html
            renderActivityTransaction transaction = [hsx|
                <tr>
                    <td>{user.nickname}</td>
                    <td>
                        <span class={classes [("text-success", isBuy), ("text-danger", not isBuy)]}>
                            {actionText}
                        </span>
                    </td>
                    <td class="text-end">{tshow absQty}</td>
                    <td>{asset.name}</td>
                    <td class="text-end">{formatMoney (abs cashFlow)}</td>
                    <td class="text-muted text-center">
                        {formatPriceRounded transaction.priceBefore}{" → " :: Text}{formatPriceRounded transaction.priceAfter}
                    </td>
                    <td class="text-end text-muted">{timeAgo transaction.createdAt}</td>
                </tr>
            |]
                where
                    asset = get #assetId transaction
                    user = get #userId transaction
                    cashFlow = transaction.cashFlow
                    absQty = abs transaction.quantity
                    isBuy = transaction.quantity > 0
                    actionText = if transaction.quantity > 0 then "Bought" else "Sold" :: Text

            renderActivityPagination :: Html
            renderActivityPagination =
                when (activityTotalPages > 1) [hsx|
                    <div id="trade-history-pagination" class="mt-3">
                        {renderSmartPagination activityCurrentPage activityTotalPages "Market activity pagination"
                            (\pageNum -> pathTo (activityPageAction pageNum) <> "#trade-history-pagination")}
                    </div>
                |]

            renderTradeInfoContainer :: Text -> Text -> Text -> Text -> Html
            renderTradeInfoContainer containerId moneyLabel netLabel netClass = [hsx|
                <div id={containerId}
                     class="trade-info-container text-end w-100 d-none"
                     style="max-width: 320px;">
                    <div class="trade-info-grid">
                        <div class="info-item">
                            <span class="info-label">{moneyLabel}</span>
                            <span class="info-value" data-trade-field="money"></span>
                        </div>
                        <div class="info-item">
                            <span class="info-label">Return</span>
                            <span class="info-value" data-trade-field="return"></span>
                        </div>
                        <div class="info-item">
                            <span class="info-label">{netLabel}</span>
                            <span class={"info-value " <> netClass} data-trade-field="net"></span>
                        </div>
                        <div class="info-item">
                            <span class="info-label">Probability</span>
                            <span class="info-value">
                                <span class="info-transition">{if netClass == "text-success" then "↑" else "↓" :: Text}</span>
                                <span data-trade-field="probability"></span>
                            </span>
                        </div>
                    </div>
                </div>
            |]

            renderAsset :: Asset -> Html
            renderAsset asset = [hsx|
                <div class={classes ["py-3 border-bottom asset-card",
                                     ("d-none", shouldHideAsset asset),
                                     ("market-status-resolved-asset", isResolvedWinner)]}
                     id={"asset-" <> show asset.id}
                     data-leading={isLeading asset}>
                    <div class="market-asset-row">
                        <div class="market-asset-main">
                            <div class="asset-info flex-shrink-0">
                                <div class="fw-semibold fs-5 me-3">
                                    {asset.name}
                                </div>
                            </div>
                            {symbolDisplayWrapped}
                        </div>
                        <div class="market-asset-side">
                            {priceDisplay}
                            {when (isTradable) buySellButtons}
                        </div>
                    </div>

                    {when (isTradable) buySellForms}
                </div>
            |]
                where
                    isTradable = market.status == MarketStatusOpen
                    isResolvedWinner = market.status == MarketStatusResolved
                        && market.outcomeAssetId == Just asset.id
                    isBuyFormOpen = tradingAssetId == Just asset.id && tradingAction == Just "buy"
                    isSellFormOpen = tradingAssetId == Just asset.id && tradingAction == Just "sell"

                    tradeQuantityValue :: Bool -> Text
                    tradeQuantityValue isFormOpen = inputValue quantity
                        where
                            quantity :: Int
                            quantity = if isFormOpen
                                then fromMaybe 10 tradeQuantity
                                else 10

                    assetPriceVal :: Double
                    assetPriceVal = case market.status of
                        MarketStatusResolved -> if isResolvedWinner then 1.0 else 0.0
                        MarketStatusRefunded -> 0.0
                        _ -> assetPrice asset.id (snd lmsrState) (fst lmsrState)

                    symbolDisplayInline :: Html
                    symbolDisplayInline = when showAssetSymbols [hsx|
                        <div class="market-asset-symbol-inline text-secondary text-center text-nowrap" style="width: 56px;">
                            {asset.symbol}
                        </div>
                    |]

                    symbolDisplayWrapped :: Html
                    symbolDisplayWrapped = when showAssetSymbols [hsx|
                        <div class="market-asset-symbol-wrapped text-secondary text-nowrap">
                            {asset.symbol}
                        </div>
                    |]

                    priceDisplay = [hsx|
                        <div class="d-flex align-items-center justify-content-end gap-2 fw-medium flex-shrink-0 me-3">
                            {symbolDisplayInline}
                            <div class="text-secondary text-center text-nowrap" style="width: 44px;">
                                {formatPriceRounded assetPriceVal}
                            </div>
                            <div class="text-end text-nowrap" style="width: 56px;">
                                {printf "%.4f" assetPriceVal :: String}
                            </div>
                        </div>
                    |]

                    buySellButtons = [hsx|
                        <div class="d-flex gap-1" style="width: 140px">
                            <a class={classes ["btn btn-sm fw-medium",
                                               ("btn-success", isBuyFormOpen),
                                               ("btn-outline-success", not isBuyFormOpen)]}
                               style="width: calc(50% - 2px);"
                               href={tradeToggleAction asset.id "buy"}>
                                BUY
                            </a>
                            <a class={classes ["btn btn-sm fw-medium",
                                               ("btn-danger", isSellFormOpen),
                                               ("btn-outline-danger", not isSellFormOpen)]}
                               style="width: calc(50% - 2px);"
                               href={tradeToggleAction asset.id "sell"}>
                                SELL
                            </a>
                        </div>
                    |]

                    buySellForms = [hsx|
                        <div id={"buy-form-" <> show asset.id}
                             class={classes ["mt-3", ("d-none", not isBuyFormOpen)]}>
                            <form action={ExecuteTradeAction asset.id} method="POST">
                                <input type="hidden" name="type" value="buy" />
                                <input type="hidden" name="showChart" value={boolText showChart} />
                                <input type="hidden" name="showDescription" value={boolText showDescription} />
                                <input type="hidden" name="showAllAssets" value={boolText showAllAssets} />
                                {renderTradeHistoryToggleInput}
                                {renderActivityPageInput}
                                {renderChatPageInput}
                                {renderChatComposerRevInput}
                                {renderBackToInput}
                                <div class="d-flex flex-column align-items-end gap-2">
                                    <div class="d-flex align-items-center gap-3">
                                        <div class="input-group" style="width: 160px">
                                            <span class="input-group-text info-label"
                                                  style="font-size: 0.64rem;">
                                                  shares
                                            </span>
                                            <input type="number" name="quantity"
                                                   id={"trade-quantity-buy-" <> inputValue asset.id}
                                                   value={tradeQuantityValue isBuyFormOpen} step="1" min="0"
                                                   class="form-control"
                                                   autofocus={isBuyFormOpen}
                                                   oninput="updateBuyInfo(this)"
                                                   data-info-id={"buy-info-" <> show asset.id}
                                                   data-a={show assetPriceVal}
                                                   data-beta={show market.beta} />
                                        </div>
                                        <button type="submit" class="btn btn-primary fw-bold"
                                                style="width: 140px">BUY</button>
                                    </div>
                                    {renderTradeInfoContainer ("buy-info-" <> show asset.id) "Invest" "Gain" "text-success"}
                                </div>
                            </form>
                        </div>

                        <div id={"sell-form-" <> show asset.id}
                             class={classes ["mt-3", ("d-none", not isSellFormOpen)]}>
                            <form action={ExecuteTradeAction asset.id} method="POST">
                                <input type="hidden" name="type" value="sell" />
                                <input type="hidden" name="showChart" value={boolText showChart} />
                                <input type="hidden" name="showDescription" value={boolText showDescription} />
                                <input type="hidden" name="showAllAssets" value={boolText showAllAssets} />
                                {renderTradeHistoryToggleInput}
                                {renderActivityPageInput}
                                {renderChatPageInput}
                                {renderChatComposerRevInput}
                                {renderBackToInput}
                                <div class="d-flex flex-column align-items-end gap-2">
                                    <div class="d-flex align-items-center gap-3">
                                        <div class="input-group" style="width: 160px">
                                            <span class="input-group-text info-label">shares</span>
                                            <input type="number" name="quantity"
                                                   id={"trade-quantity-sell-" <> inputValue asset.id}
                                                   value={tradeQuantityValue isSellFormOpen} step="1" min="0"
                                                   class="form-control"
                                                   autofocus={isSellFormOpen}
                                                   oninput="updateSellInfo(this)"
                                                   data-info-id={"sell-info-" <> show asset.id}
                                                   data-a={show assetPriceVal}
                                                   data-beta={show market.beta} />
                                        </div>
                                        <button type="submit" class="btn btn-primary fw-bold"
                                                style="width: 140px">SELL</button>
                                    </div>
                                    {renderTradeInfoContainer ("sell-info-" <> show asset.id) "Receive" "Risk" "text-danger"}
                                </div>
                            </form>
                        </div>
                    |]

            -- | Encode chart data as JSON for JavaScript
            encodeChartData :: [AssetChartData] -> Text
            encodeChartData assetsData =
                "[" <> Text.intercalate "," (map encodeAsset assetsData) <> "]"
                where
                    encodeAsset (AssetChartData aid' asym' aname' acolor' data') =
                        "{\"id\":\"" <> assetIdToText aid' <> "\",\"symbol\":\"" <> asym' <> "\",\"name\":\"" <> aname' <> "\",\"color\":\"" <> acolor' <> "\",\"data\":[" <> Text.intercalate "," (map encodePrice data') <> "]}"
                    encodePrice (PricePoint time' value') =
                        "{\"time\":" <> tshow time' <> ",\"value\":" <> tshow value' <> "}"
                    tshow :: Show a => a -> Text
                    tshow x = cs (show x)
                    assetIdToText :: Id Asset -> Text
                    assetIdToText aid = cs (show aid)

            -- | Script tag with chart data (raw HTML to prevent escaping)
            chartDataScript :: Html
            chartDataScript = preEscapedToHtml $ Text.concat
                [ "<script id=\"chart-data\" type=\"application/json\">"
                , encodeChartData chartData
                , "</script>"
                ]

            chartScript :: Html
            chartScript = [hsx|
                <script>
                    function initPriceChart() {
                        var chartContainer = document.getElementById('price-chart');
                        var chartDataScript = document.getElementById('chart-data');
                        if (!chartContainer || !chartDataScript) return;
                        if (chartContainer.classList.contains('d-none')) return;

                        if (chartContainer._chart) {
                            chartContainer._chart.remove();
                            chartContainer._chart = null;
                        }

                        var chartData;
                        try {
                            chartData = JSON.parse(chartDataScript.textContent.trim());
                        } catch (e) {
                            return;
                        }

                        if (!chartData || chartData.length === 0) return;

                        var chart = LightweightCharts.createChart(chartContainer, {
                            width: chartContainer.clientWidth,
                            height: chartContainer.clientHeight,
                            layout: {
                                background: { color: 'transparent' },
                                textColor: getComputedStyle(document.body).getPropertyValue('--bs-body-color') || '#333',
                            },
                            grid: {
                                vertLines: { color: 'rgba(128, 128, 128, 0.2)' },
                                horzLines: { color: 'rgba(128, 128, 128, 0.2)' },
                            },
                            crosshair: {
                                mode: LightweightCharts.CrosshairMode.Normal,
                            },
                            rightPriceScale: {
                                borderColor: 'rgba(128, 128, 128, 0.2)',
                            },
                            timeScale: {
                                borderColor: 'rgba(128, 128, 128, 0.2)',
                                timeVisible: false,
                            },
                        });

                        chartContainer._chart = chart;

                        var seriesArray = [];

                        chartData.forEach(function(asset) {
                            var series = chart.addSeries(LightweightCharts.LineSeries, {
                                color: asset.color,
                                lineWidth: 2,
                                title: asset.symbol,
                                priceLineVisible: false,
                                lastValueVisible: false,
                            });

                            if (asset.data && asset.data.length > 0) {
                                series.setData(asset.data.sort(function(a, b) { return a.time - b.time; }));
                            }

                            seriesArray.push({
                                series: series,
                                label: asset.symbol,
                                color: asset.color
                            });
                        });

                        var legend = document.createElement('div');
                        legend.style.position = 'absolute';
                        legend.style.top = '8px';
                        legend.style.left = '8px';
                        legend.style.background = 'rgba(255, 255, 255, 0.95)';
                        legend.style.border = '1px solid rgba(128, 128, 128, 0.3)';
                        legend.style.borderRadius = '4px';
                        legend.style.padding = '8px 12px';
                        legend.style.fontSize = '12px';
                        legend.style.color = '#000';
                        legend.style.zIndex = '10';
                        legend.style.display = 'none';
                        chartContainer.style.position = 'relative';
                        chartContainer.appendChild(legend);

                        var valueElements = {};

                        seriesArray.forEach(function(item) {
                            var row = document.createElement('div');
                            row.className = 'legend-item';
                            row.style.cssText = 'display: flex; align-items: center; gap: 4px; margin-bottom: 2px;';

                            var badge = document.createElement('span');
                            badge.style.cssText = 'display: inline-block; width: 8px; height: 8px; border-radius: 50%;';
                            badge.style.background = item.color;

                            var labelSpan = document.createElement('span');
                            labelSpan.style.cssText = 'font-weight: 500;';
                            labelSpan.textContent = item.label;

                            var valueSpan = document.createElement('span');
                            valueSpan.className = 'legend-value';
                            valueSpan.style.cssText = 'margin-left: auto; text-align: right; min-width: 35px;';
                            valueSpan.textContent = '--';

                            row.appendChild(badge);
                            row.appendChild(labelSpan);
                            row.appendChild(valueSpan);
                            legend.appendChild(row);

                            valueElements[item.label] = valueSpan;
                        });

                        chartContainer.addEventListener('mouseenter', function() {
                            legend.style.display = 'block';
                        });

                        chartContainer.addEventListener('mouseleave', function() {
                            legend.style.display = 'none';
                        });

                        chart.subscribeCrosshairMove(function(param) {
                            seriesArray.forEach(function(item) {
                                var data = param.seriesData.get(item.series);
                                var value = data ? data.value : null;
                                var el = valueElements[item.label];
                                if (el) {
                                    if (value !== null) {
                                        var percent = Math.round(value * 100);
                                        el.textContent = percent + '%';
                                    } else {
                                        el.textContent = '--';
                                    }
                                }
                            });
                        });

                        chart.timeScale().fitContent();

                        var resizeObserver = new ResizeObserver(function(entries) {
                            chart.applyOptions({
                                width: entries[0].contentRect.width,
                                height: entries[0].contentRect.height,
                            });
                        });
                        resizeObserver.observe(chartContainer);
                    }

                    document.addEventListener('turbolinks:load', initPriceChart);

                    document.addEventListener('turbolinks:before-cache', function() {
                        var chartContainer = document.getElementById('price-chart');
                        if (chartContainer && chartContainer._chart) {
                            chartContainer._chart.remove();
                            chartContainer._chart = null;
                        }
                    });
                </script>
            |]
