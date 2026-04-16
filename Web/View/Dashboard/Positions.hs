{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Web.View.Dashboard.Positions where

import Application.Domain.Position (EnrichedPosition (..))
import Application.Helper.QueryParams (normalizePageParam)
import Web.View.Dashboard.PortfolioSummary
import Web.View.Prelude

data PositionsView = PositionsView
    { positionsWithValue :: [EnrichedPosition]
    , currentPage        :: Int
    , totalPages         :: Int
    , wallet             :: Wallet
    , positionsValue     :: Integer
    , totalValue         :: Integer
    , searchFilter       :: Maybe Text
    , statusFilter       :: Maybe Text
    }

instance View PositionsView where
    html PositionsView { .. } = dashboardLayout [hsx|
        <div>
            <div class="d-flex justify-content-between align-items-baseline gap-2 mb-1">
                <h5>Positions</h5>
                {renderPortfolioSummary wallet.amount positionsValue totalValue}
            </div>
            <div class="mb-3">
                {renderSearchFormWithStatus searchFilter statusFilter}
            </div>
            {renderPositionsContent positionsWithValue currentPage totalPages searchFilter statusFilter}
        </div>
    |]

data PositionStatusFilter
    = PositionStatusFilterAll
    | PositionStatusFilterActive
    | PositionStatusFilterClosed
    | PositionStatusFilterResolved
    | PositionStatusFilterRefunded
    deriving (Eq, Show)

parsePositionStatusFilter :: Maybe Text -> PositionStatusFilter
parsePositionStatusFilter = \case
    Just "active"   -> PositionStatusFilterActive
    Just "closed"   -> PositionStatusFilterClosed
    Just "resolved" -> PositionStatusFilterResolved
    Just "refunded" -> PositionStatusFilterRefunded
    _              -> PositionStatusFilterAll

positionStatusFilterParamValue :: PositionStatusFilter -> Maybe Text
positionStatusFilterParamValue = \case
    PositionStatusFilterAll      -> Nothing
    PositionStatusFilterActive   -> Just "active"
    PositionStatusFilterClosed   -> Just "closed"
    PositionStatusFilterResolved -> Just "resolved"
    PositionStatusFilterRefunded -> Just "refunded"

positionStatusFilterFormValue :: PositionStatusFilter -> Text
positionStatusFilterFormValue = fromMaybe "" . positionStatusFilterParamValue

positionStatusFilterLabel :: PositionStatusFilter -> Text
positionStatusFilterLabel = \case
    PositionStatusFilterAll      -> "All"
    PositionStatusFilterActive   -> "Active"
    PositionStatusFilterClosed   -> "Closed"
    PositionStatusFilterResolved -> "Resolved"
    PositionStatusFilterRefunded -> "Refunded"

positionStatusFilterOptions :: [PositionStatusFilter]
positionStatusFilterOptions =
    [ PositionStatusFilterAll
    , PositionStatusFilterActive
    , PositionStatusFilterClosed
    , PositionStatusFilterResolved
    , PositionStatusFilterRefunded
    ]

renderSearchFormWithStatus :: Maybe Text -> Maybe Text -> Html
renderSearchFormWithStatus searchFilter mStatus = [hsx|
    <div class="d-flex align-items-center gap-3">
        <div class="flex-grow-1" style="min-width: 0;">
            {renderSearchForm searchFilter mStatus}
        </div>
        {renderStatusDropdownForm searchFilter mStatus}
    </div>
|]

renderSearchForm :: Maybe Text -> Maybe Text -> Html
renderSearchForm searchFilter mStatus = [hsx|
    <div class="d-flex position-relative" id="positions-search-form-container">
        <form class="w-100 position-relative"
              action={DashboardPositionsAction Nothing Nothing mStatus}
              method="GET"
              data-auto-submit-delay="300">
            {forEach (maybeToList mStatus) renderStatusHiddenInput}
            <i class="bi bi-search text-muted position-absolute"
               style="left: 12px; top: 50%; transform: translateY(-50%); z-index: 3;">
            </i>
            <input type="search"
                       id="positions-search-input"
                       class="form-control"
                       name="search"
                       value={fromMaybe "" searchFilter}
                       placeholder="Search positions by market or asset..."
                       aria-label="Search positions"
                       style="padding-left: 36px;">
        </form>
    </div>
|]

renderStatusHiddenInput :: Text -> Html
renderStatusHiddenInput statusValue = [hsx|
    <input type="hidden" name="statusFilter" value={statusValue} />
|]

renderStatusDropdownForm :: Maybe Text -> Maybe Text -> Html
renderStatusDropdownForm searchFilter mStatus = [hsx|
    <form class="d-flex align-items-center gap-2 flex-shrink-0"
          action={DashboardPositionsAction Nothing Nothing mStatus}
          method="GET">
        {forEach (maybeToList searchFilter) renderSearchHiddenInput}
        <div class="d-inline-flex align-items-center gap-2 rounded border border-secondary-subtle ps-2 pe-0 text-body-secondary bg-transparent"
             style="min-width: 133px; padding-top: 0.36rem; padding-bottom: 0.36rem;">
            <i class="bi bi-filter-right"></i>
            <select id="positions-status-filter"
                    name="statusFilter"
                    class="form-select form-select-sm flex-grow-1 border-0 bg-transparent text-body-secondary shadow-none py-0 ps-0 pe-4"
                    aria-label="Filter positions by status"
                    onchange="window.visitGetFormWithTurbolinks(this.form)">
                {forEach positionStatusFilterOptions (renderStatusOption (parsePositionStatusFilter mStatus))}
            </select>
        </div>
    </form>
|]

renderSearchHiddenInput :: Text -> Html
renderSearchHiddenInput searchQuery = [hsx|
    <input type="hidden" name="search" value={searchQuery} />
|]

renderStatusOption :: PositionStatusFilter -> PositionStatusFilter -> Html
renderStatusOption activeStatus optionStatus = [hsx|
    <option value={positionStatusFilterFormValue optionStatus}
            selected={activeStatus == optionStatus}>
        {positionStatusFilterLabel optionStatus}
    </option>
|]

renderPositionsContent :: (?context :: ControllerContext) => [EnrichedPosition] -> Int -> Int -> Maybe Text -> Maybe Text -> Html
renderPositionsContent [] _ _ Nothing _ = [hsx|
    <div class="alert alert-info">
        No positions found.
    </div>
|]
renderPositionsContent [] _ _ (Just _) _ = [hsx|
    <div class="alert alert-info">
        No positions match your search. Try a different search term.
    </div>
|]
renderPositionsContent positions currentPage totalPages searchFilter statusFilter = [hsx|
    <div class="row g-3">
        {forEach positions (renderPositionCard currentBackToPath)}
    </div>
    <div>
        {renderPositionsPagination currentPage totalPages searchFilter statusFilter}
    </div>
|]
    where
        currentBackToPath = pathTo (DashboardPositionsAction (normalizePageParam currentPage) searchFilter statusFilter)

renderPositionCard :: (?context :: ControllerContext) => Text -> EnrichedPosition -> Html
renderPositionCard backToPath ep =
    let position = get #epPosition ep
        asset = get #assetId position
        market = get #marketId position

        qty = get #quantity position
        isLong = qty > 0
        isOpen = qty /= 0
        absQty = abs qty

        probText = case market.status of
            MarketStatusResolved ->
                case market.outcomeAssetId of
                    Just oid -> if asset.id == oid then "100.00%" else "0.00%"
                    Nothing  -> "0.00%"
            MarketStatusRefunded -> "--"
            _ -> maybe "--" formatPricePercent (get #epAssetPrice ep)

        invested = get #invested position
        received = get #received position
        costBasis = abs (invested + received)

        currentVal = fromMaybe 0 (get #epCurrentValue ep)

        currentPnL = if isOpen
                then if isLong
                        then currentVal - costBasis
                        else costBasis - currentVal
                else received + invested
        isProfitable = currentPnL >= 0

        maxWin = if isLong
                then absQty * 100 - costBasis
                else costBasis
        maxLoss = if isLong
                then negate costBasis
                else costBasis - absQty * 100

        nextAction = if isLong then Just "buy" else Just "sell"
        marketUrl = if isOpen
            then ShowMarketAction market.id (Just asset.id) nextAction Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing (Just backToPath)
            else ShowMarketAction market.id Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing (Just backToPath)

        positionDisplay = renderPositionDisplay isOpen isLong absQty
        pnlDisplay = renderPnLDisplay currentPnL
        pnlClass :: Text = case currentPnL of
            n | n > 0 -> "text-success fw-bold"
              | n < 0 -> "text-danger fw-bold"
              | otherwise -> "fw-medium"
        actionBtn = renderActionButton isOpen isProfitable market.id asset.id market.status backToPath
    in [hsx|
        <div class="col-12">
            <div class="card shadow-sm">
                <div class="card-body px-3 py-1">
                    <div class="d-flex justify-content-between align-items-start mb-2 overflow-x-auto scroll-no-bar">
                        <a href={marketUrl} class="text-decoration-none">
                            <span class="mb-0 h6 fw-bold">{get #title market}</span> -
                            <span class="text-body-secondary fw-bold">{get #name asset}</span>
                        </a>
                    </div>

                    <div class="overflow-x-auto scroll-no-bar">
                        <div class="d-flex justify-content-between border-top pt-2" style="min-width: 640px;">
                            <div class="flex-shrink-0 pe-2" style="min-width: 90px;">
                                <div class="text-muted text-nowrap" style="font-size: 0.7rem;">Position</div>
                                <div class="fw-medium text-nowrap">{positionDisplay}</div>
                            </div>
                            <div class="flex-shrink-0 text-center px-2" style="min-width: 80px;">
                                <div class="text-muted text-nowrap" style="font-size: 0.7rem;">Probability</div>
                                <div class="fw-medium text-nowrap">{probText}</div>
                            </div>
                            <div class="flex-shrink-0 text-center px-2" style="min-width: 80px;">
                                <div class="text-muted text-nowrap" style="font-size: 0.7rem;">Invested</div>
                                <div class="fw-medium text-nowrap">{formatMoney (abs position.invested)}</div>
                            </div>
                            <div class="flex-shrink-0 text-center px-2" style="min-width: 80px;">
                                <div class="text-muted text-nowrap" style="font-size: 0.7rem;">Received</div>
                                <div class="fw-medium text-nowrap">{formatMoney position.received}</div>
                            </div>
                            <div class="flex-shrink-0 text-center px-2" style="min-width: 80px;">
                                <div class="text-muted text-nowrap" style="font-size: 0.7rem;">Current Stake</div>
                                <div class="fw-medium text-nowrap">{if isOpen then formatMoney costBasis else "--" :: Text}</div>
                            </div>
                            <div class="flex-shrink-0 text-center px-2" style="min-width: 80px;">
                                <div class="text-muted text-nowrap" style="font-size: 0.7rem;">Current Value</div>
                                <div class="fw-medium text-nowrap">{if isOpen then formatMoney currentVal else formatMoneyOrDash currentVal}</div>
                            </div>
                            <div class="flex-shrink-0 text-center px-2">
                                <div class="d-flex align-items-center justify-content-center" style="gap: 10px;">
                                    <div class="text-center" style="min-width: 80px;">
                                        <div class="text-muted text-nowrap" style="font-size: 0.7rem;">P&L Now</div>
                                        <div class={pnlClass}>{if isOpen then pnlDisplay else renderClosedPnL currentPnL}</div>
                                    </div>
                                    <div class="text-center" style="min-width: 100px;">
                                        {actionBtn}
                                    </div>
                                </div>
                            </div>
                            <div class="flex-shrink-0 text-center px-2" style="min-width: 80px;">
                                <div class="text-muted text-nowrap" style="font-size: 0.7rem;">Max Gain</div>
                                <div class="fw-medium text-nowrap">{if isOpen then formatMoneySigned maxWin else "--" :: Text}</div>
                            </div>
                            <div class="flex-shrink-0 text-center px-2" style="min-width: 80px;">
                                <div class="text-muted text-nowrap" style="font-size: 0.7rem;">Max Loss</div>
                                <div class="fw-medium text-nowrap">{if isOpen then formatMoneySigned maxLoss else "--" :: Text}</div>
                            </div>
                        </div>
                    </div>
                </div>
            </div>
        </div>
    |]

renderPositionDisplay :: Bool -> Bool -> Integer -> Html
renderPositionDisplay isOpen isLong qty =
    if isOpen
    then let cls :: Text = if isLong then "text-success fw-bold" else "text-danger fw-bold"
             txt :: Text = show qty <> if isLong then " long" else " short"
           in [hsx|<span class={cls}>{txt}</span>|]
    else [hsx|<span class="fw-medium">closed</span>|]

renderPnLDisplay :: Integer -> Html
renderPnLDisplay pnl = [hsx|<span>{formatMoneySigned pnl}</span>|]

renderActionButton :: Bool -> Bool -> Id Market -> Id Asset -> MarketStatus -> Text -> Html
-- For markets that are closed/resolved/refunded, always show status button
renderActionButton _ _ marketId _ marketStatus backToPath
    | marketStatus == MarketStatusClosed =
        renderStatusButton marketId backToPath "btn-outline-primary" "Closed"
    | marketStatus == MarketStatusResolved =
        renderStatusButton marketId backToPath "btn-outline-success" "Resolved"
    | marketStatus == MarketStatusRefunded =
        renderStatusButton marketId backToPath "btn-outline-danger" "Refunded"
-- For open markets with open positions, show profit/loss buttons
renderActionButton True isProfitable _ assetId _ _ =
    let (cls, txt) = if isProfitable
            then ("btn-success" :: Text, "Take Profit" :: Text)
            else ("btn-danger" :: Text, "Close Loss" :: Text)
        closeButton = renderPostForm (pathTo (ClosePositionAction assetId)) [("class", "d-inline")] [hsx|
            <button type="submit" class={"btn btn-sm text-nowrap " <> cls}
                    style="width: 94px;">{txt}</button>
        |]
    in closeButton
-- For open markets with closed positions, show Make Trade
renderActionButton False _ marketId _ _ backToPath =
    let link = ShowMarketAction marketId Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing (Just backToPath)
    in [hsx|
        <a href={link}
           class="btn btn-primary btn-sm text-nowrap"
           style="width: 94px;">Make Trade</a>
    |]

renderStatusButton :: Id Market -> Text -> Text -> Text -> Html
renderStatusButton marketId backToPath cls txt =
    let link = ShowMarketAction marketId Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing (Just backToPath)
    in [hsx|
        <a href={link}
           class={"btn btn-sm text-nowrap " <> cls}
           style="width: 94px;">{txt}</a>
    |]

renderPositionsPagination :: Int -> Int -> Maybe Text -> Maybe Text -> Html
renderPositionsPagination currentPage totalPages searchFilter statusFilter =
    renderSmartPagination currentPage totalPages "Positions pagination"
        (\pageNum -> pathTo (DashboardPositionsAction (Just pageNum) searchFilter statusFilter))

renderClosedPnL :: Integer -> Html
renderClosedPnL pnl
    | pnl == 0 = [hsx|<span class="fw-medium">--</span>|]
    | pnl > 0 = [hsx|<span class="fw-bold text-success">{formatMoneySigned pnl}</span>|]
    | otherwise = [hsx|<span class="fw-bold text-danger">{formatMoneySigned pnl}</span>|]
