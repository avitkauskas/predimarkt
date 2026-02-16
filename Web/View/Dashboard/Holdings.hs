{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Web.View.Dashboard.Holdings where

import Web.View.Prelude

data HoldingWithValue = HoldingWithValue
    { holding      :: Include' ["marketId", "assetId"] Holding
    , currentValue :: Maybe Integer
    , assetPrice   :: Maybe Double
    }

data HoldingsView = HoldingsView
    { holdingsWithValue :: [HoldingWithValue]
    , currentPage       :: Int
    , totalPages        :: Int
    , wallet            :: Wallet
    }

instance View HoldingsView where
    html HoldingsView { .. } = dashboardLayout [hsx|
        <div class="container-fluid">
            <div class="d-flex justify-content-between align-items-center mb-2" style="max-width: 900px;">
                <h5>Positions</h5>
                <div class="text-end me-1">
                    Cash Balance: <span class="fw-bold">{formatMoney wallet.amount}</span>
                </div>
            </div>
            {renderHoldingsContent holdingsWithValue currentPage totalPages}
        </div>
    |]

renderHoldingsContent :: (?context :: ControllerContext) => [HoldingWithValue] -> Int -> Int -> Html
renderHoldingsContent [] _ _ = [hsx|
    <div class="alert alert-info">
        No active positions. <a href={MarketsAction} class="alert-link">Browse markets</a> to trade.
    </div>
|]
renderHoldingsContent holdings currentPage totalPages = [hsx|
    <div class="row g-3">
        {forEach holdings renderHoldingCard}
    </div>
    <div style="max-width: 900px;">
        {renderHoldingsPagination currentPage totalPages}
    </div>
|]

renderHoldingCard :: (?context :: ControllerContext) => HoldingWithValue -> Html
renderHoldingCard hwd =
    let holding = get #holding hwd
        asset = get #assetId holding
        market = get #marketId holding

        qty = get #quantity holding
        side = get #side holding
        isLong = side == Just "long"
        isOpen = qty > 0

        probText = maybe "-" formatPricePercent (get #assetPrice hwd)

        costBasis = abs (get #costBasis holding)
        currentVal = fromMaybe 0 (get #currentValue hwd)

        -- PnL calculation with new cash-based cost basis for shorts
        currentPnL = if isLong
                then currentVal - costBasis           -- Long: value - cost
                else costBasis - currentVal           -- Short: received - cost_to_close
        isProfitable = currentPnL >= 0

        -- Open P&L Range: max loss to max profit
        -- Long: [-costBasis, +(qty*100 - costBasis)]
        -- Short: [-(qty*100 - costBasis), +costBasis]
        maxOutcome = if isLong
                then qty * 100 - costBasis
                else negate (qty * 100 - costBasis)

        nextAction = case side of
            Just "long"  -> Just "buy"
            Just "short" -> Just "sell"
            _            -> Nothing
        marketUrl = ShowMarketAction market.id (Just asset.id) nextAction

        positionDisplay = renderPositionDisplay isOpen isLong qty
        pnlDisplay = renderPnLDisplay currentPnL
        pnlClass :: Text = case currentPnL of
            n | n > 0 -> "text-success fw-bold"
              | n < 0 -> "text-danger fw-bold"
              | otherwise -> "fw-medium"
        actionBtn = renderActionButton isOpen isProfitable market.id asset.id market.status
    in [hsx|
        <div class="col-12">
            <div class="card shadow-sm" style="max-width: 900px;">
                <div class="card-body px-3 py-2">
                    <div class="d-flex justify-content-between align-items-start mb-2 overflow-x-auto scroll-no-bar">
                        <a href={marketUrl} class="text-decoration-none">
                            <span class="mb-0 h6 fw-bold">{get #title market}</span> -
                            <span class="text-body-secondary fw-bold">{get #name asset}</span>
                        </a>
                    </div>

                    <div class="overflow-x-auto scroll-no-bar">
                        <div class="d-flex justify-content-between border-top pt-2" style="min-width: 640px;">
                            <div class="flex-shrink-0 text-center px-2" style="min-width: 90px;">
                                <div class="text-muted text-nowrap" style="font-size: 0.7rem;">Position</div>
                                <div class="fw-medium text-nowrap">{positionDisplay}</div>
                            </div>
                            <div class="flex-shrink-0 text-center px-2" style="min-width: 80px;">
                                <div class="text-muted text-nowrap" style="font-size: 0.7rem;">Probability</div>
                                <div class="fw-medium text-nowrap">{probText}</div>
                            </div>
                            <div class="flex-shrink-0 text-center px-2" style="min-width: 80px;">
                                <div class="text-muted text-nowrap" style="font-size: 0.7rem;">Current Value</div>
                                <div class="fw-medium text-nowrap">{if isOpen then formatMoney currentVal else formatMoneyOrDash currentVal}</div>
                            </div>
                            <div class="flex-shrink-0 text-center px-2" style="min-width: 80px;">
                                <div class="text-muted text-nowrap" style="font-size: 0.7rem;">
                                    {if isOpen then (if isLong then ("Invested" :: Text) else ("Received" :: Text)) else ("Invested" :: Text)}
                                </div>
                                <div class="fw-medium text-nowrap">{if isOpen then formatMoney costBasis else formatMoneyOrDash costBasis}</div>
                            </div>
                            <div class="flex-shrink-0 text-center px-2">
                                <div class="d-flex align-items-center justify-content-center" style="gap: 20px;">
                                    <div class="text-center" style="min-width: 80px;">
                                        <div class="text-muted text-nowrap" style="font-size: 0.7rem;">P&L Now</div>
                                        <div class={pnlClass}>{if isOpen then pnlDisplay else renderClosedPnL currentPnL}</div>
                                    </div>
                                    <div class="text-center" style="min-width: 100px;">
                                        {actionBtn}
                                    </div>
                                    <div class="text-center" style="min-width: 80px;">
                                        <div class="text-muted text-nowrap" style="font-size: 0.7rem;">{if isLong then ("Max Win" :: Text) else ("Max Loss" :: Text)}</div>
                                        <div class="fw-medium text-nowrap">{if isOpen then formatMoneySigned maxOutcome else formatMoneyOrDash maxOutcome}</div>
                                    </div>
                                </div>
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

renderActionButton :: Bool -> Bool -> Id Market -> Id Asset -> MarketStatus -> Html
-- For markets that are closed/resolved/refunded, always show status button
renderActionButton _ _ marketId _ marketStatus
    | marketStatus == MarketStatusClosed =
        renderStatusButton marketId "btn-outline-primary" "Closed"
    | marketStatus == MarketStatusResolved =
        renderStatusButton marketId "btn-outline-success" "Resolved"
    | marketStatus == MarketStatusRefunded =
        renderStatusButton marketId "btn-outline-danger" "Refunded"
-- For open markets with open positions, show profit/loss buttons
renderActionButton True isProfitable _ assetId _ =
    let (cls, txt) = if isProfitable
            then ("btn-success" :: Text, "Take Profit" :: Text)
            else ("btn-danger" :: Text, "Close Loss" :: Text)
    in [hsx|
        <form action={ClosePositionAction assetId} method="POST" class="d-inline">
            <button type="submit" class={"btn btn-sm text-nowrap " <> cls}
                    style="width: 94px;">{txt}</button>
        </form>
    |]
-- For open markets with closed positions, show Make Trade
renderActionButton False _ marketId _ _ =
    let link = ShowMarketAction marketId Nothing Nothing
    in [hsx|
        <a href={link}
           class="btn btn-primary btn-sm text-nowrap"
           style="width: 94px;">Make Trade</a>
    |]

renderStatusButton :: Id Market -> Text -> Text -> Html
renderStatusButton marketId cls txt =
    let link = ShowMarketAction marketId Nothing Nothing
    in [hsx|
        <a href={link}
           class={"btn btn-sm text-nowrap " <> cls}
           style="width: 94px;">{txt}</a>
    |]

renderHoldingsPagination :: Int -> Int -> Html
renderHoldingsPagination currentPage totalPages =
    renderSmartPagination currentPage totalPages "Positions pagination"
        (\pageNum -> pathTo (DashboardHoldingsAction (Just pageNum)))

renderClosedPnL :: Integer -> Html
renderClosedPnL pnl
    | pnl == 0 = [hsx|<span class="fw-medium">--</span>|]
    | pnl > 0 = [hsx|<span class="fw-bold text-success">{formatMoneySigned pnl}</span>|]
    | otherwise = [hsx|<span class="fw-bold text-danger">{formatMoneySigned pnl}</span>|]

