{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Web.View.Dashboard.Positions where

import Web.View.Prelude

data PositionWithValue = PositionWithValue
    { position     :: Include' ["marketId", "assetId"] Position
    , currentValue :: Maybe Integer
    , assetPrice   :: Maybe Double
    }

data PositionsView = PositionsView
    { positionsWithValue :: [PositionWithValue]
    , currentPage        :: Int
    , totalPages         :: Int
    , wallet             :: Wallet
    }

instance View PositionsView where
    html PositionsView { .. } = dashboardLayout [hsx|
        <div class="container-fluid">
            <div class="d-flex justify-content-between align-items-center mb-2" style="max-width: 1000px;">
                <h5>Positions</h5>
                <div class="text-end me-1">
                    Cash Balance: <span class="fw-bold">{formatMoney wallet.amount}</span>
                </div>
            </div>
            {renderPositionsContent positionsWithValue currentPage totalPages}
        </div>
    |]

renderPositionsContent :: (?context :: ControllerContext) => [PositionWithValue] -> Int -> Int -> Html
renderPositionsContent [] _ _ = [hsx|
    <div class="alert alert-info">
        No active positions. Browse <a href={MarketsAction} class="alert-link">markets</a> to trade.
    </div>
|]
renderPositionsContent positions currentPage totalPages = [hsx|
    <div class="row g-3">
        {forEach positions renderPositionCard}
    </div>
    <div style="max-width: 1000px;">
        {renderPositionsPagination currentPage totalPages}
    </div>
|]

renderPositionCard :: (?context :: ControllerContext) => PositionWithValue -> Html
renderPositionCard pvd =
    let position = get #position pvd
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
            _ -> maybe "--" formatPricePercent (get #assetPrice pvd)

        invested = get #invested position
        received = get #received position
        costBasis = abs (invested + received)

        currentVal = fromMaybe 0 (get #currentValue pvd)

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
            then ShowMarketAction market.id (Just asset.id) nextAction
            else ShowMarketAction market.id Nothing Nothing

        positionDisplay = renderPositionDisplay isOpen isLong absQty
        pnlDisplay = renderPnLDisplay currentPnL
        pnlClass :: Text = case currentPnL of
            n | n > 0 -> "text-success fw-bold"
              | n < 0 -> "text-danger fw-bold"
              | otherwise -> "fw-medium"
        actionBtn = renderActionButton isOpen isProfitable market.id asset.id market.status
    in [hsx|
        <div class="col-12">
            <div class="card shadow-sm" style="max-width: 1000px;">
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

renderPositionsPagination :: Int -> Int -> Html
renderPositionsPagination currentPage totalPages =
    renderSmartPagination currentPage totalPages "Positions pagination"
        (\pageNum -> pathTo (DashboardPositionsAction (Just pageNum)))

renderClosedPnL :: Integer -> Html
renderClosedPnL pnl
    | pnl == 0 = [hsx|<span class="fw-medium">--</span>|]
    | pnl > 0 = [hsx|<span class="fw-bold text-success">{formatMoneySigned pnl}</span>|]
    | otherwise = [hsx|<span class="fw-bold text-danger">{formatMoneySigned pnl}</span>|]
