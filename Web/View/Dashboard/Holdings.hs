{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Web.View.Dashboard.Holdings where

import Admin.Controller.Prelude (render)
import Application.Helper.View (formatMoney, formatPricePercent)
import Data.Profunctor.Closed (close)
import Data.Text (pack)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Text.Printf (printf)
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
        realizedPnL = get #realizedPnl holding
        -- updatedTime = formatTime defaultTimeLocale "%F %R" holding.updatedAt
        -- closingTime = formatTime defaultTimeLocale "%F %R" market.closedAt

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
        realizedDisplay = renderRealizedDisplay realizedPnL
        actionBtn = renderActionButton isOpen isProfitable market.id asset.id
    in [hsx|
        <div class="col-12">
            <div class="card shadow-sm" style="max-width: 900px;">
                <div class="card-body px-3 py-2">
                    <div class="d-flex justify-content-between align-items-start mb-2">
                        <a href={marketUrl} class="text-decoration-none">
                            <span class="mb-0 h6 fw-bold">{get #title market}</span> -
                            <span class="text-muted">{get #name asset}</span>
                        </a>
                    </div>

                    <div class="overflow-x-auto">
                        <div class="d-flex justify-content-between border-top border-bottom py-2 mb-2" style="min-width: 640px;">
                            <div class="flex-shrink-0 text-center px-2" style="width: 90px;">
                                <div class="text-muted text-nowrap" style="font-size: 0.7rem;">Position</div>
                                <div class="fw-medium text-nowrap">{positionDisplay}</div>
                            </div>
                            <div class="flex-shrink-0 text-center px-2" style="width: 100px;">
                                <div class="text-muted text-nowrap" style="font-size: 0.7rem;">Current Value</div>
                                <div class="fw-medium text-nowrap">{formatMoney currentVal}</div>
                            </div>
                            <div class="flex-shrink-0 text-center px-2" style="width: 90px;">
                                <div class="text-muted text-nowrap" style="font-size: 0.7rem;">{if isLong then ("Paid" :: Text) else ("Received" :: Text)}</div>
                                <div class="fw-medium text-nowrap">{formatMoney costBasis}</div>
                            </div>
                            <div class="flex-shrink-0 text-center px-2" style="width: 250px;">
                                <div class="d-flex align-items-center justify-content-center" style="gap: 24px;">
                                    <div class="text-center" style="width: 75px;">
                                        <div class="text-muted text-nowrap" style="font-size: 0.7rem;">Unrealized P&L</div>
                                        <div class={pnlClass}>{pnlDisplay}</div>
                                    </div>
                                    <div>
                                        {actionBtn}
                                    </div>
                                    <div class="text-center" style="width: 75px;">
                                        <div class="text-muted text-nowrap" style="font-size: 0.7rem;">{if isLong then ("Max Win" :: Text) else ("Max Loss" :: Text)}</div>
                                        <div class="fw-medium text-nowrap">{formatMoneySigned maxOutcome}</div>
                                    </div>
                                </div>
                            </div>
                            <div class="flex-shrink-0 text-center px-2" style="width: 110px;">
                                <div class="text-muted text-nowrap" style="font-size: 0.7rem;">Realized P&L</div>
                                <div class="fw-medium text-nowrap">{realizedDisplay}</div>
                            </div>
                        </div>
                    </div>

                    <div class="overflow-x-auto">
                        <div class="d-flex justify-content-between align-items-center small flex-nowrap" style="min-width: 530px;">
                            <div class="text-start ms-1" style="max-width: 200px;">
                                <span class="text-muted text-nowrap"
                                      style="font-size: 0.7rem;">
                                    Current probability
                                </span>
                                <span class="text-nowrap">&nbsp;{probText}</span>
                            </div>
                            <div class="text-muted text-center text-nowrap mx-1"
                                 style="font-size: 0.7rem; max-width: 500px;">
                                Your last trade @ {renderTime holding.updatedAt}
                            </div>
                            <div class="text-muted text-end text-nowrap me-1" style="font-size: 0.7rem;"
                                 title="Market closing time">
                                <i class="bi bi-alarm"></i>
                                {renderTime market.closedAt}
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

renderRealizedDisplay :: Integer -> Html
renderRealizedDisplay pnl
    | pnl == 0 = [hsx|<span class="text-muted">--</span>|]
    | pnl > 0 = [hsx|<span class="fw-bold text-success">{formatMoneySigned pnl}</span>|]
    | otherwise = [hsx|<span class="fw-bold text-danger">{formatMoneySigned pnl}</span>|]

renderActionButton :: Bool -> Bool -> Id Market ->Id Asset -> Html
renderActionButton False _ marketId assetId = [hsx|
        <a href={ShowMarketAction marketId (Just assetId) (Just "buy")}
           class="btn btn-outline-primary btn-sm text-nowrap">Make Trade</a>
    |]
renderActionButton True isProfitable _ assetId =
    let (cls, txt) = if isProfitable
            then ("btn-outline-success" :: Text, "Take Profit" :: Text)
            else ("btn-outline-danger" :: Text, "Close Loss" :: Text)
    in [hsx|
        <form action={ClosePositionAction assetId} method="POST" class="d-inline">
            <button type="submit" class={"btn btn-sm text-nowrap " <> cls}>{txt}</button>
        </form>
    |]

renderPnLRange :: Bool -> Integer -> Integer -> Html
renderPnLRange isOpen maxLoss maxProfit =
    if isOpen
    then [hsx|
        <span class="fw-medium">{formatMoneySigned maxLoss}</span>&nbsp;
        <span class="fw-medium">{formatMoneySigned maxProfit}</span>
    |]
    else [hsx|<span class="fw-medium">--</span>|]

renderHoldingsPagination :: Int -> Int -> Html
renderHoldingsPagination currentPage totalPages =
    renderSmartPagination currentPage totalPages "Positions pagination"
        (\pageNum -> pathTo (DashboardHoldingsAction (Just pageNum)))

