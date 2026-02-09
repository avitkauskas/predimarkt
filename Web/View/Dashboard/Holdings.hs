{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Web.View.Dashboard.Holdings where

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

data HoldingsView = HoldingsView { holdingsWithValue :: [HoldingWithValue] }

instance View HoldingsView where
    html HoldingsView { .. } = dashboardLayout [hsx|
        <div class="container-fluid py-3">
            <h5 class="mb-3 text-muted">Positions</h5>
            {renderHoldingsContent holdingsWithValue}
        </div>
    |]

renderHoldingsContent :: (?context :: ControllerContext) => [HoldingWithValue] -> Html
renderHoldingsContent [] = [hsx|
    <div class="alert alert-info">
        No active positions. <a href={MarketsAction} class="alert-link">Browse markets</a> to trade.
    </div>
|]
renderHoldingsContent holdings = [hsx|
    <div class="row g-3">
        {forEach holdings renderHoldingCard}
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

        currentPnL = if isLong
                then currentVal - costBasis
                else qty * 100 - costBasis - currentVal
        isProfitable = currentPnL >= 0
        maxProfit = qty * 100 - costBasis
        realizedPnL = get #realizedPnl holding
        updatedTime = formatTime defaultTimeLocale "%F %R" (get #updatedAt holding)
        closingTime = formatTime defaultTimeLocale "%F %R" (get #closedAt market)

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
            <div class="card shadow-sm">
                <div class="card-body px-3 py-2">
                    <div class="d-flex justify-content-between align-items-start mb-2">
                        <a href={marketUrl} class="text-decoration-none">
                            <span class="mb-0 h6 fw-bold">{get #title market}</span> -
                            <span class="text-muted">{get #name asset}</span>
                        </a>
                    </div>

                    <div class="row text-center small border-top border-bottom py-2 mb-2">
                        <div class="col-2 border-end">
                            <div class="text-muted" style="font-size: 0.7rem;">Position</div>
                            <div class="fw-medium">{positionDisplay}</div>
                        </div>
                        <div class="col-2 border-end">
                            <div class="text-muted" style="font-size: 0.7rem;">Current Value</div>
                            <div class="fw-medium">{formatMoney currentVal}</div>
                        </div>
                        <div class="col-2">
                            <div class="text-muted" style="font-size: 0.7rem;">Unrealized P&L</div>
                            <div class={pnlClass}>
                                {pnlDisplay}
                            </div>
                        </div>
                        <div class="col-2 text-start border-end py-1">
                            {actionBtn}
                        </div>
                        <div class="col-2 border-end">
                            <div class="text-muted" style="font-size: 0.7rem;">Realized P&L</div>
                            <div class="fw-medium">{realizedDisplay}</div>
                        </div>
                        <div class="col-2">
                            <div class="text-muted" style="font-size: 0.7rem;">Max Open P&L</div>
                            <span class="fw-medium">{formatMoneySigned (negate costBasis)}</span>&nbsp;
                            <span class="fw-medium">{formatMoneySigned maxProfit}</span>
                        </div>
                    </div>

                    <div class="row">
                            <div class="col-4 small text-center fw-medium">
                                <span class="text-muted" style="font-size: 0.7rem;">
                                    Current probability
                                </span>
                                {probText}
                            </div>
                            <div class="col-4 text-muted small text-center fw-medium"
                                 style="font-size: 0.7rem;">
                                Your last trade @ {updatedTime}
                            </div>
                            <div class="col-4 text-muted small text-center fw-medium"
                                 style="font-size: 0.7rem;">
                                Market closes @ {closingTime}
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
           class="btn btn-outline-primary btn-sm">Make Trade</a>
    |]
renderActionButton True isProfitable _ assetId =
    let (cls, txt) = if isProfitable
            then ("btn-outline-success" :: Text, "Take Profit" :: Text)
            else ("btn-outline-danger" :: Text, "Close Loss" :: Text)
    in [hsx|
        <form action={ClosePositionAction assetId} method="POST" class="d-inline">
            <button type="submit" class={"btn btn-sm " <> cls}>{txt}</button>
        </form>
    |]
