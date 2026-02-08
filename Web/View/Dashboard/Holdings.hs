{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Web.View.Dashboard.Holdings where

import Application.Helper.View (formatMoney, formatPricePercent)
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

        qty = abs (get #quantity holding)
        isLong = get #side holding == Just "long"
        isOpen = qty > 0

        probText = case get #assetPrice hwd of
            Just p  -> formatPricePercent p
            Nothing -> "-"

        costBasis = abs (get #costBasis holding)
        currentVal = case get #currentValue hwd of
            Just v  -> v
            Nothing -> 0

        currentPnL = if isLong then currentVal - costBasis else costBasis - currentVal
        isProfitable = currentPnL > 0
        maxProfit = if isLong then qty * 100 - costBasis else costBasis
        realizedPnL = get #realizedPnl holding
        updatedTime = formatTime defaultTimeLocale "%H:%M %d/%m" (get #updatedAt holding)

        nextAction = if isLong then Just "sell" else Just "buy"
        marketUrl = ShowMarketAction market.id (Just asset.id) nextAction

        positionDisplay = renderPositionDisplay isOpen isLong qty
        pnlDisplay = renderPnLDisplay currentPnL
        pnlClass :: Text = if currentPnL >= 0 then "text-success fw-bold" else "text-danger fw-bold"
        maxProfitDisplay :: Text = "+€" <> formatMoneyAmount maxProfit
        realizedDisplay = renderRealizedDisplay realizedPnL
        closeBtn = renderCloseButton isOpen isProfitable asset.id
    in [hsx|
        <div class="col-12">
            <div class="card shadow-sm">
                <div class="card-body p-3">
                    <div class="d-flex justify-content-between align-items-start mb-3">
                        <div>
                            <a href={marketUrl} class="text-decoration-none">
                                <h6 class="mb-0 fw-bold">{get #title market}</h6>
                                <small class="text-muted">{get #name asset}</small>
                            </a>
                        </div>
                        <div class="text-end">
                            {positionDisplay}
                            <div class="small text-muted">{probText}</div>
                        </div>
                    </div>

                    <div class="row text-center small border-top border-bottom py-2 mb-3">
                        <div class="col-3 border-end">
                            <div class="text-muted" style="font-size: 0.7rem;">Cost Basis</div>
                            <div class="fw-medium">€{formatMoneyAmount costBasis}</div>
                        </div>
                        <div class="col-3 border-end">
                            <div class="text-muted" style="font-size: 0.7rem;">Current Value</div>
                            <div class="fw-medium">€{formatMoneyAmount currentVal}</div>
                        </div>
                        <div class="col-3 border-end">
                            <div class="text-muted" style="font-size: 0.7rem;">P&L</div>
                            <div class={pnlClass}>
                                {pnlDisplay}
                            </div>
                        </div>
                        <div class="col-3">
                            <div class="text-muted" style="font-size: 0.7rem;">Max Profit</div>
                            <div class="text-success fw-medium">{maxProfitDisplay}</div>
                        </div>
                    </div>

                    <div class="d-flex justify-content-between align-items-center">
                        <div class="small">
                            <span class="text-muted">Realized: </span>
                            {realizedDisplay}
                        </div>
                        <div class="d-flex align-items-center gap-2">
                            <small class="text-muted" style="font-size: 0.7rem;">{updatedTime}</small>
                            {closeBtn}
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
    else [hsx|<span class="badge bg-secondary">closed</span>|]

renderPnLDisplay :: Integer -> Html
renderPnLDisplay pnl =
    let sign = if pnl >= 0 then "+" else ""
        txt :: Text = sign <> "€" <> formatMoneyAmount (abs pnl)
    in [hsx|<span>{txt}</span>|]

renderRealizedDisplay :: Integer -> Html
renderRealizedDisplay pnl
    | pnl == 0 = [hsx|<span class="text-muted">-</span>|]
    | pnl > 0 = [hsx|<span class="text-success">+€{formatMoneyAmount pnl}</span>|]
    | otherwise = [hsx|<span class="text-danger">-€{formatMoneyAmount (abs pnl)}</span>|]

renderCloseButton :: Bool -> Bool -> Id Asset -> Html
renderCloseButton False _ _ = [hsx||]
renderCloseButton True isProfitable assetId =
    let (cls, txt) = if isProfitable
            then ("btn-success" :: Text, "Take Profit" :: Text)
            else ("btn-danger" :: Text, "Close Loss" :: Text)
    in [hsx|
        <form action={ClosePositionAction assetId} method="POST" class="d-inline">
            <button type="submit" class={"btn " <> cls <> " btn-sm"}>{txt}</button>
        </form>
    |]

formatMoneyAmount :: Integer -> Text
formatMoneyAmount cents =
    let euros = fromIntegral cents / 100 :: Double
    in pack (printf "%.2f" euros)
