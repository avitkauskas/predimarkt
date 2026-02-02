module Web.View.Dashboard.Holdings where

import Web.Types.Money
import Web.View.Prelude

data HoldingWithValue = HoldingWithValue
    { holding      :: Include' ["marketId", "assetId"] Holding
    , currentValue :: Maybe Money
    , assetPrice   :: Maybe Double  -- Current asset price as percentage (0-1)
    }

data HoldingsView = HoldingsView { holdingsWithValue :: [HoldingWithValue] }

instance View HoldingsView where
    html HoldingsView { .. } = dashboardLayout [hsx|
        <div class="h-100">
            <h3>My Holdings</h3>
            {forEach holdingsWithValue renderHolding}
        </div>
    |]

renderHolding :: (?context :: ControllerContext) => HoldingWithValue -> Html
renderHolding HoldingWithValue { .. } =
    let asset = holding.assetId
        market = holding.marketId
        money = formatMoney $ moneyFromCents (abs holding.amountCents)
        holdingMoney = moneyFromCents (abs holding.amountCents)

        -- Calculate profit/loss and determine color
        -- For long positions: profit if current value > money
        -- For short positions: profit if current value < money (cost to close is less than received)
        (profitLossMoney, isProfitable) = case currentValue of
            Just value ->
                let diff = moneyToCents value - moneyToCents holdingMoney
                    profit = moneyFromCents (abs diff)
                    -- For long: profitable if value >= money
                    -- For short: profitable if value <= money (negative diff means profit)
                    profitable = if holding.quantity > 0 then diff >= 0 else diff <= 0
                in (profit, profitable)
            Nothing    ->
                (holdingMoney, holding.amountCents < 0)  -- For closed positions

        profitLossColor :: Text
        profitLossColor = if isProfitable then "text-success" else "text-danger"

        -- profitLossLabel = if isProfitable then "profit" else "loss" :: Text
        profitLossLabel = if isProfitable then "+" else "-" :: Text

        profitLossHtml = [hsx|
            <div class={profitLossColor}>
                <!-- <strong>{profitLossLabel} of {formatMoney profitLossMoney}</strong> -->
                <strong>{formatMoney profitLossMoney} {profitLossLabel}</strong>
            </div>
        |]

        -- Position line with current value (for open positions)
        position = case holding.quantity of
            0 -> [hsx|closed|]
            n | n < 0 ->
                case currentValue of
                    Just value -> [hsx|open : short : {show (abs n)} shares : {money} : current value {formatMoney value} : max profit {formatMoney (moneyFromCents (abs holding.amountCents))}|]
                    Nothing    -> [hsx|open : short : {show (abs n)} shares : {money}|]
            _ ->
                case currentValue of
                    Just value -> [hsx|open : long : {show holding.quantity} shares : {money} : current value {formatMoney value} : max profit {formatMoney (moneyFromCents (fromIntegral holding.quantity * 100 - holding.amountCents))}|]
                    Nothing    -> [hsx|open : long : {show holding.quantity} shares : {money}|]

        -- Navigation URL for the title link
        -- For long positions: open buy form, for short positions: open sell form
        tradeAction = case holding.quantity of
            q | q > 0 -> Just "buy"
            q | q < 0 -> Just "sell"
            _         -> Nothing
        titleUrl = ShowMarketAction market.id (Just asset.id) tradeAction

        -- Asset price percentage display
        pricePercentageHtml = case assetPrice of
            Nothing -> [hsx||]
            Just price ->
                let percentage = round (price * 100) :: Int
                in [hsx|<span class="badge bg-secondary ms-2">{show percentage}%</span>|]

        closeButtonClass :: Text
        closeButtonClass = if isProfitable then "btn-outline-success" else "btn-outline-danger"

        closeButton = case currentValue of
            Nothing -> [hsx||]
            Just _ -> [hsx|
                <form action={ClosePositionAction asset.id} method="POST" class="d-inline">
                    <button type="submit" class={"btn " <> closeButtonClass <> " btn-sm"}>
                        Close position
                    </button>
                </form>
            |]
    in [hsx|
        <div class="card shadow-sm mb-3">
            <div class="card-header d-flex justify-content-between align-items-center">
                <a href={titleUrl} class="text-decoration-none d-flex align-items-center">
                    <h6 class="mb-0">{market.title} - {asset.name}</h6>
                    {pricePercentageHtml}
                </a>
                {closeButton}
            </div>
            <div class="card-body">
                <div class="d-flex justify-content-between align-items-center">
                    <div class="text-muted">{position}</div>
                    {profitLossHtml}
                </div>
            </div>
        </div>
    |]
