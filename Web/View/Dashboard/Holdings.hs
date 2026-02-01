module Web.View.Dashboard.Holdings where

import Web.Types.Money
import Web.View.Prelude

data HoldingWithValue = HoldingWithValue
    { holding      :: Include' ["marketId", "assetId"] Holding
    , currentValue :: Maybe Money
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
        profit =
            if holding.amountCents < 0
                then [hsx|profit of {money}|]
                else [hsx|loss of {money}|]
        position =
            case holding.quantity of
                0 -> [hsx|closed : {profit}|]
                n | n < 0 -> [hsx|open : short : {show (abs n)} shares : {money}|]
                _ -> [hsx|open : long : {show holding.quantity} shares : {money}|]

        currentValueHtml = case currentValue of
            Nothing -> [hsx||]
            Just value -> [hsx|
                <div class="mt-2 text-success">
                    <strong>Current value: {formatMoney value}</strong>
                </div>
            |]
    in [hsx|
        <div class="card shadow-sm mb-3">
            <div class="card-header">
                <h5 class="mb-0">{market.title} - {asset.name}</h5>
            </div>
            <div class="card-body">
                <div class="text-muted">{position}</div>
                {currentValueHtml}
            </div>
        </div>
    |]
