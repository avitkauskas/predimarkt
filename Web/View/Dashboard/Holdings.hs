module Web.View.Dashboard.Holdings where

import Web.Types.Money
import Web.View.Prelude

data HoldingsView = HoldingsView { holdings :: [Include' ["marketId", "assetId"] Holding] }

instance View HoldingsView where
    html HoldingsView { .. } = dashboardLayout [hsx|
        <div class="h-100">
            <h3>My Holdings</h3>
            {forEach holdings renderHolding}
        </div>
    |]

renderHolding :: (?context :: ControllerContext) => Include' ["marketId", "assetId"] Holding -> Html
renderHolding holding =
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
    in [hsx|
        <div class="card shadow-sm mb-3">
            <div class="card-header">
                <h5 class="mb-0">{market.title} - {asset.name}</h5>
            </div>
            <div class="card-body">
                <div class="text-muted">{position}</div>
            </div>
        </div>
    |]
