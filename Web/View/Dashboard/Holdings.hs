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
        holdingMoney = moneyFromCents (abs holding.amountCents)
        profit =
            if holding.amountCents < 0
                then [hsx|profit of {money}|]
                else [hsx|loss of {money}|]
        position =
            case holding.quantity of
                0 -> [hsx|closed : {profit}|]
                n | n < 0 -> [hsx|open : short : {show (abs n)} shares : {money}|]
                _ -> [hsx|open : long : {show holding.quantity} shares : {money}|]

        (currentValueHtml, closeButton) = case currentValue of
            Nothing -> ([hsx||], [hsx||])
            Just value ->
                let valueCents = moneyToCents value
                    holdingCents = moneyToCents holdingMoney
                    isProfitable = valueCents >= holdingCents
                    colorClass :: Text
                    colorClass = if isProfitable then "text-success" else "text-danger"
                    -- For long positions: sell to close
                    -- For short positions: buy to close
                    closeUrl = ClosePositionAction asset.id
                in ( [hsx|
                        <div class={colorClass}>
                            <strong>Current value: {formatMoney value}</strong>
                        </div>
                     |]
                   , [hsx|
                        <form action={closeUrl} method="POST" class="d-inline">
                            <button type="submit" class="btn btn-primary btn-sm">
                                Close position
                            </button>
                        </form>
                     |]
                   )
    in [hsx|
        <div class="card shadow-sm mb-3">
            <div class="card-header d-flex justify-content-between align-items-center">
                <h5 class="mb-0">{market.title} - {asset.name}</h5>
                {closeButton}
            </div>
            <div class="card-body">
                <div class="d-flex justify-content-between align-items-start">
                    <div class="text-muted">{position}</div>
                    <div class="text-end">{currentValueHtml}</div>
                </div>
            </div>
        </div>
    |]
