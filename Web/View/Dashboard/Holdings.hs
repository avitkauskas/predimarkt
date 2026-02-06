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
            <div class="table-responsive">
                <table class="table table-sm table-hover table-borderless">
                    <thead>
                        <tr>
                            <th>Market & Asset</th>
                            <th class="text-center">Probability</th>
                            <th class="text-center">Position</th>
                            <th class="text-center">Shares</th>
                            <th class="text-end">Stake</th>
                            <th class="text-end">Value</th>
                            <th class="text-end">Potential</th>
                            <th class="text-end pe-3">Now</th>
                            <th class="text-center"></th>
                        </tr>
                    </thead>
                    <tbody>
                        {forEach holdingsWithValue renderHoldingRow}
                    </tbody>
                </table>
            </div>
        </div>
    |]

renderHoldingRow :: (?context :: ControllerContext) => HoldingWithValue -> Html
renderHoldingRow HoldingWithValue { .. } =
    let asset = holding.assetId
        market = holding.marketId
        stake = moneyFromCents (abs holding.amountCents)

        -- Position type and styling
        (positionText, positionClass) = case holding.quantity of
            0         -> ("closed" :: Text, "text-muted" :: Text)
            n | n < 0 -> ("short" :: Text, "text-danger" :: Text)
            _         -> ("long" :: Text, "text-success" :: Text)

        shares = abs holding.quantity

        -- Shares display: "-" for closed positions, number for open
        sharesDisplay :: Text
        sharesDisplay = if holding.quantity == 0 then "-" else show shares

        -- Max gain calculation
        maxGain = case holding.quantity of
            0  -> Nothing
            n | n < 0 -> Just stake  -- For short: max gain is the stake received
            _  -> Just $ moneyFromCents (fromIntegral holding.quantity * 100 - holding.amountCents)

        -- Current P&L calculation
        (nowMoney, nowClass, isProfitable) = case currentValue of
            Just value ->
                let diff = moneyToCents value - moneyToCents stake
                    profit = moneyFromCents (abs diff)
                    profitable = if holding.quantity > 0 then diff >= 0 else diff <= 0
                    cls :: Text
                    cls = if profitable then "text-success" else "text-danger"
                in (profit, cls, profitable)
            Nothing ->
                let cls :: Text
                    cls = if holding.amountCents <= 0 then "text-success" else "text-danger"
                in (stake, cls, holding.amountCents <= 0)

        nowSign :: Text
        nowSign = if isProfitable then "+" else "-"

        -- Navigation URL for the market/asset link
        tradeAction = case holding.quantity of
            q | q > 0 -> Just "buy"
            q | q < 0 -> Just "sell"
            _         -> Nothing
        titleUrl = ShowMarketAction market.id (Just asset.id) tradeAction

        -- Probability display as simple text (for all positions including closed)
        probabilityText :: Text
        probabilityText = case assetPrice of
            Nothing    -> "-"
            Just price -> show (round (price * 100) :: Int) <> "%"

        -- Close button for open positions
        closeButton = case currentValue of
            Nothing -> [hsx||]
            Just _ ->
                let btnClass :: Text
                    btnClass = if isProfitable then "btn-outline-success" else "btn-outline-danger"
                in [hsx|
                    <form action={ClosePositionAction asset.id} method="POST" class="d-inline">
                        <button type="submit" class={"btn " <> btnClass <> " btn-sm"}>
                            Close
                        </button>
                    </form>
                |]
    in [hsx|
        <tr class="small">
            <td>
                <a href={titleUrl} class="text-decoration-none">
                    {market.title}
                </a><br/>{asset.name}
            </td>
            <td class="text-center">{probabilityText}</td>
            <td class={"text-center " <> positionClass}>{positionText}</td>
            <td class="text-center">{sharesDisplay}</td>
            <td class="text-end">{formatMoney stake}</td>
            <td class="text-end">{maybe "-" formatMoney currentValue}</td>
            <td class="text-end">{maybe "-" formatMoney maxGain}</td>
            <td class={"text-end text-nowrap " <> nowClass}>{formatMoney nowMoney}{nowSign}</td>
            <td class="text-center">{closeButton}</td>
        </tr>
    |]
