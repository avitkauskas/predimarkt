module Web.View.Dashboard.Holdings where

import Application.Helper.View (formatMoney)
import Web.View.Prelude

data HoldingWithValue = HoldingWithValue
    { holding      :: Include' ["marketId", "assetId"] Holding
    , currentValue :: Maybe Integer  -- Current value in cents
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
        stake = abs (holding.costBasis) :: Integer

        -- Position type and styling (determined by side field)
        (positionText, positionClass) = case holding.side of
            Nothing          -> ("closed" :: Text, "text-muted" :: Text)
            Just "short"     -> ("short" :: Text, "text-danger" :: Text)
            Just "long"      -> ("long" :: Text, "text-success" :: Text)
            Just _           -> ("unknown" :: Text, "text-warning" :: Text)

        shares = abs holding.quantity

        -- Shares display: "-" for closed positions, number for open
        sharesDisplay :: Text
        sharesDisplay = if holding.quantity == 0 then "-" else show shares

        -- Max gain calculation
        maxGainCents :: Maybe Integer
        maxGainCents = case holding.side of
            Just "short" -> Just stake  -- For short: max gain is the stake received
            Just "long"  -> Just (fromIntegral holding.quantity * 100 - holding.costBasis)
            _            -> Nothing

        -- Current P&L calculation
        (nowMoneyCents, nowClass, isProfitable) = case currentValue of
            Just valueCents ->
                let diff = valueCents - stake
                    profit = abs diff
                    profitable = case holding.side of
                        Just "long"  -> diff >= 0
                        Just "short" -> diff <= 0
                        _            -> diff >= 0
                    cls :: Text
                    cls = if profitable then "text-success" else "text-danger"
                in (profit, cls, profitable)
            Nothing ->
                let cls :: Text
                    cls = if holding.costBasis <= 0 then "text-success" else "text-danger"
                in (stake, cls, holding.costBasis <= 0)

        nowSign :: Text
        nowSign = if isProfitable then "+" else "-"

        -- Navigation URL for the market/asset link
        tradeAction = case holding.side of
            Just "long"  -> Just "buy"
            Just "short" -> Just "sell"
            _            -> Nothing
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
            <td class="text-end">{maybe "-" formatMoney maxGainCents}</td>
            <td class={"text-end text-nowrap " <> nowClass}>{formatMoney nowMoneyCents}{nowSign}</td>
            <td class="text-center">{closeButton}</td>
        </tr>
    |]
