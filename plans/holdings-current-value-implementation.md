# Holdings Current Value Implementation Plan

## Overview
Add current value display for each asset in the holdings view. The current value represents the money you could get now for selling an open long position or buying back (closing) an open short position.

## Data Flow Analysis

### Current State
- `DashboardHoldingsAction` fetches holdings with `assetId` and `marketId` relationships
- `HoldingsView` receives a list of holdings
- `renderHolding` displays position information (long/short, quantity, profit/loss)

### Required Data for Current Value Calculation
To calculate current value using LMSR functions, we need:
1. **Holding**: `quantity` (to determine long/short and amount)
2. **Asset**: `id`, `quantity` (for LMSR state computation)
3. **Market**: `id`, `beta` (for LMSR calculations)
4. **LMSR State**: Computed from all assets in the market

### Calculation Logic
- **Long position** (`quantity > 0`): Use `calculateSellRevenue` to get money received from selling
- **Short position** (`quantity < 0`): Use `calculateBuyCost` to get cost of buying back shares
- **Closed position** (`quantity == 0`): No current value (position is closed)

## Implementation Plan

### 1. Create Enriched Data Structure

Create a new data type `HoldingWithValue` that combines a holding with its pre-computed current value:

```haskell
-- In Web/View/Dashboard/Holdings.hs
data HoldingWithValue = HoldingWithValue
    { holding :: Include' ["marketId", "assetId"] Holding
    , currentValue :: Maybe Money  -- Nothing for closed positions
    }

data HoldingsView = HoldingsView 
    { holdingsWithValue :: [HoldingWithValue] 
    }
```

### 2. Modify DashboardHoldingsAction

Update the controller to:
1. Fetch holdings (as currently done)
2. Group holdings by market
3. For each market, fetch all assets and compute LMSR state
4. Calculate current value for each holding using LMSR functions
5. Pass enriched data to the view

```haskell
-- In Web/Controller/Dashboard.hs
action DashboardHoldingsAction = do
    holdings <- query @Holding
        |> filterWhere (#userId, currentUserId)
        |> fetch
        >>= collectionFetchRelated #assetId
        >>= collectionFetchRelated #marketId
    
    -- Group holdings by market
    let holdingsByMarket = groupBy ((==) `on` (get #marketId . get #holding)) holdings
    
    -- For each market, fetch assets and compute current values
    holdingsWithValue <- forM holdingsByMarket $ \marketHoldings -> do
        let market = get #marketId (head marketHoldings)
        assets <- query @Asset
            |> filterWhere (#marketId, market.id)
            |> fetch
        
        let lmsrState = precompute market.beta assets
        
        -- Calculate current value for each holding
        forM marketHoldings $ \holding -> do
            let asset = get #assetId holding
                assetSum = sumItem asset.id lmsrState
                assetTotal = sumTotal lmsrState
                currentPrice = assetSum / assetTotal
            
            let currentValue = case holding.quantity of
                    0 -> Nothing  -- Closed position
                    q | q > 0 -> Just $ moneyFromDouble $ 
                        calculateSellRevenue q currentPrice market.beta assetTotal
                    q -> Just $ moneyFromDouble $ 
                        calculateBuyCost (abs q) currentPrice market.beta assetTotal
            
            return HoldingWithValue { holding = holding, currentValue = currentValue }
    
    render HoldingsView { holdingsWithValue = concat holdingsWithValue }
```

### 3. Update renderHolding

Modify the view to display current value:

```haskell
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
            Nothing -> [hsx||]  -- Closed position, no current value
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
```

### 4. Required Imports

Add necessary imports to both files:

**Web/Controller/Dashboard.hs:**
```haskell
import Application.Helper.LMSR
import Web.Types.Money
import Data.List (groupBy, nub)
import Data.Function (on)
```

**Web/View/Dashboard/Holdings.hs:**
```haskell
-- Already has: import Web.Types.Money
-- Need to ensure LMSR functions are available through View prelude
```

## Efficiency Considerations

1. **Batch Market Fetching**: Group holdings by market to avoid redundant asset queries
2. **Single LMSR Computation**: Compute LMSR state once per market and reuse for all holdings in that market
3. **Lazy Evaluation**: Only calculate current values for open positions

## Edge Cases

1. **Closed positions** (`quantity == 0`): Display no current value
2. **Markets with no assets**: Should not happen in practice, but handle gracefully
3. **Short positions**: Calculate buy cost (positive value representing cost to close)

## Files to Modify

1. `Web/Controller/Dashboard.hs` - Update `DashboardHoldingsAction`
2. `Web/View/Dashboard/Holdings.hs` - Update `HoldingsView` and `renderHolding`
