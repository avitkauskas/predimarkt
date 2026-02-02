# Holdings View Table Refactoring Plan

## Overview
Refactor the Holdings view from card-based display to a table-based layout for better data density and comparison.

## Current State
- Holdings are displayed as individual Bootstrap cards
- Each card shows: Market title + Asset name, position info, profit/loss
- Cards take significant vertical space

## Target State
- Table-based display with one row per holding
- Consistent with Transactions view styling

## Table Structure

### Columns (left to right):
| Column | Description | Data Source |
|--------|-------------|-------------|
| Market & Asset | Market title + Asset name (linked) | `market.title`, `asset.name` |
| Probability | Current asset price as percentage | `assetPrice` (rounded to %) |
| Position | long / short / closed | `holding.quantity` |
| Shares | Number of shares held | `abs holding.quantity` |
| Stake | Money invested in position | `moneyFromCents (abs holding.amountCents)` |
| Current Value | Current value of position | `currentValue` (Maybe Money) |
| Max Gain | Maximum potential profit | Calculated based on position type |
| Now | Current P&L with +/- indicator | Calculated from currentValue vs stake |
| Action | Close position button (for open positions) | Form with ClosePositionAction |

### Row Styling Notes:
- Use `table-sm` for compact rows
- Use `text-end` class for numeric columns (Shares, Stake, Current Value, Max Gain, Now)
- Use `text-center` for Position column
- Use `text-success`/`text-danger` for Now column based on profitability
- Position badges: "Long" (green), "Short" (red), "Closed" (gray)
- Probability displayed as percentage badge

### Empty States:
- If `currentValue` is Nothing: show "-" for Current Value, Max Gain, Now columns
- For closed positions: Action column is empty

## Implementation Details

### HTML Structure:
```html
<div class="h-100">
    <h3>My Holdings</h3>
    <div class="table-responsive">
        <table class="table table-sm table-hover table-borderless">
            <thead>
                <tr>
                    <th>Market & Asset</th>
                    <th class="text-center">Probability</th>
                    <th class="text-center">Position</th>
                    <th class="text-end">Shares</th>
                    <th class="text-end">Stake</th>
                    <th class="text-end">Current Value</th>
                    <th class="text-end">Max Gain</th>
                    <th class="text-end">Now</th>
                    <th class="text-center">Action</th>
                </tr>
            </thead>
            <tbody>
                {forEach holdingsWithValue renderHoldingRow}
            </tbody>
        </table>
    </div>
</div>
```

### Helper Functions to Extract:
1. `getPositionType :: Holding -> Text` - Returns "Long", "Short", or "Closed"
2. `getPositionClass :: Holding -> Text` - Returns "text-success", "text-danger", or "text-muted"
3. `calculateMaxGain :: HoldingWithValue -> Maybe Money` - Calculate max potential gain
4. `calculateNow :: HoldingWithValue -> (Money, Bool)` - Calculate current P&L and profitability
5. `renderCloseButton :: HoldingWithValue -> Html` - Render close button for open positions

### Data Transformations:
- **Shares**: `abs holding.quantity` (always positive number)
- **Stake**: `formatMoney $ moneyFromCents (abs holding.amountCents)`
- **Current Value**: `maybe "-" formatMoney currentValue`
- **Max Gain**:
  - Long: `moneyFromCents (fromIntegral holding.quantity * 100 - holding.amountCents)`
  - Short: `moneyFromCents (abs holding.amountCents)` (the stake amount)
  - Closed: "-"
- **Now**: Show formatted money with +/- prefix and color class

## Files to Modify:
- `Web/View/Dashboard/Holdings.hs` - Main refactoring

## No CSS Changes Required:
- Uses existing Bootstrap table classes from Transactions view pattern
- Dark theme support already in `static/app.css`
