module Domain.Logic
    ( -- Position operations
      applyTransaction
    , resolvePosition
    , refundPosition

      -- Utility functions
    , emptyPosition
    , isFlat
    , positionValue
    ) where

import Domain.Types
import IHP.Prelude

-- | An empty position (no holdings)
emptyPosition :: Position
emptyPosition = Position
    { posSide = Nothing
    , posQuantity = Quantity 0
    , posCostBasis = Balance 0
    , posRealizedPnL = Balance 0
    }

-- | Check if position is flat (no open position)
isFlat :: Position -> Bool
isFlat pos = case posSide pos of
    Nothing -> True
    Just _  -> False

-- | Calculate current position value (for display purposes)
-- Returns Just value if position is open, Nothing if flat
positionValue :: Position -> Price -> Maybe Balance
positionValue pos currentPrice = do
    side <- posSide pos
    let Quantity qty = posQuantity pos
    return $ case side of
        Long  -> Balance (round (fromInteger qty * currentPrice * 100))
        Short -> Balance (round (fromInteger qty * (1.0 - currentPrice) * 100))

-- | Apply a transaction to update a position
applyTransaction :: Transaction -> Position -> Position
applyTransaction tx pos =
    case posSide pos of
        Nothing ->
            openPosition tx pos
        Just side
            | side == txSide tx ->
                increasePosition tx pos
            | otherwise ->
                reduceOrFlipPosition tx pos

-- | Open a new position from flat
-- For Long: cost basis = money paid (positive)
-- For Short: cost basis = potential obligation - received = q * 100 - abs(cf)
openPosition :: Transaction -> Position -> Position
openPosition tx pos =
    let Balance cf = txCashFlow tx
        Quantity q = txQuantity tx
        cost = case txSide tx of
            Long  -> abs cf           -- Money paid
            Short -> q * 100 - abs cf  -- Net risk: obligation - received
    in pos
        { posSide = Just (txSide tx)
        , posQuantity = txQuantity tx
        , posCostBasis = Balance cost
        }

-- | Increase existing position (same side)
-- For Long: add money paid to cost basis
-- For Short: add net risk (new obligation - received) to cost basis
increasePosition :: Transaction -> Position -> Position
increasePosition tx pos =
    let Quantity oldQ = posQuantity pos
        Quantity newQ = txQuantity tx
        Balance oldCost = posCostBasis pos
        Balance cf = txCashFlow tx
        Just side = posSide pos
        additionalCost = case side of
            Long  -> abs cf                -- Additional money paid
            Short -> newQ * 100 - abs cf   -- Additional net risk
    in pos
        { posQuantity = Quantity (oldQ + newQ)
        , posCostBasis = Balance (oldCost + additionalCost)
        }

-- | Reduce or flip position (opposite side transaction)
-- Uses unified formula: realized = cf - releasedCost for both longs and shorts
-- (cost basis for shorts represents net risk, so same formula applies)
reduceOrFlipPosition :: Transaction -> Position -> Position
reduceOrFlipPosition tx pos =
    let Quantity oldQ = posQuantity pos
        Quantity txQ = txQuantity tx
        Balance oldCost = posCostBasis pos
        Balance cf = txCashFlow tx
        Just side = posSide pos

        -- How many shares are being closed from original position
        closedQ = min oldQ txQ

        -- Cost basis released (proportional to closed quantity)
        releasedCost :: Integer
        releasedCost = (oldCost * closedQ) `quot` oldQ

        -- Realized PnL calculation
        -- For long: received - cost = cf - releasedCost (cf positive, releasedCost positive)
        -- For short: net risk released + cf = releasedCost + cf (cf negative when paying to close)
        realized :: Integer
        realized = case side of
            Long  -> cf - releasedCost
            Short -> releasedCost + cf
    in
        if txQ < oldQ
        then
            -- Partial reduction: reduce position size, keep same side
            let remainingQ = oldQ - txQ
            in pos
                { posQuantity = Quantity remainingQ
                , posCostBasis = Balance (oldCost - releasedCost)
                , posRealizedPnL = posRealizedPnL pos + Balance realized
                }
        else if txQ == oldQ
        then
            -- Fully closed
            pos
                { posSide = Nothing
                , posQuantity = Quantity 0
                , posCostBasis = Balance 0
                , posRealizedPnL = posRealizedPnL pos + Balance realized
                }
        else
            -- Flip to opposite side: close old position, open new on opposite side
            let newQ = txQ - oldQ
                -- Cash flow allocated to closing old position (proportional to closed quantity)
                cfForClosed = (cf * closedQ) `quot` txQ
                -- Cash flow allocated to new position (proportional to new quantity)
                cfForNew = (cf * newQ) `quot` txQ
                -- Realized PnL from closed portion only
                realizedFromClose = case side of
                    Long  -> cfForClosed - releasedCost
                    Short -> releasedCost + cfForClosed
                -- Cost basis for new position depends on side
                newCost = case txSide tx of
                    Long  -> abs cfForNew                -- Money paid for long
                    Short -> newQ * 100 - abs cfForNew   -- Net risk for short
            in pos
                { posSide = Just (txSide tx)
                , posQuantity = Quantity newQ
                , posCostBasis = Balance newCost
                , posRealizedPnL = posRealizedPnL pos + Balance realizedFromClose
                }

-- | Resolve a position when market closes
-- won = True means the Long side wins (event happened)
resolvePosition :: Bool -> Position -> Position
resolvePosition longWon pos =
    case posSide pos of
        Nothing -> pos
        Just side ->
            let Quantity q = posQuantity pos
                Balance cost = posCostBasis pos

                -- Calculate realized PnL
                -- For Long: cost basis is money paid
                -- For Short: cost basis = q * 100 - received, so received = q * 100 - cost
                realized :: Integer
                realized = case side of
                    Long ->
                        if longWon
                        then q * 100 - cost  -- Received q*100, paid cost
                        else negate cost        -- Received 0, paid cost
                    Short ->
                        if longWon
                        then negate cost           -- Lose: pay q*100 but had received (q*100 - cost), net = -cost
                        else q * 100 - cost  -- Win: keep received = q*100 - cost
            in
                pos
                    { posSide = Nothing
                    , posQuantity = Quantity 0
                    , posCostBasis = Balance 0
                    , posRealizedPnL = posRealizedPnL pos + Balance realized
                    }

-- | Calculate refund amount for a position
-- Returns the total to be refunded to user (cost basis + realized PnL)
refundPosition :: Position -> Balance
refundPosition pos =
    posCostBasis pos + posRealizedPnL pos
