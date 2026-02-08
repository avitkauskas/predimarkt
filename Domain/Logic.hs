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
openPosition :: Transaction -> Position -> Position
openPosition tx pos =
    let Balance cf = txCashFlow tx
        Quantity q = txQuantity tx
    in pos
        { posSide = Just (txSide tx)
        , posQuantity = txQuantity tx
        , posCostBasis = Balance (abs cf)  -- Store as positive cost basis
        }

-- | Increase existing position (same side)
increasePosition :: Transaction -> Position -> Position
increasePosition tx pos =
    let Quantity oldQ = posQuantity pos
        Quantity newQ = txQuantity tx
        Balance oldCost = posCostBasis pos
        Balance cf = txCashFlow tx
    in pos
        { posQuantity = Quantity (oldQ + newQ)
        , posCostBasis = Balance (oldCost + abs cf)
        }

-- | Reduce or flip position (opposite side transaction)
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

        -- Realized PnL from closing portion of original position
        -- For long: received - released cost = cf - releasedCost (cf is positive when selling)
        -- For short: released cost - paid = cf + releasedCost (cf is negative when buying to close)
        realized :: Integer
        realized = case side of
            Long  -> cf - releasedCost
            Short -> cf + releasedCost
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
                    Short -> cfForClosed + releasedCost
            in pos
                { posSide = Just (txSide tx)
                , posQuantity = Quantity newQ
                , posCostBasis = Balance (abs cfForNew)
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

                -- Calculate payout
                payout :: Integer
                payout = case side of
                    Long ->
                        if longWon
                        then q * 100  -- Each share pays 100 cents
                        else 0
                    Short ->
                        if longWon
                        then 0  -- Short loses, pays nothing (already paid when opening)
                        else q * 100  -- Short wins, keeps the obligation money

                -- Realized PnL = payout - cost basis
                realized = payout - cost
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
