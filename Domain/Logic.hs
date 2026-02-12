module Domain.Logic
    ( -- Position operations
      applyTrade
    , resolvePosition
    , refundPosition

      -- LMSR-based proportion calculation
    , calculateReleaseProportion

      -- Utility functions
    , emptyPosition
    , isFlat
    , positionValue
    ) where

import qualified Domain.LMSR as LMSR
import Domain.Types
import Generated.Types
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

-- | Calculate the proportion of position value to release based on LMSR
-- Returns ratio between 0.0 and 1.0
-- Uses LMSR sell value (for longs) or buy value (for shorts) to determine proportion
calculateReleaseProportion :: MarketContext -> Integer -> Integer -> Double
calculateReleaseProportion ctx closedQty totalQty
    | totalQty == 0 = 1.0  -- Full release if no position
    | closedQty >= totalQty = 1.0  -- Full release if closing all
    | otherwise =
        let beta = mcBeta ctx
            assetId = mcAssetId ctx
            otherAssets = mcOtherAssets ctx
            -- Build LMSR state: (asset, totalQty) + other assets
            allAssets = (assetId, totalQty) : otherAssets
            lmsrState = LMSR.precompute beta allAssets
            currentPrice = LMSR.price assetId lmsrState
            -- For both longs and shorts, we use sell revenue to determine value proportion
            -- (sell value represents what we'd get for liquidating)
            sellClosed = fromIntegral $ LMSR.calculateSellRevenue closedQty currentPrice beta
            sellTotal = fromIntegral $ LMSR.calculateSellRevenue totalQty currentPrice beta
        in if sellTotal == 0
           then fromIntegral closedQty / fromIntegral totalQty  -- Fallback to quantity-based
           else sellClosed / sellTotal

-- | Apply a trade to update a position
-- MarketContext provides LMSR data for accurate proportion calculations
applyTrade :: MarketContext -> Trade -> Position -> Position
applyTrade ctx trade pos =
    case posSide pos of
        Nothing ->
            openPosition trade pos
        Just side
            | side == tradeSide trade ->
                increasePosition trade pos
            | otherwise ->
                reduceOrFlipPosition ctx trade pos

-- | Open a new position from flat
-- For Long: cost basis = money paid (positive)
-- For Short: cost basis = money received (positive) - CHANGED from net obligation
openPosition :: Trade -> Position -> Position
openPosition trade pos =
    let Balance cf = tradeCashFlow trade
        cost = abs cf  -- For both long and short: absolute cash flow is cost basis
    in pos
        { posSide = Just (tradeSide trade)
        , posQuantity = tradeQuantity trade
        , posCostBasis = Balance cost
        }

-- | Increase existing position (same side)
-- For Long: add money paid to cost basis
-- For Short: add money received to cost basis - CHANGED from net risk
increasePosition :: Trade -> Position -> Position
increasePosition trade pos =
    let Quantity oldQ = posQuantity pos
        Quantity newQ = tradeQuantity trade
        Balance oldCost = posCostBasis pos
        Balance cf = tradeCashFlow trade
        additionalCost = abs cf  -- For both: absolute cash flow
    in pos
        { posQuantity = Quantity (oldQ + newQ)
        , posCostBasis = Balance (oldCost + additionalCost)
        }

-- | Reduce or flip position (opposite side trade)
-- Uses LMSR-based proportion calculation for accurate cost basis release
reduceOrFlipPosition :: MarketContext -> Trade -> Position -> Position
reduceOrFlipPosition ctx trade pos =
    let Quantity oldQ = posQuantity pos
        Quantity tradeQ = tradeQuantity trade
        Balance oldCost = posCostBasis pos
        Balance cf = tradeCashFlow trade
        Just side = posSide pos

        -- How many shares are being closed from original position
        closedQ = min oldQ tradeQ

        -- Cost basis released using LMSR-based proportion
        releaseRatio = calculateReleaseProportion ctx closedQ oldQ
        releasedCost :: Integer
        releasedCost = round $ fromIntegral oldCost * releaseRatio

        -- Proportional cash flow for closed portion
        cfForClosed = (cf * closedQ) `quot` tradeQ

        -- Unified realized PnL calculation for both long and short
        -- For long: cf is positive (received), cost is positive (paid earlier)
        --   realized = received - releasedCost = profit/loss
        -- For short: cf is negative (paying to close), cost is positive (received earlier)
        --   realized = cf - releasedCost = -costPaid - costReceivedEarlier = loss/profit
        realized :: Integer
        realized = cfForClosed - releasedCost
    in
        if tradeQ < oldQ
        then
            -- Partial reduction: reduce position size, keep same side
            let remainingQ = oldQ - tradeQ
            in pos
                { posQuantity = Quantity remainingQ
                , posCostBasis = Balance (oldCost - releasedCost)
                , posRealizedPnL = posRealizedPnL pos + Balance realized
                }
        else if tradeQ == oldQ
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
            let newQ = tradeQ - oldQ
                -- Remaining cash flow for new position
                cfForNew = cf - cfForClosed
                -- Cost basis for new position is absolute cash flow
                newCost = abs cfForNew
            in pos
                { posSide = Just (tradeSide trade)
                , posQuantity = Quantity newQ
                , posCostBasis = Balance newCost
                , posRealizedPnL = posRealizedPnL pos + Balance realized
                }

-- | Resolve a position when market closes
-- won = True means the Long side wins (event happened)
-- Updated for new cash-based short cost basis
resolvePosition :: Bool -> Position -> Position
resolvePosition longWon pos =
    case posSide pos of
        Nothing -> pos
        Just side ->
            let Quantity q = posQuantity pos
                Balance cost = posCostBasis pos

                -- Calculate realized PnL
                -- For Long: cost basis is money paid
                -- For Short: cost basis is money received when shorting
                realized :: Integer
                realized = case side of
                    Long ->
                        if longWon
                        then q * 100 - cost  -- Received q*100, paid cost
                        else negate cost        -- Received 0, paid cost
                    Short ->
                        if longWon
                        then negate cost - q * 100  -- Lose: pay q*100, and lose what we received
                        else cost                   -- Win: keep what we received
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
