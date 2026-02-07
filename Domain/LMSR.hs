module Domain.LMSR
    ( LMSRState (..)
    , precompute
    , sumItem
    , sumTotal
    , price
    , lmsrCore
    , lmsrPreview
    , calculateBuyCost
    , calculateSellRevenue
    ) where

import IHP.Prelude
import qualified Data.Map as M

-- | LMSR state for efficient calculations
data LMSRState = LMSRState
    { sMap :: M.Map Text Double  -- Map from asset symbol to s-value
    , sSum :: !Double           -- Sum of all s-values
    }

-- | Precompute LMSR state from quantities and beta
precompute :: Integer -> [(Text, Integer)] -> LMSRState
precompute beta items =
    let quantities = map snd items
        m = if null quantities then 0 else maximum quantities
        betaD = fromIntegral beta
        sMap = M.fromList
            [ (sym, exp (fromIntegral (q - m) / betaD))
            | (sym, q) <- items
            ]
        sSum = sum (M.elems sMap)
    in LMSRState sMap sSum

-- | Get s-value for an item
sumItem :: Text -> LMSRState -> Double
sumItem sym st = fromMaybe 0.0 (M.lookup sym (sMap st))

-- | Get total sum of s-values
sumTotal :: LMSRState -> Double
sumTotal = sSum

-- | Calculate current price for an item
price :: Text -> LMSRState -> Double
price sym st = sumItem sym st / sumTotal st

-- | Core LMSR calculation
-- Takes: current price ratio (a), normalized quantity (z), sign
-- Returns: (log denominator, new price)
lmsrCore :: Double -> Double -> Double -> (Double, Double)
lmsrCore a z sign =
    let la = log a
        lb = log (1 - a)
        t1 = la
        t2 = lb + sign * z
        m = max t1 t2
        logD = m + log (exp (t1 - m) + exp (t2 - m))
        pNew = exp (la - logD)
    in (logD, pNew)

-- | Preview trade cost/revenue and new price
-- Returns: (money in cents, new price, net shares)
lmsrPreview :: Integer -> Double -> Integer -> Double -> (Integer, Double, Integer)
lmsrPreview quantity currentPrice beta sign =
    let betaD = fromIntegral beta
        quantityD = fromIntegral quantity
        z = quantityD / betaD
        (logD, pNew) = lmsrCore currentPrice z sign
        moneyD = if sign < 0
                then quantityD + betaD * logD  -- BUY: invested
                else quantityD - betaD * logD  -- SELL: received
        money = round (moneyD * 100) :: Integer  -- Convert to cents
        net = quantity - round moneyD
    in (money, pNew, net)

-- | Calculate cost to buy shares (returns cents as Integer)
calculateBuyCost :: Integer -> Double -> Integer -> Integer
calculateBuyCost quantity currentPrice beta =
    let (money, _, _) = lmsrPreview quantity currentPrice beta (-1)
    in money

-- | Calculate revenue from selling shares (returns cents as Integer)
calculateSellRevenue :: Integer -> Double -> Integer -> Integer
calculateSellRevenue quantity currentPrice beta =
    let (money, _, _) = lmsrPreview quantity currentPrice beta 1
    in money
