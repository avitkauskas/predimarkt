module Domain.LMSR
    ( LMSRState (..)
    , precompute
    , sumItem
    , sumTotal
    , price
    , lmsrCore
    , lmsrMoney
    , calculateBuyCost
    , calculateSellRevenue
    ) where

import qualified Data.List
import qualified Data.Map as M
import Generated.Types (Asset)
import IHP.Prelude

-- | LMSR state for efficient calculations
data LMSRState = LMSRState
    { sMap :: M.Map (Id Asset) Double  -- Map from asset ID to s-value
    , sSum :: !Double           -- Sum of all s-values
    }

-- | Precompute LMSR state from quantities and beta
precompute :: Integer -> [(Id Asset, Integer)] -> LMSRState
precompute beta items =
    let quantities = map snd items
        m = if null quantities then 0 else maximum quantities
        betaD = fromIntegral beta
        sMap = M.fromList
            [ (assetId, exp (fromIntegral (q - m) / betaD))
            | (assetId, q) <- items
            ]
        sSum = sum (M.elems sMap)
    in LMSRState sMap sSum

-- | Get s-value for an item
sumItem :: Id Asset -> LMSRState -> Double
sumItem assetId st = fromMaybe 0.0 (M.lookup assetId (sMap st))

-- | Get total sum of s-values
sumTotal :: LMSRState -> Double
sumTotal = sSum

-- | Calculate current price for an item
price :: Id Asset -> LMSRState -> Double
price assetId st = sumItem assetId st / sumTotal st

-- | Core LMSR calculation
-- Takes: current price ratio (a), normalized quantity (z), sign
-- Returns: log denominator
lmsrCore :: Double -> Double -> Double -> Double
lmsrCore a z sign = logD
    where
        t1 = log a
        t2 = log (1 - a) + sign * z
        mx = max t1 t2
        logD = mx + log (exp (t1 - mx) + exp (t2 - mx))

-- | Calcutale money from trade
-- Returns: money in cents
lmsrMoney :: Integer -> Double -> Integer -> Double -> Integer
lmsrMoney quantity currentPrice beta sign = money
    where
        betaD = fromIntegral beta
        quantityD = fromIntegral quantity
        z = quantityD / betaD
        logD = lmsrCore currentPrice z sign
        moneyD = if sign < 0
                then quantityD + betaD * logD  -- BUY: invested
                else quantityD - betaD * logD  -- SELL: received
        money = round (moneyD * 100) :: Integer  -- Convert to cents

-- | Calculate cost to buy shares (returns cents as Integer)
calculateBuyCost :: Integer -> Double -> Integer -> Integer
calculateBuyCost quantity currentPrice beta =
    lmsrMoney quantity currentPrice beta (-1)

-- | Calculate revenue from selling shares (returns cents as Integer)
calculateSellRevenue :: Integer -> Double -> Integer -> Integer
calculateSellRevenue quantity currentPrice beta =
    lmsrMoney quantity currentPrice beta 1
