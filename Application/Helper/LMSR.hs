module Application.Helper.LMSR 
    ( LMSRState(..)
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
import Generated.Types
import qualified Data.Map as M

data LMSRState = LMSRState
    { sMap :: M.Map (Id Asset) Double
    , sSum :: !Double
    }

precompute :: Int -> [Asset] -> LMSRState
precompute beta assets =
    let quantities = map (.quantity) assets
        m = if null quantities then 0 else maximum quantities
        betaD = fromIntegral beta
        sMap = M.fromList
            [ (asset.id, exp (fromIntegral (asset.quantity - m) / betaD))
            | asset <- assets
            ]
        sSum = sum (M.elems sMap)
    in LMSRState sMap sSum

sumItem :: Id Asset -> LMSRState -> Double
sumItem aid st = fromMaybe 0.0 (M.lookup aid (sMap st))

sumTotal :: LMSRState -> Double
sumTotal = sSum

price :: Id Asset -> LMSRState -> Double
price aid st = sumItem aid st / sumTotal st

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

lmsrPreview :: Int -> Double -> Int -> Double -> (Double, Double, Double)
lmsrPreview quantity a beta sign =
    let betaD = fromIntegral beta
        quantityD = fromIntegral quantity
        z = quantityD / betaD
        (logD, pNew) = lmsrCore a z sign
        money = if sign < 0
                then quantityD + betaD * logD  -- BUY: invested
                else quantityD - betaD * logD  -- SELL: received
        net = quantityD - money
    in (money, pNew, net)

calculateBuyCost :: Int -> Double -> Int -> Double -> Double
calculateBuyCost quantity currentPrice beta totalSum =
    let (money, _, _) = lmsrPreview quantity currentPrice beta (-1)
    in money

calculateSellRevenue :: Int -> Double -> Int -> Double -> Double
calculateSellRevenue quantity currentPrice beta totalSum =
    let (money, _, _) = lmsrPreview quantity currentPrice beta 1
    in money
