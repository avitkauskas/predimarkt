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

precompute :: Double -> [Asset] -> LMSRState
precompute beta assets =
    let quantities = map (.quantity) assets
        m = if null quantities then 0 else maximum quantities
        sMap = M.fromList
            [ (asset.id, exp ((asset.quantity - m) / beta))
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

lmsrPreview :: Double -> Double -> Double -> Double -> (Double, Double, Double)
lmsrPreview x a beta sign =
    let z = x / beta
        (logD, pNew) = lmsrCore a z sign
        money = if sign < 0
                then x + beta * logD  -- BUY: invested
                else x - beta * logD  -- SELL: received
        net = x - money
    in (money, pNew, net)

calculateBuyCost :: Double -> Double -> Double -> Double -> Double
calculateBuyCost quantity currentPrice beta totalSum =
    let a = currentPrice
        (money, _, _) = lmsrPreview quantity a beta (-1)
    in money

calculateSellRevenue :: Double -> Double -> Double -> Double -> Double
calculateSellRevenue quantity currentPrice beta totalSum =
    let a = currentPrice
        (money, _, _) = lmsrPreview quantity a beta 1
    in money
