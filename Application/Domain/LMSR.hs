module Application.Domain.LMSR (
    tradeValue,
    allAssetPrices,
    assetPrice
) where

import Application.Domain.Types
import qualified Data.Map.Strict as M
import Generated.Types
import IHP.Prelude

logSumExp :: [Double] -> Double
logSumExp xs =
    let m = maximum xs
     in m + log (sum (map (exp . subtract m) xs))

lmsrCost :: Beta -> M.Map (Id Asset) Quantity -> Double
lmsrCost (Beta beta) qMap =
    let b  = fromIntegral beta
        xs = [ fromIntegral q / b | Quantity q <- M.elems qMap ]
     in b * logSumExp xs

adjustQuantity
    :: Id Asset
    -> Quantity
    -> M.Map (Id Asset) Quantity
    -> M.Map (Id Asset) Quantity
adjustQuantity aid delta =
    M.insertWith (+) aid delta

softmax :: [Double] -> [Double]
softmax xs =
    let m    = maximum xs
        exps = map (exp . subtract m) xs
        s    = sum exps
    in map (/ s) exps

tradeValue
    :: Id Asset
    -> Quantity
    -> Beta
    -> M.Map (Id Asset) Quantity
    -> Money
tradeValue aid delta beta qMap =
    let cBefore = lmsrCost beta qMap
        qAfter  = adjustQuantity aid delta qMap
        cAfter  = lmsrCost beta qAfter
        diff    = cBefore - cAfter
     in Money (round (diff * 100))

allAssetPrices
    :: Beta
    -> M.Map (Id Asset) Quantity
    -> M.Map (Id Asset) Double
allAssetPrices (Beta beta) qMap =
    let b = fromIntegral beta
        scaled = [ (k, fromIntegral q / b) | (k, Quantity q) <- M.toAscList qMap ]
        prices = softmax (map snd scaled)
     in M.fromAscList (zip (map fst scaled) prices)

assetPrice
    :: Id Asset
    -> Beta
    -> M.Map (Id Asset) Quantity
    -> Double
assetPrice aid (Beta beta) qMap =
    let b = fromIntegral beta
        scaled (Quantity q) = fromIntegral q / b
        m = maximum [ scaled q | q <- M.elems qMap ]
        Quantity qTarget = qMap M.! aid
        target = fromIntegral qTarget / b
        denom = M.foldl' (\acc q -> acc + exp (scaled q - m)) 0 qMap
     in exp (target - m) / denom
