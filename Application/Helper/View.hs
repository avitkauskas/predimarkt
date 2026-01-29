module Application.Helper.View where

import Generated.Enums
import Generated.Types
import IHP.ViewPrelude

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

price :: Id Asset -> LMSRState -> Double
price aid st =
    case M.lookup aid (sMap st) of
        Just v -> v / sSum st
        Nothing -> 0.0

marketStatusLabel :: MarketStatus -> Text
marketStatusLabel = \case
    MarketStatusDraft -> "draft"
    MarketStatusOpen -> "open"
    MarketStatusClosed -> "closed"
    MarketStatusResolved -> "resolved"
    MarketStatusRefunded -> "refunded"

assetStatusLabel :: AssetStatus -> Text
assetStatusLabel = \case
    AssetStatusOpen -> "open"
    AssetStatusResolved -> "resolved"
    AssetStatusRefunded -> "refunded"

assetStatusClasses :: AssetStatus -> Text
assetStatusClasses = \case
    AssetStatusResolved -> "market-status-resolved-body"
    AssetStatusRefunded -> "market-status-refunded-body"
    _ -> ""

marketStatusClasses :: MarketStatus -> Text
marketStatusClasses = \case
    MarketStatusClosed   -> "market-status-closed-body"
    MarketStatusResolved -> "market-status-resolved-body"
    MarketStatusRefunded -> "market-status-refunded-body"
    _                    -> ""

marketStatusHeaderClasses :: MarketStatus -> Text
marketStatusHeaderClasses = \case
    MarketStatusClosed   -> "market-status-closed-header"
    MarketStatusResolved -> "market-status-resolved-header"
    MarketStatusRefunded -> "market-status-refunded-header"
    _                    -> ""
