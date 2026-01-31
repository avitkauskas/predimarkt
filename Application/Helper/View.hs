module Application.Helper.View 
    ( module Application.Helper.LMSR
    , module Application.Helper.View
    ) where

import Generated.Enums
import Generated.Types
import IHP.ViewPrelude
import Application.Helper.LMSR

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
