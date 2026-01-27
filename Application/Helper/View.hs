module Application.Helper.View where

import Generated.Enums
import IHP.ViewPrelude

-- Here you can add functions which are available in all your views

marketStatusLabel :: MarketStatus -> Text
marketStatusLabel = \case
    MarketStatusOpen -> "open"
    MarketStatusClosed -> "closed"
    MarketStatusResolved -> "resolved"
    MarketStatusRefunded -> "refunded"
    _ -> "unknown"
