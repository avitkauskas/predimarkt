module Application.Helper.View
    ( module Application.Helper.View
    ) where

import Data.Text (pack)
import Generated.Enums
import Generated.Types
import IHP.ViewPrelude
import Text.Printf (printf)

-- Market Status Helpers

marketStatusLabel :: MarketStatus -> Text
marketStatusLabel = \case
    MarketStatusDraft -> "draft"
    MarketStatusOpen -> "open"
    MarketStatusClosed -> "closed"
    MarketStatusResolved -> "resolved"
    MarketStatusRefunded -> "refunded"

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

-- Price Formatting Helpers

-- | Format price as percentage with 2 decimals (e.g., "23.45%")
formatPricePercent :: Double -> Text
formatPricePercent price =
    pack $ printf "%.2f%%" (price * 100)

-- | Format price as decimal with 4 decimals (e.g., "0.2345")
formatPriceDecimal :: Double -> Text
formatPriceDecimal price =
    pack $ printf "%.4f" price

-- | Format price as rounded percentage (e.g., "23%")
formatPriceRounded :: Double -> Text
formatPriceRounded price =
    pack $ printf "%d%%" (round (price * 100) :: Int)

-- Money Formatting Helpers

-- | Format cents as money (e.g., "€10.23")
formatMoney :: Integral a => a -> Text
formatMoney cents =
    let euros = fromIntegral cents / 100 :: Double
    in "€" <> pack (printf "%.2f" euros)

-- | Format cents as signed money (e.g., "+€10.23" or "-€5.00")
formatMoneySigned :: Integral a => a -> Text
formatMoneySigned cents
    | cents == 0 = "€0.00"
    | cents > 0 = "+" <> formatMoney cents
    | otherwise = "-" <> formatMoney (abs cents)
