module Application.Helper.View
    ( module Application.Helper.View
    ) where

import Data.List (intercalate)
import Data.Text (pack, unpack)
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

marketStatusFooterClasses :: MarketStatus -> Text
marketStatusFooterClasses = \case
    MarketStatusClosed   -> "market-status-closed-footer"
    MarketStatusResolved -> "market-status-resolved-footer"
    MarketStatusRefunded -> "market-status-refunded-footer"
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

formatMoney :: Integral a => a -> Text
formatMoney cents =
    let euros = fromIntegral cents / 100 :: Double
        formatted = printf "%.2f" euros
        (intPart, decPart) = break (== '.') formatted
        intWithSeps = reverse . Data.List.intercalate "'" . chunksOf3 . reverse $ intPart
    in "€" <> pack (intWithSeps ++ decPart)
  where
    chunksOf3 [] = []
    chunksOf3 xs = take 3 xs : chunksOf3 (drop 3 xs)

-- | Format cents as signed money (e.g., "+€10.23" or "-€5.00")
formatMoneySigned :: Integral a => a -> Text
formatMoneySigned cents
    | cents == 0 = "€0.00"
    | cents > 0 = "+" <> formatMoney cents
    | otherwise = "-" <> formatMoney (abs cents)

-- | Format integer with thousand separators (e.g., "1'234'567")
formatWithSep :: Integral a => a -> Text
formatWithSep n =
    let str = unpack (show (abs (toInteger n)))
        withSeps = Data.List.intercalate "'" $ reverse $ chunksOf3 (reverse str)
        signed = if n < 0 then "-" <> pack withSeps else pack withSeps
    in signed
  where
    chunksOf3 [] = []
    chunksOf3 xs = take 3 xs : chunksOf3 (drop 3 xs)
