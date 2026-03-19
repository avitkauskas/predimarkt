module Application.Helper.Formatting where

import qualified Data.List as List
import Data.Text (pack, unpack)
import IHP.Prelude
import Text.Printf (printf)

formatPricePercent :: Double -> Text
formatPricePercent price =
    pack $ printf "%.1f%%" (price * 100)

formatPriceDecimal :: Double -> Text
formatPriceDecimal price =
    pack $ printf "%.4f" price

formatPriceRounded :: Double -> Text
formatPriceRounded price =
    pack $ printf "%d%%" (round (price * 100) :: Int)

formatMoney :: Integral a => a -> Text
formatMoney cents =
    let absCents = abs (fromIntegral cents) :: Integer
        euros = fromIntegral absCents / 100 :: Double
        formatted = printf "%.2f" euros
        (intPart, decPart) = break (== '.') formatted
        intWithSeps = reverse . List.intercalate "'" . chunksOf3 . reverse $ intPart
        sign = if cents < 0 then "-" else ""
    in sign <> pack (intWithSeps ++ decPart)
  where
    chunksOf3 [] = []
    chunksOf3 xs = take 3 xs : chunksOf3 (drop 3 xs)

formatMoneySigned :: Integral a => a -> Text
formatMoneySigned cents
    | cents > 0 = "+" <> formatMoney cents
    | otherwise = formatMoney cents

formatMoneyOrDash :: Integral a => a -> Text
formatMoneyOrDash cents
    | cents == 0 = "--"
    | otherwise = formatMoney cents

formatWithSep :: Integral a => a -> Text
formatWithSep n =
    let str = unpack (show (abs (toInteger n)))
        withSeps = reverse . List.intercalate "'" . chunksOf3 . reverse $ str
        signed = if n < 0 then "-" <> pack withSeps else pack withSeps
    in signed
  where
    chunksOf3 [] = []
    chunksOf3 xs = take 3 xs : chunksOf3 (drop 3 xs)
