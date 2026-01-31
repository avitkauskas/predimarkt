module Web.Types.Money
    ( Money
    , moneyFromDouble
    , moneyFromCents
    , moneyToCents
    , giveMoney
    , takeMoney
    , formatMoney
    ) where

import IHP.Prelude
import Data.Text (pack)
import Text.Printf (printf)

newtype Money = Money { cents :: Integer }
    deriving (Eq, Ord, Show)

moneyFromCents :: Integer -> Money
moneyFromCents = Money

moneyToCents :: Money -> Integer
moneyToCents (Money c) = c

moneyFromDouble :: Double -> Money
moneyFromDouble amount =
    Money (round (amount * 100))

giveMoney :: Money -> Money -> Money
giveMoney (Money a) (Money b) = Money (a + b)

takeMoney :: Money -> Money -> Money
takeMoney (Money a) (Money b) = Money (a - b)

formatMoney :: Money -> Text
formatMoney (Money c) =
    let euros = fromIntegral c / 100 :: Double
    in "€" <> pack (printf "%.2f" euros)