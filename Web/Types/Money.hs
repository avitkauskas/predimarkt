module Web.Types.Money
    ( Money
    , moneyFromDouble
    , moneyFromCents
    , moneyToCents
    , giveMoney
    , takeMoney
    , negateMoney
    , formatMoney
    ) where

import Data.Text (pack)
import IHP.Prelude
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

negateMoney :: Money -> Money
negateMoney (Money a) = Money (negate a)

formatMoney :: Money -> Text
formatMoney (Money c) =
    let euros = fromIntegral c / 100 :: Double
    in "€" <> pack (printf "%.2f" euros)
