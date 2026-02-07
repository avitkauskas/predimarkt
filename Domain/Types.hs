{-# LANGUAGE StrictData #-}

module Domain.Types
    ( -- Core types
      Side (..)
    , Quantity (..)
    , Balance (..)
    , Price

      -- Domain entities
    , Transaction (..)
    , Position (..)

      -- Smart constructors
    , mkQuantity
    , mkBalance
    ) where

import IHP.Prelude

-- | Side of a position: Long (bet for) or Short (bet against)
data Side = Long | Short
    deriving (Eq, Show, Enum, Bounded)

-- | Quantity of shares, always non-negative
newtype Quantity = Quantity Integer
    deriving (Eq, Ord, Show, Num)

-- | Balance in cents (signed integer money)
-- Positive = user has/received, Negative = user owes/paid
newtype Balance = Balance Integer
    deriving (Eq, Ord, Show, Num)

-- | Price as probability (0.0 to 1.0), for display only
type Price = Double

-- | Transaction represents a trade executed by a user
data Transaction = Transaction
    { txSide :: Side
    , txQuantity :: Quantity
    , txCashFlow :: Balance
    , txPriceBefore :: Price
    , txPriceAfter :: Price
    , txMarketQBefore :: Integer
    , txMarketQAfter :: Integer
    } deriving (Eq, Show)

-- | Position represents a user's current holdings in an asset
data Position = Position
    { posSide :: Maybe Side
    , posQuantity :: Quantity
    , posCostBasis :: Balance
    , posRealizedPnL :: Balance
    } deriving (Eq, Show)

-- | Smart constructor for Quantity (ensures non-negative)
mkQuantity :: Integer -> Maybe Quantity
mkQuantity n
    | n >= 0 = Just (Quantity n)
    | otherwise = Nothing

-- | Smart constructor for Balance
mkBalance :: Integer -> Balance
mkBalance = Balance
