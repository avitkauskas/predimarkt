module Application.Domain.Types where

import IHP.Prelude

newtype Quantity = Quantity Integer
    deriving (Eq, Ord, Show, Num)

newtype Money = Money Integer
    deriving (Eq, Ord, Show, Num)

newtype Beta = Beta Integer
    deriving (Eq, Ord, Show, Num)
