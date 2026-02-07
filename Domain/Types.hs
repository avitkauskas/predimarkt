module Domain.Types
  ( -- Core types
    Quantity
  , Balance
  , Side

    -- Domain entities
  , Transaction
  , Position

    -- Construction helpers
  , mkQuantity
  , paidByUser
  , receivedByUser

    -- Core logic
  , applyTransaction
  , resolvePosition
  , refundPosition
  ) where

import Data.Time (UTCTime)
import IHP.Prelude

newtype Quantity = Quantity Int
  deriving (Eq, Ord, Show)

newtype Balance = Balance Int
  deriving (Eq, Ord, Show, Num)

data Side = Long | Short
  deriving (Eq, Show)

data Transaction = Transaction
  { txSide          :: Side
  , txQuantity      :: Quantity
  , txCashFlow      :: Balance      -- + = received by user
  , txMarketQBefore :: Int
  , txMarketQAfter  :: Int
  , txCreatedAt     :: UTCTime
  }

data Position = Position
  { posSide        :: Maybe Side     -- Nothing when qty = 0
  , posQty         :: Quantity
  , posCostBasis   :: Balance        -- open exposure only
  , posRealizedPnL :: Balance
  }

mkQuantity :: Int -> Maybe Quantity
mkQuantity n
  | n >= 0    = Just (Quantity n)
  | otherwise = Nothing

paidByUser, receivedByUser :: Int -> Balance
paidByUser     x = Balance (-x)
receivedByUser x = Balance x

applyTransaction :: Transaction -> Position -> Position
applyTransaction tx pos =
  case posSide pos of
    Nothing ->
      openPosition tx pos

    Just side
      | side == txSide tx ->
          increasePosition tx pos
      | otherwise ->
          reduceOrFlipPosition tx pos

openPosition :: Transaction -> Position -> Position
openPosition tx pos =
  pos
    { posSide        = Just (txSide tx)
    , posQty         = txQuantity tx
    , posCostBasis   = negate (txCashFlow tx)
    }

increasePosition :: Transaction -> Position -> Position
increasePosition tx pos =
  pos
    { posQty       = addQty (posQty pos) (txQuantity tx)
    , posCostBasis = posCostBasis pos - txCashFlow tx
    }

reduceOrFlipPosition :: Transaction -> Position -> Position
reduceOrFlipPosition tx pos =
  let
    Quantity oldQ = posQty pos
    Quantity deltaQ = txQuantity tx
    closedQ = min oldQ deltaQ
    Balance costBasis = posCostBasis pos

    releasedCost =
      Balance $ (costBasis * closedQ) `quot` deltaQ


    realized =
      txCashFlow tx - releasedCost

    remainingQ = oldQ - closedQ
  in
    if remainingQ == 0 && deltaQ == closedQ
      then
        -- fully closed
        pos
          { posSide        = Nothing
          , posQty         = Quantity 0
          , posCostBasis   = Balance 0
          , posRealizedPnL = posRealizedPnL pos + realized
          }
      else
        -- flipped or partially closed
        pos
          { posSide        = Just (txSide tx)
          , posQty         = Quantity (deltaQ - closedQ)
          , posCostBasis   = negate (txCashFlow tx - releasedCost)
          , posRealizedPnL = posRealizedPnL pos + realized
          }

addQty :: Quantity -> Quantity -> Quantity
addQty (Quantity a) (Quantity b) = Quantity (a + b)

proportion :: Balance -> Int -> Int -> Balance
proportion (Balance total) part whole =
  Balance (total * part `div` whole)

resolvePosition :: Bool -> Position -> Position
resolvePosition won pos =
  case posSide pos of
    Nothing -> pos
    Just side ->
      let
        Quantity qty = posQty pos
        Balance costBasis = posCostBasis pos

        payout =
          case side of
            Long  -> Balance (if won then qty else 0)
            Short -> Balance (costBasis - (if won then qty else 0))

        realized = payout - posCostBasis pos
      in
        pos
          { posSide        = Nothing
          , posQty         = Quantity 0
          , posCostBasis   = Balance 0
          , posRealizedPnL = posRealizedPnL pos + realized
          }

refundPosition :: Position -> Balance
refundPosition pos =
  posCostBasis pos + posRealizedPnL pos

