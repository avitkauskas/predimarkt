module Application.Domain.Position where

import Application.Domain.LMSR
import Application.Domain.Types
import Generated.Types
import IHP.Prelude

data Side = Long | Short
    deriving (Eq, Show)

positionSide :: Integer -> Maybe Side
positionSide qty
    | qty > 0   = Just Long
    | qty < 0   = Just Short
    | otherwise = Nothing

positionValue :: Id Asset -> Quantity -> Beta -> Map (Id Asset) Quantity -> Money
positionValue assetId (Quantity qty) beta qtyMap
    | qty > 0   = let Money v = tradeValue assetId (Quantity (-qty)) beta qtyMap in Money v
    | qty < 0   = let Money v = tradeValue assetId (Quantity (abs qty)) beta qtyMap in Money (-v)
    | otherwise = Money 0

currentPnL :: Money -> Money -> Money -> Money
currentPnL currentValue invested received = currentValue - (invested - received)

resolutionPayout :: Quantity -> Side -> Bool -> Money
resolutionPayout (Quantity qty) side didWin
    | side == Long && didWin   = Money (qty * 100)
    | side == Long && not didWin = Money 0
    | side == Short && didWin   = Money 0
    | side == Short && not didWin = Money (-qty * 100)
    | otherwise = Money 0
