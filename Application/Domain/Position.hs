module Application.Domain.Position where

import Application.Domain.LMSR as LMSR
import Application.Domain.Types
import qualified Data.Map as M
import Generated.Types
import IHP.ModelSupport
import IHP.Prelude

data Side = Long | Short
    deriving (Eq, Show)

data EnrichedPosition = EnrichedPosition
    { epPosition     :: Include' ["marketId", "assetId"] Position
    , epCurrentValue :: Maybe Integer
    , epAssetPrice   :: Maybe Double
    }

positionSide :: Integer -> Maybe Side
positionSide qty
    | qty > 0   = Just Long
    | qty < 0   = Just Short
    | otherwise = Nothing

positionValue :: Id Asset -> Quantity -> Beta -> Map (Id Asset) Quantity -> Money
positionValue assetId (Quantity qty) beta qtyMap
    | qty == 0  = Money 0
    | otherwise = tradeValue assetId (Quantity (-qty)) beta qtyMap

currentPnL :: Money -> Money -> Money -> Money
currentPnL currentValue invested received = currentValue - (invested - received)

resolutionPayout :: Quantity -> Side -> Bool -> Money
resolutionPayout (Quantity qty) side didWin
    | side == Long && didWin   = Money (qty * 100)
    | side == Long && not didWin = Money 0
    | side == Short && didWin   = Money 0
    | side == Short && not didWin = Money (-qty * 100)
    | otherwise = Money 0

enrichPosition
    :: Include' ["marketId", "assetId"] Position
    -> Market
    -> M.Map (Id Asset) Quantity
    -> Beta
    -> EnrichedPosition
enrichPosition position market qtyMap beta =
    let qty = get #quantity position
        assetId = get #id (get #assetId position)
        (currentValue, assetPrice) = computePositionValue qty assetId market qtyMap beta
    in EnrichedPosition
        { epPosition = position
        , epCurrentValue = currentValue
        , epAssetPrice = assetPrice
        }

computePositionValue
    :: Integer
    -> Id Asset
    -> Market
    -> M.Map (Id Asset) Quantity
    -> Beta
    -> (Maybe Integer, Maybe Double)
computePositionValue qty assetId market qtyMap beta =
    if market.status == MarketStatusResolved
    then
        if qty == 0
        then (Nothing, Nothing)
        else
            let isWinner = case get #outcomeAssetId market of
                    Just oid -> assetId == oid
                    Nothing  -> False
                displayValue = if isWinner then abs qty * 100 else 0
                displayPrice = if isWinner then 1.0 else 0.0
            in (Just displayValue, Just displayPrice)
    else if market.status == MarketStatusRefunded
    then (Just 0, Just 0.0)
    else
        if qty == 0
        then (Nothing, Just (LMSR.assetPrice assetId beta qtyMap))
        else if qty > 0
        then
            let Money v = LMSR.tradeValue assetId (Quantity (-qty)) beta qtyMap
            in (Just v, Just (LMSR.assetPrice assetId beta qtyMap))
        else
            let Money v = LMSR.tradeValue assetId (Quantity (abs qty)) beta qtyMap
            in (Just (-v), Just (LMSR.assetPrice assetId beta qtyMap))
