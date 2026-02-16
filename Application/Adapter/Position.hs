module Application.Adapter.Position
    ( toDomainPosition
    , fromDomainPosition
    , parseSide
    , formatSide
    ) where

import qualified Domain.Types as Domain
import Generated.Types
import IHP.ModelSupport
import IHP.Prelude

-- | Parse side from database text representation
-- Throws exception for invalid values
parseSide :: Maybe Text -> Maybe Domain.Side
parseSide Nothing = Nothing
parseSide (Just "long") = Just Domain.Long
parseSide (Just "short") = Just Domain.Short
parseSide (Just invalid) =
    error $ "Invalid side value in database: " ++ show invalid

-- | Format side for database storage
formatSide :: Maybe Domain.Side -> Maybe Text
formatSide Nothing             = Nothing
formatSide (Just Domain.Long)  = Just "long"
formatSide (Just Domain.Short) = Just "short"

-- | Convert database Holding to domain Position
-- Note: This reconstructs the current state from the holding record
toDomainPosition :: Holding -> Domain.Position
toDomainPosition holding =
    let mSide = parseSide (get #side holding)
        qty = get #quantity holding
        costBasis = fromIntegral (get #costBasis holding)
    in case Domain.mkQuantity qty of
        Nothing ->
            error $ "Invalid negative quantity in holding: " ++ show qty
        Just domainQty ->
            Domain.Position
                { Domain.posSide = if qty == 0 then Nothing else mSide
                , Domain.posQuantity = domainQty
                , Domain.posCostBasis = Domain.Balance costBasis
                , Domain.posRealizedPnL = Domain.Balance 0
                }

-- | Apply domain Position back to database Holding
-- This preserves the holding's ID and other fields while updating state
fromDomainPosition :: Domain.Position -> Holding -> Holding
fromDomainPosition domainPos holding =
    let Domain.Quantity qty = Domain.posQuantity domainPos
        Domain.Balance costBasis = Domain.posCostBasis domainPos
        mSide = Domain.posSide domainPos
    in holding
        |> set #side (formatSide mSide)
        |> set #quantity qty
        |> set #costBasis (fromIntegral costBasis)
