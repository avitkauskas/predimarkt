module Application.Adapter.Position
    ( toDomainPosition
    , fromDomainPosition
    , deriveSideFromQuantity
    ) where

import qualified Domain.Types as Domain
import Generated.Types
import IHP.ModelSupport
import IHP.Prelude

-- | Derive side from quantity sign
-- Positive quantity = Long, Negative quantity = Short, Zero = Nothing (flat)
deriveSideFromQuantity :: Integer -> Maybe Domain.Side
deriveSideFromQuantity qty
    | qty > 0   = Just Domain.Long
    | qty < 0   = Just Domain.Short
    | otherwise = Nothing

-- | Calculate cost basis from invested and received amounts
-- invested is always <= 0 (money paid by user)
-- received is always >= 0 (money received by user)
-- costBasis = invested + received (both are signed from user perspective)
calculateCostBasis :: Integer -> Integer -> Integer
calculateCostBasis invested received = invested + received

-- | Convert database Position to domain Position
-- Side is derived from quantity sign
-- Cost basis is calculated as invested + received
toDomainPosition :: Position -> Domain.Position
toDomainPosition position =
    let dbQty = get #quantity position
        qty = abs dbQty  -- Domain quantity is always non-negative
        invested = get #invested position
        received = get #received position
        costBasis = calculateCostBasis invested received
        mSide = deriveSideFromQuantity dbQty
    in case Domain.mkQuantity qty of
        Nothing ->
            error $ "Invalid negative quantity in position: " ++ show qty
        Just domainQty ->
            Domain.Position
                { Domain.posSide = mSide
                , Domain.posQuantity = domainQty
                , Domain.posCostBasis = Domain.Balance costBasis
                , Domain.posRealizedPnL = Domain.Balance 0
                }

-- | Apply domain Position back to database Position
-- This preserves the position's ID and other fields while updating state
-- invested/received are calculated based on the domain position's cost basis
fromDomainPosition :: Domain.Position -> Position -> Position
fromDomainPosition domainPos position =
    let Domain.Quantity domainQty = Domain.posQuantity domainPos
        Domain.Balance costBasis = Domain.posCostBasis domainPos
        mSide = Domain.posSide domainPos
        -- Determine sign based on side
        signedQty = case mSide of
            Just Domain.Long  -> domainQty
            Just Domain.Short -> negate domainQty
            Nothing           -> 0
    in position
        |> set #quantity signedQty
