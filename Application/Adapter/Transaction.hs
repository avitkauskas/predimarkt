module Application.Adapter.Transaction
    ( toDomainTrade
    , fromDomainTrade
    , deriveSideFromQuantity
    ) where

import qualified Domain.Types as Domain
import Generated.Types
import IHP.ModelSupport
import IHP.Prelude

-- | Derive side from quantity sign
-- Positive quantity = Long (buy), Negative quantity = Short (sell)
deriveSideFromQuantity :: Integer -> Domain.Side
deriveSideFromQuantity qty
    | qty >= 0  = Domain.Long
    | otherwise = Domain.Short

-- | Convert database Transaction to domain Trade
-- Side is derived from quantity sign
toDomainTrade :: Transaction -> Domain.Trade
toDomainTrade tx =
    let dbQty = get #quantity tx
        side = deriveSideFromQuantity dbQty
        qty = abs dbQty  -- Domain quantity is always non-negative
        cashFlow = fromIntegral (get #cashFlow tx)
        priceBefore = get #priceBefore tx
        priceAfter = get #priceAfter tx
    in case Domain.mkQuantity qty of
        Nothing ->
            error $ "Invalid negative quantity in transaction: " ++ show qty
        Just domainQty ->
            Domain.Trade
                { Domain.tradeSide = side
                , Domain.tradeQuantity = domainQty
                , Domain.tradeCashFlow = Domain.Balance cashFlow
                , Domain.tradePriceBefore = priceBefore
                , Domain.tradePriceAfter = priceAfter
                }

-- | Create a database Transaction from domain Trade and base record
-- Used when creating a new transaction record
-- Quantity sign indicates side (positive=Long/buy, negative=Short/sell)
fromDomainTrade :: Domain.Trade -> Transaction -> Transaction
fromDomainTrade domainTrade tx =
    let Domain.Quantity qty = Domain.tradeQuantity domainTrade
        Domain.Balance cf = Domain.tradeCashFlow domainTrade
        side = Domain.tradeSide domainTrade
        -- Apply sign based on side
        signedQty = case side of
            Domain.Long  -> qty
            Domain.Short -> negate qty
    in tx
        |> set #quantity signedQty
        |> set #cashFlow (fromIntegral cf)
        |> set #priceBefore (Domain.tradePriceBefore domainTrade)
        |> set #priceAfter (Domain.tradePriceAfter domainTrade)
