module Application.Adapter.Transaction
    ( toDomainTrade
    , fromDomainTrade
    , parseSideRequired
    , formatSideRequired
    ) where

import qualified Domain.Types as Domain
import Generated.Types
import IHP.ModelSupport
import IHP.Prelude

-- | Parse side from database text (required, throws on invalid)
parseSideRequired :: Text -> Domain.Side
parseSideRequired "long" = Domain.Long
parseSideRequired "short" = Domain.Short
parseSideRequired invalid =
    error $ "Invalid side value in database: " ++ show invalid

-- | Format side for database storage
formatSideRequired :: Domain.Side -> Text
formatSideRequired Domain.Long  = "long"
formatSideRequired Domain.Short = "short"

-- | Convert database Transaction to domain Trade
-- Note: Transactions are immutable records, so we don't need to convert back
-- typically, but we provide fromDomainTrade for completeness
toDomainTrade :: Transaction -> Domain.Trade
toDomainTrade tx =
    let side = parseSideRequired (get #side tx)
        qty = get #quantity tx
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
fromDomainTrade :: Domain.Trade -> Transaction -> Transaction
fromDomainTrade domainTrade tx =
    let Domain.Quantity qty = Domain.tradeQuantity domainTrade
        Domain.Balance cf = Domain.tradeCashFlow domainTrade
    in tx
        |> set #side (formatSideRequired (Domain.tradeSide domainTrade))
        |> set #quantity qty
        |> set #cashFlow (fromIntegral cf)
        |> set #priceBefore (Domain.tradePriceBefore domainTrade)
        |> set #priceAfter (Domain.tradePriceAfter domainTrade)
