module Application.Adapter.Transaction
    ( toDomainTransaction
    , fromDomainTransaction
    , parseSideRequired
    , formatSideRequired
    ) where

import qualified Domain.Types as Domain
import Generated.Types
import IHP.Prelude
import IHP.ModelSupport

-- | Parse side from database text (required, throws on invalid)
parseSideRequired :: Text -> Domain.Side
parseSideRequired "long" = Domain.Long
parseSideRequired "short" = Domain.Short
parseSideRequired invalid =
    error $ "Invalid side value in database: " ++ show invalid

-- | Format side for database storage
formatSideRequired :: Domain.Side -> Text
formatSideRequired Domain.Long = "long"
formatSideRequired Domain.Short = "short"

-- | Convert database Transaction to domain Transaction
-- Note: Transactions are immutable records, so we don't need to convert back
-- typically, but we provide fromDomainTransaction for completeness
toDomainTransaction :: Transaction -> Domain.Transaction
toDomainTransaction tx =
    let side = parseSideRequired (get #side tx)
        qty = get #quantity tx
        cashFlow = fromIntegral (get #cashFlow tx)
        priceBefore = get #priceBefore tx
        priceAfter = get #priceAfter tx
        qBefore = get #marketQBefore tx
        qAfter = get #marketQAfter tx
    in case Domain.mkQuantity qty of
        Nothing ->
            error $ "Invalid negative quantity in transaction: " ++ show qty
        Just domainQty ->
            Domain.Transaction
                { Domain.txSide = side
                , Domain.txQuantity = domainQty
                , Domain.txCashFlow = Domain.Balance cashFlow
                , Domain.txPriceBefore = priceBefore
                , Domain.txPriceAfter = priceAfter
                , Domain.txMarketQBefore = qBefore
                , Domain.txMarketQAfter = qAfter
                }

-- | Create a database Transaction from domain Transaction and base record
-- Used when creating a new transaction record
fromDomainTransaction :: Domain.Transaction -> Transaction -> Transaction
fromDomainTransaction domainTx tx =
    let Domain.Quantity qty = Domain.txQuantity domainTx
        Domain.Balance cf = Domain.txCashFlow domainTx
    in tx
        |> set #side (formatSideRequired (Domain.txSide domainTx))
        |> set #quantity qty
        |> set #cashFlow (fromIntegral cf)
        |> set #priceBefore (Domain.txPriceBefore domainTx)
        |> set #priceAfter (Domain.txPriceAfter domainTx)
        |> set #marketQBefore (Domain.txMarketQBefore domainTx)
        |> set #marketQAfter (Domain.txMarketQAfter domainTx)
