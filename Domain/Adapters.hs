module Domain.Adapters where

import qualified Domain.Types as Domain

toDomainPosition :: Position -> Domain.Position
toDomainPosition pos =
  Domain.Position
    { posSide = case pos.side of
        Just "long"  -> Just Long
        Just "short" -> Just Short
        Nothing      -> Nothing
    , posQty = Quantity pos.quantity
    , posCostBasis = Balance pos.costBasis
    , posRealizedPnL = Balance pos.realizedPnl
    }

toDomainTransaction :: Transaction -> Domain.Transaction
toDomainTransaction tx =
  Domain.Transaction
    { txSide = if tx.side == "long" then Long else Short
    , txQuantity = Quantity tx.quantity
    , txCashFlow = Balance tx.cashFlow
    , txMarketQBefore = tx.marketQBefore
    , txMarketQAfter  = tx.marketQAfter
    , txCreatedAt = tx.createdAt
    }

applyDomainPosition :: Domain.Position -> Position -> Position
applyDomainPosition dpos pos =
  pos
    |> set #side (case dpos.posSide of
          Just Long  -> Just "long"
          Just Short -> Just "short"
          Nothing    -> Nothing)
    |> set #quantity q
    |> set #costBasis cb
    |> set #realizedPnl rp
  where
    Quantity q = dpos.posQty
    Balance cb = dpos.posCostBasis
    Balance rp = dpos.posRealizedPnL
