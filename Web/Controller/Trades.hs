module Web.Controller.Trades where

import Application.Domain.LMSR
import Application.Domain.Types
import Application.Helper.View (formatMoney)
import qualified Data.Map.Strict as M
import Web.Controller.Prelude

instance Controller TradesController where
    action ExecuteTradeAction { assetId } = do
        asset <- fetch assetId
        market <- fetch asset.marketId

        let paramQty = fromIntegral (param @Int "quantity") :: Integer
        let tradeType = param @Text "type"

        let isBuy = tradeType == "buy"

        assets <- query @Asset
            |> filterWhere (#marketId, market.id)
            |> fetch

        let qtyMap = foldl' (\m a -> M.insert a.id (Quantity (get #quantity a)) m) M.empty assets

        let beta = Beta market.beta
        let currentPrice = assetPrice assetId beta qtyMap

        let deltaQty = if isBuy then paramQty else (-paramQty)
        let delta = Quantity deltaQty
        let cashFlow = tradeValue assetId delta beta qtyMap

        let Money cashFlowCents = cashFlow
        let Money tradeAmountCents = abs cashFlow

        wallet <- query @Wallet
            |> filterWhere (#userId, currentUserId)
            |> fetchOne

        maybeDbPosition <- query @Position
            |> filterWhere (#userId, currentUserId)
            |> filterWhere (#assetId, assetId)
            |> fetchOneOrNothing

        withTransaction $ do
            let newAssetQty = get #quantity asset + deltaQty
            asset
                |> set #quantity newAssetQty
                |> updateRecord

            let newQtyMap = M.insert assetId (Quantity newAssetQty) qtyMap
            let priceAfter = assetPrice assetId beta newQtyMap

            market
                |> set #trades (market.trades + 1)
                |> set #volume (market.volume + paramQty)
                |> set #turnover (market.turnover + tradeAmountCents)
                |> updateRecord

            wallet
                |> set #amount (wallet.amount + cashFlowCents)
                |> updateRecord

            let txnQuantity = if isBuy then paramQty else (-paramQty)
            _ <- newRecord @Transaction
                |> set #userId currentUserId
                |> set #assetId assetId
                |> set #marketId market.id
                |> set #quantity txnQuantity
                |> set #cashFlow cashFlowCents
                |> set #priceBefore currentPrice
                |> set #priceAfter priceAfter
                |> createRecord

            case maybeDbPosition of
                Just dbPosition -> do
                    let oldInvested = get #invested dbPosition
                    let oldReceived = get #received dbPosition
                    let newInvested = if isBuy then oldInvested + cashFlowCents else oldInvested
                    let newReceived = if not isBuy then oldReceived + cashFlowCents else oldReceived
                    let newQuantity = get #quantity dbPosition + txnQuantity
                    dbPosition
                        |> set #invested newInvested
                        |> set #received newReceived
                        |> set #quantity newQuantity
                        |> updateRecord
                Nothing -> newRecord @Position
                        |> set #userId currentUserId
                        |> set #marketId market.id
                        |> set #assetId assetId
                        |> set #quantity txnQuantity
                        |> set #invested (if isBuy then cashFlowCents else 0)
                        |> set #received (if not isBuy then cashFlowCents else 0)
                        |> createRecord

        let action = if isBuy then "bought" else "sold"
        setSuccessMessage $ "Successfully " <> action <> " " <> show paramQty <> " shares for " <> formatMoney tradeAmountCents

        redirectTo (ShowMarketAction asset.marketId Nothing Nothing)

    action ClosePositionAction { assetId } = do
        dbPosition <- query @Position
            |> filterWhere (#userId, currentUserId)
            |> filterWhere (#assetId, assetId)
            |> fetchOne

        let currentQty = get #quantity dbPosition

        when (currentQty == 0) $ do
            setErrorMessage "No position to close"
            redirectTo (DashboardPositionsAction Nothing)

        asset <- fetch assetId
        market <- fetch asset.marketId

        assets <- query @Asset
            |> filterWhere (#marketId, market.id)
            |> fetch

        let qtyMap = foldl' (\m a -> M.insert a.id (Quantity (get #quantity a)) m) M.empty assets

        let beta = Beta market.beta
        let currentPrice = assetPrice assetId beta qtyMap

        let deltaQty = -currentQty
        let delta = Quantity deltaQty
        let cashFlow = tradeValue assetId delta beta qtyMap

        let Money cashFlowCents = cashFlow
        let Money tradeAmountCents = abs cashFlow

        wallet <- query @Wallet
            |> filterWhere (#userId, currentUserId)
            |> fetchOne

        withTransaction $ do
            let newAssetQty = get #quantity asset + deltaQty
            asset
                |> set #quantity newAssetQty
                |> updateRecord

            let newQtyMap = M.insert assetId (Quantity newAssetQty) qtyMap
            let priceAfter = assetPrice assetId beta newQtyMap

            market
                |> set #trades (market.trades + 1)
                |> set #volume (market.volume + abs currentQty)
                |> set #turnover (market.turnover + tradeAmountCents)
                |> updateRecord

            wallet
                |> set #amount (wallet.amount + cashFlowCents)
                |> updateRecord

            _ <- newRecord @Transaction
                |> set #userId currentUserId
                |> set #assetId assetId
                |> set #marketId market.id
                |> set #quantity (-currentQty)
                |> set #cashFlow cashFlowCents
                |> set #priceBefore currentPrice
                |> set #priceAfter priceAfter
                |> createRecord

            let oldInvested = get #invested dbPosition
            let oldReceived = get #received dbPosition
            let newInvested = if cashFlowCents < 0 then oldInvested + cashFlowCents else oldInvested
            let newReceived = if cashFlowCents > 0 then oldReceived + cashFlowCents else oldReceived
            dbPosition
                |> set #invested newInvested
                |> set #received newReceived
                |> set #quantity 0
                |> updateRecord

        let action = if currentQty > 0 then "sold" else "bought"
        setSuccessMessage $ "Successfully closed position by " <> action <> " " <> show (abs currentQty) <> " shares for " <> formatMoney tradeAmountCents

        redirectTo (DashboardPositionsAction Nothing)
