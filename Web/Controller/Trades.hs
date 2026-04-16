{-# LANGUAGE BlockArguments #-}
module Web.Controller.Trades where

import Application.Domain.LMSR
import Application.Domain.Position
import Application.Domain.Types
import Application.Helper.Formatting (formatMoney)
import qualified Application.Helper.QueryParams as QueryParams
import Application.Market.State (buildMarketState)
import qualified Data.Aeson as Aeson
import qualified Data.Map.Strict as M
import Web.Controller.Prelude

instance Controller TradesController where
    action ExecuteTradeAction { assetId } = do
        ensureIsUser
        asset <- fetch assetId
        market <- fetch asset.marketId

        let paramQty = fromIntegral (param @Int "quantity") :: Integer
        let tradeType = param @Text "type"
        let chartVisible = fromMaybe True (readQueryFlag "showChart")
        let descriptionVisible = fromMaybe False (readQueryFlag "showDescription")
        let allAssetsVisible = fromMaybe False (readQueryFlag "showAllAssets")
        let tradeHistoryVisible = fromMaybe False (readQueryFlag "showTradeHistory")
        let currentActivityPage = max 1 (fromMaybe 1 (paramOrNothing @Int "activityPage"))
        let currentChatPage = max 1 (fromMaybe 1 (paramOrNothing @Int "chatPage"))
        let currentChatComposerRev = QueryParams.normalizeOptionalTextParam
                (paramOrNothing @Text "chatComposerRev")
        let backToPath = sanitizeBackTo (paramOrNothing @Text "backTo")

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
            let marketStateJson = Aeson.toJSON $ buildMarketState newQtyMap
            _ <- newRecord @Transaction
                |> set #userId currentUserId
                |> set #assetId assetId
                |> set #marketId market.id
                |> set #quantity txnQuantity
                |> set #cashFlow cashFlowCents
                |> set #priceBefore currentPrice
                |> set #priceAfter priceAfter
                |> set #marketState marketStateJson
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

        redirectTo (ShowMarketAction asset.marketId Nothing Nothing (Just chartVisible) (Just descriptionVisible) (Just allAssetsVisible) (Just tradeHistoryVisible) (QueryParams.normalizePageParam currentActivityPage) (QueryParams.normalizePageParam currentChatPage) currentChatComposerRev Nothing backToPath)

    action ClosePositionAction { assetId } = do
        ensureIsUser
        dbPosition <- query @Position
            |> filterWhere (#userId, currentUserId)
            |> filterWhere (#assetId, assetId)
            |> fetchOne

        let currentQty = get #quantity dbPosition

        if currentQty == 0
            then do
                setErrorMessage "No position to close"
                redirectTo (DashboardPositionsAction Nothing Nothing Nothing)
            else do
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
                        |> set #marketState (Aeson.toJSON $ buildMarketState newQtyMap)
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

                let action = if currentQty > 0 then "selling" else "buying back"
                setSuccessMessage $ "Successfully closed position by " <> action <> " " <> show (abs currentQty) <> " shares for " <> formatMoney tradeAmountCents

                redirectTo (DashboardPositionsAction Nothing Nothing Nothing)

    action ResolveMarketAction { marketId } = do
        let mId = if marketId == def then param @(Id Market) "marketId" else marketId
        market <- fetch mId
        accessDeniedUnless (market.userId == Just currentUserId)
        accessDeniedUnless (market.status == MarketStatusClosed)

        let outcomeAssetId = param @(Id Asset) "outcomeAssetId"

        outcomeAsset <- fetch outcomeAssetId
        accessDeniedUnless (outcomeAsset.marketId == market.id)

        positions <- query @Position
            |> filterWhere (#marketId, market.id)
            |> filterWhereNot (#quantity, 0)
            |> fetch

        assets <- query @Asset
            |> filterWhere (#marketId, market.id)
            |> fetch

        let qtyMap = M.fromList [(a.id, Quantity a.quantity) | a <- assets]
        let beta = Beta market.beta

        now <- getCurrentTime

        withTransaction $ do
            updatedMarket <- market
                |> set #status MarketStatusResolved
                |> set #resolvedAt (Just now)
                |> set #outcomeAssetId (Just outcomeAssetId)
                |> updateRecord

            syncCloseMarketJob updatedMarket

            forM_ positions \position -> do
                wallet <- query @Wallet
                    |> filterWhere (#userId, position.userId)
                    |> fetchOne

                let qty = position.quantity
                    side = positionSide qty
                    didWin = case side of
                        Just Long  -> position.assetId == outcomeAssetId
                        Just Short -> position.assetId /= outcomeAssetId
                        Nothing    -> False
                    Just s = side
                    payoutCents = resolutionPayout (Quantity (abs qty)) s didWin

                let priceBefore = assetPrice position.assetId beta qtyMap
                let priceAfter = case side of
                        Just Long  -> if didWin then 1.0 else 0.0
                        Just Short -> if didWin then 0.0 else 1.0
                        Nothing    -> 0.0

                let Money payout = payoutCents
                let marketStateJson = Aeson.toJSON $ buildMarketState qtyMap

                wallet
                    |> set #amount (wallet.amount + payout)
                    |> updateRecord

                _ <- newRecord @Transaction
                    |> set #userId position.userId
                    |> set #assetId position.assetId
                    |> set #marketId market.id
                    |> set #quantity (-position.quantity)
                    |> set #cashFlow payout
                    |> set #priceBefore priceBefore
                    |> set #priceAfter priceAfter
                    |> set #marketState marketStateJson
                    |> createRecord

                let newInvested = case side of
                        Just Long  -> position.invested
                        Just Short -> position.invested + payout
                        Nothing    -> position.invested
                let newReceived = case side of
                        Just Long  -> position.received + payout
                        Just Short -> position.received
                        Nothing    -> position.received

                position
                    |> set #quantity 0
                    |> set #invested newInvested
                    |> set #received newReceived
                    |> updateRecord

        setSuccessMessage "Market resolved successfully"
        redirectTo $ ShowMarketAction mId Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

    action RefundMarketAction { marketId } = do
        let mId = if marketId == def then param @(Id Market) "marketId" else marketId
        market <- fetch mId
        accessDeniedUnless (market.userId == Just currentUserId)
        accessDeniedUnless (market.status == MarketStatusClosed)

        positions <- query @Position
            |> filterWhere (#marketId, market.id)
            |> fetch

        assets <- query @Asset
            |> filterWhere (#marketId, market.id)
            |> fetch

        let qtyMap = M.fromList [(a.id, Quantity a.quantity) | a <- assets]
        let beta = Beta market.beta

        now <- getCurrentTime

        withTransaction $ do
            updatedMarket <- market
                |> set #status MarketStatusRefunded
                |> set #refundedAt (Just now)
                |> updateRecord

            syncCloseMarketJob updatedMarket

            forM_ positions \position -> do
                wallet <- query @Wallet
                    |> filterWhere (#userId, position.userId)
                    |> fetchOne

                let invested = position.invested
                let received = position.received
                let refundAmount = negate (invested + received)

                let priceBefore = assetPrice position.assetId beta qtyMap
                let marketStateJson = Aeson.toJSON $ buildMarketState qtyMap

                _ <- newRecord @Transaction
                    |> set #userId position.userId
                    |> set #assetId position.assetId
                    |> set #marketId market.id
                    |> set #quantity (-position.quantity)
                    |> set #cashFlow refundAmount
                    |> set #priceBefore priceBefore
                    |> set #priceAfter 0.0
                    |> set #marketState marketStateJson
                    |> createRecord

                wallet
                    |> set #amount (wallet.amount + refundAmount)
                    |> updateRecord

                position
                    |> set #quantity 0
                    |> set #invested 0
                    |> set #received 0
                    |> updateRecord

        setSuccessMessage "Market refunded successfully"
        redirectTo $ ShowMarketAction mId Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

readQueryFlag :: (?context :: ControllerContext, ?request :: Request) => Text -> Maybe Bool
readQueryFlag name =
    QueryParams.parseBooleanText (paramOrNothing @Text (cs name))
