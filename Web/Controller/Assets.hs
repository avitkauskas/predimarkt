module Web.Controller.Assets where

import Application.Adapter.Position
import Application.Adapter.Transaction
import Application.Helper.View (formatMoney)
import qualified Domain.LMSR as LMSR
import qualified Domain.Logic as Domain
import qualified Domain.Types as Domain
import Web.Controller.Prelude
import Web.View.Assets.New

instance Controller AssetsController where
    action DeleteAssetAction { assetId } = do
        asset <- fetch assetId
        market <- fetch asset.marketId
        accessDeniedUnless (market.userId == Just currentUserId)

        assetCount <- query @Asset
            |> filterWhere (#marketId, asset.marketId)
            |> fetchCount

        if assetCount <= 2
            then do
                setErrorMessage "Cannot delete asset: A market must have at least 2 assets."
                redirectTo (ShowMarketAction asset.marketId Nothing Nothing)
            else do
                deleteRecord asset
                setSuccessMessage "Asset deleted"
                redirectTo (ShowMarketAction asset.marketId Nothing Nothing)

    action NewAssetAction = do
        let asset = newRecord @Asset
        respondHtml $ renderAssetRow asset

    action TradeAssetAction { assetId } = do
        asset <- fetch assetId
        market <- fetch asset.marketId

        -- Get trade parameters
        let paramQty = fromIntegral (param @Int "quantity") :: Integer
        let tradeType = param @Text "type"

        -- Fetch all assets for LMSR calculation
        assets <- query @Asset
            |> filterWhere (#marketId, market.id)
            |> fetch

        -- Calculate LMSR state
        let lmsrState = LMSR.precompute market.beta [(a.symbol, a.quantity) | a <- assets]
        let currentPrice = LMSR.price asset.symbol lmsrState

        -- Get user's wallet
        wallet <- query @Wallet
            |> filterWhere (#userId, currentUserId)
            |> fetchOne

        -- Calculate trade details using domain logic
        let (side, deltaQty) = if tradeType == "buy"
                then (Domain.Long, paramQty)
                else (Domain.Short, paramQty)

        let tradeAmount = if tradeType == "buy"
                then LMSR.calculateBuyCost paramQty currentPrice market.beta
                else LMSR.calculateSellRevenue paramQty currentPrice market.beta

        let deltaCents = if tradeType == "buy"
                then -tradeAmount  -- User pays
                else tradeAmount   -- User receives

        -- Get or create holding
        maybeHolding <- query @Holding
            |> filterWhere (#userId, currentUserId)
            |> filterWhere (#assetId, assetId)
            |> fetchOneOrNothing

        let currentPosition = maybe Domain.emptyPosition toDomainPosition maybeHolding

        -- Build domain transaction (prices will be set after asset update)
        let domainTx = Domain.Transaction
                { Domain.txSide = side
                , Domain.txQuantity = case Domain.mkQuantity paramQty of
                    Just q  -> q
                    Nothing -> error "Invalid quantity"
                , Domain.txCashFlow = Domain.Balance deltaCents
                , Domain.txPriceBefore = 0  -- Placeholder, will be set to currentPrice
                , Domain.txPriceAfter = 0   -- Placeholder, will be calculated after trade
                }

        -- Apply transaction to get new position
        let newPosition = Domain.applyTransaction domainTx currentPosition

        -- Calculate realized PnL from this specific transaction
        let Domain.Balance oldRealizedPnL = Domain.posRealizedPnL currentPosition
        let Domain.Balance newRealizedPnL = Domain.posRealizedPnL newPosition
        let txRealizedPnL = newRealizedPnL - oldRealizedPnL

        withTransaction do
            -- Update asset quantity
            let assetDeltaQty = if side == Domain.Long then paramQty else (-paramQty)
            let updatedAssetQty = asset.quantity + assetDeltaQty
            asset
                |> set #quantity updatedAssetQty
                |> updateRecord

            -- Calculate price_after based on new asset quantities
            let newLmsrState = LMSR.precompute market.beta
                    [(a.symbol, if a.id == asset.id then updatedAssetQty else a.quantity) | a <- assets]
            let priceAfter = LMSR.price asset.symbol newLmsrState

            -- Update market statistics
            market
                |> set #trades (market.trades + 1)
                |> set #volume (market.volume + paramQty)
                |> set #turnover (market.turnover + tradeAmount)
                |> updateRecord

            -- Update wallet balance
            wallet
                |> set #amount (wallet.amount + deltaCents)
                |> updateRecord

            -- Create transaction record with both prices
            let domainTxnWithPrices = domainTx
                    { Domain.txPriceBefore = currentPrice
                    , Domain.txPriceAfter = priceAfter
                    }
            let txnBase = newRecord @Transaction
                    |> set #userId currentUserId
                    |> set #assetId assetId
                    |> set #marketId market.id
            _ <- fromDomainTransaction domainTxnWithPrices txnBase
                    |> set #realizedPnl txRealizedPnL
                |> createRecord

            -- Update or create holding
            case maybeHolding of
                Just holding -> do
                    let updatedHolding = fromDomainPosition newPosition holding
                    updatedHolding |> updateRecord
                Nothing -> do
                    let newHoldingBase = newRecord @Holding
                            |> set #userId currentUserId
                            |> set #marketId market.id
                            |> set #assetId assetId
                    let newHolding = fromDomainPosition newPosition newHoldingBase
                    newHolding |> createRecord

        -- Set success message
        let action = if tradeType == "buy" then "bought" else "sold"
        setSuccessMessage $ "Successfully " <> action <> " " <> show paramQty <> " shares for " <> formatMoney tradeAmount

        redirectTo (ShowMarketAction asset.marketId Nothing Nothing)

    action ClosePositionAction { assetId } = do
        -- Fetch user's holding for this asset
        holding <- query @Holding
            |> filterWhere (#userId, currentUserId)
            |> filterWhere (#assetId, assetId)
            |> fetchOne

        -- Convert to domain position
        let currentPosition = toDomainPosition holding

        -- Fetch asset and market
        asset <- fetch assetId
        market <- fetch asset.marketId

        -- Fetch all assets for LMSR calculation
        assets <- query @Asset
            |> filterWhere (#marketId, market.id)
            |> fetch

        -- Calculate LMSR state
        let lmsrState = LMSR.precompute market.beta [(a.symbol, a.quantity) | a <- assets]
        let currentPrice = LMSR.price asset.symbol lmsrState

        -- Get user's wallet
        wallet <- query @Wallet
            |> filterWhere (#userId, currentUserId)
            |> fetchOne

        -- Determine close transaction based on current position
        let (closeSide, closeQty) = case Domain.posSide currentPosition of
                Just Domain.Long -> (Domain.Short, Domain.posQuantity currentPosition)
                Just Domain.Short -> (Domain.Long, Domain.posQuantity currentPosition)
                Nothing -> error "No position to close"

        let Domain.Quantity qty = closeQty
        let tradeAmount = if closeSide == Domain.Short
                then LMSR.calculateSellRevenue qty currentPrice market.beta
                else LMSR.calculateBuyCost qty currentPrice market.beta

        let deltaCents = if closeSide == Domain.Short
                then tradeAmount   -- User receives
                else -tradeAmount  -- User pays

        -- Build domain transaction to close position (prices will be set after asset update)
        let domainTx = Domain.Transaction
                { Domain.txSide = closeSide
                , Domain.txQuantity = closeQty
                , Domain.txCashFlow = Domain.Balance deltaCents
                , Domain.txPriceBefore = 0  -- Placeholder, will be set to currentPrice
                , Domain.txPriceAfter = 0   -- Placeholder, will be calculated after trade
                }

        -- Apply transaction to close position
        let closedPosition = Domain.applyTransaction domainTx currentPosition

        -- Calculate realized PnL from this closing transaction
        let Domain.Balance oldRealizedPnL = Domain.posRealizedPnL currentPosition
        let Domain.Balance newRealizedPnL = Domain.posRealizedPnL closedPosition
        let txRealizedPnL = newRealizedPnL - oldRealizedPnL

        withTransaction do
            -- Update asset quantity
            let assetDeltaQty = if closeSide == Domain.Long then qty else (-qty)
            let updatedAssetQty = asset.quantity + assetDeltaQty
            asset
                |> set #quantity updatedAssetQty
                |> updateRecord

            -- Calculate price_after based on new asset quantities
            let newLmsrState = LMSR.precompute market.beta
                    [(a.symbol, if a.id == asset.id then updatedAssetQty else a.quantity) | a <- assets]
            let priceAfter = LMSR.price asset.symbol newLmsrState

            -- Update market statistics
            market
                |> set #trades (market.trades + 1)
                |> set #volume (market.volume + qty)
                |> set #turnover (market.turnover + tradeAmount)
                |> updateRecord

            -- Update wallet balance
            wallet
                |> set #amount (wallet.amount + deltaCents)
                |> updateRecord

            -- Create transaction record with both prices
            let domainTxnWithPrices = domainTx
                    { Domain.txPriceBefore = currentPrice
                    , Domain.txPriceAfter = priceAfter
                    }
            let txnBase = newRecord @Transaction
                    |> set #userId currentUserId
                    |> set #assetId assetId
                    |> set #marketId market.id
            _ <- fromDomainTransaction domainTxnWithPrices txnBase
                    |> set #realizedPnl txRealizedPnL
                |> createRecord

            -- Update holding - position is now flat
            let updatedHolding = fromDomainPosition closedPosition holding
            updatedHolding |> updateRecord

        -- Set success message
        let action = if closeSide == Domain.Short then "sold" else "bought"
        setSuccessMessage $ "Successfully closed position by " <> action <> " " <> show qty <> " shares for " <> formatMoney tradeAmount

        redirectTo (DashboardHoldingsAction Nothing)
