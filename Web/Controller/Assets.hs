module Web.Controller.Assets where

import Web.Controller.Prelude
import Web.View.Assets.New
import Application.Helper.LMSR
import Web.Types.Money

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
        let paramQty = param @Int "quantity"
        let tradeType = param @Text "type"
        
        -- Fetch all assets for LMSR calculation
        assets <- query @Asset
            |> filterWhere (#marketId, market.id)
            |> fetch
        
        -- Calculate LMSR state
        let lmsrState = precompute market.beta assets
        let assetSum = sumItem asset.id lmsrState
        let assetTotal = sumTotal lmsrState
        let currentPrice = assetSum / assetTotal
        
        -- Get user's wallet
        wallet <- query @Wallet
            |> filterWhere (#userId, currentUserId)
            |> fetchOne

        -- Calculate money amount and new quantities
        let (deltaCents, deltaQty) = 
                if tradeType == "buy"
                then 
                    -- BUY: Calculate cost and deduct from wallet
                    let cost = calculateBuyCost paramQty currentPrice market.beta assetTotal
                        costCents = round (cost * 100)
                    in (-costCents, paramQty)
                else 
                    -- SELL: Calculate revenue and add to wallet
                    let revenue = calculateSellRevenue paramQty currentPrice market.beta assetTotal
                        revenueCents = round (revenue * 100)
                    in (revenueCents, -paramQty)
        
        -- Validate sufficient funds for buying
        -- when (tradeType == "buy" && newBalance < 0) $ do
        --     setErrorMessage $ "Insufficient funds. This trade requires " <> formatMoney (moneyFromDouble moneyAmount) <> " but you only have " <> formatMoney (moneyFromCents wallet.amountCents)
        --     redirectTo (ShowMarketAction asset.marketId (Just asset.id) (Just tradeType))
        
        -- Validate sufficient shares for selling
        -- when (tradeType == "sell" && newQuantity < 0) $ do
        --     setErrorMessage $ "Insufficient shares. You are trying to sell " <> show quantity <> " shares but only have " <> show asset.quantity
        --     redirectTo (ShowMarketAction asset.marketId (Just asset.id) (Just tradeType))
        
        -- Update asset quantity
        asset 
            |> set #quantity (asset.quantity + deltaQty)
            |> updateRecord
        
        -- Update wallet balance
        wallet
            |> set #amountCents (wallet.amountCents + deltaCents)
            |> updateRecord

        -- Create transaction
        transaction <- newRecord @Transaction
            |> set #userId currentUserId
            |> set #assetId assetId
            |> set #marketId market.id
            |> set #quantity deltaQty
            |> set #amountCents (abs deltaCents)
            |> createRecord

        -- Create or update holding
        maybeHolding <- query @Holding
            |> filterWhere (#userId, currentUserId)
            |> filterWhere (#assetId, assetId)
            |> fetchOneOrNothing

        case maybeHolding of
            Just holding ->
                holding
                    |> set #quantity (holding.quantity + deltaQty)
                    |> set #amountCents (holding.amountCents - deltaCents)
                    |> updateRecord
            Nothing ->
                newRecord @Holding
                    |> set #userId currentUserId
                    |> set #marketId market.id
                    |> set #assetId assetId
                    |> set #quantity deltaQty
                    |> set #amountCents (-deltaCents)
                    |> createRecord
        
        -- Set success message
        let action = if tradeType == "buy" then "bought" else "sold"
        setSuccessMessage $ "Successfully " <> action <> " " <> show (abs deltaQty) <> " shares for " <> formatMoney (moneyFromCents (abs deltaCents))
        
        redirectTo (ShowMarketAction asset.marketId Nothing Nothing)
