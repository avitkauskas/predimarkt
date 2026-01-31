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
        let quantity = param @Int "quantity"
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
        let (moneyAmount, newQuantity, newBalance) = 
                if tradeType == "buy"
                then 
                    -- BUY: Calculate cost and deduct from wallet
                    let cost = calculateBuyCost quantity currentPrice market.beta assetTotal
                        costCents = round (cost * 100)
                        newBal = wallet.amountCents - costCents
                        newQty = asset.quantity + quantity
                    in (cost, newQty, newBal)
                else 
                    -- SELL: Calculate revenue and add to wallet
                    let revenue = calculateSellRevenue quantity currentPrice market.beta assetTotal
                        revenueCents = round (revenue * 100)
                        newBal = wallet.amountCents + revenueCents
                        newQty = asset.quantity - quantity
                    in (revenue, newQty, newBal)
        
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
            |> set #quantity newQuantity
            |> updateRecord
        
        -- Update wallet balance
        wallet
            |> set #amountCents newBalance
            |> updateRecord
        
        -- Set success message
        let action = if tradeType == "buy" then "bought" else "sold"
        setSuccessMessage $ "Successfully " <> action <> " " <> show quantity <> " shares for " <> formatMoney (moneyFromDouble moneyAmount)
        
        redirectTo (ShowMarketAction asset.marketId Nothing Nothing)
