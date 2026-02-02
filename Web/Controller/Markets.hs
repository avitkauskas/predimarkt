{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use void" #-}
module Web.Controller.Markets where

import Application.Helper.LMSR
import Data.List (groupBy, zipWith4)
import Web.Controller.Prelude
import Web.Types
import Web.View.Markets.Edit
import Web.View.Markets.Index
import Web.View.Markets.New
import Web.View.Markets.Resolve
import Web.View.Markets.Show

instance Controller MarketsController where
    action MarketsAction = autoRefresh do
        let categoryFilter = paramOrNothing "category"

        let applyCategoryFilter queryBuilder =
                case categoryFilter of
                    Just categoryId -> queryBuilder |> filterWhere (#categoryId, categoryId)
                    Nothing         -> queryBuilder

        let applyStatusFilter queryBuilder =
                queryBuilder |> filterWhereNot (#status, MarketStatusDraft)

        markets <-
            query @Market
                |> applyCategoryFilter
                |> applyStatusFilter
                |> orderByDesc #updatedAt
                |> fetch
                >>= collectionFetchRelated #assets . map (modify #assets (orderByDesc #quantity))
                >>= collectionFetchRelated #categoryId

        categories <- fetchCategories
        render IndexView { .. }

    action NewMarketAction = do
        ensureIsUser
        now <- getCurrentTime
        let market = newRecord @Market
                |> set #closedAt (UTCTime (addDays 14 (utctDay now)) 0)
                |> set #userId (Just currentUserId)
        let assets = [ newRecord @Asset |> set #name "Yes" |> set #symbol "Yes"
                     , newRecord @Asset |> set #name "No" |> set #symbol "No"
                     ]
        categories <- fetchCategories
        render NewView { .. }

    action ShowMarketAction { marketId, tradingAssetId, tradingAction } = autoRefresh do
        ensureIsUser
        let mId = if marketId == def then param @(Id Market) "marketId" else marketId
        let tAssetId = tradingAssetId <|> paramOrNothing @(Id Asset) "tradingAssetId"
        let tAction = tradingAction <|> paramOrNothing @Text "tradingAction"

        market <- fetch mId
            >>= fetchRelated #assets . modify #assets (orderByDesc #quantity)
            >>= fetchRelated #categoryId
        render ShowView { market, tradingAssetId = tAssetId, tradingAction = tAction }

    action EditMarketAction { marketId } = do
        let mId = if marketId == def then param @(Id Market) "marketId" else marketId
        market <- fetch mId
        accessDeniedUnless (market.userId == Just currentUserId)
        accessDeniedUnless (market.status == MarketStatusDraft)
        assets <- query @Asset
            |> filterWhere (#marketId, mId)
            |> orderByAsc #quantity
            |> fetch
        categories <- fetchCategories
        render EditView { .. }

    action UpdateMarketAction { marketId } = do
        let mId = if marketId == def then param @(Id Market) "marketId" else marketId
        market <- fetch mId
        accessDeniedUnless (market.userId == Just currentUserId)
        accessDeniedUnless (market.status == MarketStatusDraft)
        now <- getCurrentTime
        assets <- fetchAssetsFromParams

        if length assets < 2
            then do
                setErrorMessage "Market must have at least 2 assets"
                categories <- fetchCategories
                render EditView { .. }
            else do
                market
                    |> buildMarket now
                    |> ifValid \case
                        Left market -> do
                            categories <- fetchCategories
                            render EditView { .. }
                        Right market -> do
                            uniqueSlug <- constructUniqueSlug
                                market.categoryId (toSlug market.title) (Just mId)

                            withTransaction do
                                market <- market
                                    |> set #slug uniqueSlug
                                    |> updateRecord

                                -- Handle assets diffing
                                existingAssets <- query @Asset |> filterWhere (#marketId, mId) |> fetch
                                let existingIds = map (.id) existingAssets
                                let newIds = map (\a -> if a.id == def then Nothing else Just a.id) assets
                                let keptIds = catMaybes newIds

                                -- Delete assets that are no longer in the form
                                let assetsToDelete = filter (\a -> a.id `notElem` keptIds) existingAssets
                                deleteRecords assetsToDelete

                                -- Create or Update remaining assets
                                forM_ assets \asset -> do
                                    if asset.id == def
                                        then asset |> set #marketId market.id |> createRecord
                                        else asset |> set #marketId market.id |> updateRecord

                            redirectTo $ DashboardMarketsAction { statusFilter = Just MarketStatusDraft }

    action CreateMarketAction = do
        ensureIsUser
        now <- getCurrentTime
        assets <- fetchAssetsFromParams
        let market = newRecord @Market

        if length assets < 2
            then do
                setErrorMessage "Market must have at least 2 assets"
                categories <- fetchCategories
                render NewView { .. }
            else do
                market
                    |> buildMarket now
                    |> set #userId (Just currentUserId)
                    |> ifValid \case
                        Left market -> do
                            categories <- fetchCategories
                            render NewView { .. }
                        Right market -> do
                            withTransaction do
                                uniqueSlug <- constructUniqueSlug
                                    market.categoryId (toSlug market.title) Nothing
                                market <- market
                                    |> set #slug uniqueSlug
                                    |> createRecord

                                forM_ assets \asset -> do
                                    asset |> set #marketId market.id |> createRecord

                            setSuccessMessage "Market created"
                            redirectTo $ DashboardMarketsAction { statusFilter = Just MarketStatusDraft }

    action DeleteMarketAction { marketId } = do
        let mId = if marketId == def then param @(Id Market) "marketId" else marketId
        market <- fetch mId
        accessDeniedUnless (market.userId == Just currentUserId)
        accessDeniedUnless (market.status == MarketStatusDraft)
        deleteRecord market
        setSuccessMessage "Market deleted"
        redirectTo $ DashboardMarketsAction { statusFilter = Just MarketStatusDraft }

    action ResolveMarketAction { marketId } = do
        let mId = if marketId == def then param @(Id Market) "marketId" else marketId
        market <- fetch mId
        accessDeniedUnless (market.userId == Just currentUserId)
        accessDeniedUnless (market.status == MarketStatusOpen)
        assets <- query @Asset
            |> filterWhere (#marketId, mId)
            |> orderByDesc #quantity
            |> fetch
        render ResolveView { .. }

    action ChangeMarketStatusWithResolutionAction { marketId } = do
        let mId = if marketId == def then param @(Id Market) "marketId" else marketId
        market <- fetch mId
        accessDeniedUnless (market.userId == Just currentUserId)
        accessDeniedUnless (market.status == MarketStatusOpen)

        -- Get the selected outcome asset ID
        let outcomeAssetIdParam = param @(Id Asset) "outcomeAssetId"

        -- Verify the asset belongs to this market
        outcomeAsset <- fetch outcomeAssetIdParam
        accessDeniedUnless (outcomeAsset.marketId == market.id)

        -- Get all assets for LMSR calculation and all holdings
        assets <- query @Asset
            |> filterWhere (#marketId, market.id)
            |> fetch

        holdings <- query @Holding
            |> filterWhere (#marketId, market.id)
            |> filterWhereNot (#quantity, 0)
            |> fetch

        -- Group holdings by user
        let holdingsByUser = groupBy (\h1 h2 -> h1.userId == h2.userId) holdings

        now <- getCurrentTime

        -- Perform all settlement operations in a transaction
        withTransaction do
            -- Update market status
            market <- market
                |> set #status MarketStatusResolved
                |> set #resolvedAt (Just now)
                |> updateRecord

            -- Update all assets to resolved status
            forM_ assets \asset -> do
                asset
                    |> set #status AssetStatusResolved
                    |> updateRecord

            -- Process each user's holdings
            forM_ holdingsByUser \userHoldings -> do
                processUserSettlement market assets userHoldings outcomeAsset

        setSuccessMessage "Market resolved successfully"
        redirectTo $ ShowMarketAction mId Nothing Nothing

processUserSettlement :: (?modelContext :: ModelContext) => Market -> [Asset] -> [Holding] -> Asset -> IO ()
processUserSettlement market assets userHoldings outcomeAsset = do
    case userHoldings of
        [] -> pure ()
        (firstHolding:_) -> do
            let userId = firstHolding.userId
            wallet <- query @Wallet
                |> filterWhere (#userId, userId)
                |> fetchOne

            -- Calculate LMSR state at settlement
            let lmsrState = precompute market.beta assets

            -- Process each holding for this user
            forM_ userHoldings \holding -> do
                -- Get the asset
                asset <- fetch holding.assetId

                -- Determine settlement price and action
                let (settlePrice, tradeType) =
                        if holding.assetId == outcomeAsset.id
                        then (1.0, if holding.quantity > 0 then "sell" else "buy" :: Text)  -- Outcome asset: 1 for winners, 0 for losers
                        else (0.0, if holding.quantity > 0 then "sell" else "buy" :: Text)  -- Other assets: 0

                let settleQty = abs holding.quantity

                -- Calculate settlement amount using LMSR at settlement price
                let (settleCents, deltaQty) =
                        if tradeType == "buy"
                        then
                            let cost = calculateBuyCost settleQty settlePrice market.beta (sumTotal lmsrState)
                                costCents = round (cost * 100)
                            in (-costCents, settleQty)
                        else
                            let revenue = calculateSellRevenue settleQty settlePrice market.beta (sumTotal lmsrState)
                                revenueCents = round (revenue * 100)
                            in (revenueCents, -settleQty)

                -- Update asset quantity
                asset
                    |> set #quantity (asset.quantity + deltaQty)
                    |> updateRecord

                -- Update wallet balance
                wallet
                    |> set #amountCents (wallet.amountCents + settleCents)
                    |> updateRecord

                -- Create transaction record
                _ <- newRecord @Transaction
                    |> set #userId userId
                    |> set #assetId holding.assetId
                    |> set #marketId market.id
                    |> set #quantity deltaQty
                    |> set #amountCents (abs settleCents)
                    |> createRecord

                -- Update holding - set quantity to 0 (closed)
                holding
                    |> set #quantity 0
                    |> set #amountCents (holding.amountCents - settleCents)
                    |> updateRecord

fetchAssetsFromParams :: (?context :: ControllerContext) => IO [Asset]
fetchAssetsFromParams =
    pure $ zipWith4 (\assetId name symbol quantity ->
        let asset = newRecord @Asset
                |> set #name name
                |> set #symbol symbol
                |> set #quantity quantity
        in if assetId == def
            then asset
            else asset |> set #id assetId)
        assetIds assetNames assetSymbols assetQuantities
    where
        assetIds = paramList "asset_id"
        assetNames = paramList "asset_name"
        assetSymbols = paramList "asset_symbol"
        assetQuantities = paramList "asset_quantity"

buildMarket now market = market
    |> fill @'["title", "description", "categoryId", "closedAt"]
    |> validateField #title nonEmpty
    |> validateField #description nonEmpty
    |> validateField #categoryId nonEmpty
    |> validateField #closedAt (isGreaterThan now)

fetchCategories :: (?modelContext :: ModelContext) => IO [Category]
fetchCategories = query @Category |> orderByAsc #sortIdx |> fetch
