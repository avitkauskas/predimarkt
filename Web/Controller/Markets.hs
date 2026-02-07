{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use void" #-}
module Web.Controller.Markets where

import Application.Adapter.Position
import Application.Helper.View (formatMoney)
import Data.List (zipWith4)
import qualified Domain.Logic as Domain
import qualified Domain.Types as Domain
import Network.Wai (Request)
import Web.Controller.Prelude
import Web.Types
import Web.View.Markets.Edit
import Web.View.Markets.Index
import Web.View.Markets.New
import Web.View.Markets.Refund
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

    action SetResolveAssetAction { marketId } = do
        let mId = if marketId == def then param @(Id Market) "marketId" else marketId
        market <- fetch mId
        accessDeniedUnless (market.userId == Just currentUserId)
        accessDeniedUnless (market.status == MarketStatusClosed)
        assets <- query @Asset
            |> filterWhere (#marketId, mId)
            |> orderByDesc #quantity
            |> fetch
        render ResolveView { .. }

    action ResolveMarketAction { marketId } = do
        let mId = if marketId == def then param @(Id Market) "marketId" else marketId
        market <- fetch mId
        accessDeniedUnless (market.userId == Just currentUserId)
        accessDeniedUnless (market.status == MarketStatusClosed)

        let outcomeAssetId = param @(Id Asset) "outcomeAssetId"

        outcomeAsset <- fetch outcomeAssetId
        accessDeniedUnless (outcomeAsset.marketId == market.id)

        holdings <- query @Holding
            |> filterWhere (#marketId, market.id)
            |> filterWhereNot (#quantity, 0)
            |> fetch

        now <- getCurrentTime

        withTransaction do
            market <- market
                |> set #status MarketStatusResolved
                |> set #resolvedAt (Just now)
                |> updateRecord

            forM_ holdings \holding -> do
                wallet <- query @Wallet
                    |> filterWhere (#userId, holding.userId)
                    |> fetchOne

                -- Convert to domain position and resolve
                let position = toDomainPosition holding
                let resolvedPosition = Domain.resolvePosition (holding.assetId == outcomeAssetId) position
                let Domain.Balance refundAmount = Domain.refundPosition resolvedPosition

                -- Update wallet
                wallet
                    |> set #amount (wallet.amount + refundAmount)
                    |> updateRecord

                -- Create transaction for resolution
                let Domain.Balance realizedPnlValue = Domain.posRealizedPnL resolvedPosition
                _ <- newRecord @Transaction
                    |> set #userId holding.userId
                    |> set #assetId holding.assetId
                    |> set #marketId market.id
                    |> set #quantity (-holding.quantity)
                    |> set #cashFlow refundAmount
                    |> set #side "long"  -- Resolution is treated as closing long position
                    |> set #marketQBefore 0
                    |> set #marketQAfter 0
                    |> set #priceBefore 0
                    |> set #priceAfter 0
                    |> set #realizedPnl realizedPnlValue
                    |> createRecord

                -- Update holding to reflect resolved state
                let updatedHolding = fromDomainPosition resolvedPosition holding
                updatedHolding |> updateRecord

        setSuccessMessage "Market resolved successfully"
        redirectTo $ ShowMarketAction mId Nothing Nothing

    action ConfirmRefundMarketAction { marketId } = do
        let mId = if marketId == def then param @(Id Market) "marketId" else marketId
        market <- fetch mId
        accessDeniedUnless (market.userId == Just currentUserId)
        render RefundView { .. }

    action RefundMarketAction { marketId } = do
        let mId = if marketId == def then param @(Id Market) "marketId" else marketId
        market <- fetch mId
        accessDeniedUnless (market.userId == Just currentUserId)
        accessDeniedUnless (market.status == MarketStatusClosed)

        -- Get all holdings (both open and closed) for refunding
        holdings <- query @Holding
            |> filterWhere (#marketId, market.id)
            |> fetch

        now <- getCurrentTime

        withTransaction do
            -- Update market status
            market <- market
                |> set #status MarketStatusRefunded
                |> set #refundedAt (Just now)
                |> updateRecord

            -- Process each holding and refund
            forM_ holdings \holding -> do
                wallet <- query @Wallet
                    |> filterWhere (#userId, holding.userId)
                    |> fetchOne

                -- Calculate refund amount (cost basis + realized PnL)
                let position = toDomainPosition holding
                let Domain.Balance refundAmount = Domain.refundPosition position

                -- Create transaction record for the refund
                let Domain.Balance realizedPnlValue = Domain.posRealizedPnL position
                _ <- newRecord @Transaction
                    |> set #userId holding.userId
                    |> set #assetId holding.assetId
                    |> set #marketId market.id
                    |> set #quantity (-holding.quantity)
                    |> set #cashFlow refundAmount
                    |> set #side (fromMaybe "long" holding.side)
                    |> set #marketQBefore 0
                    |> set #marketQAfter 0
                    |> set #priceBefore 0
                    |> set #priceAfter 0
                    |> set #realizedPnl realizedPnlValue
                    |> createRecord

                -- Update wallet balance with refund
                wallet
                    |> set #amount (wallet.amount + refundAmount)
                    |> updateRecord

                -- Close the position
                let closedPosition = Domain.Position
                        { Domain.posSide = Nothing
                        , Domain.posQuantity = Domain.Quantity 0
                        , Domain.posCostBasis = Domain.Balance 0
                        , Domain.posRealizedPnL = Domain.posRealizedPnL position
                        }
                let updatedHolding = fromDomainPosition closedPosition holding
                updatedHolding |> updateRecord

        setSuccessMessage "Market refunded successfully"
        redirectTo $ ShowMarketAction mId Nothing Nothing

fetchAssetsFromParams :: (?request :: Request) => IO [Asset]
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
