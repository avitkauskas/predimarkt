{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use void" #-}
module Web.Controller.Markets where

import Application.Adapter.Position
import Data.List (zipWith4)
import qualified Data.List as List
import Data.Time (utctDay)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import qualified Domain.LMSR as LMSR
import qualified Domain.Logic as Domain
import qualified Domain.Types as Domain
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

        markets' <-
            query @Market
                |> applyCategoryFilter
                |> applyStatusFilter
                |> orderByDesc #updatedAt
                |> fetch
                >>= collectionFetchRelated #assets
                >>= collectionFetchRelated #categoryId

        categories <- fetchCategories
        let markets = map (\m -> m |> set #assets (sortAssetsForDisplay (get #assets m))) markets'
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

        market' <- fetch mId
            >>= fetchRelated #assets
            >>= fetchRelated #categoryId
        let sortedAssets = sortAssetsForDisplay (get #assets market')
        let market = market' |> set #assets sortedAssets

        assets' <- query @Asset
            |> filterWhere (#marketId, mId)
            |> orderByAsc #quantity
            |> fetch
        chartData <- fetchChartData mId assets' market.beta

        render ShowView { market, tradingAssetId = tAssetId, tradingAction = tAction, chartData }

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

        positions <- query @Position
            |> filterWhere (#marketId, market.id)
            |> filterWhereNot (#quantity, 0)
            |> fetch

        now <- getCurrentTime

        withTransaction do
            market <- market
                |> set #status MarketStatusResolved
                |> set #resolvedAt (Just now)
                |> set #outcomeAssetId (Just outcomeAssetId)
                |> updateRecord

            forM_ positions \position -> do
                wallet <- query @Wallet
                    |> filterWhere (#userId, position.userId)
                    |> fetchOne

                -- Convert to domain position and resolve
                let domainPosition = toDomainPosition position
                let resolvedPosition = Domain.resolvePosition (position.assetId == outcomeAssetId) domainPosition
                let Domain.Balance refundAmount = Domain.refundPosition resolvedPosition

                -- Update wallet
                wallet
                    |> set #amount (wallet.amount + refundAmount)
                    |> updateRecord

                -- Create transaction for resolution
                -- Side is derived from quantity: negative quantity = closing transaction
                _ <- newRecord @Transaction
                    |> set #userId position.userId
                    |> set #assetId position.assetId
                    |> set #marketId market.id
                    |> set #quantity (-position.quantity)
                    |> set #cashFlow refundAmount
                    |> set #priceBefore 0
                    |> set #priceAfter 0
                    |> createRecord

                -- Update position to reflect resolved state
                let updatedPosition = fromDomainPosition resolvedPosition position
                updatedPosition |> updateRecord

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

        -- Get all positions (both open and closed) for refunding
        positions <- query @Position
            |> filterWhere (#marketId, market.id)
            |> fetch

        now <- getCurrentTime

        withTransaction do
            -- Update market status
            market <- market
                |> set #status MarketStatusRefunded
                |> set #refundedAt (Just now)
                |> updateRecord

            -- Process each position and refund
            forM_ positions \position -> do
                wallet <- query @Wallet
                    |> filterWhere (#userId, position.userId)
                    |> fetchOne

                -- Calculate refund amount (cost basis + realized PnL)
                let domainPosition = toDomainPosition position
                let Domain.Balance refundAmount = Domain.refundPosition domainPosition

                -- Create transaction record for the refund
                -- Side is derived from quantity: negative quantity = closing transaction
                _ <- newRecord @Transaction
                    |> set #userId position.userId
                    |> set #assetId position.assetId
                    |> set #marketId market.id
                    |> set #quantity (-position.quantity)
                    |> set #cashFlow refundAmount
                    |> set #priceBefore 0
                    |> set #priceAfter 0
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
                        , Domain.posRealizedPnL = Domain.posRealizedPnL domainPosition
                        }
                let updatedPosition = fromDomainPosition closedPosition position
                updatedPosition |> updateRecord

        setSuccessMessage "Market refunded successfully"
        redirectTo $ ShowMarketAction mId Nothing Nothing

fetchAssetsFromParams :: (?context :: ControllerContext, ?request :: Request) => IO [Asset]
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

-- | Fetch and aggregate transaction data into OHLC format by day
fetchChartData :: (?modelContext::ModelContext) => Id Market -> [Asset] -> Integer -> IO [AssetChartData]
fetchChartData marketId assets beta = do
    transactions <- query @Transaction
        |> filterWhere (#marketId, marketId)
        |> orderByAsc #createdAt
        |> fetch

    now <- getCurrentTime
    let currentTimestamp = floor (utcTimeToPOSIXSeconds now)
    let lmsrState = LMSR.precompute beta [(get #id a, get #quantity a) | a <- assets]
    let currentPrices = [(get #id a, LMSR.price (get #id a) lmsrState) | a <- assets]

    if null transactions
        then pure $ map (makeFlatChartData currentPrices assets currentTimestamp) assets
        else pure $ map (aggregateTransactions transactions assets beta currentTimestamp) assets
  where
    -- For markets with no transactions, show flat line at current price
    makeFlatChartData :: [(Id Asset, Double)] -> [Asset] -> Int -> Asset -> AssetChartData
    makeFlatChartData prices allAssets currentTime asset =
        let currentPrice = maybe 0.0 (\x -> x) (List.lookup (get #id asset) prices)
        in AssetChartData
            { chartAssetId = get #id asset
            , chartAssetName = get #name asset
            , chartAssetColor = assetColorFor allAssets asset
            , chartOhlcData = [OhlcPoint currentTime currentPrice currentPrice currentPrice currentPrice]
            }

    -- Aggregate transactions by day for each asset
    aggregateTransactions :: [Transaction] -> [Asset] -> Integer -> Int -> Asset -> AssetChartData
    aggregateTransactions txns allAssets beta currentTime asset =
        let assetTxns = filter (\t -> get #assetId t == get #id asset) txns
            byDay = List.groupBy (\t1 t2 -> utctDay (get #createdAt t1) == utctDay (get #createdAt t2)) assetTxns
            ohlcPoints = if null assetTxns
                then -- No transactions for this asset, show flat line at current price
                    let lmsrState = LMSR.precompute beta [(get #id a, get #quantity a) | a <- allAssets]
                        currentPrice = LMSR.price (get #id asset) lmsrState
                    in [OhlcPoint currentTime currentPrice currentPrice currentPrice currentPrice]
                else map transactionsToOhlc byDay
        in AssetChartData
            { chartAssetId = get #id asset
            , chartAssetName = get #name asset
            , chartAssetColor = assetColorFor allAssets asset
            , chartOhlcData = ohlcPoints
            }

    -- Convert a day's transactions to OHLC point
    transactionsToOhlc :: [Transaction] -> OhlcPoint
    transactionsToOhlc [] = OhlcPoint 0 0.0 0.0 0.0 0.0
    transactionsToOhlc dayTxns =
        let mkPoint :: Transaction -> (Int, Double)
            mkPoint txn =
                let time = floor (utcTimeToPOSIXSeconds (get #createdAt txn))
                    price = get #priceAfter txn
                in (time, price)
            points :: [(Int, Double)]
            points = map mkPoint dayTxns
            times = map fst points
            prices :: [Double]
            prices = map snd points
            minTime = minimum times
            (open, high, low, close) = case prices of
                [] -> (0.0, 0.0, 0.0, 0.0)
                (p:ps) -> (p, maximum (p:ps), minimum (p:ps), case reverse (p:ps) of (x:_) -> x; _ -> p)
        in OhlcPoint minTime open high low close

    -- Assign colors to assets based on index
    assetColorFor :: [Asset] -> Asset -> Text
    assetColorFor allAssets asset =
        let colors = ["#2962FF", "#E91E63", "#4CAF50", "#FF9800", "#9C27B0", "#00BCD4"]
            idx = fromMaybe 0 (List.elemIndex asset allAssets)
        in colors !! (idx `mod` length colors)
