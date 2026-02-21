{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use void" #-}
module Web.Controller.Markets where

import Application.Domain.LMSR
import Application.Domain.Position
import Application.Domain.Types
import Control.Lens (Field1 (_1))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as AesonTypes
import Data.List (zipWith4)
import qualified Data.List as List
import qualified Data.Map.Strict as M
import Data.Time (Day, addDays, utctDay)
import Data.Time.Clock (UTCTime (..))
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.UUID (UUID)
import qualified Data.UUID as UUID
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

        market :: Market <- fetch mId
        category <- fetch (market.categoryId)
        assets <- query @Asset
            |> filterWhere (#marketId, mId)
            |> orderByAsc #quantity
            |> fetch
        let sortedAssets = sortAssetsForDisplay assets

        chartData <- fetchChartData market assets market.beta

        render ShowView { market, category, assets = sortedAssets, tradingAssetId = tAssetId, tradingAction = tAction, chartData }

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

    action ConfirmRefundMarketAction { marketId } = do
        let mId = if marketId == def then param @(Id Market) "marketId" else marketId
        market <- fetch mId
        accessDeniedUnless (market.userId == Just currentUserId)
        render RefundView { .. }

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

-- | Fetch transaction data and aggregate into daily price points for line chart
fetchChartData :: (?modelContext::ModelContext) => Market -> [Asset] -> Integer -> IO [AssetChartData]
fetchChartData market assets beta = do
    now <- getCurrentTime
    let today = utctDay now

    let endDay = min (utctDay market.closedAt) today

    let startDay = case market.openedAt of
            Just openedTime -> utctDay openedTime
            Nothing         -> utctDay (get #createdAt market)

    transactions <- query @Transaction
        |> filterWhere (#marketId, market.id)
        |> orderByAsc #createdAt
        |> fetch

    let validTransactions = filter (\t -> get #marketState t /= Aeson.object []) transactions

    let currentQtyMap = M.fromList [(get #id a, Quantity (get #quantity a)) | a <- assets]
    let currentPrices = allAssetPrices (Beta beta) currentQtyMap

    if null validTransactions
        then pure $ makeFlatLineFromDay startDay endDay assets currentPrices
        else do
            let filteredTxns = filter (\t -> utctDay (get #createdAt t) <= endDay) validTransactions
            let assetTxns = groupAssetTransactions filteredTxns
            let lastTxnPerDay = getLastTransactionPerDay assetTxns
            let dayRange = generateDayRange startDay endDay
            let pricesPerDay = computePricesPerDay lastTxnPerDay assets beta
            let filledData = fillMissingDays dayRange pricesPerDay currentPrices

            pure $ map (buildAssetChartData filledData endDay) assets
  where
    groupAssetTransactions :: [Transaction] -> M.Map (Id Asset) [(Day, Transaction)]
    groupAssetTransactions txns = M.fromListWith (++)
        [ (get #assetId t, [(utctDay (get #createdAt t), t)])
        | t <- txns
        ]

    getLastTransactionPerDay :: M.Map (Id Asset) [(Day, Transaction)] -> M.Map (Id Asset) (M.Map Day Transaction)
    getLastTransactionPerDay assetTxns = M.map processAssetTxns assetTxns
      where
        processAssetTxns :: [(Day, Transaction)] -> M.Map Day Transaction
        processAssetTxns dayTxns =
            let sorted = List.sortBy (\(d1, _) (d2, _) -> compare d1 d2) dayTxns
            in M.fromList (getLastPerDay sorted)

        getLastPerDay :: [(Day, Transaction)] -> [(Day, Transaction)]
        getLastPerDay [] = []
        getLastPerDay [x] = [x]
        getLastPerDay ((d1, t1):(d2, t2):rest)
            | d1 == d2 = getLastPerDay ((d1, t2):rest)
            | otherwise = (d1, t1) : getLastPerDay ((d2, t2):rest)

    generateDayRange :: Day -> Day -> [Day]
    generateDayRange start end = takeWhile (<= end) (iterate (addDays 1) start)

    computePricesPerDay :: M.Map (Id Asset) (M.Map Day Transaction) -> [Asset] -> Integer -> M.Map Day (M.Map (Id Asset) Double)
    computePricesPerDay lastTxns assets' beta = M.fromListWith M.union
        [ (day, prices)
        | (assetId, dayTxns) <- M.toList lastTxns
        , (day, txn) <- M.toList dayTxns
        , let prices = case parseMarketState (get #marketState txn) of
                Just qtyMap -> allAssetPrices (Beta beta) qtyMap
                Nothing     -> M.empty
        ]

    parseMarketState :: Aeson.Value -> Maybe (M.Map (Id Asset) Quantity)
    parseMarketState value = case Aeson.fromJSON value of
        AesonTypes.Success (obj :: [(Text, Int)]) -> Just $ M.fromList
            [ (Id uid, Quantity (fromIntegral qty))
            | (key, qty) <- obj
            , Just uid <- [UUID.fromText key]
            ]
        AesonTypes.Error _ -> Nothing

    makeFlatLineFromDay :: Day -> Day -> [Asset] -> M.Map (Id Asset) Double -> [AssetChartData]
    makeFlatLineFromDay startDay endDay assets' prices = do
        let dayRange = generateDayRange startDay endDay
        map (buildFlatAssetChart dayRange prices) assets'

    buildFlatAssetChart :: [Day] -> M.Map (Id Asset) Double -> Asset -> AssetChartData
    buildFlatAssetChart days prices asset =
        let assetId = get #id asset
            price = fromMaybe 0.0 (M.lookup assetId prices)
            points = map (\d -> PricePoint (dayToTimestamp d) price) days
        in AssetChartData
            { chartAssetId = assetId
            , chartAssetSymbol = get #symbol asset
            , chartAssetName = get #name asset
            , chartAssetColor = assetColorFor assets asset
            , chartData = points
            }

    fillMissingDays :: [Day] -> M.Map Day (M.Map (Id Asset) Double) -> M.Map (Id Asset) Double -> M.Map Day (M.Map (Id Asset) Double)
    fillMissingDays [] _ _ = M.empty
    fillMissingDays (day:days) pricesPerDay lastKnownPrices =
        let dayPrices = case M.lookup day pricesPerDay of
                Just p  -> p
                Nothing -> lastKnownPrices
            nextPrices = if M.null dayPrices then lastKnownPrices else dayPrices
            rest = fillMissingDays days pricesPerDay nextPrices
        in M.insert day dayPrices rest

    buildAssetChartData :: M.Map Day (M.Map (Id Asset) Double) -> Day -> Asset -> AssetChartData
    buildAssetChartData filledData today asset =
        let assetId = get #id asset
            points = map (\day ->
                let price = case M.lookup day filledData of
                        Just dayPrices -> fromMaybe 0.0 (M.lookup assetId dayPrices)
                        Nothing -> 0.0
                in PricePoint (dayToTimestamp day) price
                ) (sort $ M.keys filledData)
        in AssetChartData
            { chartAssetId = assetId
            , chartAssetSymbol = get #symbol asset
            , chartAssetName = get #name asset
            , chartAssetColor = assetColorFor assets asset
            , chartData = points
            }

    dayToTimestamp :: Day -> Int
    dayToTimestamp day = floor (utcTimeToPOSIXSeconds (UTCTime day 0))

    assetColorFor :: [Asset] -> Asset -> Text
    assetColorFor allAssets asset =
        let colors = ["#2962FF", "#E91E63", "#4CAF50", "#FF9800", "#9C27B0", "#00BCD4"]
            idx = fromMaybe 0 (List.elemIndex asset allAssets)
        in colors !! (idx `mod` length colors)
