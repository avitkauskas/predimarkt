{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE QuasiQuotes    #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use void" #-}
module Web.Controller.Markets where

import Application.Domain.ChartData
import Application.Domain.Types
import Data.List (zipWith4)
import Data.Text (strip)
import Data.Time (addDays, utctDay)
import Data.Time.Clock (UTCTime (..))
import Text.RawString.QQ (r)
import Web.Controller.Prelude
import Web.Job.CloseMarket
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
        let searchFilter = paramOrNothing "search"

        let applyCategoryFilter queryBuilder =
                case categoryFilter of
                    Just categoryId -> queryBuilder |> filterWhere (#categoryId, categoryId)
                    Nothing         -> queryBuilder

        matchingMarketIds <- case searchFilter of
            Just searchQuery -> do
                (rows :: [Only (Id Market)]) <- sqlQuery
                    [r|
                        SELECT DISTINCT m.id
                        FROM markets m
                        LEFT JOIN assets a ON a.market_id = m.id
                        WHERE m.title ILIKE ? OR a.name ILIKE ?
                    |]
                    ("%" <> searchQuery <> "%", "%" <> searchQuery <> "%")
                pure $ map fromOnly rows
            Nothing -> pure []

        let applySearchFilter queryBuilder =
                case searchFilter of
                    Just _  -> queryBuilder |> filterWhereIn (#id, matchingMarketIds)
                    Nothing -> queryBuilder

        let applyStatusFilter queryBuilder =
                queryBuilder
                    |> filterWhereNot (#status, MarketStatusDraft)

        let applyRecentActivityFilter queryBuilder =
                queryBuilder
                    |> queryOr
                        (filterWhere (#status, MarketStatusOpen))
                        (filterWhereSql (#updatedAt, ">= CURRENT_DATE - INTERVAL '10 days'"))

        markets' <-
            query @Market
                |> applyRecentActivityFilter
                |> applyCategoryFilter
                |> applySearchFilter
                |> applyStatusFilter
                |> orderByDesc #openedAt
                |> fetch
                >>= collectionFetchRelated #categoryId
                >>= collectionFetchRelated #assets

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
        let marketWithFormData = fillMarketWithFormData market

        if length assets < 2
            then do
                setErrorMessage "Market must have at least 2 assets"
                categories <- fetchCategories
                render EditView { market = marketWithFormData, .. }
            else case validateAssetSymbols assets of
                Just errorMsg -> do
                    setErrorMessage errorMsg
                    categories <- fetchCategories
                    render EditView { market = marketWithFormData, .. }
                Nothing -> case validateAssetNames assets of
                    Just errorMsg -> do
                        setErrorMessage errorMsg
                        categories <- fetchCategories
                        render EditView { market = marketWithFormData, .. }
                    Nothing -> do
                        marketWithFormData
                        |> buildMarket now
                        |> ifValid \case
                            Left market -> do
                                categories <- fetchCategories
                                render EditView { market = marketWithFormData, .. }
                            Right market -> do
                                uniqueSlug <- constructUniqueSlug
                                    market.categoryId (toSlug market.title) (Just mId)

                                withTransaction do
                                    market <- market
                                        |> set #slug uniqueSlug
                                        |> updateRecord

                                    existingAssets <- query @Asset |> filterWhere (#marketId, mId) |> fetch
                                    let existingIds = map (.id) existingAssets
                                    let newIds = map (\a -> if a.id == def then Nothing else Just a.id) assets
                                    let keptIds = catMaybes newIds

                                    let assetsToDelete = filter (\a -> a.id `notElem` keptIds) existingAssets
                                    deleteRecords assetsToDelete

                                    forM_ assets \asset -> do
                                        if asset.id == def
                                            then asset |> set #marketId market.id |> createRecord
                                            else asset |> set #marketId market.id |> updateRecord

                                    existingJobs <- query @CloseMarketJob
                                        |> filterWhere (#marketId, market.id)
                                        |> fetch
                                    deleteRecords existingJobs
                                    newRecord @CloseMarketJob
                                        |> set #marketId market.id
                                        |> set #runAt market.closedAt
                                        |> createRecord

                                redirectTo $ DashboardMarketsAction { statusFilter = Just MarketStatusDraft }

    action CreateMarketAction = do
        ensureIsUser
        now <- getCurrentTime
        assets <- fetchAssetsFromParams
        let marketWithFormData = fillMarketWithFormData (newRecord @Market)

        if length assets < 2
            then do
                setErrorMessage "Market must have at least 2 assets"
                categories <- fetchCategories
                render NewView { market = marketWithFormData, .. }
            else case validateAssetSymbols assets of
                Just errorMsg -> do
                    setErrorMessage errorMsg
                    categories <- fetchCategories
                    render NewView { market = marketWithFormData, .. }
                Nothing -> case validateAssetNames assets of
                    Just errorMsg -> do
                        setErrorMessage errorMsg
                        categories <- fetchCategories
                        render NewView { market = marketWithFormData, .. }
                    Nothing -> do
                        marketWithFormData
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

                                    newRecord @CloseMarketJob
                                        |> set #marketId market.id
                                        |> set #runAt market.closedAt
                                        |> createRecord

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
                |> set #name (strip name)
                |> set #symbol (strip symbol)
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

validateAssetSymbols :: [Asset] -> Maybe Text
validateAssetSymbols assets =
    let symbols = map (get #symbol) assets
        emptySymbols = filter (isEmpty . strip) symbols
        uniqueSymbols = nub symbols
    in if not (null emptySymbols)
        then Just "Asset symbols cannot be empty"
        else if length uniqueSymbols /= length symbols
            then Just "Asset symbols must be unique within the market"
            else Nothing

validateAssetNames :: [Asset] -> Maybe Text
validateAssetNames assets =
    let names = map (get #name) assets
        emptyNames = filter (isEmpty . strip) names
        uniqueNames = nub names
    in if not (null emptyNames)
        then Just "Asset names cannot be empty"
        else if length uniqueNames /= length names
            then Just "Asset names must be unique within the market"
            else Nothing

fillMarketWithFormData :: (?context :: ControllerContext, ?request :: Request) => Market -> Market
fillMarketWithFormData market =
    let title = fromMaybe (get #title market) (paramOrNothing @Text "title")
        description = fromMaybe (get #description market) (paramOrNothing @Text "description")
        categoryId = fromMaybe (get #categoryId market) (paramOrNothing @(Id Category) "categoryId")
        closedAt = fromMaybe (get #closedAt market) (paramOrNothing @UTCTime "closedAt")
    in market
        |> set #title (strip title)
        |> set #description description
        |> set #categoryId categoryId
        |> set #closedAt closedAt

buildMarket now market = market
    |> validateField #title nonEmpty
    |> validateField #description nonEmpty
    |> validateField #categoryId nonEmpty
    |> validateField #closedAt (isGreaterThan now)

fetchCategories :: (?modelContext :: ModelContext) => IO [Category]
fetchCategories = query @Category |> orderByAsc #sortIdx |> fetch
