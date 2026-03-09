{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE QuasiQuotes    #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use void" #-}
module Web.Controller.Markets where

import Application.Domain.ChartData
import Application.Domain.Types
import Data.List (zipWith4)
import Data.Text (strip)
import qualified Data.Text as Text
import Data.Time (addDays, utctDay)
import Data.Time.Clock (UTCTime (..))
import IHP.ModelSupport (trackTableRead)
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
        let searchFilter = normalizeSearchQuery (paramOrNothing "search")
        let currentPage = max 1 $ fromMaybe 1 (paramOrNothing @Int "page")
        let marketsPerPage = 12
        let visibleMarkets = currentPage * marketsPerPage

        when (isJust searchFilter) do
            trackTableRead "assets"

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
                pure $ Just (map fromOnly rows)
            Nothing -> pure Nothing

        let applySearchFilter queryBuilder =
                case matchingMarketIds of
                    Just marketIds -> queryBuilder |> filterWhereIn (#id, marketIds)
                    Nothing -> queryBuilder

        let applyStatusFilter queryBuilder =
                queryBuilder
                    |> filterWhereNot (#status, MarketStatusDraft)

        let applyRecentActivityFilter queryBuilder =
                queryBuilder
                    |> queryOr
                        (filterWhere (#status, MarketStatusOpen))
                        (filterWhereSql (#updatedAt, ">= CURRENT_DATE - INTERVAL '10 days'"))

        (totalMarkets, markets') <- case matchingMarketIds of
            Just [] -> pure (0, [])
            _ -> do
                let filteredMarketsQuery =
                        query @Market
                            |> applyRecentActivityFilter
                            |> applyCategoryFilter
                            |> applySearchFilter
                            |> applyStatusFilter

                totalMarkets <- filteredMarketsQuery |> fetchCount
                markets' <- filteredMarketsQuery
                    |> orderByDesc #openedAt
                    |> limit visibleMarkets
                    |> fetch
                    >>= collectionFetchRelated #categoryId
                    >>= collectionFetchRelated #assets
                pure (totalMarkets, markets')

        categories <- fetchCategories
        let markets = map (\m -> m |> set #assets (sortAssetsForDisplay (get #assets m))) markets'
        let hasMoreMarkets = length markets < totalMarkets
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

    action ShowMarketAction { marketId, tradingAssetId, tradingAction, showChart, showDescription, showAllAssets, showTradeHistory, activityPage, chatPage, chatComposerRev, tradeQuantity, backTo } = autoRefresh do
        ensureIsUser
        let mId = if marketId == def then param @(Id Market) "marketId" else marketId
        let tAssetId = tradingAssetId <|> paramOrNothing @(Id Asset) "tradingAssetId"
        let tAction = sanitizeTradingAction (tradingAction <|> paramOrNothing @Text "tradingAction")
        let chartVisible = fromMaybe True (showChart <|> readQueryFlag "showChart")
        let descriptionVisible = fromMaybe False (showDescription <|> readQueryFlag "showDescription")
        let allAssetsVisible = fromMaybe False (showAllAssets <|> readQueryFlag "showAllAssets")
        let tradeHistoryVisible = fromMaybe False (showTradeHistory <|> readQueryFlag "showTradeHistory")
        let requestedActivityPage = max 1 (fromMaybe 1 (activityPage <|> paramOrNothing @Int "activityPage"))
        let requestedChatPage = max 1 (fromMaybe 1 (chatPage <|> paramOrNothing @Int "chatPage"))
        let currentChatComposerRev = normalizeOptionalTextParam (chatComposerRev <|> paramOrNothing @Text "chatComposerRev")
        let currentTradeQuantity = sanitizeTradeQuantity (tradeQuantity <|> paramOrNothing @Int "tradeQuantity")
        let backToPath = sanitizeBackTo (backTo <|> paramOrNothing @Text "backTo")

        market :: Market <- fetch mId
        category <- fetch (market.categoryId)
        assets <- query @Asset
            |> filterWhere (#marketId, mId)
            |> orderByAsc #quantity
            |> fetch
        let sortedAssets = sortAssetsForDisplay assets

        chartData <- fetchChartData market assets market.beta

        let activityItemsPerPage = 10
        activityTransactionsCount <- query @Transaction
            |> filterWhere (#marketId, mId)
            |> fetchCount

        let activityTotalPages = max 1 ((activityTransactionsCount + activityItemsPerPage - 1) `div` activityItemsPerPage)
        let currentActivityPage = min requestedActivityPage activityTotalPages
        let activityOffset = (currentActivityPage - 1) * activityItemsPerPage

        activityTransactions <- query @Transaction
            |> filterWhere (#marketId, mId)
            |> orderByDesc #createdAt
            |> limit activityItemsPerPage
            |> offset activityOffset
            |> fetch
            >>= collectionFetchRelated #assetId
            >>= collectionFetchRelated #userId

        let chatItemsPerPage = 20
        chatMessagesCount <- query @MarketChatMessage
            |> filterWhere (#marketId, mId)
            |> fetchCount

        let chatTotalPages = max 1 ((chatMessagesCount + chatItemsPerPage - 1) `div` chatItemsPerPage)
        let currentChatPage = min requestedChatPage chatTotalPages
        let visibleChatMessages = currentChatPage * chatItemsPerPage

        chatMessages <- query @MarketChatMessage
            |> filterWhere (#marketId, mId)
            |> orderByDesc #createdAt
            |> limit visibleChatMessages
            |> fetch
            >>= collectionFetchRelated #userId

        let chatEntries = map (\message -> MarketChatEntry { message = message, author = get #userId message }) chatMessages

        let hasOlderChatMessages = length chatMessages < chatMessagesCount

        render ShowView
            { market
            , category
            , assets = sortedAssets
            , tradingAssetId = tAssetId
            , tradingAction = tAction
            , showChart = chartVisible
            , showDescription = descriptionVisible
            , showAllAssets = allAssetsVisible
            , showTradeHistory = tradeHistoryVisible
            , activityTransactions
            , activityCurrentPage = currentActivityPage
            , activityTotalPages
            , chatMessages = chatEntries
            , chatCurrentPage = currentChatPage
            , hasOlderChatMessages
            , chatComposerRev = currentChatComposerRev
            , tradeQuantity = currentTradeQuantity
            , backTo = backToPath
            , chartData
            }

    action CreateMarketChatMessageAction { marketId } = do
        ensureIsUser
        let mId = if marketId == def then param @(Id Market) "marketId" else marketId
        _market :: Market <- fetch mId

        let tAssetId = paramOrNothing @(Id Asset) "tradingAssetId"
        let tAction = sanitizeTradingAction (paramOrNothing @Text "tradingAction")
        let chartVisible = fromMaybe True (readQueryFlag "showChart")
        let descriptionVisible = fromMaybe False (readQueryFlag "showDescription")
        let allAssetsVisible = fromMaybe False (readQueryFlag "showAllAssets")
        let tradeHistoryVisible = fromMaybe False (readQueryFlag "showTradeHistory")
        let currentActivityPage = max 1 (fromMaybe 1 (paramOrNothing @Int "activityPage"))
        let currentChatPage = max 1 (fromMaybe 1 (paramOrNothing @Int "chatPage"))
        let currentChatComposerRev = normalizeOptionalTextParam (paramOrNothing @Text "chatComposerRev")
        let currentTradeQuantity = sanitizeTradeQuantity (paramOrNothing @Int "tradeQuantity")
        let backToPath = sanitizeBackTo (paramOrNothing @Text "backTo")
        let messageBody = normalizeChatMessageBody (param @Text "body")
        let redirectToShowMarket chatComposerRevision = redirectTo ShowMarketAction
                { marketId = mId
                , tradingAssetId = tAssetId
                , tradingAction = tAction
                , showChart = Just chartVisible
                , showDescription = Just descriptionVisible
                , showAllAssets = Just allAssetsVisible
                , showTradeHistory = Just tradeHistoryVisible
                , activityPage = normalizePageParam currentActivityPage
                , chatPage = normalizePageParam currentChatPage
                , chatComposerRev = chatComposerRevision
                , tradeQuantity = currentTradeQuantity
                , backTo = backToPath
                }

        case validateChatMessageBody messageBody of
            Just errorMessage -> do
                setErrorMessage errorMessage
                redirectToShowMarket currentChatComposerRev
            Nothing -> do
                message <- newRecord @MarketChatMessage
                    |> set #marketId mId
                    |> set #userId currentUserId
                    |> set #body messageBody
                    |> createRecord

                redirectToShowMarket (Just (cs (show message.id)))

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

normalizeSearchQuery :: Maybe Text -> Maybe Text
normalizeSearchQuery (Just query)
    | strip query /= "" = Just (strip query)
normalizeSearchQuery _ = Nothing

normalizePageParam :: Int -> Maybe Int
normalizePageParam pageNum
    | pageNum > 1 = Just pageNum
    | otherwise = Nothing

normalizeOptionalTextParam :: Maybe Text -> Maybe Text
normalizeOptionalTextParam = \case
    Just value | strip value /= "" -> Just (strip value)
    _ -> Nothing

sanitizeTradeQuantity :: Maybe Int -> Maybe Int
sanitizeTradeQuantity = \case
    Just quantity | quantity >= 0 -> Just quantity
    _ -> Nothing

sanitizeTradingAction :: Maybe Text -> Maybe Text
sanitizeTradingAction = \case
    Just "buy" -> Just "buy"
    Just "sell" -> Just "sell"
    _ -> Nothing

normalizeChatMessageBody :: Text -> Text
normalizeChatMessageBody = strip

validateChatMessageBody :: Text -> Maybe Text
validateChatMessageBody body
    | body == "" = Just "Please enter a message"
    | Text.any (\char -> char == '\n' || char == '\r') body = Just "Message must be a single line"
    | Text.length body > 500 = Just "Message must be at most 500 characters"
    | otherwise = Nothing

readQueryFlag :: (?context :: ControllerContext, ?request :: Request) => Text -> Maybe Bool
readQueryFlag name =
    case Text.toLower <$> paramOrNothing @Text (cs name) of
        Just "true"  -> Just True
        Just "1"     -> Just True
        Just "false" -> Just False
        Just "0"     -> Just False
        _            -> Nothing
