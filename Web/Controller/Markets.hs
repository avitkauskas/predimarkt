{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE QuasiQuotes    #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use void" #-}
module Web.Controller.Markets where

import Application.Domain.ChartData
import Application.Domain.MarketAssets (sortAssetsForDisplay)
import Application.Domain.Types
import qualified Application.Helper.QueryParams as QueryParams
import qualified Application.Market.Input as MarketInput
import Data.List (zipWith4)
import qualified Data.Map.Strict as M
import Data.Text (strip)
import qualified Data.Text as Text
import Data.Time (addDays, utctDay)
import Data.Time.Clock (UTCTime (..))
import qualified Generated.ActualTypes.Market as GeneratedMarket
import IHP.ModelSupport (trackTableRead)
import Text.RawString.QQ (r)
import Web.Controller.Prelude
import Web.Job.CloseMarket
import Web.Types
import Web.View.Layout (withFooterLayout)
import Web.View.Markets.Edit
import Web.View.Markets.Index
import Web.View.Markets.New
import Web.View.Markets.Refund
import Web.View.Markets.Resolve
import Web.View.Markets.Show

instance Controller MarketsController where
    action MarketsAction = autoRefresh do
        setLayout withFooterLayout
        let categoryFilter = paramOrNothing "category"
        let statusFilter = parseMarketIndexStatusFilter (paramOrNothing @Text "status")
        let searchFilter = QueryParams.normalizeSearchQuery (paramOrNothing "search")
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
                case statusFilter of
                    MarketIndexStatusOpenAndRecentOther ->
                        queryBuilder
                            |> filterWhereNot (#status, MarketStatusDraft)
                            |> queryOr
                                (filterWhere (#status, MarketStatusOpen))
                                (filterWhereSql (#updatedAt, ">= CURRENT_DATE - INTERVAL '10 days'"))
                    MarketIndexStatusAllClosed ->
                        queryBuilder |> filterWhere (#status, MarketStatusClosed)
                    MarketIndexStatusAllResolved ->
                        queryBuilder |> filterWhere (#status, MarketStatusResolved)
                    MarketIndexStatusAllRefunded ->
                        queryBuilder |> filterWhere (#status, MarketStatusRefunded)

        (totalMarkets, markets') <- case matchingMarketIds of
            Just [] -> pure (0, [])
            _ -> do
                let filteredMarketsQuery =
                        query @Market
                            |> applyStatusFilter
                            |> applyCategoryFilter
                            |> applySearchFilter

                totalMarkets <- filteredMarketsQuery |> fetchCount
                markets <- filteredMarketsQuery
                    |> orderByDesc #openedAt
                    |> limit visibleMarkets
                    |> fetch
                    >>= collectionFetchRelated #categoryId

                let marketIds = map (.id) markets

                assets <- query @Asset
                    |> filterWhereIn (#marketId, marketIds)
                    |> fetch

                let assetsByMarket =
                        M.fromListWith (<>)
                            (map (\asset -> (asset.marketId, [asset])) assets)

                let markets' =
                        map
                            (\market ->
                                attachAssetsToMarket market
                                    (fromMaybe [] (M.lookup market.id assetsByMarket))
                            )
                            markets

                pure (totalMarkets, markets')

        categories <- fetchCategories
        let markets = markets'
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
        -- ensureIsUser
        let mId = if marketId == def then param @(Id Market) "marketId" else marketId
        let tAssetId = tradingAssetId <|> paramOrNothing @(Id Asset) "tradingAssetId"
        let tAction = MarketInput.sanitizeTradingAction
                (tradingAction <|> paramOrNothing @Text "tradingAction")
        let chartVisible = fromMaybe True (showChart <|> readQueryFlag "showChart")
        let descriptionVisible = fromMaybe False (showDescription <|> readQueryFlag "showDescription")
        let allAssetsVisible = fromMaybe False (showAllAssets <|> readQueryFlag "showAllAssets")
        let tradeHistoryVisible = fromMaybe False (showTradeHistory <|> readQueryFlag "showTradeHistory")
        let requestedActivityPage = max 1 (fromMaybe 1 (activityPage <|> paramOrNothing @Int "activityPage"))
        let requestedChatPage = max 1 (fromMaybe 1 (chatPage <|> paramOrNothing @Int "chatPage"))
        let currentChatComposerRev = QueryParams.normalizeOptionalTextParam
                (chatComposerRev <|> paramOrNothing @Text "chatComposerRev")
        let currentTradeQuantity = MarketInput.sanitizeTradeQuantity
                (tradeQuantity <|> paramOrNothing @Int "tradeQuantity")
        let backToPath = sanitizeBackTo (backTo <|> paramOrNothing @Text "backTo")

        market :: Market <- fetch mId
        owner :: Maybe User <- fetchOneOrNothing market.userId
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
            , owner
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
        let tAction = MarketInput.sanitizeTradingAction
                (paramOrNothing @Text "tradingAction")
        let chartVisible = fromMaybe True (readQueryFlag "showChart")
        let descriptionVisible = fromMaybe False (readQueryFlag "showDescription")
        let allAssetsVisible = fromMaybe False (readQueryFlag "showAllAssets")
        let tradeHistoryVisible = fromMaybe False (readQueryFlag "showTradeHistory")
        let currentActivityPage = max 1 (fromMaybe 1 (paramOrNothing @Int "activityPage"))
        let currentChatPage = max 1 (fromMaybe 1 (paramOrNothing @Int "chatPage"))
        let currentChatComposerRev = QueryParams.normalizeOptionalTextParam
                (paramOrNothing @Text "chatComposerRev")
        let currentTradeQuantity = MarketInput.sanitizeTradeQuantity
                (paramOrNothing @Int "tradeQuantity")
        let backToPath = sanitizeBackTo (paramOrNothing @Text "backTo")
        let messageBody = MarketInput.normalizeChatMessageBody (param @Text "body")
        let redirectToShowMarket chatComposerRevision = redirectTo ShowMarketAction
                { marketId = mId
                , tradingAssetId = tAssetId
                , tradingAction = tAction
                , showChart = Just chartVisible
                , showDescription = Just descriptionVisible
                , showAllAssets = Just allAssetsVisible
                , showTradeHistory = Just tradeHistoryVisible
                , activityPage = QueryParams.normalizePageParam currentActivityPage
                , chatPage = QueryParams.normalizePageParam currentChatPage
                , chatComposerRev = chatComposerRevision
                , tradeQuantity = currentTradeQuantity
                , backTo = backToPath
                }

        case MarketInput.validateChatMessageBody messageBody of
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

    action DeleteMarketChatMessageAction { marketChatMessageId } = do
        ensureIsUser
        let mId = paramOrNothing @(Id Market) "marketId"
        let msgId = if marketChatMessageId == def then param @(Id MarketChatMessage) "marketChatMessageId" else marketChatMessageId
        let tAssetId = paramOrNothing @(Id Asset) "tradingAssetId"
        let tAction = MarketInput.sanitizeTradingAction
                (paramOrNothing @Text "tradingAction")
        let chartVisible = fromMaybe True (readQueryFlag "showChart")
        let descriptionVisible = fromMaybe False (readQueryFlag "showDescription")
        let allAssetsVisible = fromMaybe False (readQueryFlag "showAllAssets")
        let tradeHistoryVisible = fromMaybe False (readQueryFlag "showTradeHistory")
        let currentActivityPage = max 1 (fromMaybe 1 (paramOrNothing @Int "activityPage"))
        let currentChatPage = max 1 (fromMaybe 1 (paramOrNothing @Int "chatPage"))
        let currentChatComposerRev = QueryParams.normalizeOptionalTextParam
                (paramOrNothing @Text "chatComposerRev")
        let currentTradeQuantity = MarketInput.sanitizeTradeQuantity
                (paramOrNothing @Int "tradeQuantity")
        let backToPath = sanitizeBackTo (paramOrNothing @Text "backTo")

        message :: MarketChatMessage <- fetch msgId
        accessDeniedUnless (message.userId == currentUserId)

        deleteRecord message

        redirectTo ShowMarketAction
            { marketId = message.marketId
            , tradingAssetId = tAssetId
            , tradingAction = tAction
            , showChart = Just chartVisible
            , showDescription = Just descriptionVisible
            , showAllAssets = Just allAssetsVisible
            , showTradeHistory = Just tradeHistoryVisible
            , activityPage = QueryParams.normalizePageParam currentActivityPage
            , chatPage = QueryParams.normalizePageParam currentChatPage
            , chatComposerRev = currentChatComposerRev
            , tradeQuantity = currentTradeQuantity
            , backTo = backToPath
            }

    action EditMarketAction { marketId, page } = do
        let mId = if marketId == def then param @(Id Market) "marketId" else marketId
        let returnPage = page <|> paramOrNothing @Int "page"
        let searchFilter = paramOrNothing @Text "search"
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
        let returnPage = paramOrNothing @Int "returnPage"
        let searchFilter = paramOrNothing @Text "search"
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
            else case MarketInput.validateAssetSymbols assets of
                Just errorMsg -> do
                    setErrorMessage errorMsg
                    categories <- fetchCategories
                    render EditView { market = marketWithFormData, .. }
                Nothing -> case MarketInput.validateAssetNames assets of
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
                                render EditView { .. }
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

                                redirectToPath $ pathTo (DashboardMarketsAction { statusFilter = Just MarketStatusDraft, page = returnPage, searchFilter = searchFilter })

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
            else case MarketInput.validateAssetSymbols assets of
                Just errorMsg -> do
                    setErrorMessage errorMsg
                    categories <- fetchCategories
                    render NewView { market = marketWithFormData, .. }
                Nothing -> case MarketInput.validateAssetNames assets of
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
                                redirectTo $ DashboardMarketsAction { statusFilter = Just MarketStatusDraft, page = Nothing, searchFilter = Nothing }

    action DeleteMarketAction { marketId, page, searchFilter } = do
        let mId = if marketId == def then param @(Id Market) "marketId" else marketId
        let mSearchFilter = searchFilter <|> paramOrNothing @Text "search"
        market <- fetch mId
        accessDeniedUnless (market.userId == Just currentUserId)
        accessDeniedUnless (market.status == MarketStatusDraft)
        deleteRecord market
        setSuccessMessage "Market deleted"
        redirectTo $ DashboardMarketsAction { statusFilter = Just MarketStatusDraft, page = Nothing, searchFilter = mSearchFilter }

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

attachAssetsToMarket
    :: Include "categoryId" Market
    -> [Asset]
    -> Include' '["categoryId", "assets"] Market
attachAssetsToMarket market assets =
    market { GeneratedMarket.assets = sortAssetsForDisplay assets }

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

readQueryFlag :: (?context :: ControllerContext, ?request :: Request) => Text -> Maybe Bool
readQueryFlag name =
    QueryParams.parseBooleanText (paramOrNothing @Text (cs name))
