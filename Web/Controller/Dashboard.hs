{-# LANGUAGE QuasiQuotes #-}
module Web.Controller.Dashboard where

import Application.Domain.LMSR as LMSR
import Application.Domain.Position
import Application.Domain.Types
import Application.Helper.QueryParams (normalizeSearchQuery)
import qualified Data.Map as M
import IHP.ModelSupport (trackTableRead)
import Text.RawString.QQ (r)
import Web.Controller.Prelude
import Web.Job.CloseMarket
import Web.View.Dashboard.Markets
import Web.View.Dashboard.OpenMarket
import Web.View.Dashboard.Positions
import Web.View.Dashboard.Transactions

instance Controller DashboardController where
    beforeAction = ensureIsUser

    action DashboardPositionsAction { page, searchFilter } = autoRefresh do
        let currentPage = fromMaybe 1 (page <|> paramOrNothing @Int "page")
        let searchQuery = normalizeSearchQuery (searchFilter <|> paramOrNothing @Text "search")
        let itemsPerPage = 5

        trackTableRead "positions"
        when (isJust searchQuery) $ do
            trackTableRead "markets"
            trackTableRead "assets"

        -- Get total count for pagination (with search filter if provided)
        totalCount <- case searchQuery of
            Just query -> do
                -- Use sqlQueryScalar for efficient counting
                -- Search in both market title and asset name
                count <- sqlQueryScalar
                    [r|
                        SELECT COUNT(*)::INTEGER
                        FROM positions p
                        JOIN markets m ON p.market_id = m.id
                        JOIN assets a ON p.asset_id = a.id
                        WHERE p.user_id = ?
                        AND (m.title ILIKE ? OR a.name ILIKE ?)
                    |]
                    (currentUserId, "%" <> query <> "%", "%" <> query <> "%")
                pure count
            Nothing -> query @Position
                |> filterWhere (#userId, currentUserId)
                |> fetchCount

        let totalPages = max 1 ((totalCount + itemsPerPage - 1) `div` itemsPerPage)
        let validPage = max 1 (min currentPage totalPages)
        let pageOffset = (validPage - 1) * itemsPerPage

        -- Use raw SQL with window function for efficient sorting:
        -- 1. Markets ordered by their most recently updated position (desc)
        -- 2. Within each market, positions ordered by updated_at (desc)
        -- 3. Search filter on market title and asset name (case-insensitive)
        let positionQuery = case searchQuery of
                Just query -> [r|
                    SELECT p.id, p.user_id, p.market_id, p.asset_id, p.quantity, p.invested, p.received, p.updated_at
                    FROM (
                        SELECT *, MAX(updated_at) OVER (PARTITION BY market_id) as market_max
                        FROM positions
                        WHERE user_id = ?
                    ) p
                    JOIN markets m ON p.market_id = m.id
                    JOIN assets a ON p.asset_id = a.id
                    WHERE m.title ILIKE ? OR a.name ILIKE ?
                    ORDER BY market_max DESC, p.updated_at DESC
                    LIMIT ? OFFSET ?
                |]
                Nothing -> [r|
                    SELECT id, user_id, market_id, asset_id, quantity, invested, received, updated_at
                    FROM (
                        SELECT *, MAX(updated_at) OVER (PARTITION BY market_id) as market_max
                        FROM positions
                        WHERE user_id = ?
                    ) sub
                    ORDER BY market_max DESC, updated_at DESC
                    LIMIT ? OFFSET ?
                |]

        -- Fetch positions with pagination using raw SQL
        let userId = currentUserId :: Id User
        let posLimit = itemsPerPage :: Int
        let posOffset = pageOffset :: Int
        positionsRaw <- case searchQuery of
            Just query -> sqlQuery positionQuery (userId, "%" <> query <> "%", "%" <> query <> "%", posLimit, posOffset) :: IO [Position]
            Nothing -> sqlQuery positionQuery (userId, posLimit, posOffset) :: IO [Position]
        positions <- collectionFetchRelated #assetId positionsRaw >>= collectionFetchRelated #marketId

        -- Get unique market IDs from positions
        let marketIds = nub (map (\p -> get #id (get #marketId p)) positions)

        -- Fetch all markets with their assets
        marketsWithAssets <- forM marketIds $ \mId -> do
            market <- fetch mId
            assets <- query @Asset
                |> filterWhere (#marketId, mId)
                |> fetch
            let qtyMap = M.fromList [(a.id, Quantity a.quantity) | a <- assets]
            let beta = Beta market.beta
            return (mId, market, assets, (qtyMap, beta))

        let marketDataMap = M.fromList [
                      (marketId, (market, lmsrData))
                    | (marketId, market, _, lmsrData) <- marketsWithAssets ]

        let positionsWithValue = map enrichPosition' positions
              where
                enrichPosition' position =
                  let mId = get #id (get #marketId position)
                  in case M.lookup mId marketDataMap of
                       Just (market, (qtyMap, beta)) -> enrichPosition position market qtyMap beta
                       Nothing -> EnrichedPosition
                         { epPosition = position
                         , epCurrentValue = Nothing
                         , epAssetPrice = Nothing
                         }

        wallet <- query @Wallet
            |> filterWhere (#userId, currentUserId)
            |> fetchOne

        render PositionsView
            { positionsWithValue = positionsWithValue
            , currentPage = validPage
            , totalPages = totalPages
            , wallet = wallet
            , searchFilter = searchQuery
            }

    action DashboardMarketsAction { statusFilter, page, searchFilter } = do
        let activeStatus = fromMaybe MarketStatusDraft $ statusFilter
                <|> paramOrNothing @MarketStatus "statusFilter"
        let currentPage = fromMaybe 1 (page <|> paramOrNothing @Int "page")
        let searchQuery = normalizeSearchQuery (searchFilter <|> paramOrNothing @Text "search")
        let itemsPerPage = 10

        totalCount <- case searchQuery of
            Just query -> do
                count <- sqlQueryScalar
                    [r|
                        SELECT COUNT(*)::INTEGER
                        FROM markets
                        WHERE user_id = ?
                        AND status = ?
                        AND title ILIKE ?
                    |]
                    (currentUserId, activeStatus, "%" <> query <> "%")
                pure count
            Nothing ->
                case activeStatus of
                    MarketStatusDraft -> query @Market |> filterWhere (#userId, Just currentUserId) |> filterWhere (#status, activeStatus) |> fetchCount
                    MarketStatusOpen -> query @Market |> filterWhere (#userId, Just currentUserId) |> filterWhere (#status, activeStatus) |> fetchCount
                    MarketStatusClosed -> query @Market |> filterWhere (#userId, Just currentUserId) |> filterWhere (#status, activeStatus) |> fetchCount
                    MarketStatusResolved -> query @Market |> filterWhere (#userId, Just currentUserId) |> filterWhere (#status, activeStatus) |> fetchCount
                    MarketStatusRefunded -> query @Market |> filterWhere (#userId, Just currentUserId) |> filterWhere (#status, activeStatus) |> fetchCount

        let totalPages = max 1 ((totalCount + itemsPerPage - 1) `div` itemsPerPage)
        let validPage = max 1 (min currentPage totalPages)
        let pageOffset = (validPage - 1) * itemsPerPage

        markets <- case searchQuery of
            Just query -> do
                (marketIdRows :: [Only UUID]) <- case activeStatus of
                    MarketStatusDraft -> sqlQuery
                        [r|SELECT id FROM markets WHERE user_id = ? AND status = ? AND title ILIKE ? ORDER BY created_at DESC LIMIT ? OFFSET ?|]
                        (currentUserId, activeStatus, "%" <> query <> "%", itemsPerPage, pageOffset)
                    MarketStatusOpen -> sqlQuery
                        [r|SELECT id FROM markets WHERE user_id = ? AND status = ? AND title ILIKE ? ORDER BY opened_at DESC LIMIT ? OFFSET ?|]
                        (currentUserId, activeStatus, "%" <> query <> "%", itemsPerPage, pageOffset)
                    MarketStatusClosed -> sqlQuery
                        [r|SELECT id FROM markets WHERE user_id = ? AND status = ? AND title ILIKE ? ORDER BY closed_at DESC LIMIT ? OFFSET ?|]
                        (currentUserId, activeStatus, "%" <> query <> "%", itemsPerPage, pageOffset)
                    MarketStatusResolved -> sqlQuery
                        [r|SELECT id FROM markets WHERE user_id = ? AND status = ? AND title ILIKE ? ORDER BY resolved_at DESC LIMIT ? OFFSET ?|]
                        (currentUserId, activeStatus, "%" <> query <> "%", itemsPerPage, pageOffset)
                    MarketStatusRefunded -> sqlQuery
                        [r|SELECT id FROM markets WHERE user_id = ? AND status = ? AND title ILIKE ? ORDER BY refunded_at DESC LIMIT ? OFFSET ?|]
                        (currentUserId, activeStatus, "%" <> query <> "%", itemsPerPage, pageOffset)
                let marketIds = map (\(Only uuid) -> Id uuid :: Id Market) marketIdRows
                mapM fetch marketIds
            Nothing ->
                case activeStatus of
                    MarketStatusDraft -> query @Market |> filterWhere (#userId, Just currentUserId) |> filterWhere (#status, activeStatus) |> orderByDesc #createdAt |> limit itemsPerPage |> offset pageOffset |> fetch
                    MarketStatusOpen -> query @Market |> filterWhere (#userId, Just currentUserId) |> filterWhere (#status, activeStatus) |> orderByDesc #openedAt |> limit itemsPerPage |> offset pageOffset |> fetch
                    MarketStatusClosed -> query @Market |> filterWhere (#userId, Just currentUserId) |> filterWhere (#status, activeStatus) |> orderByDesc #closedAt |> limit itemsPerPage |> offset pageOffset |> fetch
                    MarketStatusResolved -> query @Market |> filterWhere (#userId, Just currentUserId) |> filterWhere (#status, activeStatus) |> orderByDesc #resolvedAt |> limit itemsPerPage |> offset pageOffset |> fetch
                    MarketStatusRefunded -> query @Market |> filterWhere (#userId, Just currentUserId) |> filterWhere (#status, activeStatus) |> orderByDesc #refundedAt |> limit itemsPerPage |> offset pageOffset |> fetch

        render MarketsView
            { markets = markets
            , activeStatus = activeStatus
            , currentPage = validPage
            , totalPages = totalPages
            , searchFilter = searchQuery
            }

    action ChangeMarketStatusAction { marketId, status, page, searchFilter } = do
        let mId = fromMaybe (param @(Id Market) "marketId") marketId
        let st = fromMaybe (param @MarketStatus "status") status
        let mPage = page <|> paramOrNothing @Int "page"
        let mSearchFilter = searchFilter <|> paramOrNothing @Text "search"
        market <- fetch mId
        accessDeniedUnless (market.userId == Just currentUserId)

        when (st == MarketStatusOpen && market.status `notElem` [MarketStatusDraft, MarketStatusClosed]) $ do
            accessDeniedUnless False

        now <- getCurrentTime

        when (st == MarketStatusOpen && market.closedAt <= now) $ do
            setModal OpenMarketView { market, page = mPage, searchFilter = mSearchFilter }
            jumpToAction $ DashboardMarketsAction { statusFilter = Just market.status, page = mPage, searchFilter = mSearchFilter }

        let marketWithStatus = market |> set #status st

        let marketWithTimestamps = case st of
                MarketStatusOpen -> marketWithStatus |> set #openedAt (market.openedAt <|> Just now)
                MarketStatusResolved -> marketWithStatus |> set #resolvedAt (Just now)
                MarketStatusRefunded -> marketWithStatus |> set #refundedAt (Just now)
                MarketStatusClosed -> marketWithStatus |> set #closedAt now
                _ -> marketWithStatus

        marketWithTimestamps |> updateRecord

        when (st == MarketStatusOpen) $ do
            existingJobs <- query @CloseMarketJob
                |> filterWhere (#marketId, market.id)
                |> fetch
            deleteRecords existingJobs
            _ <- newRecord @CloseMarketJob
                |> set #marketId market.id
                |> set #runAt market.closedAt
                |> createRecord
            pure ()

        let message = case st of
                MarketStatusOpen     -> "Market opened successfully"
                MarketStatusClosed   -> "Market closed successfully"
                MarketStatusResolved -> "Market resolved successfully"
                MarketStatusRefunded -> "Market refunded successfully"
                _                    -> "Market status updated"

        setSuccessMessage message
        redirectTo $ DashboardMarketsAction { statusFilter = Just st, page = Nothing, searchFilter = mSearchFilter }

    action OpenMarketAction { marketId, page, searchFilter } = do
        let mId = fromMaybe (param @(Id Market) "marketId") marketId
        let mPage = page <|> paramOrNothing @Int "page"
        let mSearchFilter = searchFilter <|> paramOrNothing @Text "search"
        market <- fetch mId
        accessDeniedUnless (market.userId == Just currentUserId)

        let newClosedAt :: Maybe UTCTime = paramOrNothing "closedAt"
        now <- getCurrentTime

        case newClosedAt of
            Nothing -> do
                let marketWithError = market
                        |> validateField #closedAt (const $ Failure "Please provide a closing time.")
                setModal OpenMarketView { market = marketWithError, page = mPage, searchFilter = mSearchFilter }
                jumpToAction $ DashboardMarketsAction { statusFilter = Just market.status, page = mPage, searchFilter = mSearchFilter }
            Just closedAtVal ->
                if closedAtVal <= now
                    then do
                        let marketWithError = market
                                |> set #closedAt closedAtVal
                                |> validateField #closedAt (const $ Failure "Closing time must be in the future.")
                        setModal OpenMarketView { market = marketWithError, page = mPage, searchFilter = mSearchFilter }
                        jumpToAction $ DashboardMarketsAction { statusFilter = Just market.status, page = mPage, searchFilter = mSearchFilter }
                    else do
                        market
                            |> set #closedAt closedAtVal
                            |> set #status MarketStatusOpen
                            |> set #openedAt (market.openedAt <|> Just now)
                            |> updateRecord

                        existingJobs <- query @CloseMarketJob
                            |> filterWhere (#marketId, market.id)
                            |> fetch
                        deleteRecords existingJobs
                        _ <- newRecord @CloseMarketJob
                            |> set #marketId market.id
                            |> set #runAt closedAtVal
                            |> createRecord
                        pure ()

                        setSuccessMessage "Market opened successfully"
                        redirectTo $ DashboardMarketsAction { statusFilter = Just MarketStatusOpen, page = Nothing, searchFilter = mSearchFilter }

    action DashboardTransactionsAction { page, searchFilter } = do
        let currentPage = fromMaybe 1 (page <|> paramOrNothing @Int "page")
        let searchQuery = normalizeSearchQuery (searchFilter <|> paramOrNothing @Text "search")
        let itemsPerPage = 5

        -- Get total count for pagination (with search filter if provided)
        totalCount <- case searchQuery of
            Just query -> do
                -- Use sqlQueryScalar for efficient counting
                -- Search in both market title and asset name
                count <- sqlQueryScalar
                    [r|
                        SELECT COUNT(*)::INTEGER
                        FROM transactions t
                        JOIN markets m ON t.market_id = m.id
                        JOIN assets a ON t.asset_id = a.id
                        WHERE t.user_id = ?
                        AND (m.title ILIKE ? OR a.name ILIKE ?)
                    |]
                    (currentUserId, "%" <> query <> "%", "%" <> query <> "%")
                pure count
            Nothing -> query @Transaction
                |> filterWhere (#userId, currentUserId)
                |> fetchCount

        let totalPages = max 1 ((totalCount + itemsPerPage - 1) `div` itemsPerPage)
        let validPage = max 1 (min currentPage totalPages)
        let pageOffset = (validPage - 1) * itemsPerPage

        -- Fetch transactions with pagination
        -- For search, first get matching IDs then fetch full records (avoids JSONB decoding issues)
        let userId = currentUserId :: Id User
        let txnLimit = itemsPerPage :: Int
        let txnOffset = pageOffset :: Int
        transactions <- case searchQuery of
            Just query -> do
                -- First get matching transaction IDs
                (txnIdRows :: [Only UUID]) <- sqlQuery
                    [r|
                        SELECT t.id
                        FROM transactions t
                        JOIN markets m ON t.market_id = m.id
                        JOIN assets a ON t.asset_id = a.id
                        WHERE t.user_id = ?
                        AND (m.title ILIKE ? OR a.name ILIKE ?)
                        ORDER BY t.created_at DESC
                        LIMIT ? OFFSET ?
                    |]
                    (userId, "%" <> query <> "%", "%" <> query <> "%", txnLimit, txnOffset)
                -- Fetch full records by IDs (N+1 acceptable for small page size)
                let txnIds = map (\(Only uuid) -> Id uuid :: Id Transaction) txnIdRows
                txnRecords <- mapM fetch txnIds
                collectionFetchRelated #assetId txnRecords >>= collectionFetchRelated #marketId
            Nothing ->
                query @Transaction
                    |> filterWhere (#userId, currentUserId)
                    |> orderByDesc #createdAt
                    |> limit itemsPerPage
                    |> offset pageOffset
                    |> fetch
                    >>= collectionFetchRelated #assetId
                    >>= collectionFetchRelated #marketId

        let transactionsWithDetails = map (\t -> TransactionWithDetails { transaction = t }) transactions

        -- Fetch wallet for balance display
        wallet <- query @Wallet
            |> filterWhere (#userId, currentUserId)
            |> fetchOne

        render TransactionsView
            { transactionsWithDetails = transactionsWithDetails
            , currentPage = validPage
            , totalPages = totalPages
            , wallet = wallet
            , searchFilter = searchQuery
            }
