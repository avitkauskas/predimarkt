{-# LANGUAGE QuasiQuotes #-}
module Web.Controller.Dashboard where

import Application.Domain.LMSR as LMSR
import Application.Domain.Position
import Application.Domain.Types
import qualified Application.Helper.Pagination as Pagination
import Application.Helper.QueryParams (normalizeSearchQuery)
import qualified Data.Map as M
import IHP.ModelSupport (trackTableRead)
import IHP.TypedSql (sqlQueryTyped, typedSql)
import Web.Controller.Prelude
import Web.View.Dashboard.DeleteMarket
import Web.View.Dashboard.Markets
import Web.View.Dashboard.OpenMarket
import Web.View.Dashboard.Positions
import Web.View.Dashboard.Transactions

instance Controller DashboardController where
    beforeAction = ensureIsUser

    action DashboardPositionsAction { page, searchFilter, positionStatusFilter } = autoRefresh do
        let currentPage = fromMaybe 1 (page <|> paramOrNothing @Int "page")
        let searchQuery = normalizeSearchQuery (searchFilter <|> paramOrNothing @Text "search")
        let mStatusText :: Maybe Text = positionStatusFilter <|> paramOrNothing @Text "statusFilter"
        let activeStatus = case mStatusText of
                Just "active"   -> Just MarketStatusOpen
                Just "closed"   -> Just MarketStatusClosed
                Just "resolved" -> Just MarketStatusResolved
                Just "refunded" -> Just MarketStatusRefunded
                _               -> Nothing
        let itemsPerPage = 5

        trackTableRead "positions"
        when (isJust searchQuery) $ do
            trackTableRead "markets"
            trackTableRead "assets"

        -- Get total count for pagination (with search and status filters if provided)
        totalCount <- case (searchQuery, activeStatus) of
            (Just query, mStatus) -> do
                let searchPattern = "%" <> query <> "%"
                case mStatus of
                    Just st ->
                        typedCountScalar <$> sqlQueryTyped [typedSql|
                            SELECT COUNT(*)
                            FROM positions p
                            JOIN markets m ON p.market_id = m.id
                            JOIN assets a ON p.asset_id = a.id
                            WHERE p.user_id = ${currentUserId}
                            AND (m.title ILIKE ${searchPattern} OR a.name ILIKE ${searchPattern})
                            AND m.status = ${st}
                        |]
                    Nothing ->
                        typedCountScalar <$> sqlQueryTyped [typedSql|
                            SELECT COUNT(*)
                            FROM positions p
                            JOIN markets m ON p.market_id = m.id
                            JOIN assets a ON p.asset_id = a.id
                            WHERE p.user_id = ${currentUserId}
                            AND (m.title ILIKE ${searchPattern} OR a.name ILIKE ${searchPattern})
                        |]
            (Nothing, mStatus) -> do
                case mStatus of
                    Just st ->
                        typedCountScalar <$> sqlQueryTyped [typedSql|
                            SELECT COUNT(*)
                            FROM positions p
                            JOIN markets m ON p.market_id = m.id
                            WHERE p.user_id = ${currentUserId}
                            AND m.status = ${st}
                        |]
                    Nothing -> query @Position |> filterWhere (#userId, currentUserId) |> fetchCount

        let pagination = Pagination.paginate currentPage itemsPerPage totalCount
        let sqlLimit = Pagination.paginationSqlLimit pagination
        let sqlOffset = Pagination.paginationSqlOffset pagination

        -- Use raw SQL with window function for efficient sorting:
        -- 1. Markets ordered by their most recently updated position (desc)
        -- 2. Within each market, positions ordered by updated_at (desc)
        -- 3. Search filter on market title and asset name (case-insensitive)
        -- 4. Optional status filter on market status
        -- Fetch positions with pagination using raw SQL
        let userId = currentUserId :: Id User
        positionsRaw <- case (searchQuery, activeStatus) of
            (Just query, Just st) -> do
                let searchPattern = "%" <> query <> "%"
                sqlQueryTyped [typedSql|
                    SELECT p.id, p.user_id, p.market_id, p.asset_id, p.quantity, p.invested, p.received, p.updated_at
                    FROM positions p
                    JOIN (
                        SELECT market_id, MAX(updated_at) AS market_max
                        FROM positions
                        WHERE user_id = ${userId}
                        GROUP BY market_id
                    ) market_sort ON market_sort.market_id = p.market_id
                    JOIN markets m ON p.market_id = m.id
                    JOIN assets a ON p.asset_id = a.id
                    WHERE p.user_id = ${userId}
                    AND (m.title ILIKE ${searchPattern} OR a.name ILIKE ${searchPattern})
                    AND m.status = ${st}
                    ORDER BY market_sort.market_max DESC, p.updated_at DESC
                    LIMIT ${sqlLimit} OFFSET ${sqlOffset}
                |]
            (Just query, Nothing) -> do
                let searchPattern = "%" <> query <> "%"
                sqlQueryTyped [typedSql|
                    SELECT p.id, p.user_id, p.market_id, p.asset_id, p.quantity, p.invested, p.received, p.updated_at
                    FROM positions p
                    JOIN (
                        SELECT market_id, MAX(updated_at) AS market_max
                        FROM positions
                        WHERE user_id = ${userId}
                        GROUP BY market_id
                    ) market_sort ON market_sort.market_id = p.market_id
                    JOIN markets m ON p.market_id = m.id
                    JOIN assets a ON p.asset_id = a.id
                    WHERE p.user_id = ${userId}
                    AND (m.title ILIKE ${searchPattern} OR a.name ILIKE ${searchPattern})
                    ORDER BY market_sort.market_max DESC, p.updated_at DESC
                    LIMIT ${sqlLimit} OFFSET ${sqlOffset}
                |]
            (Nothing, Just st) ->
                sqlQueryTyped [typedSql|
                    SELECT p.id, p.user_id, p.market_id, p.asset_id, p.quantity, p.invested, p.received, p.updated_at
                    FROM positions p
                    JOIN (
                        SELECT market_id, MAX(updated_at) AS market_max
                        FROM positions
                        WHERE user_id = ${userId}
                        GROUP BY market_id
                    ) market_sort ON market_sort.market_id = p.market_id
                    JOIN markets m ON p.market_id = m.id
                    WHERE p.user_id = ${userId}
                    AND m.status = ${st}
                    ORDER BY market_sort.market_max DESC, p.updated_at DESC
                    LIMIT ${sqlLimit} OFFSET ${sqlOffset}
                |]
            (Nothing, Nothing) ->
                sqlQueryTyped [typedSql|
                    SELECT p.id, p.user_id, p.market_id, p.asset_id, p.quantity, p.invested, p.received, p.updated_at
                    FROM positions p
                    JOIN (
                        SELECT market_id, MAX(updated_at) AS market_max
                        FROM positions
                        WHERE user_id = ${userId}
                        GROUP BY market_id
                    ) market_sort ON market_sort.market_id = p.market_id
                    WHERE p.user_id = ${userId}
                    ORDER BY market_sort.market_max DESC, p.updated_at DESC
                    LIMIT ${sqlLimit} OFFSET ${sqlOffset}
                |]
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

        totalPositionsValue <- fetchUserPositionsValue currentUserId
        let totalValue = wallet.amount + totalPositionsValue

        render PositionsView
            { positionsWithValue = positionsWithValue
            , currentPage = Pagination.paginationCurrentPage pagination
            , totalPages = Pagination.paginationTotalPages pagination
            , wallet = wallet
            , positionsValue = totalPositionsValue
            , totalValue = totalValue
            , searchFilter = searchQuery
            , statusFilter = mStatusText
            }

    action DashboardMarketsAction { statusFilter, page, searchFilter } = do
        let activeStatus = fromMaybe MarketStatusDraft $ statusFilter
                <|> paramOrNothing @MarketStatus "statusFilter"
        let currentPage = fromMaybe 1 (page <|> paramOrNothing @Int "page")
        let searchQuery = normalizeSearchQuery (searchFilter <|> paramOrNothing @Text "search")
        let itemsPerPage = 10

        totalCount <- case searchQuery of
            Just query -> do
                let searchPattern = "%" <> query <> "%"
                typedCountScalar <$> sqlQueryTyped [typedSql|
                    SELECT COUNT(*)
                    FROM markets
                    WHERE user_id = ${currentUserId}
                    AND status = ${activeStatus}
                    AND title ILIKE ${searchPattern}
                |]
            Nothing ->
                query @Market
                    |> filterWhere (#userId, Just currentUserId)
                    |> filterWhere (#status, activeStatus)
                    |> fetchCount

        let pagination = Pagination.paginate currentPage itemsPerPage totalCount
        let sqlLimit = Pagination.paginationSqlLimit pagination
        let sqlOffset = Pagination.paginationSqlOffset pagination

        markets <- case searchQuery of
            Just query -> do
                let searchPattern = "%" <> query <> "%"
                marketIds <- case activeStatus of
                    MarketStatusDraft -> sqlQueryTyped [typedSql|
                        SELECT id
                        FROM markets
                        WHERE user_id = ${currentUserId}
                            AND status = ${activeStatus}
                            AND title ILIKE ${searchPattern}
                        ORDER BY created_at DESC
                        LIMIT ${sqlLimit} OFFSET ${sqlOffset}
                    |]
                    MarketStatusOpen -> sqlQueryTyped [typedSql|
                        SELECT id
                        FROM markets
                        WHERE user_id = ${currentUserId}
                            AND status = ${activeStatus}
                            AND title ILIKE ${searchPattern}
                        ORDER BY opened_at DESC
                        LIMIT ${sqlLimit} OFFSET ${sqlOffset}
                    |]
                    MarketStatusClosed -> sqlQueryTyped [typedSql|
                        SELECT id
                        FROM markets
                        WHERE user_id = ${currentUserId}
                            AND status = ${activeStatus}
                            AND title ILIKE ${searchPattern}
                        ORDER BY closed_at DESC
                        LIMIT ${sqlLimit} OFFSET ${sqlOffset}
                    |]
                    MarketStatusResolved -> sqlQueryTyped [typedSql|
                        SELECT id
                        FROM markets
                        WHERE user_id = ${currentUserId}
                            AND status = ${activeStatus}
                            AND title ILIKE ${searchPattern}
                        ORDER BY resolved_at DESC
                        LIMIT ${sqlLimit} OFFSET ${sqlOffset}
                    |]
                    MarketStatusRefunded -> sqlQueryTyped [typedSql|
                        SELECT id
                        FROM markets
                        WHERE user_id = ${currentUserId}
                            AND status = ${activeStatus}
                            AND title ILIKE ${searchPattern}
                        ORDER BY refunded_at DESC
                        LIMIT ${sqlLimit} OFFSET ${sqlOffset}
                    |]
                mapM fetch marketIds
            Nothing ->
                case activeStatus of
                    MarketStatusDraft ->
                        query @Market
                            |> filterWhere (#userId, Just currentUserId)
                            |> filterWhere (#status, activeStatus)
                            |> orderByDesc #createdAt
                            |> limit sqlLimit
                            |> offset sqlOffset
                            |> fetch
                    MarketStatusOpen ->
                        query @Market
                            |> filterWhere (#userId, Just currentUserId)
                            |> filterWhere (#status, activeStatus)
                            |> orderByDesc #openedAt
                            |> limit sqlLimit
                            |> offset sqlOffset
                            |> fetch
                    MarketStatusClosed ->
                        query @Market
                            |> filterWhere (#userId, Just currentUserId)
                            |> filterWhere (#status, activeStatus)
                            |> orderByDesc #closedAt
                            |> limit sqlLimit
                            |> offset sqlOffset
                            |> fetch
                    MarketStatusResolved ->
                        query @Market
                            |> filterWhere (#userId, Just currentUserId)
                            |> filterWhere (#status, activeStatus)
                            |> orderByDesc #resolvedAt
                            |> limit sqlLimit
                            |> offset sqlOffset
                            |> fetch
                    MarketStatusRefunded ->
                        query @Market
                            |> filterWhere (#userId, Just currentUserId)
                            |> filterWhere (#status, activeStatus)
                            |> orderByDesc #refundedAt
                            |> limit sqlLimit
                            |> offset sqlOffset
                            |> fetch

        render MarketsView
            { markets = markets
            , activeStatus = activeStatus
            , currentPage = Pagination.paginationCurrentPage pagination
            , totalPages = Pagination.paginationTotalPages pagination
            , searchFilter = searchQuery
            }

    action ConfirmDeleteMarketAction { confirmDeleteMarketId, page, searchFilter } = do
        let mId = if confirmDeleteMarketId == def then param @(Id Market) "marketId" else confirmDeleteMarketId
        let mPage = page <|> paramOrNothing @Int "page"
        let mSearchFilter = normalizeSearchQuery (searchFilter <|> paramOrNothing @Text "search")
        market <- fetch mId
        accessDeniedUnless (market.userId == Just currentUserId)
        accessDeniedUnless (market.status == MarketStatusDraft)
        setModal DeleteMarketView
            { market
            , page = mPage
            , searchFilter = mSearchFilter
            }
        jumpToAction DashboardMarketsAction
            { statusFilter = Just MarketStatusDraft
            , page = mPage
            , searchFilter = mSearchFilter
            }

    action ChangeMarketStatusAction { marketId, status, page, searchFilter } = do
        let st = fromMaybe (param @MarketStatus "status") status
        let mPage = page <|> paramOrNothing @Int "page"
        let mSearchFilter = searchFilter <|> paramOrNothing @Text "search"
        let newClosedAt :: Maybe UTCTime
            newClosedAt = paramOrNothing "closedAt"
        market <- fetch marketId
        accessDeniedUnless (market.userId == Just currentUserId)

        when (st == MarketStatusOpen && market.status `notElem` [MarketStatusDraft, MarketStatusClosed]) $ do
            accessDeniedUnless False

        now <- getCurrentTime
        let marketWithClosedAt = maybe market (\closedAt -> market |> set #closedAt closedAt) newClosedAt

        if st == MarketStatusOpen && marketWithClosedAt.closedAt <= now
            then do
                let modalMarket = case newClosedAt of
                        Just _ ->
                            marketWithClosedAt
                                |> validateField #closedAt
                                    (const $ Failure "Closing time must be in the future.")
                        Nothing -> marketWithClosedAt
                setModal OpenMarketView
                    { market = modalMarket
                    , page = mPage
                    , searchFilter = mSearchFilter
                    }
                jumpToAction $ DashboardMarketsAction { statusFilter = Just market.status, page = mPage, searchFilter = mSearchFilter }
            else do
                let marketWithStatus = marketWithClosedAt |> set #status st

                let marketWithTimestamps = case st of
                        MarketStatusOpen -> marketWithStatus |> set #openedAt (market.openedAt <|> Just now)
                        MarketStatusResolved -> marketWithStatus |> set #resolvedAt (Just now)
                        MarketStatusRefunded -> marketWithStatus |> set #refundedAt (Just now)
                        MarketStatusClosed -> marketWithStatus |> set #closedAt now
                        _ -> marketWithStatus

                updatedMarket <- withTransaction do
                    updatedMarket <- marketWithTimestamps |> updateRecord
                    syncCloseMarketJob updatedMarket
                    pure updatedMarket

                let message = case st of
                        MarketStatusOpen     -> "Market opened successfully"
                        MarketStatusClosed   -> "Market closed successfully"
                        MarketStatusResolved -> "Market resolved successfully"
                        MarketStatusRefunded -> "Market refunded successfully"
                        _                    -> "Market status updated"

                setSuccessMessage message
                redirectTo $ DashboardMarketsAction { statusFilter = Just st, page = Nothing, searchFilter = mSearchFilter }

    action DashboardTransactionsAction { page, searchFilter, typeFilter } = do
        let currentPage = fromMaybe 1 (page <|> paramOrNothing @Int "page")
        let searchQuery = normalizeSearchQuery (searchFilter <|> paramOrNothing @Text "search")
        let mTypeFilter :: Maybe Text = typeFilter <|> paramOrNothing @Text "typeFilter"
        let itemsPerPage = 5

        -- Get total count for pagination (with search and type filters if provided)
        totalCount <- case (searchQuery, mTypeFilter) of
            (Just query, mType) -> do
                let searchPattern = "%" <> query <> "%"
                case mType of
                    Just "buy" ->
                        typedCountScalar <$> sqlQueryTyped [typedSql|
                            SELECT COUNT(*)
                            FROM transactions t
                            JOIN markets m ON t.market_id = m.id
                            JOIN assets a ON t.asset_id = a.id
                            WHERE t.user_id = ${currentUserId}
                                AND (m.title ILIKE ${searchPattern} OR a.name ILIKE ${searchPattern})
                                AND t.quantity > 0
                        |]
                    Just "sell" ->
                        typedCountScalar <$> sqlQueryTyped [typedSql|
                            SELECT COUNT(*)
                            FROM transactions t
                            JOIN markets m ON t.market_id = m.id
                            JOIN assets a ON t.asset_id = a.id
                            WHERE t.user_id = ${currentUserId}
                                AND (m.title ILIKE ${searchPattern} OR a.name ILIKE ${searchPattern})
                                AND t.quantity < 0
                        |]
                    _ ->
                        typedCountScalar <$> sqlQueryTyped [typedSql|
                            SELECT COUNT(*)
                            FROM transactions t
                            JOIN markets m ON t.market_id = m.id
                            JOIN assets a ON t.asset_id = a.id
                            WHERE t.user_id = ${currentUserId}
                                AND (m.title ILIKE ${searchPattern} OR a.name ILIKE ${searchPattern})
                        |]
            (Nothing, mType) -> case mType of
                Just "buy" ->
                    query @Transaction
                        |> filterWhere (#userId, currentUserId)
                        |> filterWhereGreaterThan (#quantity, 0)
                        |> fetchCount
                Just "sell" ->
                    query @Transaction
                        |> filterWhere (#userId, currentUserId)
                        |> filterWhereLessThan (#quantity, 0)
                        |> fetchCount
                _ ->
                    query @Transaction
                        |> filterWhere (#userId, currentUserId)
                        |> fetchCount

        -- Fetch transactions with pagination
        -- For search, first get matching IDs then fetch full records (avoids JSONB decoding issues)
        let userId = currentUserId :: Id User
        let pagination = Pagination.paginate currentPage itemsPerPage totalCount
        let sqlLimit = Pagination.paginationSqlLimit pagination
        let sqlOffset = Pagination.paginationSqlOffset pagination

        transactions <- case (searchQuery, mTypeFilter) of
            (Just query, mType) -> do
                let searchPattern = "%" <> query <> "%"
                txnIds <- case mType of
                    Just "buy" -> sqlQueryTyped [typedSql|
                        SELECT t.id
                        FROM transactions t
                        JOIN markets m ON t.market_id = m.id
                        JOIN assets a ON t.asset_id = a.id
                        WHERE t.user_id = ${userId}
                            AND (m.title ILIKE ${searchPattern} OR a.name ILIKE ${searchPattern})
                            AND t.quantity > 0
                        ORDER BY t.created_at DESC
                        LIMIT ${sqlLimit} OFFSET ${sqlOffset}
                    |]
                    Just "sell" -> sqlQueryTyped [typedSql|
                        SELECT t.id
                        FROM transactions t
                        JOIN markets m ON t.market_id = m.id
                        JOIN assets a ON t.asset_id = a.id
                        WHERE t.user_id = ${userId}
                            AND (m.title ILIKE ${searchPattern} OR a.name ILIKE ${searchPattern})
                            AND t.quantity < 0
                        ORDER BY t.created_at DESC
                        LIMIT ${sqlLimit} OFFSET ${sqlOffset}
                    |]
                    _ ->
                        sqlQueryTyped [typedSql|
                            SELECT t.id
                            FROM transactions t
                            JOIN markets m ON t.market_id = m.id
                            JOIN assets a ON t.asset_id = a.id
                            WHERE t.user_id = ${userId}
                                AND (m.title ILIKE ${searchPattern} OR a.name ILIKE ${searchPattern})
                            ORDER BY t.created_at DESC
                            LIMIT ${sqlLimit} OFFSET ${sqlOffset}
                        |]
                txnRecords <- mapM fetch txnIds
                collectionFetchRelated #assetId txnRecords >>= collectionFetchRelated #marketId
            (Nothing, mType) -> case mType of
                Just "buy" -> do
                    query @Transaction
                        |> filterWhere (#userId, currentUserId)
                        |> filterWhereGreaterThan (#quantity, 0)
                        |> orderByDesc #createdAt
                        |> limit sqlLimit
                        |> offset sqlOffset
                        |> fetch
                        >>= collectionFetchRelated #assetId
                        >>= collectionFetchRelated #marketId
                Just "sell" -> do
                    query @Transaction
                        |> filterWhere (#userId, currentUserId)
                        |> filterWhereLessThan (#quantity, 0)
                        |> orderByDesc #createdAt
                        |> limit sqlLimit
                        |> offset sqlOffset
                        |> fetch
                        >>= collectionFetchRelated #assetId
                        >>= collectionFetchRelated #marketId
                _ ->
                    query @Transaction
                        |> filterWhere (#userId, currentUserId)
                        |> orderByDesc #createdAt
                        |> limit sqlLimit
                        |> offset sqlOffset
                        |> fetch
                        >>= collectionFetchRelated #assetId
                        >>= collectionFetchRelated #marketId

        let transactionsWithDetails = map (\t -> TransactionWithDetails { transaction = t }) transactions

        -- Fetch wallet for balance display
        wallet <- query @Wallet
            |> filterWhere (#userId, currentUserId)
            |> fetchOne

        totalPositionsValue <- fetchUserPositionsValue currentUserId
        let totalValue = wallet.amount + totalPositionsValue

        render TransactionsView
            { transactionsWithDetails = transactionsWithDetails
            , currentPage = Pagination.paginationCurrentPage pagination
            , totalPages = Pagination.paginationTotalPages pagination
            , wallet = wallet
            , positionsValue = totalPositionsValue
            , totalValue = totalValue
            , searchFilter = searchQuery
            , typeFilter = mTypeFilter
            }

typedCountScalar :: (Integral a, HasCallStack) => [a] -> Int
typedCountScalar result = case result of
    [value]      -> fromIntegral value
    []           -> error "typedCountScalar: Query returned no rows"
    _            -> error $ "typedCountScalar: Expected 1 row, got " <> tshow (length result)

fetchUserPositionsValue :: (?modelContext :: ModelContext) => Id User -> IO Integer
fetchUserPositionsValue userId = do
    userPositions <- query @Position
        |> filterWhere (#userId, userId)
        |> filterWhereNot (#quantity, 0)
        |> fetch
        >>= collectionFetchRelated #assetId
        >>= collectionFetchRelated #marketId

    let marketsById =
            M.fromList
                [ (market.id, Beta market.beta)
                | market <- map (get #marketId) userPositions
                ]

    let marketIds = M.keys marketsById
    marketAssets <- forM marketIds $ \marketId -> do
        assets <- query @Asset
            |> filterWhere (#marketId, marketId)
            |> fetch
        let assetMap = M.fromList [(asset.id, Quantity asset.quantity) | asset <- assets]
        pure (marketId, assetMap)

    let marketAssetMap = M.fromList marketAssets
    let marketContext =
            M.fromList
                [ (marketId, (beta, assetMap))
                | (marketId, beta) <- M.toList marketsById
                , Just assetMap <- [M.lookup marketId marketAssetMap]
                ]

    pure (calculatePositionsValue userPositions marketContext)
