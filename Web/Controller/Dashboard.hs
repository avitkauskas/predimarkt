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
                let statusClause = case mStatus of
                        Just st -> " AND m.status = ?"
                        Nothing -> ""
                let baseQuery = [r|
                    SELECT COUNT(*)::INTEGER
                    FROM positions p
                    JOIN markets m ON p.market_id = m.id
                    JOIN assets a ON p.asset_id = a.id
                    WHERE p.user_id = ?
                    AND (m.title ILIKE ? OR a.name ILIKE ?)
                |] <> statusClause
                count <- case mStatus of
                        Just st -> sqlQueryScalar baseQuery (currentUserId, "%" <> query <> "%", "%" <> query <> "%", st)
                        Nothing -> sqlQueryScalar baseQuery (currentUserId, "%" <> query <> "%", "%" <> query <> "%")
                pure count
            (Nothing, mStatus) -> do
                let statusClause = case mStatus of
                        Just st -> " AND m.status = ?"
                        Nothing -> ""
                let baseQuery = [r|
                    SELECT COUNT(*)::INTEGER
                    FROM positions p
                    JOIN markets m ON p.market_id = m.id
                    WHERE p.user_id = ?
                |] <> statusClause
                case mStatus of
                        Just st -> sqlQueryScalar baseQuery (currentUserId, st)
                        Nothing -> sqlQueryScalar baseQuery (Only currentUserId)

        let totalPages = max 1 ((totalCount + itemsPerPage - 1) `div` itemsPerPage)
        let validPage = max 1 (min currentPage totalPages)
        let pageOffset = (validPage - 1) * itemsPerPage

        -- Use raw SQL with window function for efficient sorting:
        -- 1. Markets ordered by their most recently updated position (desc)
        -- 2. Within each market, positions ordered by updated_at (desc)
        -- 3. Search filter on market title and asset name (case-insensitive)
        -- 4. Optional status filter on market status
        let statusClause = case activeStatus of
                Just st -> " AND m.status = ?"
                Nothing -> ""
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
                    WHERE (m.title ILIKE ? OR a.name ILIKE ?)
                |] <> statusClause <> [r|
                    ORDER BY market_max DESC, p.updated_at DESC
                    LIMIT ? OFFSET ?
                |]
                Nothing -> [r|
                    SELECT sub.id, sub.user_id, sub.market_id, sub.asset_id, sub.quantity, sub.invested, sub.received, sub.updated_at
                    FROM (
                        SELECT *, MAX(updated_at) OVER (PARTITION BY market_id) as market_max
                        FROM positions
                        WHERE user_id = ?
                    ) sub
                    JOIN markets m ON sub.market_id = m.id
                |] <> statusClause <> [r|
                    ORDER BY market_max DESC, sub.updated_at DESC
                    LIMIT ? OFFSET ?
                |]

        -- Fetch positions with pagination using raw SQL
        let userId = currentUserId :: Id User
        let posLimit = itemsPerPage :: Int
        let posOffset = pageOffset :: Int
        positionsRaw <- case (searchQuery, activeStatus) of
            (Just query, Just st) -> sqlQuery positionQuery (userId, "%" <> query <> "%", "%" <> query <> "%", st, posLimit, posOffset) :: IO [Position]
            (Just query, Nothing) -> sqlQuery positionQuery (userId, "%" <> query <> "%", "%" <> query <> "%", posLimit, posOffset) :: IO [Position]
            (Nothing, Just st) -> sqlQuery positionQuery (userId, st, posLimit, posOffset) :: IO [Position]
            (Nothing, Nothing) -> sqlQuery positionQuery (userId, posLimit, posOffset) :: IO [Position]
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
            , currentPage = validPage
            , totalPages = totalPages
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
        let mId = fromMaybe (param @(Id Market) "marketId") marketId
        let st = fromMaybe (param @MarketStatus "status") status
        let mPage = page <|> paramOrNothing @Int "page"
        let mSearchFilter = searchFilter <|> paramOrNothing @Text "search"
        let newClosedAt :: Maybe UTCTime
            newClosedAt = paramOrNothing "closedAt"
        market <- fetch mId
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
        let mTypeFilter :: Maybe Text = typeFilter <|> paramOrNothing @Text "type"
        let itemsPerPage = 5

        -- Get total count for pagination (with search and type filters if provided)
        totalCount <- case (searchQuery, mTypeFilter) of
            (Just query, mType) -> do
                let typeClause = case mType of
                        Just "buy"  -> " AND t.quantity > 0"
                        Just "sell" -> " AND t.quantity < 0"
                        Just _      -> ""
                        Nothing     -> ""
                let baseQuery = [r|
                    SELECT COUNT(*)::INTEGER
                    FROM transactions t
                    JOIN markets m ON t.market_id = m.id
                    JOIN assets a ON t.asset_id = a.id
                    WHERE t.user_id = ?
                    AND (m.title ILIKE ? OR a.name ILIKE ?)
                |] <> typeClause
                sqlQueryScalar baseQuery (currentUserId, "%" <> query <> "%", "%" <> query <> "%")
            (Nothing, mType) -> do
                let typeClause = case mType of
                        Just "buy"  -> " AND t.quantity > 0"
                        Just "sell" -> " AND t.quantity < 0"
                        Just _      -> ""
                        Nothing     -> ""
                let baseQuery = [r|
                    SELECT COUNT(*)::INTEGER
                    FROM transactions t
                    WHERE t.user_id = ?
                |] <> typeClause
                sqlQueryScalar baseQuery (Only currentUserId)

        let totalPages = max 1 ((totalCount + itemsPerPage - 1) `div` itemsPerPage)
        let validPage = max 1 (min currentPage totalPages)
        let pageOffset = (validPage - 1) * itemsPerPage

        -- Fetch transactions with pagination
        -- For search, first get matching IDs then fetch full records (avoids JSONB decoding issues)
        let userId = currentUserId :: Id User
        let txnLimit = itemsPerPage :: Int
        let txnOffset = pageOffset :: Int

        transactions <- case (searchQuery, mTypeFilter) of
            (Just query, mType) -> do
                let typeClauseInner = case mType of
                        Just "buy"  -> " AND t.quantity > 0"
                        Just "sell" -> " AND t.quantity < 0"
                        Just _      -> ""
                        Nothing     -> ""
                (txnIdRows :: [Only UUID]) <- sqlQuery
                    ([r|
                        SELECT t.id
                        FROM transactions t
                        JOIN markets m ON t.market_id = m.id
                        JOIN assets a ON t.asset_id = a.id
                        WHERE t.user_id = ?
                        AND (m.title ILIKE ? OR a.name ILIKE ?)
                    |] <> typeClauseInner <> [r|
                        ORDER BY t.created_at DESC
                        LIMIT ? OFFSET ?
                    |])
                    (userId, "%" <> query <> "%", "%" <> query <> "%", txnLimit, txnOffset)
                let txnIds = map (\(Only uuid) -> Id uuid :: Id Transaction) txnIdRows
                txnRecords <- mapM fetch txnIds
                collectionFetchRelated #assetId txnRecords >>= collectionFetchRelated #marketId
            (Nothing, mType) -> case mType of
                Just "buy" -> do
                    (txnIdRows :: [Only UUID]) <- sqlQuery
                        [r|
                            SELECT id FROM transactions
                            WHERE user_id = ? AND quantity > 0
                            ORDER BY created_at DESC
                            LIMIT ? OFFSET ?
                        |]
                        (currentUserId, txnLimit, txnOffset)
                    let txnIds = map (\(Only uuid) -> Id uuid :: Id Transaction) txnIdRows
                    txnRecords <- mapM fetch txnIds
                    collectionFetchRelated #assetId txnRecords >>= collectionFetchRelated #marketId
                Just "sell" -> do
                    (txnIdRows :: [Only UUID]) <- sqlQuery
                        [r|
                            SELECT id FROM transactions
                            WHERE user_id = ? AND quantity < 0
                            ORDER BY created_at DESC
                            LIMIT ? OFFSET ?
                        |]
                        (currentUserId, txnLimit, txnOffset)
                    let txnIds = map (\(Only uuid) -> Id uuid :: Id Transaction) txnIdRows
                    txnRecords <- mapM fetch txnIds
                    collectionFetchRelated #assetId txnRecords >>= collectionFetchRelated #marketId
                _ -> do
                    (txnIdRows :: [Only UUID]) <- sqlQuery
                        [r|
                            SELECT id FROM transactions
                            WHERE user_id = ?
                            ORDER BY created_at DESC
                            LIMIT ? OFFSET ?
                        |]
                        (currentUserId, txnLimit, txnOffset)
                    let txnIds = map (\(Only uuid) -> Id uuid :: Id Transaction) txnIdRows
                    txnRecords <- mapM fetch txnIds
                    collectionFetchRelated #assetId txnRecords >>= collectionFetchRelated #marketId

        let transactionsWithDetails = map (\t -> TransactionWithDetails { transaction = t }) transactions

        -- Fetch wallet for balance display
        wallet <- query @Wallet
            |> filterWhere (#userId, currentUserId)
            |> fetchOne

        totalPositionsValue <- fetchUserPositionsValue currentUserId
        let totalValue = wallet.amount + totalPositionsValue

        render TransactionsView
            { transactionsWithDetails = transactionsWithDetails
            , currentPage = validPage
            , totalPages = totalPages
            , wallet = wallet
            , positionsValue = totalPositionsValue
            , totalValue = totalValue
            , searchFilter = searchQuery
            , typeFilter = mTypeFilter
            }

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
