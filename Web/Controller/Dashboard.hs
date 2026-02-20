module Web.Controller.Dashboard where

import Application.Domain.LMSR as LMSR
import Application.Domain.Types
import qualified Data.Map as M
import Web.Controller.Prelude
import Web.View.Dashboard.Markets
import Web.View.Dashboard.Positions
import Web.View.Dashboard.Transactions
import Web.View.Dashboard.Wallets

instance Controller DashboardController where
    beforeAction = ensureIsUser

    action DashboardPositionsAction { page } = autoRefresh do
        let currentPage = fromMaybe 1 (page <|> paramOrNothing @Int "page")
        let itemsPerPage = 5

        -- Get total count for pagination
        totalCount <- query @Position
            |> filterWhere (#userId, currentUserId)
            |> fetchCount

        let totalPages = max 1 ((totalCount + itemsPerPage - 1) `div` itemsPerPage)
        let validPage = max 1 (min currentPage totalPages)
        let pageOffset = (validPage - 1) * itemsPerPage

        positions <- query @Position
            |> filterWhere (#userId, currentUserId)
            |> orderByDesc #updatedAt
            |> limit itemsPerPage
            |> offset pageOffset
            |> fetch
            >>= collectionFetchRelated #assetId
            >>= collectionFetchRelated #marketId

        -- Get unique market IDs from positions
        let marketIds = nub (map (\p -> p.marketId.id) positions)

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

        -- Calculate current value for each position
        positionsWithValue <- forM positions $ \position -> do
            let mId = position.marketId.id
                assetId = position.assetId.id
                qty = position.quantity

            case M.lookup mId marketDataMap of
                Just (market, (qtyMap, beta)) ->
                    let (currentValue, assetPrice) =
                            if market.status == MarketStatusResolved
                            then
                                if qty == 0
                                then (Nothing, Nothing)
                                else
                                    let isWinner = case market.outcomeAssetId of
                                            Just oid -> assetId == oid
                                            Nothing  -> False
                                        displayValue = if isWinner then abs qty * 100 else 0
                                        displayPrice = if isWinner then 1.0 else 0.0
                                    in (Just displayValue, Just displayPrice)
                            else if market.status == MarketStatusRefunded
                            then (Just 0, Just 0.0)
                            else
                                if qty == 0
                                then (Nothing, Nothing)
                                else if qty > 0
                                then
                                    let Money v = LMSR.tradeValue assetId (Quantity (-qty)) beta qtyMap
                                    in (Just v, Just (LMSR.assetPrice assetId beta qtyMap))
                                else
                                    let Money v = LMSR.tradeValue assetId (Quantity (abs qty)) beta qtyMap
                                    in (Just (-v), Just (LMSR.assetPrice assetId beta qtyMap))
                    in return PositionWithValue { .. }
                Nothing -> return PositionWithValue
                    { position = position, currentValue = Nothing, assetPrice = Nothing }

        -- Fetch wallet for balance display
        wallet <- query @Wallet
            |> filterWhere (#userId, currentUserId)
            |> fetchOne

        render PositionsView
            { positionsWithValue = positionsWithValue
            , currentPage = validPage
            , totalPages = totalPages
            , wallet = wallet
            }

    action DashboardWalletsAction = do
        wallet <- query @Wallet
            |> filterWhere (#userId, currentUserId)
            |> fetchOne
        render WalletsView { .. }

    action DashboardMarketsAction { statusFilter } = do
        let activeStatus = fromMaybe MarketStatusDraft $ statusFilter
                <|> paramOrNothing @MarketStatus "statusFilter"
        let applySorting queryBuilder =
                case activeStatus of
                    MarketStatusDraft -> queryBuilder |> orderByDesc #createdAt
                    MarketStatusOpen -> queryBuilder |> orderByDesc #openedAt
                    MarketStatusClosed -> queryBuilder |> orderByDesc #closedAt
                    MarketStatusResolved -> queryBuilder |> orderByDesc #resolvedAt
                    MarketStatusRefunded -> queryBuilder |> orderByDesc #refundedAt

        markets <- query @Market
            |> filterWhere (#userId, Just currentUserId)
            |> filterWhere (#status, activeStatus)
            |> applySorting
            |> fetch
        render MarketsView { .. }

    action ChangeMarketStatusAction { marketId, status } = do
        let mId = case marketId of
                Just id -> id
                Nothing -> param @(Id Market) "marketId"
        let st = case status of
                Just s  -> s
                Nothing -> param @MarketStatus "status"

        market <- fetch mId
        accessDeniedUnless (market.userId == Just currentUserId)
        now <- getCurrentTime

        let marketWithStatus = market |> set #status st

        let marketWithTimestamps = case st of
                MarketStatusOpen -> marketWithStatus |> set #openedAt (Just now)
                MarketStatusResolved -> marketWithStatus |> set #resolvedAt (Just now)
                MarketStatusRefunded -> marketWithStatus |> set #refundedAt (Just now)
                _ -> marketWithStatus

        marketWithTimestamps |> updateRecord

        setSuccessMessage "Market status updated"
        redirectTo $ DashboardMarketsAction { statusFilter = Just st }

    action DashboardTransactionsAction { page } = do
        let currentPage = fromMaybe 1 (page <|> paramOrNothing @Int "page")
        let itemsPerPage = 5

        -- Get total count for pagination
        totalCount <- query @Transaction
            |> filterWhere (#userId, currentUserId)
            |> fetchCount

        let totalPages = max 1 ((totalCount + itemsPerPage - 1) `div` itemsPerPage)
        let validPage = max 1 (min currentPage totalPages)
        let pageOffset = (validPage - 1) * itemsPerPage

        -- Fetch transactions with pagination using IHP's limit and offset
        transactions <- query @Transaction
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
            }
