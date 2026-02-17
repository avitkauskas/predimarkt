module Web.Controller.Dashboard where

import qualified Data.Map as M
import qualified Domain.LMSR as LMSR
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
            let lmsrState = LMSR.precompute market.beta [(a.id, a.quantity) | a <- assets]
            return (mId, market, assets, lmsrState)

        let marketDataMap = M.fromList [
                      (marketId, (market, lmsrState))
                    | (marketId, market, _, lmsrState) <- marketsWithAssets ]

        -- Calculate current value for each position
        positionsWithValue <- forM positions $ \position -> do
            let mId = position.marketId.id
            case M.lookup mId marketDataMap of
                Just (market, lmsrState) -> do
                    let asset = get #assetId position
                        currentPrice = LMSR.price asset.id lmsrState
                        qty = position.quantity
                        absQty = abs qty

                    -- Derive side from quantity sign: positive = long, negative = short
                    let currentValue = if qty == 0
                            then Nothing
                            else if qty > 0  -- Long position
                                then Just $ LMSR.calculateSellRevenue absQty currentPrice market.beta
                                else Just $ LMSR.calculateBuyCost absQty currentPrice market.beta

                    let assetPrice = Just currentPrice

                    return PositionWithValue { .. }
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
