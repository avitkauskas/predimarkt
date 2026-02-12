module Web.Controller.Dashboard where

import qualified Data.Map as M
import qualified Domain.LMSR as LMSR
import Web.Controller.Prelude
import Web.View.Dashboard.Holdings
import Web.View.Dashboard.Markets
import Web.View.Dashboard.Transactions
import Web.View.Dashboard.Wallets

instance Controller DashboardController where
    beforeAction = ensureIsUser

    action DashboardHoldingsAction { page } = autoRefresh do
        let currentPage = fromMaybe 1 (page <|> paramOrNothing @Int "page")
        let itemsPerPage = 4

        -- Get total count for pagination
        totalCount <- query @Holding
            |> filterWhere (#userId, currentUserId)
            |> fetchCount

        let totalPages = max 1 ((totalCount + itemsPerPage - 1) `div` itemsPerPage)
        let validPage = max 1 (min currentPage totalPages)
        let pageOffset = (validPage - 1) * itemsPerPage

        holdings <- query @Holding
            |> filterWhere (#userId, currentUserId)
            |> orderByDesc #updatedAt
            |> limit itemsPerPage
            |> offset pageOffset
            |> fetch
            >>= collectionFetchRelated #assetId
            >>= collectionFetchRelated #marketId

        -- Get unique market IDs from holdings
        let marketIds = nub (map (\h -> h.marketId.id) holdings)

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

        -- Calculate current value for each holding
        holdingsWithValue <- forM holdings $ \holding -> do
            let mId = holding.marketId.id
            case M.lookup mId marketDataMap of
                Just (market, lmsrState) -> do
                    let asset = get #assetId holding
                        currentPrice = LMSR.price asset.id lmsrState
                        qty = holding.quantity

                    let currentValue = case (qty, holding.side) of
                            (0, _) -> Nothing
                            (q, Just "long") -> Just $ LMSR.calculateSellRevenue q currentPrice market.beta
                            (q, Just "short") -> Just $ LMSR.calculateBuyCost q currentPrice market.beta
                            (_, _) -> Nothing

                        assetPrice = Just currentPrice

                    return HoldingWithValue { .. }
                Nothing -> return HoldingWithValue
                    { holding = holding, currentValue = Nothing, assetPrice = Nothing }

        render HoldingsView
            { holdingsWithValue = holdingsWithValue
            , currentPage = validPage
            , totalPages = totalPages
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

        render TransactionsView
            { transactionsWithDetails = transactionsWithDetails
            , currentPage = validPage
            , totalPages = totalPages
            }
