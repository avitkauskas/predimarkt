module Web.Controller.Dashboard where

import Application.Domain.LMSR as LMSR
import Application.Domain.Position
import Application.Domain.Types
import qualified Data.Map as M
import Web.Controller.Prelude
import Web.Job.CloseMarket
import Web.View.Dashboard.Markets
import Web.View.Dashboard.OpenMarket
import Web.View.Dashboard.Positions
import Web.View.Dashboard.Transactions

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

        let positionsWithValue = map enrichPosition' positions
              where
                enrichPosition' position =
                  let mId = position.marketId.id
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
            }

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
        let mId = fromMaybe (param @(Id Market) "marketId") marketId
        let st = fromMaybe (param @MarketStatus "status") status
        market <- fetch mId
        accessDeniedUnless (market.userId == Just currentUserId)

        when (st == MarketStatusOpen && market.status `notElem` [MarketStatusDraft, MarketStatusClosed]) $ do
            accessDeniedUnless False

        now <- getCurrentTime

        when (st == MarketStatusOpen && market.closedAt <= now) $ do
            setModal OpenMarketView { market }
            jumpToAction $ DashboardMarketsAction { statusFilter = Just market.status }

        let marketWithStatus = market |> set #status st

        let marketWithTimestamps = case st of
                MarketStatusOpen -> marketWithStatus |> set #openedAt (Just now)
                MarketStatusResolved -> marketWithStatus |> set #resolvedAt (Just now)
                MarketStatusRefunded -> marketWithStatus |> set #refundedAt (Just now)
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

        setSuccessMessage "Market status updated"
        redirectTo $ DashboardMarketsAction { statusFilter = Just st }

    action OpenMarketAction { marketId } = do
        let mId = fromMaybe (param @(Id Market) "marketId") marketId
        market <- fetch mId
        accessDeniedUnless (market.userId == Just currentUserId)

        let newClosedAt :: Maybe UTCTime = paramOrNothing "closedAt"
        now <- getCurrentTime

        case newClosedAt of
            Nothing -> do
                let marketWithError = market
                        |> validateField #closedAt (const $ Failure "Please provide a closing time.")
                setModal OpenMarketView { market = marketWithError }
                jumpToAction $ DashboardMarketsAction { statusFilter = Just market.status }
            Just closedAtVal ->
                if closedAtVal <= now
                    then do
                        let marketWithError = market
                                |> set #closedAt closedAtVal
                                |> validateField #closedAt (const $ Failure "Closing time must be in the future.")
                        setModal OpenMarketView { market = marketWithError }
                        jumpToAction $ DashboardMarketsAction { statusFilter = Just market.status }
                    else do
                        market
                            |> set #closedAt closedAtVal
                            |> set #status MarketStatusOpen
                            |> set #openedAt (Just now)
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
                        redirectTo $ DashboardMarketsAction { statusFilter = Just MarketStatusOpen }

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
