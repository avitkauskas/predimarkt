module Web.Controller.Dashboard where

import Application.Helper.LMSR
import qualified Data.Map as M
import Web.Controller.Prelude
import Web.Types.Money
import Web.View.Dashboard.Holdings
import Web.View.Dashboard.Markets
import Web.View.Dashboard.Wallets

instance Controller DashboardController where
    beforeAction = ensureIsUser

    action DashboardHoldingsAction = do
        holdings <- query @Holding
            |> filterWhere (#userId, currentUserId)
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
            let lmsrState = precompute market.beta assets
            return (mId, market, assets, lmsrState)

        let marketDataMap = M.fromList [(marketId, (market, lmsrState)) | (marketId, market, _, lmsrState) <- marketsWithAssets]

        -- Calculate current value for each holding
        holdingsWithValue <- forM holdings $ \holding -> do
            let mId = holding.marketId.id
            case M.lookup mId marketDataMap of
                Just (market, lmsrState) -> do
                    let asset = get #assetId holding
                        assetSum = sumItem asset.id lmsrState
                        assetTotal = sumTotal lmsrState
                        currentPrice = assetSum / assetTotal
                        qty = holding.quantity

                    let currentValue = case qty of
                            0 -> Nothing
                            q | q > 0 -> Just $ moneyFromDouble $
                                calculateSellRevenue q currentPrice market.beta assetTotal
                            q -> Just $ moneyFromDouble $
                                calculateBuyCost (abs q) currentPrice market.beta assetTotal

                    return HoldingWithValue { holding = holding, currentValue = currentValue }
                Nothing -> return HoldingWithValue { holding = holding, currentValue = Nothing }

        render HoldingsView { .. }

    action DashboardWalletsAction = do
        wallet <- query @Wallet
            |> filterWhere (#userId, currentUserId)
            |> fetchOne
        render WalletsView { .. }

    action DashboardMarketsAction { statusFilter } = do
        let activeStatus = fromMaybe MarketStatusDraft $ statusFilter <|> paramOrNothing @MarketStatus "statusFilter"
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
