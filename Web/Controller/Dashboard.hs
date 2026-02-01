module Web.Controller.Dashboard where

import Web.Controller.Prelude
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
                Just s -> s
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
