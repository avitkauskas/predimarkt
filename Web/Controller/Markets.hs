{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use void" #-}
module Web.Controller.Markets where

import Web.Controller.Prelude
import Web.View.Markets.Edit
import Web.View.Markets.Index
import Web.View.Markets.New
import Web.View.Markets.Show

instance Controller MarketsController where
    action MarketsAction = autoRefresh do
        let categoryFilter = paramOrNothing "category"

        let applyCategoryFilter queryBuilder =
                case categoryFilter of
                    Just categoryId -> queryBuilder |> filterWhere (#categoryId, categoryId)
                    Nothing         -> queryBuilder

        let applyStatusFilter queryBuilder =
                queryBuilder |> filterWhereNot (#status, MarketStatusDraft)

        markets <-
            query @Market
                |> applyCategoryFilter
                |> applyStatusFilter
                |> fetch
                >>= collectionFetchRelated #categoryId

        categories <- query @Category |> fetch
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
        categories <- query @Category |> fetch
        render NewView { .. }

    action ShowMarketAction { marketId } = do
        market <- fetch marketId >>= fetchRelated #assets
        render ShowView { .. }

    action EditMarketAction { marketId } = do
        market <- fetch marketId
        accessDeniedUnless (market.userId == Just currentUserId)
        assets <- query @Asset |> filterWhere (#marketId, marketId) |> fetch
        categories <- query @Category |> fetch
        render EditView { .. }

    action UpdateMarketAction { marketId } = do
        market <- fetch marketId
        accessDeniedUnless (market.userId == Just currentUserId)
        now <- getCurrentTime
        assets <- fetchAssetsFromParams
        market
            |> buildMarket now
            |> ifValid \case
                Left market -> do
                    categories <- query @Category |> fetch
                    render EditView { .. }
                Right market -> do
                        uniqueSlug <- constructUniqueSlug 
                            market.categoryId (toSlug market.title) (Just marketId)
                        market <- market
                            |> set #slug uniqueSlug
                            |> updateRecord

                        forM_ assets \asset -> do
                            if asset.id == def
                                then asset |> set #marketId market.id |> createRecord
                                else asset |> set #marketId market.id |> updateRecord

                        setSuccessMessage "Market updated"
                        redirectTo DashboardMarketsAction

    action CreateMarketAction = do
        ensureIsUser
        now <- getCurrentTime
        assets <- fetchAssetsFromParams
        let market = newRecord @Market
        market
            |> buildMarket now
            |> set #userId (Just currentUserId)
            |> ifValid \case
                Left market -> do
                    categories <- query @Category |> fetch
                    render NewView { .. }
                Right market -> do
                    uniqueSlug <- constructUniqueSlug
                        market.categoryId (toSlug market.title) Nothing
                    market <- market
                        |> set #slug uniqueSlug
                        |> createRecord

                    forM_ assets \asset -> do
                        asset |> set #marketId market.id |> createRecord

                    setSuccessMessage "Market created"
                    redirectTo DashboardMarketsAction

    action DeleteMarketAction { marketId } = do
        market <- fetch marketId
        accessDeniedUnless (market.userId == Just currentUserId)
        deleteRecord market
        setSuccessMessage "Market deleted"
        redirectTo MarketsAction

fetchAssetsFromParams :: (?context :: ControllerContext) => IO [Asset]
fetchAssetsFromParams =
    pure $ zipWith3 (\assetId name symbol ->
        let asset = newRecord @Asset
                |> set #name name
                |> set #symbol symbol
        in if assetId == def
            then asset
            else asset |> set #id assetId)
        assetIds assetNames assetSymbols
    where
        assetIds = paramList "asset_id"
        assetNames = paramList "asset_name"
        assetSymbols = paramList "asset_symbol"

buildMarket now market = market
    |> fill @'["title", "description", "categoryId", "closedAt"]
    |> validateField #title nonEmpty
    |> validateField #description nonEmpty
    |> validateField #categoryId nonEmpty
    |> validateField #closedAt (isGreaterThan now)
