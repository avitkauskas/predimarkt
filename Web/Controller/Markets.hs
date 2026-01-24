{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use void" #-}
module Web.Controller.Markets where

import Web.Controller.Prelude
import Web.View.Markets.Edit
import Web.View.Markets.Index
import Web.View.Markets.New
import Web.View.Markets.Show

instance Controller MarketsController where
    action MarketsAction = do
        let categoryFilter = paramOrNothing "category"

        let applyCategoryFilter queryBuilder =
                case categoryFilter of
                    Just categoryId -> queryBuilder |> filterWhere (#categoryId, categoryId)
                    Nothing         -> queryBuilder

        let applyStatusFilter queryBuilder =
                queryBuilder |> filterWhereNot (#status, Draft)

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
                |> set #closedAt (UTCTime (addDays 7 (utctDay now)) 0)
                |> set #userId currentUserId
        let assets = [ newRecord @Asset |> set #name "Yes" |> set #label_ "Yes"
                     , newRecord @Asset |> set #name "No" |> set #label_ "No"
                     ]
        categories <- query @Category |> fetch
        render NewView { .. }

    action ShowMarketAction { marketId } = do
        market <- fetch marketId
        assets <- query @Asset |> filterWhere (#marketId, marketId) |> fetch
        render ShowView { .. }

    action EditMarketAction { marketId } = do
        market <- fetch marketId
        accessDeniedUnless (market.userId == currentUserId)
        assets <- query @Asset |> filterWhere (#marketId, marketId) |> fetch
        categories <- query @Category |> fetch
        render EditView { .. }

    action UpdateMarketAction { marketId } = do
        market <- fetch marketId
        accessDeniedUnless (market.userId == currentUserId)
        now <- getCurrentTime
        let market' = market |> buildMarket now
        market'
            |> set #slug (toSlug market'.title)
            |> ifValid \case
                Left market -> do
                    assets <- fetchAssetsFromParams
                    categories <- query @Category |> fetch
                    render EditView { .. }
                Right market -> do
                    market <- market |> updateRecord

                    assets <- fetchAssetsFromParams
                    forM_ assets \asset -> do
                        let asset' = asset |> set #marketId market.id
                        if get #id asset' == def
                            then createRecord asset' >> pure ()
                            else updateRecord asset' >> pure ()

                    setSuccessMessage "Market updated"
                    redirectTo MarketsAction

    action CreateMarketAction = do
        ensureIsUser
        now <- getCurrentTime
        let market = newRecord @Market
        let market' = market |> buildMarket now
        market'
            |> set #userId currentUserId
            |> set #slug (toSlug market'.title)
            |> ifValid \case
                Left market -> do
                    assets <- fetchAssetsFromParams
                    categories <- query @Category |> fetch
                    render NewView { .. }
                Right market -> do
                    market <- market |> createRecord

                    assets <- fetchAssetsFromParams
                    forM_ assets \asset -> do
                        asset
                            |> set #marketId market.id
                            |> createRecord

                    setSuccessMessage "Market created"
                    redirectTo MarketsAction

    action DeleteMarketAction { marketId } = do
        market <- fetch marketId
        accessDeniedUnless (market.userId == currentUserId)
        deleteRecord market
        setSuccessMessage "Market deleted"
        redirectTo MarketsAction

fetchAssetsFromParams :: (?context :: ControllerContext) => IO [Asset]
fetchAssetsFromParams = loop 0 []
    where
        loop :: (?context :: ControllerContext) => Int -> [Asset] -> IO [Asset]
        loop i acc = do
            let iStr = show i
            let name = paramOrNothing @Text (cs ("asset_name_" ++ iStr))
            case name of
                Just n -> do
                    let label = paramOrDefault "" (cs ("asset_label_" ++ iStr))
                    let assetId = paramOrNothing @(Id Asset) (cs ("asset_id_" ++ iStr))
                    let baseAsset = newRecord @Asset |> set #name n |> set #label_ label
                    let asset = case assetId of
                            Just uid | uid /= def -> baseAsset |> set #id uid
                            _                     -> baseAsset
                    loop (i + 1) (acc ++ [asset])
                Nothing -> pure acc

buildMarket now market = market
    |> fill @'["title", "description", "categoryId", "closedAt"]
    |> validateField #title nonEmpty
    |> validateField #description nonEmpty
    |> validateField #categoryId nonEmpty
    |> validateField #closedAt (isGreaterThan now)
