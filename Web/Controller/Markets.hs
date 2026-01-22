module Web.Controller.Markets where

import Web.Controller.Prelude
import Web.View.Markets.Edit
import Web.View.Markets.Index
import Web.View.Markets.New
import Web.View.Markets.Show

instance Controller MarketsController where
    action MarketsAction = do
        (marketsQ, pagination) <- query @Market |> paginate
        markets <- marketsQ |> fetch
        markets <- collectionFetchRelated #categoryId markets
        render IndexView { .. }

    action NewMarketAction = do
        ensureIsUser
        now <- getCurrentTime
        let market = newRecord @Market
                |> set #closedAt (UTCTime (addDays 7 (utctDay now)) 0)
                |> set #userId currentUserId
        categories <- query @Category |> fetch
        render NewView { .. }

    action ShowMarketAction { marketId } = do
        market <- fetch marketId
        render ShowView { .. }

    action EditMarketAction { marketId } = do
        market <- fetch marketId
        accessDeniedUnless (market.userId == currentUserId)
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
                    categories <- query @Category |> fetch
                    render EditView { .. }
                Right market -> do
                    market <- market |> updateRecord
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
                    categories <- query @Category |> fetch
                    render NewView { .. }
                Right market -> do
                    market <- market |> createRecord
                    setSuccessMessage "Market created"
                    redirectTo MarketsAction

    action DeleteMarketAction { marketId } = do
        market <- fetch marketId
        accessDeniedUnless (market.userId == currentUserId)
        deleteRecord market
        setSuccessMessage "Market deleted"
        redirectTo MarketsAction

buildMarket now market = market
    |> fill @'["title", "description", "categoryId", "closedAt"]
    |> validateField #title nonEmpty
    |> validateField #description nonEmpty
    |> validateField #categoryId nonEmpty
    |> validateField #closedAt (isGreaterThan now)
