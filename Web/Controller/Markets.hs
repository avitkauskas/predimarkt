module Web.Controller.Markets where

import Web.Controller.Prelude
import Web.View.Markets.Index
import Web.View.Markets.New
import Web.View.Markets.Edit
import Web.View.Markets.Show

instance Controller MarketsController where
    action MarketsAction = do
        (marketsQ, pagination) <- query @Market |> paginate
        markets <- marketsQ |> fetch
        render IndexView { .. }

    action NewMarketAction = do
        let market = newRecord
        render NewView { .. }

    action ShowMarketAction { marketId } = do
        market <- fetch marketId
        render ShowView { .. }

    action EditMarketAction { marketId } = do
        market <- fetch marketId
        render EditView { .. }

    action UpdateMarketAction { marketId } = do
        market <- fetch marketId
        market
            |> buildMarket
            |> ifValid \case
                Left market -> render EditView { .. }
                Right market -> do
                    market <- market |> updateRecord
                    setSuccessMessage "Market updated"
                    redirectTo EditMarketAction { .. }

    action CreateMarketAction = do
        let market = newRecord @Market
        market
            |> buildMarket
            |> ifValid \case
                Left market -> render NewView { .. } 
                Right market -> do
                    market <- market |> createRecord
                    setSuccessMessage "Market created"
                    redirectTo MarketsAction

    action DeleteMarketAction { marketId } = do
        market <- fetch marketId
        deleteRecord market
        setSuccessMessage "Market deleted"
        redirectTo MarketsAction

buildMarket market = market
    |> fill @'["userId", "title", "slug", "description", "categoryId", "beta", "status", "closedAt"]
