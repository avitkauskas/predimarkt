module Web.Job.CloseMarket where

import Data.Time.Clock (getCurrentTime)
import Web.Controller.Prelude

instance Job CloseMarketJob where
    perform CloseMarketJob { .. } = do
        market :: Market <- fetch marketId
        now <- getCurrentTime

        if market.status /= MarketStatusOpen
            then pure ()
            else if market.closedAt < now
                then do
                    market
                        |> set #status MarketStatusClosed
                        |> updateRecord
                    pure ()
                else do
                    existingJobs <- query @CloseMarketJob
                        |> filterWhere (#marketId, marketId)
                        |> fetch
                    deleteRecords existingJobs
                    newRecord @CloseMarketJob
                        |> set #marketId marketId
                        |> set #runAt market.closedAt
                        |> createRecord
                    pure ()
