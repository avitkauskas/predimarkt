module Web.Controller.Leaderboard where

import Application.Domain.LMSR as LMSR
import Application.Domain.Position
import Application.Domain.Types
import qualified Data.Map as M
import Web.Controller.Prelude
import Web.View.Leaderboard.Index

instance Controller LeaderboardController where
    action LeaderboardAction = do
        users <- query @User |> fetch

        walletAmounts <- query @Wallet |> fetch
        let walletAmountMap = M.fromList [(w.userId, w.amount) | w <- walletAmounts]

        positions <- query @Position
            |> filterWhereNot (#quantity, 0)
            |> fetch
            >>= collectionFetchRelated #assetId
            >>= collectionFetchRelated #marketId

        allAssets <- query @Asset |> fetch

        let assetsByMarket =
                M.fromListWith (<>) $
                    map (\a -> (a.marketId, [a])) allAssets

        let marketBetaMap =
                M.fromList $
                    map
                        ( \p ->
                            let market = get #marketId p
                             in (get #id market, get #beta market)
                        )
                        positions

        let marketAssetMap :: M.Map (Id Market) (Beta, M.Map (Id Asset) Quantity)
            marketAssetMap =
                M.fromList $
                    map
                        ( \(mId, assets) ->
                            let beta = Beta (fromMaybe 300 (M.lookup mId marketBetaMap))
                                qtyMap = M.fromList [(a.id, Quantity a.quantity) | a <- assets]
                             in (mId, (beta, qtyMap))
                        )
                        (M.toList assetsByMarket)

        let userPositions =
                M.fromListWith (<>) $
                    map (\p -> (p.userId, [p])) positions

        let userSummaries =
                flip map users $ \user ->
                    let userId = user.id
                        cashAmount = fromMaybe 0 (M.lookup userId walletAmountMap)
                        userPos = fromMaybe [] (M.lookup userId userPositions)
                        positionsValue =
                            sum $
                                map
                                    ( \position ->
                                        let asset = get #assetId position
                                            assetId = get #id asset
                                            market = get #marketId position
                                            mId = get #id market
                                            qty = get #quantity position
                                         in case M.lookup mId marketAssetMap of
                                                Just (beta, assetMap) ->
                                                    let Money v =
                                                            positionValue
                                                                assetId
                                                                (Quantity qty)
                                                                beta
                                                                assetMap
                                                     in v
                                                Nothing -> 0
                                    )
                                    userPos
                        totalValue = cashAmount + positionsValue
                     in UserSummary
                        { userId = userId
                        , nickname = user.nickname
                        , cash = cashAmount
                        , positionsValue = positionsValue
                        , totalValue = totalValue
                        }

        let rankedUsers =
                reverse $ sortOn (\s -> get #totalValue s) userSummaries

        render IndexView { rankedUsers }
