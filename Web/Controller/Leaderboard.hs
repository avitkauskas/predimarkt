module Web.Controller.Leaderboard where

import Application.Domain.LMSR as LMSR
import Application.Domain.Position
import Application.Domain.Types
import qualified Data.Map as M
import Data.Ord (Down (..))
import Data.Time.Clock (NominalDiffTime, diffUTCTime, nominalDay)
import Web.Controller.Prelude
import Web.View.Leaderboard.Index

initialPortfolioValue :: Double
initialPortfolioValue = 1000

minimumPortfolioValue :: Double
minimumPortfolioValue = 0.0001

minimumArrDuration :: NominalDiffTime
minimumArrDuration = nominalDay

annualDisplayThreshold :: NominalDiffTime
annualDisplayThreshold = 30 * nominalDay

secondsPerYear :: Double
secondsPerYear = 365.25 * 24 * 60 * 60

stabilizationYears :: Double
stabilizationYears = 0.5

yearsSinceRegistration :: UTCTime -> UTCTime -> Double
yearsSinceRegistration now registeredAt =
    let elapsed = max minimumArrDuration (diffUTCTime now registeredAt)
    in realToFrac elapsed / secondsPerYear

portfolioValue :: Integer -> Double
portfolioValue totalValue =
    max minimumPortfolioValue (fromIntegral totalValue / 100)

valueRatio :: Integer -> Double
valueRatio totalValue =
    portfolioValue totalValue / initialPortfolioValue

portfolioReturn :: Integer -> Double
portfolioReturn totalValue =
    valueRatio totalValue - 1

leaderboardScore :: UTCTime -> UTCTime -> Integer -> Double
leaderboardScore now registeredAt totalValue =
    let yearsRegistered = yearsSinceRegistration now registeredAt
    in log (valueRatio totalValue) / (yearsRegistered + stabilizationYears)

shouldDisplayAnnualReturn :: UTCTime -> UTCTime -> Bool
shouldDisplayAnnualReturn now registeredAt =
    diffUTCTime now registeredAt >= annualDisplayThreshold

annualRateOfReturn :: UTCTime -> UTCTime -> Integer -> Double
annualRateOfReturn now registeredAt totalValue =
    let yearsRegistered = yearsSinceRegistration now registeredAt
    in valueRatio totalValue ** (1 / yearsRegistered) - 1

instance Controller LeaderboardController where
    action LeaderboardAction = do
        now <- getCurrentTime
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

        let userSummariesWithRank =
                let summaries :: [UserSummary]
                    summaries =
                        flip map users $ \user ->
                            let cashAmount = fromMaybe 0 (M.lookup user.id walletAmountMap)
                                userPos = fromMaybe [] (M.lookup user.id userPositions)
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
                                yearsActive = yearsSinceRegistration now user.createdAt
                                returnValue = portfolioReturn totalValue
                                annualReturnValue = annualRateOfReturn now user.createdAt totalValue
                                showAnnualReturn = shouldDisplayAnnualReturn now user.createdAt
                                score = leaderboardScore now user.createdAt totalValue
                             in UserSummary
                                { nickname = user.nickname
                                , cash = cashAmount
                                , positionsValue = positionsValue
                                , totalValue = totalValue
                                , yearsActive = yearsActive
                                , totalReturn = returnValue
                                , annualReturn = annualReturnValue
                                , showAnnualReturn = showAnnualReturn
                                , score = score
                                , rank = 0
                                }
                    sorted = sortOn (\s -> (Down (get #score s), Down (get #totalValue s), get #nickname s)) summaries
                 in zipWith (\s r -> s { rank = r }) sorted [1 ..]

        let topCount = 20
        let topUsers = take topCount userSummariesWithRank

        let currentUserNickname :: Maybe Text = get #nickname <$> (currentUserOrNothing @User)
        let currentUserData :: Maybe UserSummary
            currentUserData = do
                nick <- currentUserNickname
                find (\s -> get #nickname s == nick) userSummariesWithRank

        let displayUsers :: [UserSummary]
            displayUsers =
                case currentUserData of
                    Nothing -> topUsers
                    Just user ->
                        let userRank = get #rank user
                         in if userRank <= topCount
                                then topUsers
                                else topUsers <> [user]

        let showOverflow = maybe False (\u -> get #rank u > topCount + 1) currentUserData

        render IndexView
            { displayUsers
            , currentUserNickname
            , showOverflow
            }
