module Application.Domain.ChartData
    ( fetchChartData
    , buildChartData
    , PricePoint (..)
    , AssetChartData (..)
    ) where

import Application.Domain.LMSR
import Application.Domain.Types
import Application.Market.State (parseMarketState)
import qualified Data.List as List
import qualified Data.Map.Strict as M
import Data.Time (Day, addDays, utctDay)
import Data.Time.Clock (UTCTime (..), getCurrentTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Generated.Types
import IHP.Fetch
import IHP.ModelSupport
import IHP.Prelude
import IHP.QueryBuilder

data PricePoint = PricePoint
    { priceTime  :: Int
    , priceValue :: Double
    } deriving (Eq, Show)

data AssetChartData = AssetChartData
    { chartAssetId     :: Id Asset
    , chartAssetSymbol :: Text
    , chartAssetName   :: Text
    , chartAssetColor  :: Text
    , chartData        :: [PricePoint]
    } deriving (Eq, Show)

fetchChartData
    :: (?modelContext :: ModelContext)
    => Market
    -> [Asset]
    -> Integer
    -> IO [AssetChartData]
fetchChartData market assets beta = do
    now <- getCurrentTime
    let today = utctDay now
    let maxEndDay = min (utctDay market.closedAt) today
    let startDay = utctDay (fromMaybe market.createdAt market.openedAt)

    transactions <- query @Transaction
        |> filterWhere (#marketId, get #id market)
        |> orderByAsc #createdAt
        |> fetch

    pure $ buildChartData startDay maxEndDay assets beta transactions

buildChartData
    :: Day
    -> Day
    -> [Asset]
    -> Integer
    -> [Transaction]
    -> [AssetChartData]
buildChartData startDay maxEndDay assets beta transactions =
    let currentQtyMap = M.fromList [(get #id a, Quantity (get #quantity a)) | a <- assets]
        currentPrices = allAssetPrices (Beta beta) currentQtyMap
        topAssets = take 6 $ sortBy (\a1 a2 ->
            let p1 = fromMaybe 0.0 (M.lookup (get #id a1) currentPrices)
                p2 = fromMaybe 0.0 (M.lookup (get #id a2) currentPrices)
             in compare p2 p1) assets
        dayRange = generateDayRange startDay maxEndDay
    in if null transactions
        then map (buildFlatAssetChart assets dayRange currentPrices) topAssets
        else
            let txnDays = map (utctDay . get #createdAt) transactions
                chartStartDay = min startDay (minimum txnDays)
                filteredTxns = filter (\t -> utctDay (get #createdAt t) <= maxEndDay) transactions
                lastTxnPerDay = getLastTxnPerDay filteredTxns
                chartDays = generateDayRange chartStartDay maxEndDay
                pricesPerDay = computePricesFromLastTxn beta lastTxnPerDay
                initialPrices =
                    let firstTxn = List.head filteredTxns
                    in case parseMarketState (get #marketState firstTxn) of
                        Just qtyMap -> allAssetPrices (Beta beta) qtyMap
                        Nothing     -> currentPrices
                filledData = fillMissingDays chartDays pricesPerDay initialPrices
            in map (buildAssetChartData assets filledData) topAssets
  where
    getLastTxnPerDay :: [Transaction] -> M.Map Day Transaction
    getLastTxnPerDay txns =
        let sortedTxns = List.sortBy (\t1 t2 -> compare (get #createdAt t1) (get #createdAt t2)) txns
            dayTxnsList = List.groupBy (\t1 t2 -> utctDay (get #createdAt t1) == utctDay (get #createdAt t2)) sortedTxns
            lastPerDay = map List.last dayTxnsList
        in M.fromList [(utctDay (get #createdAt t), t) | t <- lastPerDay]

    generateDayRange :: Day -> Day -> [Day]
    generateDayRange start end
        | start > end = [end]
        | otherwise = takeWhile (<= end) (iterate (addDays 1) start)

    computePricesFromLastTxn
        :: Integer
        -> M.Map Day Transaction
        -> M.Map Day (M.Map (Id Asset) Double)
    computePricesFromLastTxn beta' lastTxns = M.map computeDayPrices lastTxns
      where
        computeDayPrices txn = case parseMarketState (get #marketState txn) of
            Just qtyMap -> allAssetPrices (Beta beta') qtyMap
            Nothing     -> M.empty

    buildFlatAssetChart
        :: [Asset]
        -> [Day]
        -> M.Map (Id Asset) Double
        -> Asset
        -> AssetChartData
    buildFlatAssetChart allAssets days prices asset =
        let assetId = get #id asset
            price = fromMaybe 0.0 (M.lookup assetId prices)
            points = map (\d -> PricePoint (dayToTimestamp d) price) days
        in AssetChartData
            { chartAssetId = assetId
            , chartAssetSymbol = get #symbol asset
            , chartAssetName = get #name asset
            , chartAssetColor = assetColorFor allAssets asset
            , chartData = points
            }

    fillMissingDays
        :: [Day]
        -> M.Map Day (M.Map (Id Asset) Double)
        -> M.Map (Id Asset) Double
        -> M.Map Day (M.Map (Id Asset) Double)
    fillMissingDays [] _ _ = M.empty
    fillMissingDays (day:days) pricesPerDay lastKnownPrices =
        let dayPrices = case M.lookup day pricesPerDay of
                Just p
                    | not (M.null p) -> p
                _ -> lastKnownPrices
            nextPrices = dayPrices
            rest = fillMissingDays days pricesPerDay nextPrices
        in M.insert day dayPrices rest

    buildAssetChartData
        :: [Asset]
        -> M.Map Day (M.Map (Id Asset) Double)
        -> Asset
        -> AssetChartData
    buildAssetChartData allAssets filledData asset =
        let assetId = get #id asset
            points = map (\day ->
                let price = case M.lookup day filledData of
                        Just dayPrices -> fromMaybe 0.0 (M.lookup assetId dayPrices)
                        Nothing -> 0.0
                in PricePoint (dayToTimestamp day) price
                ) (sort $ M.keys filledData)
        in AssetChartData
            { chartAssetId = assetId
            , chartAssetSymbol = get #symbol asset
            , chartAssetName = get #name asset
            , chartAssetColor = assetColorFor allAssets asset
            , chartData = points
            }

    dayToTimestamp :: Day -> Int
    dayToTimestamp day = floor (utcTimeToPOSIXSeconds (UTCTime day 0))

    assetColorFor :: [Asset] -> Asset -> Text
    assetColorFor allAssets asset =
        let colors = ["#2962FF", "#E91E63", "#4CAF50", "#FF9800", "#9C27B0", "#00BCD4"]
            idx = fromMaybe 0 $ List.findIndex (\a -> get #id a == get #id asset) allAssets
        in colors !! (idx `mod` length colors)
