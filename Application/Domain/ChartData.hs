module Application.Domain.ChartData
    ( fetchChartData
    , PricePoint (..)
    , AssetChartData (..)
    ) where

import Application.Domain.LMSR
import Application.Domain.Types
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as AesonTypes
import qualified Data.List as List
import qualified Data.Map.Strict as M
import Data.Time (Day, addDays, utctDay)
import Data.Time.Clock (UTCTime (..), getCurrentTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.UUID (UUID)
import qualified Data.UUID as UUID
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

    transactions <- query @Transaction
        |> filterWhere (#marketId, get #id market)
        |> orderByAsc #createdAt
        |> fetch

    let validTransactions = transactions

    let currentQtyMap = M.fromList [(get #id a, Quantity (get #quantity a)) | a <- assets]
    let currentPrices = allAssetPrices (Beta beta) currentQtyMap

    if null validTransactions
        then pure $ map (buildFlatAssetChart [] currentPrices) assets
        else do
            let txnDays = map (utctDay . get #createdAt) validTransactions
            let startDay = minimum txnDays
            let lastTxnDay = maximum txnDays
            let endDay = min maxEndDay (max lastTxnDay today)
            let filteredTxns = filter (\t -> utctDay (get #createdAt t) <= endDay) validTransactions
            let lastTxnPerDay = getLastTxnPerDay filteredTxns
            let dayRange = generateDayRange startDay endDay
            let pricesPerDay = computePricesFromLastTxn lastTxnPerDay assets beta
            let filledData = fillMissingDays dayRange pricesPerDay currentPrices

            pure $ map (buildAssetChartData filledData) assets
  where
    getLastTxnPerDay :: [Transaction] -> M.Map Day Transaction
    getLastTxnPerDay txns =
        let sortedTxns = List.sortBy (\t1 t2 -> compare (get #createdAt t1) (get #createdAt t2)) txns
            dayTxnsList = List.groupBy (\t1 t2 -> utctDay (get #createdAt t1) == utctDay (get #createdAt t2)) sortedTxns
            lastPerDay = map List.last dayTxnsList
        in M.fromList [(utctDay (get #createdAt t), t) | t <- lastPerDay]

    generateDayRange :: Day -> Day -> [Day]
    generateDayRange start end = takeWhile (<= end) (iterate (addDays 1) start)

    computePricesFromLastTxn
        :: M.Map Day Transaction
        -> [Asset]
        -> Integer
        -> M.Map Day (M.Map (Id Asset) Double)
    computePricesFromLastTxn lastTxns assets' beta' = M.map computeDayPrices lastTxns
      where
        computeDayPrices txn = case parseMarketState (get #marketState txn) of
            Just qtyMap -> allAssetPrices (Beta beta') qtyMap
            Nothing     -> M.empty

    parseMarketState :: Aeson.Value -> Maybe (M.Map (Id Asset) Quantity)
    parseMarketState value = case Aeson.fromJSON value of
        AesonTypes.Success (obj :: [(Text, Int)]) -> Just $ M.fromList
            [ (Id uid, Quantity (fromIntegral qty))
            | (key, qty) <- obj
            , Just uid <- [UUID.fromText key]
            ]
        AesonTypes.Error _ -> Nothing

    buildFlatAssetChart :: [Day] -> M.Map (Id Asset) Double -> Asset -> AssetChartData
    buildFlatAssetChart days prices asset =
        let assetId = get #id asset
            price = fromMaybe 0.0 (M.lookup assetId prices)
            points = map (\d -> PricePoint (dayToTimestamp d) price) days
        in AssetChartData
            { chartAssetId = assetId
            , chartAssetSymbol = get #symbol asset
            , chartAssetName = get #name asset
            , chartAssetColor = assetColorFor assets asset
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
        :: M.Map Day (M.Map (Id Asset) Double) -> Asset -> AssetChartData
    buildAssetChartData filledData asset =
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
            , chartAssetColor = assetColorFor assets asset
            , chartData = points
            }

    dayToTimestamp :: Day -> Int
    dayToTimestamp day = floor (utcTimeToPOSIXSeconds (UTCTime day 0))

    assetColorFor :: [Asset] -> Asset -> Text
    assetColorFor allAssets asset =
        let colors = ["#2962FF", "#E91E63", "#4CAF50", "#FF9800", "#9C27B0", "#00BCD4"]
            idx = fromMaybe 0 $ List.findIndex (\a -> get #id a == get #id asset) allAssets
        in colors !! (idx `mod` length colors)
