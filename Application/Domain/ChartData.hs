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

    let endDay = min (utctDay market.closedAt) today

    let startDay = case get #openedAt market of
            Just openedTime -> utctDay openedTime
            Nothing         -> utctDay (get #createdAt market)

    transactions <- query @Transaction
        |> filterWhere (#marketId, get #id market)
        |> orderByAsc #createdAt
        |> fetch

    let validTransactions = filter (\t -> get #marketState t /= Aeson.object []) transactions

    let currentQtyMap = M.fromList [(get #id a, Quantity (get #quantity a)) | a <- assets]
    let currentPrices = allAssetPrices (Beta beta) currentQtyMap

    if null validTransactions
        then pure $ makeFlatLineFromDay startDay endDay assets currentPrices
        else do
            let filteredTxns = filter (\t -> utctDay (get #createdAt t) <= endDay) validTransactions
            let assetTxns = groupAssetTransactions filteredTxns
            let lastTxnPerDay = getLastTransactionPerDay assetTxns
            let dayRange = generateDayRange startDay endDay
            let pricesPerDay = computePricesPerDay lastTxnPerDay assets beta
            let filledData = fillMissingDays dayRange pricesPerDay currentPrices

            pure $ map (buildAssetChartData filledData endDay) assets
  where
    groupAssetTransactions :: [Transaction] -> M.Map (Id Asset) [(Day, Transaction)]
    groupAssetTransactions txns = M.fromListWith (++)
        [ (get #assetId t, [(utctDay (get #createdAt t), t)])
        | t <- txns
        ]

    getLastTransactionPerDay
        :: M.Map (Id Asset) [(Day, Transaction)]
        -> M.Map (Id Asset) (M.Map Day Transaction)
    getLastTransactionPerDay assetTxns = M.map processAssetTxns assetTxns
      where
        processAssetTxns :: [(Day, Transaction)] -> M.Map Day Transaction
        processAssetTxns dayTxns =
            let sorted = List.sortBy (\(d1, _) (d2, _) -> compare d1 d2) dayTxns
            in M.fromList (getLastPerDay sorted)

        getLastPerDay :: [(Day, Transaction)] -> [(Day, Transaction)]
        getLastPerDay [] = []
        getLastPerDay [x] = [x]
        getLastPerDay ((d1, t1):(d2, t2):rest)
            | d1 == d2 = getLastPerDay ((d1, t2):rest)
            | otherwise = (d1, t1) : getLastPerDay ((d2, t2):rest)

    generateDayRange :: Day -> Day -> [Day]
    generateDayRange start end = takeWhile (<= end) (iterate (addDays 1) start)

    computePricesPerDay
        :: M.Map (Id Asset) (M.Map Day Transaction)
        -> [Asset]
        -> Integer
        -> M.Map Day (M.Map (Id Asset) Double)
    computePricesPerDay lastTxns assets' beta' = M.fromListWith M.union
        [ (day, prices)
        | (_, dayTxns) <- M.toList lastTxns
        , (day, txn) <- M.toList dayTxns
        , let prices = case parseMarketState (get #marketState txn) of
                Just qtyMap -> allAssetPrices (Beta beta') qtyMap
                Nothing     -> M.empty
        ]

    parseMarketState :: Aeson.Value -> Maybe (M.Map (Id Asset) Quantity)
    parseMarketState value = case Aeson.fromJSON value of
        AesonTypes.Success (obj :: [(Text, Int)]) -> Just $ M.fromList
            [ (Id uid, Quantity (fromIntegral qty))
            | (key, qty) <- obj
            , Just uid <- [UUID.fromText key]
            ]
        AesonTypes.Error _ -> Nothing

    makeFlatLineFromDay
        :: Day -> Day -> [Asset] -> M.Map (Id Asset) Double -> [AssetChartData]
    makeFlatLineFromDay startDay endDay assets' prices = do
        let dayRange = generateDayRange startDay endDay
        map (buildFlatAssetChart dayRange prices) assets'

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
                Just p  -> p
                Nothing -> lastKnownPrices
            nextPrices = if M.null dayPrices then lastKnownPrices else dayPrices
            rest = fillMissingDays days pricesPerDay nextPrices
        in M.insert day dayPrices rest

    buildAssetChartData
        :: M.Map Day (M.Map (Id Asset) Double) -> Day -> Asset -> AssetChartData
    buildAssetChartData filledData _today asset =
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
            idx = fromMaybe 0 (List.elemIndex asset allAssets)
        in colors !! (idx `mod` length colors)
