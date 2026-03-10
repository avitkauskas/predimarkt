{-# LANGUAGE OverloadedStrings #-}
module Test.Main where

import Application.Domain.LMSR
import qualified Application.Domain.Types as NewDomain
import Application.Helper.Controller (sanitizeBackTo)
import Application.Helper.View (textParagraphs)
import qualified Data.Map.Strict as M
import qualified Data.Text as Text
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime (..), secondsToDiffTime)
import Generated.Types
import IHP.ModelSupport (newRecord)
import IHP.Prelude
import IHP.ValidationSupport (getValidationFailure)
import Test.Hspec
import Test.QuickCheck
import Unsafe.Coerce
import Web.Controller.Leaderboard (annualRateOfReturn, leaderboardScore,
                                   portfolioReturn, shouldDisplayAnnualReturn,
                                   yearsSinceRegistration)
import Web.Controller.Markets (buildMarket)
import Web.Controller.Trades ()
import Web.Routes (buildShowMarketPath)
import Web.View.Markets.Index (MarketIndexStatusFilter (..), buildMarketsPath,
                               parseMarketIndexStatusFilter)

testBeta :: Integer
testBeta = 300

testYesAsset :: Id Asset
testYesAsset = unsafeCoerce ("test-yes-001" :: Text)

testNoAsset :: Id Asset
testNoAsset = unsafeCoerce ("test-no-002" :: Text)

main :: IO ()
main = hspec do
    describe "New LMSR Module Tests" do
        let beta = NewDomain.Beta 300
            assetOne = unsafeCoerce ("one" :: Text)
            assetTwo = unsafeCoerce ("two" :: Text)
            assetThree = unsafeCoerce ("three" :: Text)

        let initialState = M.fromList
                [ (assetOne, NewDomain.Quantity 0)
                , (assetTwo, NewDomain.Quantity 0)
                , (assetThree, NewDomain.Quantity 0)
                ]

        let price4digits p = round (p * 10000) :: Integer

        describe "Initial state prices" do
            it "allAssetPrices returns equal prices for equal quantities" do
                let prices = allAssetPrices beta initialState
                price4digits (prices M.! assetOne) `shouldBe` 3333
                price4digits (prices M.! assetTwo) `shouldBe` 3333
                price4digits (prices M.! assetThree) `shouldBe` 3333

        describe "Buy 100 of asset one" do
            it "tradeValue returns correct negative money (cost)" do
                let cost = tradeValue assetOne (NewDomain.Quantity 100) beta initialState
                cost `shouldBe` NewDomain.Money (-3716)

            it "prices update after buying" do
                let stateAfter = M.insertWith (+) assetOne (NewDomain.Quantity 100) initialState
                    prices = allAssetPrices beta stateAfter
                price4digits (prices M.! assetOne) `shouldBe` 4110
                price4digits (prices M.! assetTwo) `shouldBe` 2945
                price4digits (prices M.! assetThree) `shouldBe` 2945

        describe "Sell 100 of asset two" do
            let stateAfterBuy = M.fromList
                    [ (assetOne, NewDomain.Quantity 100)
                    , (assetTwo, NewDomain.Quantity 0)
                    , (assetThree, NewDomain.Quantity 0)
                    ]

            it "tradeValue returns positive money (revenue from selling)" do
                let revenue = tradeValue assetTwo (NewDomain.Quantity (-100)) beta stateAfterBuy
                revenue `shouldBe` NewDomain.Money 2615

            it "prices update after selling" do
                let stateAfterSell = M.insertWith (+) assetTwo (NewDomain.Quantity (-100)) stateAfterBuy
                    prices = allAssetPrices beta stateAfterSell
                price4digits (prices M.! assetOne) `shouldBe` 4484
                price4digits (prices M.! assetTwo) `shouldBe` 2302
                price4digits (prices M.! assetThree) `shouldBe` 3213

        describe "assetPrice function" do
            let finalState = M.fromList
                    [ (assetOne, NewDomain.Quantity 100)
                    , (assetTwo, NewDomain.Quantity (-100))
                    , (assetThree, NewDomain.Quantity 0)
                    ]

            it "returns correct price for asset three" do
                let p = assetPrice assetThree beta finalState
                price4digits p `shouldBe` 3213

        describe "Market show path building" do
            it "URL-encodes backTo so nested market filters survive round-trips" do
                let marketId = unsafeCoerce ("test-market-001" :: Text) :: Id Market
                    backToPath = "/Markets?category=test-category&status=refunded&search=one"
                    path = buildShowMarketPath
                        marketId
                        (Nothing :: Maybe (Id Asset))
                        (Nothing :: Maybe Text)
                        (Nothing :: Maybe Bool)
                        (Nothing :: Maybe Bool)
                        (Nothing :: Maybe Bool)
                        (Nothing :: Maybe Bool)
                        (Nothing :: Maybe Int)
                        (Nothing :: Maybe Int)
                        (Nothing :: Maybe Text)
                        (Nothing :: Maybe Int)
                        (Just backToPath)
                path `shouldSatisfy` Text.isInfixOf "backTo=%2FMarkets%3Fcategory%3Dtest-category%26status%3Drefunded%26search%3Done"
                path `shouldSatisfy` not . Text.isInfixOf "&search=one"

            it "includes chat state parameters when present" do
                let marketId = unsafeCoerce ("test-market-001" :: Text) :: Id Market
                    path = buildShowMarketPath
                        marketId
                        (Nothing :: Maybe (Id Asset))
                        (Nothing :: Maybe Text)
                        (Nothing :: Maybe Bool)
                        (Nothing :: Maybe Bool)
                        (Nothing :: Maybe Bool)
                        (Nothing :: Maybe Bool)
                        (Nothing :: Maybe Int)
                        (Just (3 :: Int))
                        (Just ("rev-123" :: Text))
                        (Nothing :: Maybe Int)
                        (Nothing :: Maybe Text)
                path `shouldSatisfy` Text.isInfixOf "chatPage=3"
                path `shouldSatisfy` Text.isInfixOf "chatComposerRev=rev-123"

            it "includes trade quantity when present" do
                let marketId = unsafeCoerce ("test-market-001" :: Text) :: Id Market
                    path = buildShowMarketPath
                        marketId
                        (Nothing :: Maybe (Id Asset))
                        (Nothing :: Maybe Text)
                        (Nothing :: Maybe Bool)
                        (Nothing :: Maybe Bool)
                        (Nothing :: Maybe Bool)
                        (Nothing :: Maybe Bool)
                        (Nothing :: Maybe Int)
                        (Nothing :: Maybe Int)
                        (Nothing :: Maybe Text)
                        (Just (37 :: Int))
                        (Nothing :: Maybe Text)
                path `shouldSatisfy` Text.isInfixOf "tradeQuantity=37"

            it "URL-encodes dashboard backTo paths with page and search state" do
                let marketId = unsafeCoerce ("test-market-001" :: Text) :: Id Market
                    backToPath = "/DashboardPositions?page=3&search=gold"
                    path = buildShowMarketPath
                        marketId
                        (Nothing :: Maybe (Id Asset))
                        (Nothing :: Maybe Text)
                        (Nothing :: Maybe Bool)
                        (Nothing :: Maybe Bool)
                        (Nothing :: Maybe Bool)
                        (Nothing :: Maybe Bool)
                        (Nothing :: Maybe Int)
                        (Nothing :: Maybe Int)
                        (Nothing :: Maybe Text)
                        (Nothing :: Maybe Int)
                        (Just backToPath)
                path `shouldSatisfy` Text.isInfixOf "backTo=%2FDashboardPositions%3Fpage%3D3%26search%3Dgold"

        describe "sanitizeBackTo" do
            it "accepts any local path" do
                sanitizeBackTo (Just "/DashboardPositions?page=3&search=gold")
                    `shouldBe` Just "/DashboardPositions?page=3&search=gold"
                sanitizeBackTo (Just "/DashboardTransactions?page=2")
                    `shouldBe` Just "/DashboardTransactions?page=2"
                sanitizeBackTo (Just "/DashboardMarkets?statusFilter=Open")
                    `shouldBe` Just "/DashboardMarkets?statusFilter=Open"
                sanitizeBackTo (Just "/Users") `shouldBe` Just "/Users"
                sanitizeBackTo (Just " /Leaderboard ") `shouldBe` Just "/Leaderboard"

            it "rejects external or non-local paths" do
                sanitizeBackTo (Just "https://evil.example") `shouldBe` Nothing
                sanitizeBackTo (Just "http://evil.example") `shouldBe` Nothing
                sanitizeBackTo (Just "//evil.example") `shouldBe` Nothing
                sanitizeBackTo (Just "Markets") `shouldBe` Nothing

        describe "textParagraphs" do
            it "keeps single newlines inside the same paragraph" do
                textParagraphs "First paragraph\nSecond paragraph"
                    `shouldBe` ["First paragraph\nSecond paragraph"]

            it "splits paragraphs on double newlines" do
                textParagraphs "First paragraph\n\nSecond paragraph"
                    `shouldBe` ["First paragraph", "Second paragraph"]

            it "condenses longer blank runs into a single paragraph break" do
                textParagraphs "First paragraph\n\n\n\nSecond paragraph"
                    `shouldBe` ["First paragraph", "Second paragraph"]

            it "normalizes Windows newlines" do
                textParagraphs "First paragraph\r\nSecond paragraph\r\n"
                    `shouldBe` ["First paragraph\nSecond paragraph"]

        describe "buildMarket" do
            it "adds a field validation error when closing time is in the past" do
                let now = UTCTime (fromGregorian 2026 3 10) (secondsToDiffTime 0)
                    pastTime = UTCTime (fromGregorian 2026 3 9) (secondsToDiffTime 0)
                    categoryId = unsafeCoerce ("test-category" :: Text) :: Id Category
                    market =
                        newRecord @Market
                            |> set #title "Test market"
                            |> set #description "Description"
                            |> set #categoryId categoryId
                            |> set #closedAt pastTime
                            |> buildMarket now
                getValidationFailure #closedAt market `shouldSatisfy` isJust

        describe "Market index status filters" do
            it "defaults to open and recent other for missing or unknown params" do
                parseMarketIndexStatusFilter Nothing
                    `shouldBe` MarketIndexStatusOpenAndRecentOther
                parseMarketIndexStatusFilter (Just "unexpected")
                    `shouldBe` MarketIndexStatusOpenAndRecentOther

            it "parses all explicit status filter options" do
                parseMarketIndexStatusFilter (Just "closed")
                    `shouldBe` MarketIndexStatusAllClosed
                parseMarketIndexStatusFilter (Just "resolved")
                    `shouldBe` MarketIndexStatusAllResolved
                parseMarketIndexStatusFilter (Just "refunded")
                    `shouldBe` MarketIndexStatusAllRefunded

            it "builds market index paths with status, category, search, and page" do
                let categoryId = unsafeCoerce ("test-category" :: Text) :: Id Category
                    path = buildMarketsPath
                        (Just categoryId)
                        MarketIndexStatusAllResolved
                        (Just "inflation")
                        (Just 3)
                path `shouldSatisfy` Text.isPrefixOf "/Markets?category="
                path `shouldSatisfy`
                    Text.isInfixOf "&status=resolved&search=inflation&page=3"

            it "omits the status param for the default filter" do
                buildMarketsPath
                    Nothing
                    MarketIndexStatusOpenAndRecentOther
                    Nothing
                    Nothing
                    `shouldBe` "/Markets"

        describe "leaderboardScore" do
            it "uses the stabilized log return with C = 0.5" do
                let registeredAt = UTCTime (fromGregorian 2025 1 1) (secondsToDiffTime 0)
                    now = UTCTime (fromGregorian 2026 1 1) (secondsToDiffTime 0)
                    score = leaderboardScore now registeredAt 120000
                abs (score - 0.1216) `shouldSatisfy` (< 0.001)

        describe "portfolioReturn" do
            it "returns the total portfolio return since signup" do
                abs (portfolioReturn 120000 - 0.2) `shouldSatisfy` (< 0.000001)

        describe "annualRateOfReturn" do
            it "returns annualized return once time since registration is known" do
                let registeredAt = UTCTime (fromGregorian 2025 1 1) (secondsToDiffTime 0)
                    now = UTCTime (fromGregorian 2026 1 1) (secondsToDiffTime 0)
                    arr = annualRateOfReturn now registeredAt 120000
                abs (arr - 0.2001) `shouldSatisfy` (< 0.001)

            it "reduces the displayed annual return for longer participation periods" do
                let registeredAt = UTCTime (fromGregorian 2024 1 1) (secondsToDiffTime 0)
                    now = UTCTime (fromGregorian 2026 1 1) (secondsToDiffTime 0)
                    arr = annualRateOfReturn now registeredAt 120000
                abs (arr - 0.0955) `shouldSatisfy` (< 0.001)

            it "clamps non-positive portfolio values to a tiny positive floor" do
                let registeredAt = UTCTime (fromGregorian 2025 1 1) (secondsToDiffTime 0)
                    now = UTCTime (fromGregorian 2026 1 1) (secondsToDiffTime 0)
                    arr = annualRateOfReturn now registeredAt 0
                arr `shouldSatisfy` (\x -> not (isNaN x) && not (isInfinite x) && x > (-1))

        describe "shouldDisplayAnnualReturn" do
            it "shows only total return for accounts younger than 30 days" do
                let registeredAt = UTCTime (fromGregorian 2026 1 1) (secondsToDiffTime 0)
                    beforeThreshold = UTCTime (fromGregorian 2026 1 30) (secondsToDiffTime 0)
                shouldDisplayAnnualReturn beforeThreshold registeredAt `shouldBe` False

            it "shows annual return starting at 30 days" do
                let registeredAt = UTCTime (fromGregorian 2026 1 1) (secondsToDiffTime 0)
                    atThreshold = UTCTime (fromGregorian 2026 1 31) (secondsToDiffTime 0)
                shouldDisplayAnnualReturn atThreshold registeredAt `shouldBe` True

        describe "yearsSinceRegistration" do
            it "computes years since registration in years" do
                let registeredAt = UTCTime (fromGregorian 2025 1 1) (secondsToDiffTime 0)
                    now = UTCTime (fromGregorian 2026 1 1) (secondsToDiffTime 0)
                    yearsActive = yearsSinceRegistration now registeredAt
                abs (yearsActive - 0.9993) `shouldSatisfy` (< 0.001)

            it "clamps registration age to at least one day" do
                let registeredAt = UTCTime (fromGregorian 2026 1 1) (secondsToDiffTime 0)
                    oneDayLater = UTCTime (fromGregorian 2026 1 2) (secondsToDiffTime 0)
                    sameMoment = registeredAt
                    yearsAtSameMoment = yearsSinceRegistration sameMoment registeredAt
                    yearsOneDayLater = yearsSinceRegistration oneDayLater registeredAt
                abs (yearsAtSameMoment - yearsOneDayLater) `shouldSatisfy` (< 0.000001)
