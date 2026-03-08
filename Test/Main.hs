{-# LANGUAGE OverloadedStrings #-}
module Test.Main where

import Application.Domain.LMSR
import qualified Application.Domain.Types as NewDomain
import Application.Helper.Controller (sanitizeBackTo)
import qualified Data.Map.Strict as M
import qualified Data.Text as Text
import Generated.Types
import IHP.Prelude
import Test.Hspec
import Test.QuickCheck
import Unsafe.Coerce
import Web.Controller.Markets ()
import Web.Controller.Trades ()
import Web.Routes (buildShowMarketPath)

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
                    backToPath = "/Markets?category=test-category&search=one"
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
                path `shouldSatisfy` Text.isInfixOf "backTo=%2FMarkets%3Fcategory%3Dtest-category%26search%3Done"
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
