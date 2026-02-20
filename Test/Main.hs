{-# LANGUAGE OverloadedStrings #-}
module Test.Main where

import Application.Domain.LMSR
import qualified Application.Domain.Types as NewDomain
import qualified Data.Map.Strict as M
import Generated.Types
import IHP.Prelude
import Test.Hspec
import Test.QuickCheck
import Unsafe.Coerce

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
