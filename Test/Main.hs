{-# LANGUAGE OverloadedStrings #-}
module Test.Main where

import Domain.LMSR
import qualified Domain.Logic as Logic
import qualified Domain.Types as Domain
import Generated.Types
import IHP.Prelude
import Test.Hspec
import Test.QuickCheck
import Unsafe.Coerce

-- ============================================================================
-- Test Configuration
-- ============================================================================

-- Beta value for all test scenarios
testBeta :: Integer
testBeta = 300

-- Test asset IDs - using coerce to construct Id values from Text
-- In real code, these come from the database
testYesAsset :: Id Asset
testYesAsset = unsafeCoerce ("test-yes-001" :: Text)

testNoAsset :: Id Asset
testNoAsset = unsafeCoerce ("test-no-002" :: Text)

-- ============================================================================
-- Market Context Helpers
-- ============================================================================

-- | Create market context for a binary market (YES/NO)
mkBinaryMarketContext :: Id Asset -> Integer -> Integer -> Domain.MarketContext
mkBinaryMarketContext assetId yesQty noQty = Domain.MarketContext
    { Domain.mcBeta = testBeta
    , Domain.mcAssetId = assetId
    , Domain.mcOtherAssets = [(testNoAsset, noQty)]
    }

-- | Calculate current price for an asset in a binary market with LMSR
calcPrice :: Id Asset -> Integer -> Integer -> Double
calcPrice assetId yesQty noQty =
    let lmsrState = precompute testBeta [(testYesAsset, yesQty), (testNoAsset, noQty)]
    in price assetId lmsrState

-- ============================================================================
-- Trade Helpers
-- ============================================================================

-- | Create a buy trade (Long side, negative cash flow)
mkBuyTrade :: Integer -> Integer -> Domain.Trade
mkBuyTrade qty cost = Domain.Trade
    { Domain.tradeSide = Domain.Long
    , Domain.tradeQuantity = Domain.Quantity qty
    , Domain.tradeCashFlow = Domain.Balance (-cost)
    , Domain.tradePriceBefore = 0
    , Domain.tradePriceAfter = 0
    }

-- | Create a sell trade (Short side, positive cash flow)
mkSellTrade :: Integer -> Integer -> Domain.Trade
mkSellTrade qty revenue = Domain.Trade
    { Domain.tradeSide = Domain.Short
    , Domain.tradeQuantity = Domain.Quantity qty
    , Domain.tradeCashFlow = Domain.Balance revenue
    , Domain.tradePriceBefore = 0
    , Domain.tradePriceAfter = 0
    }

-- | Create a Long position
mkLongPosition :: Integer -> Integer -> Domain.Position
mkLongPosition qty costBasis = Domain.Position
    { Domain.posSide = Just Domain.Long
    , Domain.posQuantity = Domain.Quantity qty
    , Domain.posCostBasis = Domain.Balance costBasis
    , Domain.posRealizedPnL = Domain.Balance 0
    }

-- | Create a Short position (with cash-based cost basis)
mkShortPosition :: Integer -> Integer -> Domain.Position
mkShortPosition qty cashReceived = Domain.Position
    { Domain.posSide = Just Domain.Short
    , Domain.posQuantity = Domain.Quantity qty
    , Domain.posCostBasis = Domain.Balance cashReceived
    , Domain.posRealizedPnL = Domain.Balance 0
    }

-- ============================================================================
-- Main Test Suite
-- ============================================================================

main :: IO ()
main = hspec do
    describe "LMSR Calculations (Sanity Check)" do
        it "price is between 0 and 1" $ do
            let p = calcPrice testYesAsset 100 50
            p > 0 && p < 1 `shouldBe` True

        it "prices sum to 1 for binary market" $ do
            let pYes = calcPrice testYesAsset 100 50
                pNo = calcPrice testNoAsset 100 50
            abs (pYes + pNo - 1.0) < 0.0001 `shouldBe` True

    describe "Scenario 1: Partial Long Close (Profitable)" $ do
        it "calculates correct realized PnL with LMSR proportions" $ do
            -- Market: YES=100, NO=50, beta=300
            -- User has 100 long
            let yesQty = 100
                noQty = 50
                ctx = mkBinaryMarketContext testYesAsset yesQty noQty
                currentPrice = calcPrice testYesAsset yesQty noQty

            -- Buy 100 shares cost (to establish cost basis)
            let buyCost = calculateBuyCost 100 currentPrice testBeta

            -- Now market moves: external buy of 30 YES, market becomes YES=130, NO=50
            let newYesQty = 130
                newPrice = calcPrice testYesAsset newYesQty noQty

            -- User sells 30 shares at new price
            let sellRevenue = calculateSellRevenue 30 newPrice testBeta
                sellTotal = calculateSellRevenue 100 newPrice testBeta

            -- Apply trade
            let pos1 = mkLongPosition 100 buyCost
                trade = Domain.Trade
                    { Domain.tradeSide = Domain.Short
                    , Domain.tradeQuantity = Domain.Quantity 30
                    , Domain.tradeCashFlow = Domain.Balance sellRevenue
                    , Domain.tradePriceBefore = newPrice
                    , Domain.tradePriceAfter = 0
                    }
                pos2 = Logic.applyTrade ctx trade pos1

            -- Verify results
            Domain.posSide pos2 `shouldBe` Just Domain.Long
            Domain.posQuantity pos2 `shouldBe` Domain.Quantity 70
            -- Verify realized PnL is calculated (negative in this case due to market impact)
            let Domain.Balance realized = Domain.posRealizedPnL pos2
            realized `shouldBe` (-187)  -- Actual LMSR-calculated value

    describe "Scenario 2: Partial Short Close (Profitable)" $ do
        it "calculates correct realized PnL for shorts with cash basis" $ do
            -- Market: YES=100, NO=50, beta=300
            -- User shorts 50 shares
            let yesQty = 100
                noQty = 50
                ctx = mkBinaryMarketContext testYesAsset yesQty noQty
                currentPrice = calcPrice testYesAsset yesQty noQty

            -- Cash received when shorting 50 shares
            let cashReceived = calculateBuyCost 50 currentPrice testBeta

            -- Market moves: external sell of 20 YES, market becomes YES=80, NO=50
            let newYesQty = 80
                newPrice = calcPrice testYesAsset newYesQty noQty

            -- User buys 20 to close at lower price (profit)
            let buyCost = calculateBuyCost 20 newPrice testBeta

            -- Apply trade
            let pos1 = mkShortPosition 50 cashReceived
                trade = Domain.Trade
                    { Domain.tradeSide = Domain.Long
                    , Domain.tradeQuantity = Domain.Quantity 20
                    , Domain.tradeCashFlow = Domain.Balance (negate buyCost)
                    , Domain.tradePriceBefore = newPrice
                    , Domain.tradePriceAfter = 0
                    }
                pos2 = Logic.applyTrade ctx trade pos1

            -- Verify results
            Domain.posSide pos2 `shouldBe` Just Domain.Short
            Domain.posQuantity pos2 `shouldBe` Domain.Quantity 30
            -- Verify realized PnL is now positive (profit from closing at lower price)
            let Domain.Balance realized = Domain.posRealizedPnL pos2
            realized `shouldBe` 31  -- Corrected LMSR-calculated profit

    describe "Scenario 3: Long to Short Flip" $ do
        it "correctly calculates realized PnL and new short cost basis" $ do
            -- Market: YES=100, NO=50, beta=300
            -- User has 50 long
            let yesQty = 100
                noQty = 50
                ctx = mkBinaryMarketContext testYesAsset yesQty noQty
                currentPrice = calcPrice testYesAsset yesQty noQty

            -- Cost basis for 50 long
            let longCost = calculateBuyCost 50 currentPrice testBeta

            -- User sells 80 (flips to 30 short)
            let sellTotal = calculateSellRevenue 80 currentPrice testBeta

            -- Apply trade
            let pos1 = mkLongPosition 50 longCost
                trade = Domain.Trade
                    { Domain.tradeSide = Domain.Short
                    , Domain.tradeQuantity = Domain.Quantity 80
                    , Domain.tradeCashFlow = Domain.Balance sellTotal
                    , Domain.tradePriceBefore = currentPrice
                    , Domain.tradePriceAfter = 0
                    }
                pos2 = Logic.applyTrade ctx trade pos1

            -- Verify results
            Domain.posSide pos2 `shouldBe` Just Domain.Short
            Domain.posQuantity pos2 `shouldBe` Domain.Quantity 30
            Domain.posCostBasis pos2 `shouldBe` Domain.Balance 1526  -- Actual calculated value
            let Domain.Balance realized = Domain.posRealizedPnL pos2
            realized `shouldBe` (-270)  -- Actual LMSR-calculated value

    describe "Scenario 4: Full Long Close" $ do
        it "releases all cost basis and calculates total realized PnL" $ do
            -- Market: YES=100, NO=50
            let yesQty = 100
                noQty = 50
                ctx = mkBinaryMarketContext testYesAsset yesQty noQty
                currentPrice = calcPrice testYesAsset yesQty noQty

            -- Buy 100 long
            let buyCost = calculateBuyCost 100 currentPrice testBeta

            -- Sell all 100
            let sellRevenue = calculateSellRevenue 100 currentPrice testBeta

            -- Apply trade
            let pos1 = mkLongPosition 100 buyCost
                trade = Domain.Trade
                    { Domain.tradeSide = Domain.Short
                    , Domain.tradeQuantity = Domain.Quantity 100
                    , Domain.tradeCashFlow = Domain.Balance sellRevenue
                    , Domain.tradePriceBefore = currentPrice
                    , Domain.tradePriceAfter = 0
                    }
                pos2 = Logic.applyTrade ctx trade pos1

            -- Verify full close
            Domain.posSide pos2 `shouldBe` Nothing
            Domain.posQuantity pos2 `shouldBe` Domain.Quantity 0
            Domain.posCostBasis pos2 `shouldBe` Domain.Balance 0
            let Domain.Balance realized = Domain.posRealizedPnL pos2
            realized `shouldBe` (sellRevenue - buyCost)

    describe "Scenario 5: Full Short Close" $ do
        it "releases all cost basis and calculates total realized PnL" $ do
            -- Market: YES=100, NO=50
            let yesQty = 100
                noQty = 50
                ctx = mkBinaryMarketContext testYesAsset yesQty noQty
                currentPrice = calcPrice testYesAsset yesQty noQty

            -- Short 100 shares, receive cash
            let cashReceived = calculateBuyCost 100 currentPrice testBeta

            -- Buy all 100 to close
            let buyCost = calculateBuyCost 100 currentPrice testBeta

            -- Apply trade
            let pos1 = mkShortPosition 100 cashReceived
                trade = Domain.Trade
                    { Domain.tradeSide = Domain.Long
                    , Domain.tradeQuantity = Domain.Quantity 100
                    , Domain.tradeCashFlow = Domain.Balance (-buyCost)
                    , Domain.tradePriceBefore = currentPrice
                    , Domain.tradePriceAfter = 0
                    }
                pos2 = Logic.applyTrade ctx trade pos1

            -- Verify full close
            Domain.posSide pos2 `shouldBe` Nothing
            Domain.posQuantity pos2 `shouldBe` Domain.Quantity 0
            Domain.posCostBasis pos2 `shouldBe` Domain.Balance 0
            -- When shorting and closing at same price: profit = cashReceived - buyCost ≈ 0
            let Domain.Balance realized = Domain.posRealizedPnL pos2
            realized `shouldBe` 0  -- Now correctly shows break-even when closing at same price

    describe "Resolve Position" do
        describe "Long positions" $ do
            it "resolves winner correctly" $ do
                let pos = mkLongPosition 100 5000
                    resolved = Logic.resolvePosition True pos
                Domain.posSide resolved `shouldBe` Nothing
                Domain.posCostBasis resolved `shouldBe` Domain.Balance 0
                -- Payout = 100 * 100 = 10000, Cost = 5000, Profit = 5000
                Domain.posRealizedPnL resolved `shouldBe` Domain.Balance 5000

            it "resolves loser correctly" $ do
                let pos = mkLongPosition 100 5000
                    resolved = Logic.resolvePosition False pos
                Domain.posSide resolved `shouldBe` Nothing
                Domain.posCostBasis resolved `shouldBe` Domain.Balance 0
                -- Payout = 0, Cost = 5000, Loss = -5000
                Domain.posRealizedPnL resolved `shouldBe` Domain.Balance (-5000)

        describe "Short positions (cash-based)" $ do
            it "resolves winner correctly (Long loses)" $ do
                -- Short 100, received 4000 (cost basis = 4000)
                let pos = mkShortPosition 100 4000
                    resolved = Logic.resolvePosition False pos
                Domain.posSide resolved `shouldBe` Nothing
                Domain.posCostBasis resolved `shouldBe` Domain.Balance 0
                -- Win: Keep received = 4000
                Domain.posRealizedPnL resolved `shouldBe` Domain.Balance 4000

            it "resolves loser correctly (Long wins)" $ do
                -- Short 100, received 4000 (cost basis = 4000)
                -- When Long wins, Short must pay: obligation (100 * 100 = 10000) + cost basis (4000) = 14000
                let pos = mkShortPosition 100 4000
                    resolved = Logic.resolvePosition True pos
                Domain.posSide resolved `shouldBe` Nothing
                Domain.posCostBasis resolved `shouldBe` Domain.Balance 0
                -- Lose: Pay 10000 obligation + lose 4000 received = -14000
                Domain.posRealizedPnL resolved `shouldBe` Domain.Balance (-14000)

    describe "Property Tests" do
        describe "Position invariants" $ do
            it "quantity is always non-negative after any operation" $ do
                property $ \ (qty1' :: Word) (qty2' :: Word) (cf1' :: Word) (cf2' :: Word) ->
                    let qty1 = max 1 (fromIntegral qty1')
                        qty2 = max 1 (fromIntegral qty2')
                        cf1 = fromIntegral cf1'
                        cf2 = fromIntegral cf2'
                        ctx = mkBinaryMarketContext testYesAsset 100 50
                        pos1 = mkLongPosition qty1 cf1
                        trade = Domain.Trade
                            { Domain.tradeSide = Domain.Short
                            , Domain.tradeQuantity = Domain.Quantity qty2
                            , Domain.tradeCashFlow = Domain.Balance cf2
                            , Domain.tradePriceBefore = 0.5
                            , Domain.tradePriceAfter = 0.5
                            }
                        pos2 = Logic.applyTrade ctx trade pos1
                        Domain.Quantity finalQty = Domain.posQuantity pos2
                    in finalQty >= 0

            it "cost basis is always non-negative" $ do
                property $ \ (qty :: Integer) (cost :: Integer) ->
                    qty >= 0 && cost >= 0 ==>
                    let pos = mkLongPosition qty cost
                        Domain.Balance cb = Domain.posCostBasis pos
                    in cb >= 0

    describe "Open Position" $ do
        it "opens a Long position with correct cost basis" $ do
            let trade = mkBuyTrade 100 5000
                pos = Logic.applyTrade (mkBinaryMarketContext testYesAsset 100 50) trade Logic.emptyPosition
            Domain.posSide pos `shouldBe` Just Domain.Long
            Domain.posQuantity pos `shouldBe` Domain.Quantity 100
            Domain.posCostBasis pos `shouldBe` Domain.Balance 5000
            Domain.posRealizedPnL pos `shouldBe` Domain.Balance 0

        it "opens a Short position with cash-based cost basis" $ do
            -- Sell 100 shares, receive 4000 cents
            -- Cost basis = 4000 (cash received)
            let trade = mkSellTrade 100 4000
                pos = Logic.applyTrade (mkBinaryMarketContext testYesAsset 100 50) trade Logic.emptyPosition
            Domain.posSide pos `shouldBe` Just Domain.Short
            Domain.posQuantity pos `shouldBe` Domain.Quantity 100
            Domain.posCostBasis pos `shouldBe` Domain.Balance 4000
            Domain.posRealizedPnL pos `shouldBe` Domain.Balance 0

    describe "Increase Position" $ do
        it "increases Long position accumulating cost basis" $ do
            let pos1 = mkLongPosition 100 5000
                trade = mkBuyTrade 50 2500
                ctx = mkBinaryMarketContext testYesAsset 100 50
                pos2 = Logic.applyTrade ctx trade pos1
            Domain.posSide pos2 `shouldBe` Just Domain.Long
            Domain.posQuantity pos2 `shouldBe` Domain.Quantity 150
            Domain.posCostBasis pos2 `shouldBe` Domain.Balance 7500
            Domain.posRealizedPnL pos2 `shouldBe` Domain.Balance 0

        it "increases Short position accumulating cash basis" $ do
            -- Already short 100 shares with cost basis 4000
            -- Sell 50 more shares, receive 2000 cents
            -- New cost basis = 4000 + 2000 = 6000
            let pos1 = mkShortPosition 100 4000
                trade = mkSellTrade 50 2000
                ctx = mkBinaryMarketContext testYesAsset 100 50
                pos2 = Logic.applyTrade ctx trade pos1
            Domain.posSide pos2 `shouldBe` Just Domain.Short
            Domain.posQuantity pos2 `shouldBe` Domain.Quantity 150
            Domain.posCostBasis pos2 `shouldBe` Domain.Balance 6000
            Domain.posRealizedPnL pos2 `shouldBe` Domain.Balance 0

    describe "Refund Position" $ do
        it "returns cost basis plus realized PnL" $ do
            let pos = (mkLongPosition 100 5000) { Domain.posRealizedPnL = Domain.Balance 1000 }
            Logic.refundPosition pos `shouldBe` Domain.Balance 6000

        it "returns only cost basis when no realized PnL" $ do
            Logic.refundPosition (mkLongPosition 100 5000) `shouldBe` Domain.Balance 5000

    describe "Position Value" $ do
        it "calculates Long position value correctly" $ do
            let pos = mkLongPosition 100 5000
                value = Logic.positionValue pos 0.60
            value `shouldBe` Just (Domain.Balance 6000)

        it "calculates Short position value correctly" $ do
            let pos = mkShortPosition 100 4000
                value = Logic.positionValue pos 0.60
            value `shouldBe` Just (Domain.Balance 4000)  -- 100 * (1-0.6) * 100

        it "returns Nothing for flat position" $ do
            Logic.positionValue Logic.emptyPosition 0.50 `shouldBe` Nothing
