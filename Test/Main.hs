module Main where

import Domain.LMSR
import Domain.Logic
import Domain.Types
import IHP.Prelude
import Test.Hspec
import Test.QuickCheck

-- | Helper to create a buy transaction (Long side, negative cash flow)
mkBuyTx :: Integer -> Integer -> Double -> Double -> Transaction
mkBuyTx qty cost priceBefore priceAfter = Transaction
    { txSide = Long
    , txQuantity = Quantity qty
    , txCashFlow = Balance (-cost)
    , txPriceBefore = priceBefore
    , txPriceAfter = priceAfter
    , txMarketQBefore = 0
    , txMarketQAfter = qty
    }

-- | Helper to create a sell transaction (Short side, positive cash flow)
mkSellTx :: Integer -> Integer -> Double -> Double -> Transaction
mkSellTx qty revenue priceBefore priceAfter = Transaction
    { txSide = Short
    , txQuantity = Quantity qty
    , txCashFlow = Balance revenue
    , txPriceBefore = priceBefore
    , txPriceAfter = priceAfter
    , txMarketQBefore = 0
    , txMarketQAfter = qty
    }

-- | Helper to create a Long position
mkLongPosition :: Integer -> Integer -> Position
mkLongPosition qty costBasis = Position
    { posSide = Just Long
    , posQuantity = Quantity qty
    , posCostBasis = Balance costBasis
    , posRealizedPnL = Balance 0
    }

-- | Helper to create a Short position
mkShortPosition :: Integer -> Integer -> Position
mkShortPosition qty costBasis = Position
    { posSide = Just Short
    , posQuantity = Quantity qty
    , posCostBasis = Balance costBasis
    , posRealizedPnL = Balance 0
    }

main :: IO ()
main = hspec do
    describe "Position Operations" do
        describe "openPosition" do
            it "opens a Long position with correct cost basis" $ do
                let tx = mkBuyTx 100 5000 0.50 0.52
                    pos = applyTransaction tx emptyPosition
                posSide pos `shouldBe` Just Long
                posQuantity pos `shouldBe` Quantity 100
                posCostBasis pos `shouldBe` Balance 5000
                posRealizedPnL pos `shouldBe` Balance 0

            it "opens a Short position with correct cost basis" $ do
                let tx = mkSellTx 100 4000 0.60 0.58
                    pos = applyTransaction tx emptyPosition
                posSide pos `shouldBe` Just Short
                posQuantity pos `shouldBe` Quantity 100
                posCostBasis pos `shouldBe` Balance 4000
                posRealizedPnL pos `shouldBe` Balance 0

        describe "increasePosition" do
            it "increases Long position accumulating cost basis" $ do
                let pos1 = mkLongPosition 100 5000
                    tx = mkBuyTx 50 2500 0.50 0.52
                    pos2 = applyTransaction tx pos1
                posSide pos2 `shouldBe` Just Long
                posQuantity pos2 `shouldBe` Quantity 150
                posCostBasis pos2 `shouldBe` Balance 7500
                posRealizedPnL pos2 `shouldBe` Balance 0

            it "increases Short position accumulating cost basis" $ do
                let pos1 = mkShortPosition 100 4000
                    tx = mkSellTx 50 2000 0.40 0.38
                    pos2 = applyTransaction tx pos1
                posSide pos2 `shouldBe` Just Short
                posQuantity pos2 `shouldBe` Quantity 150
                posCostBasis pos2 `shouldBe` Balance 6000
                posRealizedPnL pos2 `shouldBe` Balance 0

        describe "reducePosition (partial close)" do
            it "reduces Long position with correct PnL when profitable" $ do
                -- Buy 100 shares at $50 each (cost basis = $50)
                -- Sell 50 shares at $60 each (receive $30)
                -- Cost basis released = $50 * (50/100) = $25
                -- Realized PnL = $30 - $25 = $5 profit
                let pos1 = mkLongPosition 100 5000
                    tx = Transaction
                        { txSide = Short  -- Selling to close Long
                        , txQuantity = Quantity 50
                        , txCashFlow = Balance 3000  -- Received $30
                        , txPriceBefore = 0.60
                        , txPriceAfter = 0.58
                        , txMarketQBefore = 100
                        , txMarketQAfter = 50
                        }
                    pos2 = applyTransaction tx pos1
                posSide pos2 `shouldBe` Just Long
                posQuantity pos2 `shouldBe` Quantity 50
                posCostBasis pos2 `shouldBe` Balance 2500  -- Remaining cost basis
                -- BUG: Current implementation returns 5500 instead of 500
                posRealizedPnL pos2 `shouldBe` Balance 500  -- $5 profit

            it "reduces Long position with correct PnL when losing" $ do
                -- Buy 100 shares at $50 each
                -- Sell 50 shares at $40 each (receive $20)
                -- Cost basis released = $25
                -- Realized PnL = $20 - $25 = -$5 loss
                let pos1 = mkLongPosition 100 5000
                    tx = Transaction
                        { txSide = Short
                        , txQuantity = Quantity 50
                        , txCashFlow = Balance 2000
                        , txPriceBefore = 0.40
                        , txPriceAfter = 0.38
                        , txMarketQBefore = 100
                        , txMarketQAfter = 50
                        }
                    pos2 = applyTransaction tx pos1
                posQuantity pos2 `shouldBe` Quantity 50
                posCostBasis pos2 `shouldBe` Balance 2500
                posRealizedPnL pos2 `shouldBe` Balance (-500)

            it "reduces Short position with correct PnL when profitable" $ do
                -- Short 100 shares, receive $40 (cost basis = $40)
                -- Buy 50 shares to close at $30 (pay $15)
                -- Cost basis released = $40 * (50/100) = $20
                -- Realized PnL = -$15 + $20 = $5 profit
                let pos1 = mkShortPosition 100 4000
                    tx = Transaction
                        { txSide = Long  -- Buying to close Short
                        , txQuantity = Quantity 50
                        , txCashFlow = Balance (-1500)  -- Paid $15
                        , txPriceBefore = 0.30
                        , txPriceAfter = 0.32
                        , txMarketQBefore = 100
                        , txMarketQAfter = 50
                        }
                    pos2 = applyTransaction tx pos1
                posSide pos2 `shouldBe` Just Short
                posQuantity pos2 `shouldBe` Quantity 50
                posCostBasis pos2 `shouldBe` Balance 2000
                posRealizedPnL pos2 `shouldBe` Balance 500

            it "reduces Short position with correct PnL when losing" $ do
                -- Short 100 shares, receive $40
                -- Buy 50 shares to close at $50 (pay $25)
                -- Cost basis released = $20
                -- Realized PnL = -$25 + $20 = -$5 loss
                let pos1 = mkShortPosition 100 4000
                    tx = Transaction
                        { txSide = Long
                        , txQuantity = Quantity 50
                        , txCashFlow = Balance (-2500)
                        , txPriceBefore = 0.50
                        , txPriceAfter = 0.52
                        , txMarketQBefore = 100
                        , txMarketQAfter = 50
                        }
                    pos2 = applyTransaction tx pos1
                posQuantity pos2 `shouldBe` Quantity 50
                posCostBasis pos2 `shouldBe` Balance 2000
                posRealizedPnL pos2 `shouldBe` Balance (-500)

        describe "closePosition (full close)" do
            it "closes Long position with correct PnL" $ do
                let pos1 = mkLongPosition 100 5000
                    tx = Transaction
                        { txSide = Short
                        , txQuantity = Quantity 100
                        , txCashFlow = Balance 6000  -- Sold at profit
                        , txPriceBefore = 0.60
                        , txPriceAfter = 0.58
                        , txMarketQBefore = 100
                        , txMarketQAfter = 0
                        }
                    pos2 = applyTransaction tx pos1
                posSide pos2 `shouldBe` Nothing
                posQuantity pos2 `shouldBe` Quantity 0
                posCostBasis pos2 `shouldBe` Balance 0
                posRealizedPnL pos2 `shouldBe` Balance 1000  -- $10 profit

            it "closes Short position with correct PnL" $ do
                let pos1 = mkShortPosition 100 4000
                    tx = Transaction
                        { txSide = Long
                        , txQuantity = Quantity 100
                        , txCashFlow = Balance (-3000)  -- Bought back at profit
                        , txPriceBefore = 0.30
                        , txPriceAfter = 0.32
                        , txMarketQBefore = 100
                        , txMarketQAfter = 0
                        }
                    pos2 = applyTransaction tx pos1
                posSide pos2 `shouldBe` Nothing
                posQuantity pos2 `shouldBe` Quantity 0
                posCostBasis pos2 `shouldBe` Balance 0
                posRealizedPnL pos2 `shouldBe` Balance 1000  -- $10 profit

        describe "flipPosition" do
            it "flips from Long to Short with correct PnL and cost basis" $ do
                -- Long 100 shares, cost basis $50
                -- Sell 150 shares total, receive $90
                -- Closed portion: 100 shares
                -- Proportional CF for closed: $90 * 100/150 = $60
                -- Released cost: $50
                -- Realized PnL: $60 - $50 = $10 profit
                -- Remaining Short 50 shares, cost basis: $90 * 50/150 = $30
                let pos1 = mkLongPosition 100 5000
                    tx = Transaction
                        { txSide = Short
                        , txQuantity = Quantity 150
                        , txCashFlow = Balance 9000
                        , txPriceBefore = 0.60
                        , txPriceAfter = 0.55
                        , txMarketQBefore = 100
                        , txMarketQAfter = -50
                        }
                    pos2 = applyTransaction tx pos1
                posSide pos2 `shouldBe` Just Short
                posQuantity pos2 `shouldBe` Quantity 50
                -- BUG: Current implementation gives wrong cost basis
                posCostBasis pos2 `shouldBe` Balance 3000
                posRealizedPnL pos2 `shouldBe` Balance 1000

            it "flips from Short to Long with correct PnL and cost basis" $ do
                -- Short 100 shares, cost basis $40 (received $40)
                -- Buy 150 shares total, pay $90
                -- Closed portion: 100 shares
                -- Proportional CF for closed: -$90 * 100/150 = -$60
                -- Released cost: $40
                -- Realized PnL: -$60 + $40 = -$20 loss
                -- Remaining Long 50 shares, cost basis: $90 * 50/150 = $30
                let pos1 = mkShortPosition 100 4000
                    tx = Transaction
                        { txSide = Long
                        , txQuantity = Quantity 150
                        , txCashFlow = Balance (-9000)
                        , txPriceBefore = 0.60
                        , txPriceAfter = 0.62
                        , txMarketQBefore = 100
                        , txMarketQAfter = -50
                        }
                    pos2 = applyTransaction tx pos1
                posSide pos2 `shouldBe` Just Long
                posQuantity pos2 `shouldBe` Quantity 50
                -- BUG: Current implementation gives wrong cost basis
                posCostBasis pos2 `shouldBe` Balance 3000
                posRealizedPnL pos2 `shouldBe` Balance (-2000)

        describe "resolvePosition" do
            it "resolves Long winner correctly" $ do
                let pos = mkLongPosition 100 5000
                    resolved = resolvePosition True pos
                posSide resolved `shouldBe` Nothing
                posQuantity resolved `shouldBe` Quantity 0
                posCostBasis resolved `shouldBe` Balance 0
                -- Payout = 100 * 100 cents = 10000 cents, Profit = 10000 - 5000 = 5000
                posRealizedPnL resolved `shouldBe` Balance 5000

            it "resolves Long loser correctly" $ do
                let pos = mkLongPosition 100 5000
                    resolved = resolvePosition False pos
                posSide resolved `shouldBe` Nothing
                posQuantity resolved `shouldBe` Quantity 0
                posCostBasis resolved `shouldBe` Balance 0
                -- Payout = 0, Loss = 0 - 5000 = -5000
                posRealizedPnL resolved `shouldBe` Balance (-5000)

            it "resolves Short winner (Long loses) correctly" $ do
                let pos = mkShortPosition 100 4000
                    resolved = resolvePosition False pos
                posSide resolved `shouldBe` Nothing
                posQuantity resolved `shouldBe` Quantity 0
                posCostBasis resolved `shouldBe` Balance 0
                -- Payout = 100 * 100 = 10000, Profit = 10000 - 4000 = 6000
                posRealizedPnL resolved `shouldBe` Balance 6000

            it "resolves Short loser (Long wins) correctly" $ do
                let pos = mkShortPosition 100 4000
                    resolved = resolvePosition True pos
                posSide resolved `shouldBe` Nothing
                posQuantity resolved `shouldBe` Quantity 0
                posCostBasis resolved `shouldBe` Balance 0
                -- Payout = 0, Loss = 0 - 4000 = -4000
                posRealizedPnL resolved `shouldBe` Balance (-4000)

            it "resolves flat position as unchanged" $ do
                let resolved = resolvePosition True emptyPosition
                resolved `shouldBe` emptyPosition

        describe "refundPosition" do
            it "returns cost basis plus realized PnL" $ do
                let pos = (mkLongPosition 100 5000) { posRealizedPnL = Balance 1000 }
                refundPosition pos `shouldBe` Balance 6000

            it "returns only cost basis when no realized PnL" $ do
                refundPosition (mkLongPosition 100 5000) `shouldBe` Balance 5000

        describe "positionValue" do
            it "calculates Long position value correctly" $ do
                let pos = mkLongPosition 100 5000
                positionValue pos 0.60 `shouldBe` Just (Balance 6000)

            it "calculates Short position value correctly" $ do
                let pos = mkShortPosition 100 4000
                positionValue pos 0.60 `shouldBe` Just (Balance 4000)  -- 100 * (1-0.6) * 100

            it "returns Nothing for flat position" $ do
                positionValue emptyPosition 0.50 `shouldBe` Nothing

    describe "LMSR Operations" do
        describe "price" do
            it "returns price between 0 and 1" $ do
                let state = precompute 100 [("YES", 100), ("NO", 50)]
                    p = price "YES" state
                p >= 0.0 && p <= 1.0 `shouldBe` True

            it "prices sum to 1 for binary market" $ do
                let state = precompute 100 [("YES", 100), ("NO", 50)]
                    pYes = price "YES" state
                    pNo = price "NO" state
                abs (pYes + pNo - 1.0) < 0.0001 `shouldBe` True

        describe "calculateBuyCost" do
            it "buying increases cost with market impact" $ do
                let cost1 = calculateBuyCost 1 0.50 100
                    cost10 = calculateBuyCost 10 0.50 100
                    cost100 = calculateBuyCost 100 0.50 100
                cost1 > 0 && cost10 > cost1 && cost100 > cost10 `shouldBe` True

            it "cost is positive" $ do
                let cost = calculateBuyCost 50 0.50 100
                cost > 0 `shouldBe` True

        describe "calculateSellRevenue" do
            it "selling generates revenue" $ do
                let revenue = calculateSellRevenue 50 0.50 100
                revenue > 0 `shouldBe` True

        describe "round-trip invariance" do
            it "buying then selling same quantity costs money (spread)" $ do
                let qty = 50
                    price' = 0.50
                    beta = 100
                    buyCost = calculateBuyCost qty price' beta
                    sellRevenue = calculateSellRevenue qty price' beta
                -- Due to LMSR market impact, buying costs more than selling generates
                buyCost > sellRevenue `shouldBe` True

    describe "Property Tests" do
        describe "Position invariants" do
            it "quantity is always non-negative after any operation" $ do
                property $ \ (qty1' :: Word) (qty2' :: Word) (cf1' :: Word) (cf2' :: Word) ->
                    let qty1 = max 1 (fromIntegral qty1')  -- Ensure at least 1
                        qty2 = fromIntegral qty2'
                        cf1 = fromIntegral cf1'
                        cf2 = fromIntegral cf2'
                        pos1 = mkLongPosition qty1 cf1
                        tx = Transaction
                            { txSide = Short
                            , txQuantity = Quantity qty2
                            , txCashFlow = Balance cf2
                            , txPriceBefore = 0.5
                            , txPriceAfter = 0.5
                            , txMarketQBefore = qty1
                            , txMarketQAfter = qty1 - qty2
                            }
                        pos2 = applyTransaction tx pos1
                        Quantity finalQty = posQuantity pos2
                    in finalQty >= 0

            it "cost basis is always non-negative" $ do
                property $ \ (qty :: Integer) (cost :: Integer) ->
                    qty >= 0 && cost >= 0 ==>
                    let pos = mkLongPosition qty cost
                        Balance cb = posCostBasis pos
                    in cb >= 0
