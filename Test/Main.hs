{-# LANGUAGE OverloadedStrings #-}
module Test.Main where

import Application.Domain.ChartData (AssetChartData (..), PricePoint (..),
                                     buildChartData)
import Application.Domain.LMSR
import Application.Domain.Position (Side (Long, Short), currentPnL,
                                    positionSide, positionValue,
                                    resolutionPayout)
import qualified Application.Domain.Types as NewDomain
import Application.Helper.Formatting (formatMoney, formatMoneyOrDash,
                                      formatMoneySigned, formatPricePercent,
                                      formatWithSep)
import Application.Helper.Navigation (sanitizeBackTo)
import Application.Helper.Pagination (PaginationItem (Ellipsis, PageNumber),
                                      generatePaginationItems)
import Application.Helper.Passkeys (allowedOrigins,
                                    authenticationCredentialOptions,
                                    passkeyRelyingPartyName,
                                    registrationCredentialOptions,
                                    rpIdTextFromHost, rpIdTextFromRequest,
                                    userHandleForUserId)
import Application.Helper.QueryParams (normalizeOptionalTextParam,
                                       normalizePageParam, normalizeSearchQuery,
                                       parseBooleanText)
import Application.Helper.Text (textParagraphs)
import Application.Market.Input (normalizeChatMessageBody,
                                 sanitizeTradeQuantity, sanitizeTradingAction,
                                 validateAssetNames, validateAssetSymbols,
                                 validateChatMessageBody)
import Application.Market.State (buildMarketState, parseMarketState)
import Crypto.WebAuthn.Cose.SignAlg
import Crypto.WebAuthn.Model.Types (AttestationConveyancePreference (AttestationConveyancePreferenceNone),
                                    AuthenticatorSelectionCriteria (AuthenticatorSelectionCriteria),
                                    Challenge (Challenge),
                                    CredentialOptions (CredentialOptionsAuthentication, CredentialOptionsRegistration),
                                    CredentialParameters (CredentialParameters),
                                    CredentialRpEntity (CredentialRpEntity),
                                    CredentialType (CredentialTypePublicKey),
                                    CredentialUserEntity (CredentialUserEntity),
                                    Origin (Origin),
                                    RelyingPartyName (RelyingPartyName),
                                    ResidentKeyRequirement (ResidentKeyRequirementPreferred),
                                    RpId (RpId), Timeout (Timeout),
                                    UserAccountDisplayName (UserAccountDisplayName),
                                    UserAccountName (UserAccountName),
                                    UserVerificationRequirement (UserVerificationRequirementPreferred),
                                    unOrigin)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as ByteString
import qualified Data.CaseInsensitive as CI

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust)
import qualified Data.Text as Text
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime (..), secondsToDiffTime)
import qualified Data.UUID as UUID
import Generated.Types
import IHP.ModelSupport (newRecord)
import IHP.Prelude
import IHP.RouterPrelude (pathTo)
import IHP.ValidationSupport (getValidationFailure)
import Network.Wai (Request, defaultRequest, isSecure, requestHeaderHost,
                    requestHeaders)
import Test.Hspec
import Test.QuickCheck
import Unsafe.Coerce
import Web.Controller.Leaderboard (annualRateOfReturn, leaderboardScore,
                                   portfolioReturn, shouldDisplayAnnualReturn,
                                   yearsSinceRegistration)
import Web.Controller.Markets (buildMarket)
import Web.Controller.Trades ()
import Web.Types (MarketsController (..), StaticController (..))
import Web.View.Markets.Index (MarketIndexStatusFilter (..), buildMarketsPath,
                               parseMarketIndexStatusFilter)

testBeta :: Integer
testBeta = 300

testYesAsset :: Id Asset
testYesAsset = unsafeCoerce ("test-yes-001" :: Text)

testNoAsset :: Id Asset
testNoAsset = unsafeCoerce ("test-no-002" :: Text)

mkTestAsset :: Text -> Text -> Integer -> Asset
mkTestAsset name symbol quantity =
    newRecord @Asset
        |> set #id (unsafeCoerce (name <> "-" <> symbol) :: Id Asset)
        |> set #name name
        |> set #symbol symbol
        |> set #quantity quantity

mkUuidAsset :: Text -> Text -> Integer -> Text -> Asset
mkUuidAsset name symbol quantity uuidText =
    newRecord @Asset
        |> set #id (unsafeCoerce (fromJust (UUID.fromText uuidText)) :: Id Asset)
        |> set #name name
        |> set #symbol symbol
        |> set #quantity quantity

mkTransactionAt
    :: Id Asset
    -> UTCTime
    -> Aeson.Value
    -> Transaction
mkTransactionAt assetId createdAt marketState =
    newRecord @Transaction
        |> set #assetId assetId
        |> set #createdAt createdAt
        |> set #marketState marketState

mkRequest :: Bool -> Maybe ByteString.ByteString -> [(ByteString.ByteString, ByteString.ByteString)] -> Request
mkRequest secure host headers =
    defaultRequest
        { isSecure = secure
        , requestHeaderHost = host
        , requestHeaders = map (\(name, value) -> (CI.mk name, value)) headers
        }

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
                    path = pathTo ShowMarketAction
                        { marketId = marketId
                        , tradingAssetId = Nothing
                        , tradingAction = Nothing
                        , showChart = Nothing
                        , showDescription = Nothing
                        , showAllAssets = Nothing
                        , showTradeHistory = Nothing
                        , activityPage = Nothing
                        , chatPage = Nothing
                        , chatComposerRev = Nothing
                        , tradeQuantity = Nothing
                        , backTo = Just backToPath
                        }
                path `shouldSatisfy` Text.isInfixOf "backTo=%2FMarkets%3Fcategory%3Dtest-category%26status%3Drefunded%26search%3Done"
                path `shouldSatisfy` not . Text.isInfixOf "&search=one"

            it "includes chat state parameters when present" do
                let marketId = unsafeCoerce ("test-market-001" :: Text) :: Id Market
                    path = pathTo ShowMarketAction
                        { marketId = marketId
                        , tradingAssetId = Nothing
                        , tradingAction = Nothing
                        , showChart = Nothing
                        , showDescription = Nothing
                        , showAllAssets = Nothing
                        , showTradeHistory = Nothing
                        , activityPage = Nothing
                        , chatPage = Just 3
                        , chatComposerRev = Just "rev-123"
                        , tradeQuantity = Nothing
                        , backTo = Nothing
                        }
                path `shouldSatisfy` Text.isInfixOf "chatPage=3"
                path `shouldSatisfy` Text.isInfixOf "chatComposerRev=rev-123"

            it "includes trade quantity when present" do
                let marketId = unsafeCoerce ("test-market-001" :: Text) :: Id Market
                    path = pathTo ShowMarketAction
                        { marketId = marketId
                        , tradingAssetId = Nothing
                        , tradingAction = Nothing
                        , showChart = Nothing
                        , showDescription = Nothing
                        , showAllAssets = Nothing
                        , showTradeHistory = Nothing
                        , activityPage = Nothing
                        , chatPage = Nothing
                        , chatComposerRev = Nothing
                        , tradeQuantity = Just 37
                        , backTo = Nothing
                        }
                path `shouldSatisfy` Text.isInfixOf "tradeQuantity=37"

            it "URL-encodes dashboard backTo paths with page and search state" do
                let marketId = unsafeCoerce ("test-market-001" :: Text) :: Id Market
                    backToPath = "/DashboardPositions?page=3&search=gold"
                    path = pathTo ShowMarketAction
                        { marketId = marketId
                        , tradingAssetId = Nothing
                        , tradingAction = Nothing
                        , showChart = Nothing
                        , showDescription = Nothing
                        , showAllAssets = Nothing
                        , showTradeHistory = Nothing
                        , activityPage = Nothing
                        , chatPage = Nothing
                        , chatComposerRev = Nothing
                        , tradeQuantity = Nothing
                        , backTo = Just backToPath
                        }
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

        describe "Static page routes" do
            it "builds the public information and legal routes" do
                pathTo AboutAction `shouldBe` "/about"
                pathTo HowItWorksAction `shouldBe` "/how-it-works"
                pathTo CommunityRulesAction `shouldBe` "/community-rules"
                pathTo TermsAction `shouldBe` "/terms-of-service"
                pathTo PrivacyPolicyAction `shouldBe` "/privacy-policy"
                pathTo CookiePolicyAction `shouldBe` "/cookie-policy"
                pathTo ModerationPolicyAction `shouldBe` "/moderation-policy"
                pathTo LegalNoticeAction `shouldBe` "/legal-notice"

        describe "passkey helpers" do
            it "extracts the RP id from hosts with ports" do
                rpIdTextFromHost "localhost:8000" `shouldBe` "localhost"
                rpIdTextFromHost "predimarkt.example" `shouldBe` "predimarkt.example"

            it "prefers forwarded host and proto when building request-derived values" do
                let req = mkRequest False (Just "ignored.example:80")
                        [ ("x-forwarded-host", "app.predimarkt.example:443")
                        , ("x-forwarded-proto", "https")
                        ]
                let ?request = req in do
                    rpIdTextFromRequest `shouldBe` "app.predimarkt.example"
                    fmap unOrigin (NonEmpty.toList allowedOrigins)
                        `shouldBe` ["https://app.predimarkt.example:443"]

            it "falls back to host header and request security when forwarded headers are missing" do
                let req = mkRequest True (Just "predimarkt.example:8443") []
                let ?request = req in do
                    rpIdTextFromRequest `shouldBe` "predimarkt.example"
                    fmap unOrigin (NonEmpty.toList allowedOrigins)
                        `shouldBe` ["https://predimarkt.example:8443"]

            it "falls back to localhost when no host information is present" do
                let req = mkRequest False Nothing []
                let ?request = req in do
                    rpIdTextFromRequest `shouldBe` "localhost"
                    fmap unOrigin (NonEmpty.toList allowedOrigins)
                        `shouldBe` ["http://localhost:8000"]

            it "builds registration options from the derived RP id and user data" do
                let req = mkRequest False (Just "predimarkt.example:8000") []
                    challenge = Challenge "test-challenge"
                    userId = unsafeCoerce ("test-user-001" :: Text) :: Id User
                    nickname = "alice"
                let ?request = req in do
                    let options =
                            registrationCredentialOptions challenge userId nickname []
                    case options of
                        CredentialOptionsRegistration rpEntity userEntity actualChallenge algorithms timeout excludeCredentials authSelection attestation extensions -> do
                            rpEntity
                                `shouldBe` CredentialRpEntity
                                    (Just (RpId "predimarkt.example"))
                                    passkeyRelyingPartyName
                            userEntity
                                `shouldBe` CredentialUserEntity
                                    (userHandleForUserId userId)
                                    (UserAccountDisplayName nickname)
                                    (UserAccountName nickname)
                            actualChallenge `shouldBe` challenge
                            algorithms
                                `shouldBe`
                                    [ CredentialParameters
                                        CredentialTypePublicKey
                                        (CoseSignAlgECDSA CoseHashAlgECDSASHA256)
                                    , CredentialParameters
                                        CredentialTypePublicKey
                                        CoseSignAlgEdDSA
                                    , CredentialParameters
                                        CredentialTypePublicKey
                                        (CoseSignAlgRSA CoseHashAlgRSASHA256)
                                    ]
                            timeout `shouldBe` Just (Timeout 60000)
                            excludeCredentials `shouldBe` []
                            authSelection
                                `shouldBe` Just
                                    (AuthenticatorSelectionCriteria
                                        Nothing
                                        ResidentKeyRequirementPreferred
                                        UserVerificationRequirementPreferred
                                    )
                            attestation `shouldBe` AttestationConveyancePreferenceNone
                            extensions `shouldBe` Nothing

            it "builds authentication options from the derived RP id" do
                let req = mkRequest False (Just "predimarkt.example:8000") []
                    challenge = Challenge "test-auth-challenge"
                let ?request = req in do
                    let options = authenticationCredentialOptions challenge
                    case options of
                        CredentialOptionsAuthentication actualChallenge timeout rpId allowCredentials userVerification extensions -> do
                            actualChallenge `shouldBe` challenge
                            timeout `shouldBe` Just (Timeout 60000)
                            rpId `shouldBe` Just (RpId "predimarkt.example")
                            allowCredentials `shouldBe` []
                            userVerification `shouldBe` UserVerificationRequirementPreferred
                            extensions `shouldBe` Nothing

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

        describe "Market input helpers" do
            it "rejects empty and duplicate asset symbols" do
                validateAssetSymbols
                    [ mkTestAsset "One" "  " 0
                    , mkTestAsset "Two" "TWO" 0
                    ]
                    `shouldBe` Just "Asset symbols cannot be empty"

                validateAssetSymbols
                    [ mkTestAsset "One" "YES" 0
                    , mkTestAsset "Two" "YES" 0
                    ]
                    `shouldBe` Just "Asset symbols must be unique within the market"

            it "rejects empty and duplicate asset names" do
                validateAssetNames
                    [ mkTestAsset "  " "ONE" 0
                    , mkTestAsset "Two" "TWO" 0
                    ]
                    `shouldBe` Just "Asset names cannot be empty"

                validateAssetNames
                    [ mkTestAsset "Outcome" "ONE" 0
                    , mkTestAsset "Outcome" "TWO" 0
                    ]
                    `shouldBe` Just "Asset names must be unique within the market"

            it "accepts valid distinct asset names and symbols" do
                validateAssetSymbols
                    [ mkTestAsset "Yes" "YES" 0
                    , mkTestAsset "No" "NO" 0
                    ]
                    `shouldBe` Nothing
                validateAssetNames
                    [ mkTestAsset "Yes" "YES" 0
                    , mkTestAsset "No" "NO" 0
                    ]
                    `shouldBe` Nothing

            it "normalizes and validates chat message bodies" do
                normalizeChatMessageBody "  hello  " `shouldBe` "hello"
                validateChatMessageBody "" `shouldBe` Just "Please enter a message"
                validateChatMessageBody "hello\nworld" `shouldBe` Nothing
                validateChatMessageBody (Text.replicate 2001 "a")
                    `shouldBe` Just "Message must be at most 2000 characters"
                validateChatMessageBody (Text.replicate 2000 "a")
                    `shouldBe` Nothing

            it "sanitizes trade quantity and trading action" do
                sanitizeTradeQuantity (Just 0) `shouldBe` Just 0
                sanitizeTradeQuantity (Just 42) `shouldBe` Just 42
                sanitizeTradeQuantity (Just (-1)) `shouldBe` Nothing
                sanitizeTradeQuantity Nothing `shouldBe` Nothing

                sanitizeTradingAction (Just "buy") `shouldBe` Just "buy"
                sanitizeTradingAction (Just "sell") `shouldBe` Just "sell"
                sanitizeTradingAction (Just "hold") `shouldBe` Nothing
                sanitizeTradingAction Nothing `shouldBe` Nothing

        describe "Query param helpers" do
            it "normalizes search and optional text params" do
                normalizeSearchQuery (Just "  inflation  ")
                    `shouldBe` Just "inflation  "
                normalizeSearchQuery (Just "   ") `shouldBe` Nothing
                normalizeSearchQuery Nothing `shouldBe` Nothing

                normalizeOptionalTextParam (Just "  rev-123  ")
                    `shouldBe` Just "rev-123"
                normalizeOptionalTextParam (Just "   ") `shouldBe` Nothing
                normalizeOptionalTextParam Nothing `shouldBe` Nothing

            it "normalizes page params and parses boolean text" do
                normalizePageParam 1 `shouldBe` Nothing
                normalizePageParam 2 `shouldBe` Just 2
                normalizePageParam 0 `shouldBe` Nothing

                parseBooleanText (Just "true") `shouldBe` Just True
                parseBooleanText (Just "FALSE") `shouldBe` Just False
                parseBooleanText (Just "1") `shouldBe` Just True
                parseBooleanText (Just "0") `shouldBe` Just False
                parseBooleanText (Just "maybe") `shouldBe` Nothing
                parseBooleanText Nothing `shouldBe` Nothing

        describe "Market state helpers" do
            it "round-trips market state through JSON" do
                let assetOne = unsafeCoerce <$> UUID.fromText
                        "11111111-1111-1111-1111-111111111111" :: Maybe (Id Asset)
                    assetTwo = unsafeCoerce <$> UUID.fromText
                        "22222222-2222-2222-2222-222222222222" :: Maybe (Id Asset)
                    state = M.fromList
                        [ (fromJust assetOne, NewDomain.Quantity 15)
                        , (fromJust assetTwo, NewDomain.Quantity (-7))
                        ]
                parseMarketState (Aeson.toJSON (buildMarketState state))
                    `shouldBe` Just state

            it "rejects invalid shapes and skips invalid UUID keys" do
                parseMarketState (Aeson.toJSON ["not-an-object" :: Text])
                    `shouldBe` Nothing

                let partiallyValid = Aeson.toJSON
                        [ ("not-a-uuid" :: Text, 5 :: Int)
                        , ("33333333-3333-3333-3333-333333333333" :: Text, 9 :: Int)
                        ]
                    expectedAssetId = unsafeCoerce <$> UUID.fromText
                        "33333333-3333-3333-3333-333333333333" :: Maybe (Id Asset)
                parseMarketState partiallyValid
                    `shouldBe` Just
                        (M.fromList [(fromJust expectedAssetId, NewDomain.Quantity 9)])

        describe "Chart data helpers" do
            it "returns only the top 6 assets with a flat series when there are no transactions" do
                let assets =
                        [ mkUuidAsset "A1" "A1" 100 "11111111-1111-1111-1111-111111111111"
                        , mkUuidAsset "A2" "A2" 90 "22222222-2222-2222-2222-222222222222"
                        , mkUuidAsset "A3" "A3" 80 "33333333-3333-3333-3333-333333333333"
                        , mkUuidAsset "A4" "A4" 70 "44444444-4444-4444-4444-444444444444"
                        , mkUuidAsset "A5" "A5" 60 "55555555-5555-5555-5555-555555555555"
                        , mkUuidAsset "A6" "A6" 50 "66666666-6666-6666-6666-666666666666"
                        , mkUuidAsset "A7" "A7" 40 "77777777-7777-7777-7777-777777777777"
                        ]
                    builtChartData = buildChartData
                        (fromGregorian 2026 1 10)
                        (fromGregorian 2026 1 10)
                        assets
                        300
                        []
                fmap chartAssetName builtChartData `shouldBe` ["A1", "A2", "A3", "A4", "A5", "A6"]
                fmap (length . chartData) builtChartData `shouldBe` replicate 6 1

            it "fills missing days from the last known prices and clamps to the end day" do
                let assetA = mkUuidAsset "Alpha" "ALP" 100 "aaaaaaaa-aaaa-aaaa-aaaa-aaaaaaaaaaaa"
                    assetB = mkUuidAsset "Beta" "BET" 0 "bbbbbbbb-bbbb-bbbb-bbbb-bbbbbbbbbbbb"
                    assets = [assetA, assetB]
                    stateDay1 = M.fromList
                        [ (get #id assetA, NewDomain.Quantity 30)
                        , (get #id assetB, NewDomain.Quantity 0)
                        ]
                    stateDay4 = M.fromList
                        [ (get #id assetA, NewDomain.Quantity 0)
                        , (get #id assetB, NewDomain.Quantity 30)
                        ]
                    txns =
                        [ mkTransactionAt
                            (get #id assetA)
                            (UTCTime (fromGregorian 2026 1 1) 0)
                            (Aeson.toJSON (buildMarketState stateDay1))
                        , mkTransactionAt
                            (get #id assetA)
                            (UTCTime (fromGregorian 2026 1 4) 0)
                            (Aeson.toJSON (buildMarketState stateDay4))
                        ]
                    built = buildChartData
                        (fromGregorian 2026 1 3)
                        (fromGregorian 2026 1 3)
                        assets
                        300
                        txns
                    alphaSeries =
                        chartData (fromJust (head (filter (\entry -> chartAssetName entry == "Alpha") built)))
                    alphaPrices = fmap priceValue alphaSeries
                length alphaSeries `shouldBe` 3
                alphaPrices !! 1 `shouldBe` alphaPrices !! 0
                alphaPrices !! 2 `shouldBe` alphaPrices !! 0

            it "falls back to current prices when a day's market state is invalid" do
                let assetA = mkUuidAsset "Alpha" "ALP" 100 "aaaaaaaa-aaaa-aaaa-aaaa-aaaaaaaaaaaa"
                    assetB = mkUuidAsset "Beta" "BET" 0 "bbbbbbbb-bbbb-bbbb-bbbb-bbbbbbbbbbbb"
                    assets = [assetA, assetB]
                    currentPrices = allAssetPrices (NewDomain.Beta 300)
                        (M.fromList
                            [ (get #id assetA, NewDomain.Quantity 100)
                            , (get #id assetB, NewDomain.Quantity 0)
                            ])
                    txns =
                        [ mkTransactionAt
                            (get #id assetA)
                            (UTCTime (fromGregorian 2026 1 1) 0)
                            (Aeson.toJSON ["invalid" :: Text])
                        ]
                    built = buildChartData
                        (fromGregorian 2026 1 1)
                        (fromGregorian 2026 1 1)
                        assets
                        300
                        txns
                    alphaSeries =
                        chartData (fromJust (head (filter (\entry -> chartAssetName entry == "Alpha") built)))
                fmap priceValue alphaSeries
                    `shouldBe` [currentPrices M.! get #id assetA]

        describe "Formatting helpers" do
            it "formats money and signed money values" do
                formatMoney (123456 :: Integer) `shouldBe` "1'234.56"
                formatMoney ((-123456) :: Integer) `shouldBe` "-1'234.56"
                formatMoneySigned (500 :: Integer) `shouldBe` "+5.00"
                formatMoneySigned ((-500) :: Integer) `shouldBe` "-5.00"
                formatMoneyOrDash (0 :: Integer) `shouldBe` "--"
                formatWithSep (1234567 :: Integer) `shouldBe` "1'234'567"

            it "formats price percentages" do
                formatPricePercent 0.123 `shouldBe` "12.3%"

        describe "Pagination helpers" do
            it "returns all pages for short paginations" do
                generatePaginationItems 3 5
                    `shouldBe` map PageNumber [1, 2, 3, 4, 5]

            it "builds early, middle, and late pagination windows" do
                generatePaginationItems 3 20
                    `shouldBe`
                        (map PageNumber [1 .. 8]
                            <> [Ellipsis 13]
                            <> map PageNumber [19, 20])

                generatePaginationItems 10 20
                    `shouldBe`
                        [ PageNumber 1, PageNumber 2, Ellipsis 5
                        , PageNumber 8, PageNumber 9, PageNumber 10
                        , PageNumber 11, PageNumber 12
                        , Ellipsis 15, PageNumber 19, PageNumber 20
                        ]

                generatePaginationItems 18 20
                    `shouldBe`
                        ([PageNumber 1, PageNumber 2, Ellipsis 7]
                            <> map PageNumber [13 .. 20])

        describe "Market index status filters" do
            it "defaults to popular for missing or unknown params" do
                parseMarketIndexStatusFilter Nothing
                    `shouldBe` MarketIndexStatusPopular
                parseMarketIndexStatusFilter (Just "unexpected")
                    `shouldBe` MarketIndexStatusPopular

            it "parses all explicit status filter options" do
                parseMarketIndexStatusFilter (Just "newest")
                    `shouldBe` MarketIndexStatusNewest
                parseMarketIndexStatusFilter (Just "closed")
                    `shouldBe` MarketIndexStatusClosed
                parseMarketIndexStatusFilter (Just "resolved")
                    `shouldBe` MarketIndexStatusResolved
                parseMarketIndexStatusFilter (Just "refunded")
                    `shouldBe` MarketIndexStatusRefunded

            it "builds market index paths with status, category, search, and page" do
                let categoryId = unsafeCoerce ("test-category" :: Text) :: Id Category
                    path = buildMarketsPath
                        (Just categoryId)
                        MarketIndexStatusResolved
                        (Just "inflation")
                        (Just 3)
                path `shouldSatisfy` Text.isPrefixOf "/Markets?category="
                path `shouldSatisfy`
                    Text.isInfixOf "&status=resolved&search=inflation&page=3"

            it "omits the status param for the default filter" do
                buildMarketsPath
                    Nothing
                    MarketIndexStatusPopular
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

        describe "Position helpers" do
            it "computes position side, value, pnl, and resolution payouts" do
                positionSide 5 `shouldBe` Just Long
                positionSide (-5) `shouldBe` Just Short
                positionSide 0 `shouldBe` Nothing

                let qtyMap = M.fromList
                        [ (testYesAsset, NewDomain.Quantity 100)
                        , (testNoAsset, NewDomain.Quantity 0)
                        ]
                    beta = NewDomain.Beta testBeta
                positionValue testYesAsset (NewDomain.Quantity 0) beta qtyMap
                    `shouldBe` NewDomain.Money 0
                positionValue testYesAsset (NewDomain.Quantity 10) beta qtyMap
                    `shouldBe` tradeValue testYesAsset (NewDomain.Quantity (-10)) beta qtyMap

                currentPnL (NewDomain.Money 1500) (NewDomain.Money (-1000)) (NewDomain.Money 200)
                    `shouldBe` NewDomain.Money 2700

                resolutionPayout (NewDomain.Quantity 7) Long True
                    `shouldBe` NewDomain.Money 700
                resolutionPayout (NewDomain.Quantity 7) Long False
                    `shouldBe` NewDomain.Money 0
                resolutionPayout (NewDomain.Quantity 7) Short True
                    `shouldBe` NewDomain.Money 0
                resolutionPayout (NewDomain.Quantity 7) Short False
                    `shouldBe` NewDomain.Money (-700)

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
