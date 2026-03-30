module Web.Routes where

import qualified Data.ByteString as ByteString
import qualified Data.Char as Char
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Word (Word8)
import IHP.ModelSupport
import IHP.Prelude
import IHP.RouterPrelude
import Numeric (showHex)
import Web.Types

-- Generator Marker
instance AutoRoute SessionsController
instance AutoRoute UsersController

instance HasPath PasskeysController where
    pathTo UpdatePasskeyNameAction { passkeyId } =
        "/UpdatePasskeyName?passkeyId=" <> inputValue passkeyId
    pathTo DeletePasskeyAction { passkeyId } =
        "/DeletePasskey?passkeyId=" <> inputValue passkeyId

instance CanRoute PasskeysController where
    parseRoute' =
        (string "/UpdatePasskeyName" >> onlyAllowMethods [POST] >> pure (UpdatePasskeyNameAction def))
        <|> (string "/DeletePasskey" >> onlyAllowMethods [POST] >> pure (DeletePasskeyAction def))

instance AutoRoute CategoriesController
instance AutoRoute AssetsController
instance AutoRoute TradesController
instance AutoRoute LeaderboardController

instance HasPath StaticController where
    pathTo AboutAction            = "/about"
    pathTo HowItWorksAction       = "/how-it-works"
    pathTo CommunityRulesAction   = "/community-rules"
    pathTo TermsAction            = "/terms-of-service"
    pathTo PrivacyPolicyAction    = "/privacy-policy"
    pathTo CookiePolicyAction     = "/cookie-policy"
    pathTo ModerationPolicyAction = "/moderation-policy"
    pathTo LegalNoticeAction      = "/legal-notice"

instance CanRoute StaticController where
    parseRoute' =
        (string "/about" >> pure AboutAction)
        <|> (string "/how-it-works" >> pure HowItWorksAction)
        <|> (string "/community-rules" >> pure CommunityRulesAction)
        <|> (string "/terms-of-service" >> pure TermsAction)
        <|> (string "/privacy-policy" >> pure PrivacyPolicyAction)
        <|> (string "/cookie-policy" >> pure CookiePolicyAction)
        <|> (string "/moderation-policy" >> pure ModerationPolicyAction)
        <|> (string "/legal-notice" >> pure LegalNoticeAction)

instance HasPath AuthController where
    pathTo LoginAction                       = "/NewSession"
    pathTo BeginPasskeyRegistrationAction    = "/passkeys/registration/begin"
    pathTo FinishPasskeyRegistrationAction   = "/passkeys/registration/finish"
    pathTo BeginPasskeyAuthenticationAction  = "/passkeys/authentication/begin"
    pathTo FinishPasskeyAuthenticationAction = "/passkeys/authentication/finish"

instance CanRoute AuthController where
    parseRoute' =
        (string "/NewSession" >> pure LoginAction)
        <|> (string "/passkeys/registration/begin" >> onlyAllowMethods [POST] >> pure BeginPasskeyRegistrationAction)
        <|> (string "/passkeys/registration/finish" >> onlyAllowMethods [POST] >> pure FinishPasskeyRegistrationAction)
        <|> (string "/passkeys/authentication/begin" >> onlyAllowMethods [POST] >> pure BeginPasskeyAuthenticationAction)
        <|> (string "/passkeys/authentication/finish" >> onlyAllowMethods [POST] >> pure FinishPasskeyAuthenticationAction)

instance HasPath MarketsController where
    pathTo MarketsAction            = "/Markets"
    pathTo NewMarketAction          = "/NewMarket"
    pathTo CreateMarketAction       = "/CreateMarket"
    pathTo ShowMarketAction { marketId, tradingAssetId, tradingAction, showChart, showDescription, showAllAssets, showTradeHistory, activityPage, chatPage, chatComposerRev, tradeQuantity, backTo } =
        "/ShowMarket"
            |> addQueryParam "marketId" (Just $ inputValue marketId)
            |> addQueryParam "tradingAssetId" (inputValue <$> tradingAssetId)
            |> addQueryParam "tradingAction" (inputValue <$> tradingAction)
            |> addMarketFlag "showChart" showChart
            |> addMarketFlag "showDescription" showDescription
            |> addMarketFlag "showAllAssets" showAllAssets
            |> addMarketFlag "showTradeHistory" showTradeHistory
            |> addQueryParam "activityPage" (inputValue <$> activityPage)
            |> addQueryParam "chatPage" (inputValue <$> chatPage)
            |> addQueryParam "chatComposerRev" (inputValue <$> chatComposerRev)
            |> addQueryParam "tradeQuantity" (inputValue <$> tradeQuantity)
            |> addQueryParam "backTo" backTo
    pathTo CreateMarketChatMessageAction { marketId } = "/CreateMarketChatMessage?marketId=" <> inputValue marketId
    pathTo DeleteMarketChatMessageAction { marketChatMessageId, marketId, tradingAssetId, tradingAction, showChart, showDescription, showAllAssets, showTradeHistory, activityPage, chatPage, chatComposerRev, tradeQuantity, backTo } =
        "/DeleteMarketChatMessage"
            |> addQueryParam "marketChatMessageId" (Just $ inputValue marketChatMessageId)
            |> addQueryParam "marketId" (Just $ inputValue marketId)
            |> addQueryParam "tradingAssetId" (inputValue <$> tradingAssetId)
            |> addQueryParam "tradingAction" (inputValue <$> tradingAction)
            |> addMarketFlag "showChart" showChart
            |> addMarketFlag "showDescription" showDescription
            |> addMarketFlag "showAllAssets" showAllAssets
            |> addMarketFlag "showTradeHistory" showTradeHistory
            |> addQueryParam "activityPage" (inputValue <$> activityPage)
            |> addQueryParam "chatPage" (inputValue <$> chatPage)
            |> addQueryParam "chatComposerRev" (inputValue <$> chatComposerRev)
            |> addQueryParam "tradeQuantity" (inputValue <$> tradeQuantity)
            |> addQueryParam "backTo" backTo
    pathTo EditMarketAction { marketId, page, searchFilter } =
        "/EditMarket"
            |> addQueryParam "marketId" (Just $ inputValue marketId)
            |> addQueryParam "page" (inputValue <$> page)
            |> addQueryParam "search" searchFilter
    pathTo UpdateMarketAction { marketId } = "/UpdateMarket?marketId=" <> inputValue marketId
    pathTo DeleteMarketAction { marketId, page, searchFilter } =
        "/DeleteMarket"
            |> addQueryParam "marketId" (Just $ inputValue marketId)
            |> addQueryParam "page" (inputValue <$> page)
            |> addQueryParam "search" searchFilter
    pathTo SetResolveAssetAction { marketId } = "/SetResolveAsset?marketId=" <> inputValue marketId
    pathTo ConfirmRefundMarketAction { marketId } = "/ConfirmRefundMarket?marketId=" <> inputValue marketId

instance CanRoute MarketsController where
    parseRoute' =
        (string "/Markets" >> pure MarketsAction)
        <|> (string "/NewMarket" >> pure NewMarketAction)
        <|> (string "/CreateMarket" >> pure CreateMarketAction)
        <|> (string "/ShowMarket" >> pure (ShowMarketAction def Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing))
        <|> (string "/CreateMarketChatMessage" >> onlyAllowMethods [POST] >> pure (CreateMarketChatMessageAction def))
        <|> (string "/DeleteMarketChatMessage" >> onlyAllowMethods [POST] >> pure (DeleteMarketChatMessageAction def def Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing))
        <|> (string "/EditMarket" >> pure (EditMarketAction def Nothing Nothing))
        <|> (string "/UpdateMarket" >> pure (UpdateMarketAction def))
        <|> (string "/DeleteMarket" >> pure (DeleteMarketAction def Nothing Nothing))
        <|> (string "/SetResolveAsset" >> pure (SetResolveAssetAction def))
        <|> (string "/ConfirmRefundMarket" >> pure (ConfirmRefundMarketAction def))

instance HasPath DashboardController where
    pathTo DashboardPositionsAction { page, searchFilter } =
        "/DashboardPositions"
            |> addQueryParam "page" (inputValue <$> page)
            |> addQueryParam "search" searchFilter
    pathTo DashboardMarketsAction { statusFilter, page, searchFilter } =
        "/DashboardMarkets"
            |> addQueryParam "statusFilter" (inputValue <$> statusFilter)
            |> addQueryParam "page" (inputValue <$> page)
            |> addQueryParam "search" searchFilter
    pathTo DashboardTransactionsAction { page, searchFilter } =
        "/DashboardTransactions"
            |> addQueryParam "page" (inputValue <$> page)
            |> addQueryParam "search" searchFilter
    pathTo ChangeMarketStatusAction { marketId, status, page, searchFilter } =
        "/ChangeMarketStatus"
            |> addQueryParam "marketId" (inputValue <$> marketId)
            |> addQueryParam "status" (inputValue <$> status)
            |> addQueryParam "page" (inputValue <$> page)
            |> addQueryParam "search" searchFilter

instance CanRoute DashboardController where
    parseRoute' =
        (string "/DashboardPositions" >> pure (DashboardPositionsAction Nothing Nothing))
        <|> (string "/DashboardMarkets" >> pure (DashboardMarketsAction Nothing Nothing Nothing))
        <|> (string "/DashboardTransactions" >> pure (DashboardTransactionsAction Nothing Nothing))
        <|> (string "/ChangeMarketStatus" >> pure (ChangeMarketStatusAction Nothing Nothing Nothing Nothing))

addMarketFlag :: Text -> Maybe Bool -> Text -> Text
addMarketFlag name mValue base =
    case mValue of
        Nothing    -> base
        Just True  -> base <> separator <> name <> "=true"
        Just False -> base <> separator <> name <> "=false"
  where
    separator = if Text.any (== '?') base then "&" else "?"

encodeQueryValue :: Text -> Text
encodeQueryValue value = cs $ ByteString.foldr encodeByte "" (Text.encodeUtf8 value)
    where
        encodeByte :: Word8 -> String -> String
        encodeByte byte rest
            | isUnreserved char = char : rest
            | otherwise = '%' : renderHex byte <> rest
            where
                char = Char.chr (fromIntegral byte)

        isUnreserved :: Char -> Bool
        isUnreserved char =
            Char.isAsciiLower char
                || Char.isAsciiUpper char
                || Char.isDigit char
                || char `IHP.Prelude.elem` ['-', '.', '_', '~']

        renderHex :: Word8 -> String
        renderHex byte =
            case IHP.Prelude.map Char.toUpper (showHex byte "") of
                [singleDigit] -> ['0', singleDigit]
                digits        -> digits

addQueryParam :: Text -> Maybe Text -> Text -> Text
addQueryParam name mValue base =
    case mValue of
        Nothing -> base
        Just value | Text.null value -> base
        Just value -> base <> separator <> name <> "=" <> encodeQueryValue value
  where
    separator = if Text.any (== '?') base then "&" else "?"
