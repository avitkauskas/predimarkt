module Web.Routes where

import qualified Data.ByteString as ByteString
import qualified Data.Char as Char
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Word (Word8)
import Generated.Types (Asset)
import IHP.ModelSupport
import IHP.Prelude
import IHP.Router.DSL
import IHP.RouterPrelude
import Numeric (showHex)
import Web.Types

-- Generator Marker
instance AutoRoute SessionsController

instance HasPath UsersController where
    pathTo EditUserAction { userId } = "/EditUser?userId=" <> inputValue userId
    pathTo UpdateUserAction { userId } = "/UpdateUser?userId=" <> inputValue userId
    pathTo ConfirmDeletePasskeyAction { passkeyId } = "/ConfirmDeletePasskey?passkeyId=" <> inputValue passkeyId
    pathTo ConfirmDeleteUserAction { userId } = "/ConfirmDeleteUser?userId=" <> inputValue userId
    pathTo DeleteUserAction { userId } = "/DeleteUser?userId=" <> inputValue userId

instance CanRoute UsersController where
    parseRoute' =
        (string "/EditUser" >> pure (EditUserAction def))
        <|> (string "/UpdateUser" >> onlyAllowMethods [POST] >> pure (UpdateUserAction def))
        <|> (string "/ConfirmDeletePasskey" >> pure (ConfirmDeletePasskeyAction def))
        <|> (string "/ConfirmDeleteUser" >> pure (ConfirmDeleteUserAction def))
        <|> (string "/DeleteUser" >> onlyAllowMethods [POST] >> pure (DeleteUserAction def))

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

[routes|StaticController
GET /about                AboutAction
GET /how-it-works     HowItWorksAction
GET /community-rules  CommunityRulesAction
GET /terms-of-service    TermsAction
GET /privacy-policy       PrivacyPolicyAction
GET /cookie-policy        CookiePolicyAction
GET /moderation-policy   ModerationPolicyAction
GET /legal-notice     LegalNoticeAction
|]

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
            |> addMarketViewParams tradingAssetId tradingAction showChart showDescription showAllAssets showTradeHistory activityPage chatPage chatComposerRev tradeQuantity backTo
    pathTo CreateMarketChatMessageAction { marketId } = "/CreateMarketChatMessage?marketId=" <> inputValue marketId
    pathTo DeleteMarketChatMessageAction { marketChatMessageId, marketId, tradingAssetId, tradingAction, showChart, showDescription, showAllAssets, showTradeHistory, activityPage, chatPage, chatComposerRev, tradeQuantity, backTo } =
        "/DeleteMarketChatMessage"
            |> addQueryParam "marketChatMessageId" (Just $ inputValue marketChatMessageId)
            |> addQueryParam "marketId" (Just $ inputValue marketId)
            |> addMarketViewParams tradingAssetId tradingAction showChart showDescription showAllAssets showTradeHistory activityPage chatPage chatComposerRev tradeQuantity backTo
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
        <|> (string "/DeleteMarket" >> onlyAllowMethods [POST] >> pure (DeleteMarketAction def Nothing Nothing))
        <|> (string "/SetResolveAsset" >> pure (SetResolveAssetAction def))
        <|> (string "/ConfirmRefundMarket" >> pure (ConfirmRefundMarketAction def))

instance HasPath DashboardController where
    pathTo DashboardPositionsAction { page, searchFilter, positionStatusFilter } =
        "/DashboardPositions"
            |> addQueryParam "page" (inputValue <$> page)
            |> addQueryParam "search" searchFilter
            |> addQueryParam "statusFilter" positionStatusFilter
    pathTo DashboardMarketsAction { page, searchFilter, statusFilter } =
        "/DashboardMarkets"
            |> addQueryParam "statusFilter" (inputValue <$> statusFilter)
            |> addQueryParam "page" (inputValue <$> page)
            |> addQueryParam "search" searchFilter
    pathTo DashboardTransactionsAction { page, searchFilter, typeFilter } =
        "/DashboardTransactions"
            |> addQueryParam "page" (inputValue <$> page)
            |> addQueryParam "search" searchFilter
            |> addQueryParam "type" typeFilter
    pathTo ConfirmDeleteMarketAction { confirmDeleteMarketId, page, searchFilter } =
        "/ConfirmDeleteMarket"
            |> addQueryParam "marketId" (Just $ inputValue confirmDeleteMarketId)
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
        (string "/DashboardPositions" >> pure (DashboardPositionsAction Nothing Nothing Nothing))
        <|> (string "/DashboardMarkets" >> pure (DashboardMarketsAction Nothing Nothing Nothing))
        <|> (string "/DashboardTransactions" >> pure (DashboardTransactionsAction Nothing Nothing Nothing))
        <|> (string "/ConfirmDeleteMarket" >> pure (ConfirmDeleteMarketAction def Nothing Nothing))
        <|> (string "/ChangeMarketStatus" >> pure (ChangeMarketStatusAction Nothing Nothing Nothing Nothing))

addMarketFlag :: Text -> Maybe Bool -> Text -> Text
addMarketFlag name mValue base =
    case mValue of
        Nothing    -> base
        Just True  -> base <> separator <> name <> "=true"
        Just False -> base <> separator <> name <> "=false"
  where
    separator = if Text.any (== '?') base then "&" else "?"

addMarketViewParams :: Maybe (Id Asset) -> Maybe Text -> Maybe Bool -> Maybe Bool -> Maybe Bool -> Maybe Bool -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe Int -> Maybe Text -> Text -> Text
addMarketViewParams tradingAssetId tradingAction showChart showDescription showAllAssets showTradeHistory activityPage chatPage chatComposerRev tradeQuantity backTo =
    addQueryParam "tradingAssetId" (inputValue <$> tradingAssetId)
        . addQueryParam "tradingAction" (inputValue <$> tradingAction)
        . addMarketFlag "showChart" (omitDefaultMarketFlag True showChart)
        . addMarketFlag "showDescription" (omitDefaultMarketFlag True showDescription)
        . addMarketFlag "showAllAssets" (omitDefaultMarketFlag False showAllAssets)
        . addMarketFlag "showTradeHistory" (omitDefaultMarketFlag True showTradeHistory)
        . addQueryParam "activityPage" (inputValue <$> normalizeDefaultPage activityPage)
        . addQueryParam "chatPage" (inputValue <$> normalizeDefaultPage chatPage)
        . addQueryParam "chatComposerRev" chatComposerRev
        . addQueryParam "tradeQuantity" (inputValue <$> omitDefaultTradeQuantity tradeQuantity)
        . addQueryParam "backTo" backTo

omitDefaultMarketFlag :: Bool -> Maybe Bool -> Maybe Bool
omitDefaultMarketFlag defaultValue = \case
    Just value | value == defaultValue -> Nothing
    other -> other

normalizeDefaultPage :: Maybe Int -> Maybe Int
normalizeDefaultPage = \case
    Just 1 -> Nothing
    other  -> other

omitDefaultTradeQuantity :: Maybe Int -> Maybe Int
omitDefaultTradeQuantity = \case
    Just 10 -> Nothing
    other   -> other

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
