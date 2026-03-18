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
        buildShowMarketPath marketId tradingAssetId tradingAction showChart showDescription showAllAssets showTradeHistory activityPage chatPage chatComposerRev tradeQuantity backTo
    pathTo CreateMarketChatMessageAction { marketId } = "/CreateMarketChatMessage?marketId=" <> inputValue marketId
    pathTo DeleteMarketChatMessageAction { marketChatMessageId, marketId, tradingAssetId, tradingAction, showChart, showDescription, showAllAssets, showTradeHistory, activityPage, chatPage, chatComposerRev, tradeQuantity, backTo } =
        let queryParams = Maybe.catMaybes
                [ Just ("marketChatMessageId=" <> inputValue marketChatMessageId)
                , Just ("marketId=" <> inputValue marketId)
                , fmap (\aid -> "tradingAssetId=" <> inputValue aid) tradingAssetId
                , fmap (\action -> "tradingAction=" <> inputValue action) tradingAction
                , fmap (showMarketFlagParam "showChart") showChart
                , fmap (showMarketFlagParam "showDescription") showDescription
                , fmap (showMarketFlagParam "showAllAssets") showAllAssets
                , fmap (showMarketFlagParam "showTradeHistory") showTradeHistory
                , fmap (\page -> "activityPage=" <> inputValue page) activityPage
                , fmap (\page -> "chatPage=" <> inputValue page) chatPage
                , fmap (\rev -> "chatComposerRev=" <> encodeQueryValue rev) chatComposerRev
                , fmap (\quantity -> "tradeQuantity=" <> inputValue quantity) tradeQuantity
                , fmap (\path -> "backTo=" <> encodeQueryValue path) backTo
                ]
            in "/DeleteMarketChatMessage?" <> Text.intercalate "&" queryParams
    pathTo EditMarketAction { marketId, page }   = "/EditMarket?marketId=" <> inputValue marketId <> maybe "" (("&page=" <>) . inputValue) page
    pathTo UpdateMarketAction { marketId } = "/UpdateMarket?marketId=" <> inputValue marketId
    pathTo DeleteMarketAction { marketId } = "/DeleteMarket?marketId=" <> inputValue marketId
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
        <|> (string "/EditMarket" >> pure (EditMarketAction def Nothing))
        <|> (string "/UpdateMarket" >> pure (UpdateMarketAction def))
        <|> (string "/DeleteMarket" >> pure (DeleteMarketAction def))
        <|> (string "/SetResolveAsset" >> pure (SetResolveAssetAction def))
        <|> (string "/ConfirmRefundMarket" >> pure (ConfirmRefundMarketAction def))

buildShowMarketPath marketId tradingAssetId tradingAction showChart showDescription showAllAssets showTradeHistory activityPage chatPage chatComposerRev tradeQuantity backTo =
    let queryParams = Maybe.catMaybes
            [ Just ("marketId=" <> inputValue marketId)
            , fmap (\aid -> "tradingAssetId=" <> inputValue aid) tradingAssetId
            , fmap (\action -> "tradingAction=" <> inputValue action) tradingAction
            , fmap (showMarketFlagParam "showChart") showChart
            , fmap (showMarketFlagParam "showDescription") showDescription
            , fmap (showMarketFlagParam "showAllAssets") showAllAssets
            , fmap (showMarketFlagParam "showTradeHistory") showTradeHistory
            , fmap (\page -> "activityPage=" <> inputValue page) activityPage
            , fmap (\page -> "chatPage=" <> inputValue page) chatPage
            , fmap (\rev -> "chatComposerRev=" <> encodeQueryValue rev) chatComposerRev
            , fmap (\quantity -> "tradeQuantity=" <> inputValue quantity) tradeQuantity
            , fmap (\path -> "backTo=" <> encodeQueryValue path) backTo
            ]
    in "/ShowMarket?" <> Text.intercalate "&" queryParams

showMarketFlagParam :: Text -> Bool -> Text
showMarketFlagParam name value = name <> "=" <> if value then "true" else "false"

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

instance HasPath DashboardController where
    pathTo DashboardPositionsAction { page, searchFilter } =
        buildPaginatedSearchPath "/DashboardPositions" page searchFilter
    pathTo DashboardMarketsAction { statusFilter, page } =
        let base = "/DashboardMarkets" <> maybe "" (\s -> "?statusFilter=" <> inputValue s) statusFilter
            pageParam = maybe "" (\p -> (if isNothing statusFilter then "?" else "&") <> "page=" <> inputValue p) page
        in base <> pageParam
    pathTo DashboardTransactionsAction { page, searchFilter } =
        buildPaginatedSearchPath "/DashboardTransactions" page searchFilter
    pathTo ChangeMarketStatusAction { marketId, status } =
        "/ChangeMarketStatus"
        <> maybe "" (\mid -> "?marketId=" <> inputValue mid) marketId
        <> maybe "" (\s -> "&status=" <> inputValue s) status
    pathTo OpenMarketAction { marketId } =
        "/OpenMarket" <> maybe "" (\mid -> "?marketId=" <> inputValue mid) marketId

instance CanRoute DashboardController where
    parseRoute' =
        (string "/DashboardPositions" >> pure (DashboardPositionsAction Nothing Nothing))
        <|> (string "/DashboardMarkets" >> pure (DashboardMarketsAction Nothing Nothing))
        <|> (string "/DashboardTransactions" >> pure (DashboardTransactionsAction Nothing Nothing))
        <|> (string "/ChangeMarketStatus" >> pure (ChangeMarketStatusAction Nothing Nothing))
        <|> (string "/OpenMarket" >> pure (OpenMarketAction Nothing))

buildPaginatedSearchPath :: Text -> Maybe Int -> Maybe Text -> Text
buildPaginatedSearchPath basePath page searchFilter =
    let pageParam = maybe "" (\p -> "?page=" <> inputValue p) page
        searchParam = case (page, searchFilter) of
            (_, Just search) | search /= "" -> (if isNothing page then "?" else "&") <> "search=" <> inputValue search
            _ -> ""
    in basePath <> pageParam <> searchParam
