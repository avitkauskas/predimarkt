module Application.Market.Input where

import Data.Text (strip)
import qualified Data.Text as Text
import Generated.Types
import IHP.Prelude

validateAssetSymbols :: [Asset] -> Maybe Text
validateAssetSymbols assets =
    let symbols = map (strip . get #symbol) assets
        emptySymbols = filter isEmpty symbols
        uniqueSymbols = nub symbols
    in if not (null emptySymbols)
        then Just "Asset symbols cannot be empty"
        else if length uniqueSymbols /= length symbols
            then Just "Asset symbols must be unique within the market"
            else Nothing

validateAssetNames :: [Asset] -> Maybe Text
validateAssetNames assets =
    let names = map (strip . get #name) assets
        emptyNames = filter isEmpty names
        uniqueNames = nub names
    in if not (null emptyNames)
        then Just "Asset names cannot be empty"
        else if length uniqueNames /= length names
            then Just "Asset names must be unique within the market"
            else Nothing

sanitizeTradeQuantity :: Maybe Int -> Maybe Int
sanitizeTradeQuantity = \case
    Just quantity | quantity >= 0 -> Just quantity
    _ -> Nothing

sanitizeTradingAction :: Maybe Text -> Maybe Text
sanitizeTradingAction = \case
    Just "buy" -> Just "buy"
    Just "sell" -> Just "sell"
    _ -> Nothing

normalizeChatMessageBody :: Text -> Text
normalizeChatMessageBody = strip

validateChatMessageBody :: Text -> Maybe Text
validateChatMessageBody body
    | body == "" = Just "Please enter a message"
    | Text.any (\char -> char == '\n' || char == '\r') body =
        Just "Message must be a single line"
    | Text.length body > 280 = Just "Message must be at most 280 characters"
    | otherwise = Nothing
