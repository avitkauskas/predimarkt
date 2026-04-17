module Application.Helper.QueryParams where

import qualified Data.Text as Text
import IHP.Prelude

normalizeSearchQuery :: Maybe Text -> Maybe Text
normalizeSearchQuery (Just query)
    | Text.stripStart query /= "" = Just (Text.stripStart query)
normalizeSearchQuery _ = Nothing

normalizeOptionalTextParam :: Maybe Text -> Maybe Text
normalizeOptionalTextParam = \case
    Just value | Text.strip value /= "" -> Just (Text.strip value)
    _ -> Nothing

normalizePageParam :: Int -> Maybe Int
normalizePageParam pageNum
    | pageNum > 1 = Just pageNum
    | otherwise = Nothing

parseBooleanText :: Maybe Text -> Maybe Bool
parseBooleanText = \case
    Just value ->
        case Text.toLower value of
            "true"  -> Just True
            "1"     -> Just True
            "false" -> Just False
            "0"     -> Just False
            _       -> Nothing
    Nothing -> Nothing
