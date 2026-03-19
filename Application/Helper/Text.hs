module Application.Helper.Text where

import qualified Data.Text as Text
import IHP.Prelude

textParagraphs :: Text -> [Text]
textParagraphs =
    go [] []
        . Text.lines
        . Text.replace "\r" "\n"
        . Text.replace "\r\n" "\n"
  where
    go :: [Text] -> [Text] -> [Text] -> [Text]
    go paragraphs currentParagraph remainingLines =
        case remainingLines of
            [] -> reverse (finishParagraph paragraphs currentParagraph)
            line : rest
                | Text.strip line == "" ->
                    go (finishParagraph paragraphs currentParagraph) [] rest
                | otherwise ->
                    go paragraphs (currentParagraph <> [line]) rest

    finishParagraph :: [Text] -> [Text] -> [Text]
    finishParagraph paragraphs [] = paragraphs
    finishParagraph paragraphs currentParagraph =
        Text.strip (Text.intercalate "\n" currentParagraph) : paragraphs
