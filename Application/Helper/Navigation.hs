module Application.Helper.Navigation where

import qualified Data.Text as Text
import IHP.Prelude

sanitizeBackTo :: Maybe Text -> Maybe Text
sanitizeBackTo = (>>= sanitizeLocalPath)
    where
        sanitizeLocalPath path
            | Text.null trimmedPath = Nothing
            | "/" `Text.isPrefixOf` trimmedPath
                && not ("//" `Text.isPrefixOf` trimmedPath) = Just trimmedPath
            | otherwise = Nothing
            where
                trimmedPath = Text.strip path
