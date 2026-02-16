module Application.Helper.Controller where

import Data.List (sortBy)
import Generated.Types
import IHP.ControllerPrelude

-- Here you can add functions which are available in all your controllers

-- | Sort assets for display: Yes/No markets show Yes first, others by quantity descending
sortAssetsForDisplay :: [Asset] -> [Asset]
sortAssetsForDisplay [] = []
sortAssetsForDisplay assets =
    case assets of
        [a, b]
            | a.name == "Yes" && b.name == "No" -> assets
            | a.name == "No" && b.name == "Yes" -> [b, a]
        _ -> sortBy (\a b -> compare (b.quantity) (a.quantity)) assets

constructUniqueSlug :: (?modelContext :: ModelContext) => Id Category -> Text -> Maybe (Id Market) -> IO Text
constructUniqueSlug categoryId baseSlug maybeExcludeId = loop 0
    where
        loop :: Int -> IO Text
        loop i = do
            let slug = if i == 0 then baseSlug else baseSlug <> "-" <> tshow i
            let queryBuilder = query @Market
                    |> filterWhere (#categoryId, categoryId)
                    |> filterWhere (#slug, slug)

            exists <- case maybeExcludeId of
                Just excludeId -> queryBuilder |> filterWhereNot (#id, excludeId) |> fetchExists
                Nothing -> queryBuilder |> fetchExists

            if exists
                then loop (i + 1)
                else pure slug
