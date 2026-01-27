module Application.Helper.Controller where

import IHP.ControllerPrelude
import Generated.Types

-- Here you can add functions which are available in all your controllers

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
