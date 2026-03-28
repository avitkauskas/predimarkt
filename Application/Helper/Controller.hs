module Application.Helper.Controller where

import Data.Functor (void)
import Generated.Types
import IHP.ControllerPrelude

syncCloseMarketJob :: (?modelContext :: ModelContext) => Market -> IO ()
syncCloseMarketJob market = do
    existingJobs <- query @CloseMarketJob
        |> filterWhere (#marketId, market.id)
        |> fetch
    deleteRecords existingJobs

    when (market.status == MarketStatusOpen) $
        void $ newRecord @CloseMarketJob
            |> set #marketId market.id
            |> set #runAt market.closedAt
            |> createRecord

fetchPasskeyCount :: (?modelContext :: ModelContext) => Id User -> IO Int
fetchPasskeyCount userId =
    query @Passkey
        |> filterWhere (#userId, userId)
        |> fetchCount

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
