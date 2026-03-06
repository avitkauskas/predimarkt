{-# LANGUAGE OverloadedStrings #-}

module Web.View.Leaderboard.Index where

import Distribution.PackageDescription.Utils (userBug)
import Web.View.Prelude

data UserSummary = UserSummary
    { nickname       :: Text
    , cash           :: Integer
    , positionsValue :: Integer
    , totalValue     :: Integer
    , rank           :: Int
    }

data IndexView = IndexView
    { displayUsers        :: [UserSummary]
    , currentUserNickname :: Maybe Text
    , showOverflow        :: Bool
    }

instance View IndexView where
    html IndexView { .. } = [hsx|
        <div class="row">
            <div class="col-md-9 col-lg-7 col-xl-6">
                <h5 class="mb-3 ps-2">Leaderboard</h5>
                <div class="table-responsive">
                    <table class="table table-borderless table-hover">
                        <thead>
                            <tr class="align-middle">
                                <th scope="col" class="py-1 info-label text-center">Rank</th>
                                <th scope="col" class="py-1 info-label">User</th>
                                <th scope="col" class="py-1 info-label text-end">Cash</th>
                                <th scope="col" class="py-1 info-label text-end">Positions</th>
                                <th scope="col" class="py-1 info-label text-end text-nowrap">Total Value</th>
                            </tr>
                        </thead>
                        <tbody>
                            {renderRows displayUsers currentUserNickname showOverflow}
                        </tbody>
                    </table>
                </div>
                {when (null displayUsers) renderEmptyState}
            </div>
        </div>
    |]

renderRows :: (?context :: ControllerContext) => [UserSummary] -> Maybe Text -> Bool -> Html
renderRows [] _ _ = mempty
renderRows users currentUserNickname showOverflow =
    if showOverflow
        then case (init users, last users) of
                 (Just allButLast, Just lastRow) ->
                     mconcat (map (`renderUserRow` currentUserNickname) allButLast)
                     <> renderOverflowRow
                     <> renderUserRow lastRow currentUserNickname
                 _ -> mempty
        else mconcat (map (`renderUserRow` currentUserNickname) users)

renderUserRow :: (?context :: ControllerContext) => UserSummary -> Maybe Text -> Html
renderUserRow summary currentUserNickname =
    let isCurrentUser = maybe False (\nick -> get #nickname summary == nick) currentUserNickname
        rank = get #rank summary
        cellBg = rankBackgroundStyle (get #rank summary)
        userBg :: Text = if (isCurrentUser && rank `notElem` [1..3]) then "bg-info-subtle" else ""
    in [hsx|
            <tr class="align-middle">
                <td class={"py-1 text-center fw-medium " <> userBg}
                    style={cellBg <> " width: 60px;"}>
                    {get #rank summary}
                </td>
                <td class={"py-1 fw-medium " <> userBg}
                    style={cellBg}>
                    {get #nickname summary}
                </td>
                <td class={"py-1 text-end fw-medium " <> userBg}
                    style={cellBg}>
                    {formatMoney (get #cash summary)}
                </td>
                <td class={"py-1 text-end fw-medium " <> userBg}
                    style={cellBg}>
                    {formatMoney (get #positionsValue summary)}
                </td>
                <td class={"py-1 text-end fw-medium " <> userBg}
                    style={cellBg}>
                    {formatMoney (get #totalValue summary)}
                </td>
            </tr>
        |]

rankBackgroundStyle :: Int -> Text
rankBackgroundStyle 1 = "background-color: rgba(255, 215, 0, 0.35);"
rankBackgroundStyle 2 = "background-color: rgba(192, 192, 192, 0.25);"
rankBackgroundStyle 3 = "background-color: rgba(205, 127, 50, 0.15);"
rankBackgroundStyle _ = ""

renderOverflowRow :: Html
renderOverflowRow = [hsx|
    <tr>
        <td class="py-0 text-center text-muted">...</td>
        <td class="py-0 text-muted">...</td>
        <td class="py-0 text-end text-muted">...</td>
        <td class="py-0 text-end text-muted">...</td>
        <td class="py-0 text-end text-muted">...</td>
    </tr>
|]

renderEmptyState :: Html
renderEmptyState = [hsx|
    <div class="alert alert-info text-center">
        No users yet.
    </div>
|]
