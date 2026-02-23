{-# LANGUAGE OverloadedStrings #-}

module Web.View.Leaderboard.Index where

import Web.View.Prelude

data UserSummary = UserSummary
    { userId         :: Id User
    , nickname       :: Text
    , cash           :: Integer
    , positionsValue :: Integer
    , totalValue     :: Integer
    }

data IndexView = IndexView
    { rankedUsers :: [UserSummary] }

instance View IndexView where
    html IndexView { .. } = [hsx|
        <div class="row">
            <div class="col-md-9 col-lg-7 col-xl-6">
                <h5 class="mb-3 ps-2">Leaderboard</h5>
                <div class="table-responsive">
                    <table class="table table-borderless table-hover">
                        <thead>
                            <tr>
                                <th scope="col" class="text-center" style="width: 60px;">Rank</th>
                                <th scope="col">User</th>
                                <th scope="col" class="text-end">Cash</th>
                                <th scope="col" class="text-end">Positions</th>
                                <th scope="col" class="text-end">Total</th>
                            </tr>
                        </thead>
                        <tbody>
                            {forEach (zip [1..] rankedUsers) renderUserRow}
                        </tbody>
                    </table>
                </div>
                {when (null rankedUsers) renderEmptyState}
            </div>
        </div>
    |]

renderUserRow :: (?context :: ControllerContext) => (Int, UserSummary) -> Html
renderUserRow (rank, summary) = [hsx|
    <tr>
        <td class="py-0 text-center">{rank}</td>
        <td class="py-0 fw-medium">{get #nickname summary}</td>
        <td class="py-0 text-end font-monospace small">{formatMoney (get #cash summary)}</td>
        <td class="py-0 text-end font-monospace small">{formatMoney (get #positionsValue summary)}</td>
        <td class="py-0 text-end font-monospace small">{formatMoney (get #totalValue summary)}</td>
    </tr>
|]

renderEmptyState :: Html
renderEmptyState = [hsx|
    <div class="alert alert-info text-center">
        No users yet.
    </div>
|]
