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
        <div class="container">
            <div class="row justify-content-center">
                <div class="col-md-10 col-lg-8">
                    <h2 class="mb-4">Leaderboard</h2>
                    <div class="table-responsive">
                        <table class="table table-striped table-hover">
                            <thead class="table-dark">
                                <tr>
                                    <th scope="col" class="text-center" style="width: 60px;">Rank</th>
                                    <th scope="col">User</th>
                                    <th scope="col" class="text-end">Cash</th>
                                    <th scope="col" class="text-end">Positions Value</th>
                                    <th scope="col" class="text-end">Total Value</th>
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
        </div>
    |]

renderUserRow :: (?context :: ControllerContext) => (Int, UserSummary) -> Html
renderUserRow (rank, summary) = [hsx|
    <tr>
        <td class="text-center">{rank}</td>
        <td class="fw-medium">{get #nickname summary}</td>
        <td class="text-end">{formatMoney (get #cash summary)}</td>
        <td class="text-end">{formatMoney (get #positionsValue summary)}</td>
        <td class="text-end fw-bold">{formatMoney (get #totalValue summary)}</td>
    </tr>
|]

renderEmptyState :: Html
renderEmptyState = [hsx|
    <div class="alert alert-info text-center">
        No users yet.
    </div>
|]
