{-# LANGUAGE OverloadedStrings #-}

module Web.View.Leaderboard.Index where

import Data.Text (pack)
import Text.Printf (printf)
import Web.View.Prelude

data UserSummary = UserSummary
    { nickname         :: Text
    , cash             :: Integer
    , positionsValue   :: Integer
    , totalValue       :: Integer
    , yearsActive      :: Double
    , totalReturn      :: Double
    , annualReturn     :: Double
    , showAnnualReturn :: Bool
    , score            :: Double
    , rank             :: Int
    }

data IndexView = IndexView
    { displayUsers        :: [UserSummary]
    , currentUserNickname :: Maybe Text
    , showOverflow        :: Bool
    }

instance View IndexView where
    html IndexView { .. } = [hsx|
        <div class="row">
            <div class="col-12">
                <h5 class="mb-3 ps-2">Leaderboard</h5>
                <div class="overflow-auto">
                    <table class="table table-sm table-borderless table-hover w-auto mb-0">
                        <thead>
                            <tr class="align-middle">
                                <th scope="col" class="py-1 info-label text-center">Rank</th>
                                <th scope="col" class="px-3 py-1 info-label">User</th>
                                {renderHeaderWithTooltip "Cash" "available funds" "text-end"}
                                {renderHeaderWithTooltip "Positions" "current value of positions" "text-end"}
                                {renderHeaderWithTooltip "Total Value" "cash plus positions" "text-end text-nowrap"}
                                {renderHeaderWithTooltip "Return" "total return since signup" "text-end text-nowrap"}
                                {renderHeaderWithTooltip "CAGR" "compound annual growth rate" "text-end text-nowrap"}
                                {renderHeaderWithTooltip "Years" "time since signup" "text-end text-nowrap"}
                                {renderHeaderWithTooltip "Score" "time-adjusted stabilised score" "text-end text-nowrap"}
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
        rowClass :: Text = if isCurrentUser then "table-primary" else ""
    in [hsx|
            <tr class={"align-middle " <> rowClass}>
                <td class="px-3 py-1 text-center fw-medium">
                    {get #rank summary}
                </td>
                <td class="px-3 py-1 fw-medium text-nowrap">
                    {get #nickname summary}
                </td>
                <td class="px-3 py-1 text-end fw-medium">
                    {formatMoney (get #cash summary)}
                </td>
                <td class="px-3 py-1 text-end fw-medium">
                    {formatMoney (get #positionsValue summary)}
                </td>
                <td class="px-3 py-1 text-end fw-medium">
                    {formatMoney (get #totalValue summary)}
                </td>
                <td class="px-3 py-1 text-end fw-medium text-nowrap">
                    {formatPercent (get #totalReturn summary)}
                </td>
                <td class="px-3 py-1 text-end fw-medium text-nowrap">
                    {if get #showAnnualReturn summary
                        then formatPercent (get #annualReturn summary)
                        else "--"}
                </td>
                <td class="px-3 py-1 text-end fw-medium text-nowrap">
                    {formatYears (get #yearsActive summary)}
                </td>
                <td class="px-3 py-1 text-end fw-medium text-nowrap">
                    {formatScore (get #score summary)}
                </td>
            </tr>
        |]

renderHeaderWithTooltip :: Text -> Text -> Text -> Html
renderHeaderWithTooltip label tooltip extraClasses = [hsx|
    <th scope="col"
        class={"px-3 py-1 info-label " <> extraClasses}
        data-bs-toggle="tooltip"
        data-bs-placement="top"
        title={tooltip}>
        {label}
    </th>
|]

formatPercent :: Double -> Text
formatPercent value =
    pack (printf "%.2f%%" (value * 100))

formatYears :: Double -> Text
formatYears value =
    pack (printf "%.3f" value)

formatScore :: Double -> Text
formatScore value =
    pack (printf "%.0f" (value * 10000))

renderOverflowRow :: Html
renderOverflowRow = [hsx|
    <tr>
        <td class="px-3 py-0 text-center text-muted">...</td>
        <td class="px-3 py-0 text-muted">...</td>
        <td class="px-3 py-0 text-end text-muted">...</td>
        <td class="px-3 py-0 text-end text-muted">...</td>
        <td class="px-3 py-0 text-end text-muted">...</td>
        <td class="px-3 py-0 text-end text-muted">...</td>
        <td class="px-3 py-0 text-end text-muted">...</td>
        <td class="px-3 py-0 text-end text-muted">...</td>
        <td class="px-3 py-0 text-end text-muted">...</td>
    </tr>
|]

renderEmptyState :: Html
renderEmptyState = [hsx|
    <div class="alert alert-info text-center">
        No users yet.
    </div>
|]
