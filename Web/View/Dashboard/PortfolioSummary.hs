{-# LANGUAGE OverloadedStrings #-}

module Web.View.Dashboard.PortfolioSummary where

import Web.View.Prelude

renderPortfolioSummary :: Integer -> Integer -> Integer -> Html
renderPortfolioSummary cashValue positionsValue totalValue = [hsx|
    <div class="text-end me-1 ps-1 overflow-x-auto scroll-no-bar" style="min-width: 0;">
        <div class="d-inline-flex gap-2">
            {renderSummaryItem "Cash" "C" cashValue}
            {renderSummaryItem "Positions" "P" positionsValue}
            {renderSummaryItem "Total" "T" totalValue}
        </div>
    </div>
|]

renderSummaryItem :: Text -> Text -> Integer -> Html
renderSummaryItem desktopLabel mobileLabel value = [hsx|
    <span>
        <span class="d-none d-sm-inline">{desktopLabel <> ": "}</span>
        <span class="d-inline d-sm-none">{mobileLabel <> ": "}</span>
        <span class="fw-bold">{formatMoney value}</span>
    </span>
|]
