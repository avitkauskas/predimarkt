{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Web.View.Dashboard.Transactions where

import Application.Helper.View (formatPricePercent)
import Data.Text (pack)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Text.Printf (printf)
import Web.View.Prelude

data TransactionWithDetails = TransactionWithDetails
    { transaction :: Include' ["marketId", "assetId"] Transaction
    }

data TransactionsView = TransactionsView
    { transactionsWithDetails :: [TransactionWithDetails]
    , currentPage             :: Int
    , totalPages              :: Int
    }

instance View TransactionsView where
    html TransactionsView { .. } = dashboardLayout [hsx|
        <div class="container-fluid py-3">
            <h5 class="mb-3 text-muted">Transactions</h5>
            {renderTransactionsContent transactionsWithDetails currentPage totalPages}
        </div>
    |]

renderTransactionsContent :: (?context :: ControllerContext) => [TransactionWithDetails] -> Int -> Int -> Html
renderTransactionsContent [] _ _ = [hsx|
    <div class="alert alert-info">
        No transactions yet. Start trading to see your history here.
    </div>
|]
renderTransactionsContent txns currentPage totalPages = [hsx|
    <div class="row g-3">
        {forEach txns renderTransactionCard}
    </div>
    {renderTxnPagination currentPage totalPages}
|]

renderTransactionCard :: (?context :: ControllerContext) => TransactionWithDetails -> Html
renderTransactionCard twd =
    let txn = get #transaction twd
        asset = get #assetId txn
        market = get #marketId txn

        isBuy = get #side txn == "long"
        qty = abs (get #quantity txn)
        cashFlow = get #cashFlow txn
        realizedPnL = get #realizedPnl txn

        timeStr = formatTime defaultTimeLocale "%H:%M" (get #createdAt txn)
        dateStr = formatTime defaultTimeLocale "%d/%m/%Y" (get #createdAt txn)

        priceBefore = get #priceBefore txn
        priceAfter = get #priceAfter txn
        priceImpact = if priceBefore > 0 && priceAfter > 0
            then formatPricePercent priceBefore <> " → " <> formatPricePercent priceAfter
            else "-"

        nextAction = if isBuy then Just "buy" else Just "sell"
        marketUrl = ShowMarketAction market.id (Just asset.id) nextAction

        typeText = if isBuy then "bought" else "sold" :: Text
        typeColor = if isBuy then "text-success" else "text-danger" :: Text
        amountPrefix = if isBuy then "-" :: Text else "+" :: Text
        pnlClass = if realizedPnL >= 0 then "text-success fw-bold" else "text-danger fw-bold" :: Text
        pnlSign = if realizedPnL >= 0 then "+" else "-" :: Text
    in [hsx|
        <div class="col-12">
            <div class="card shadow-sm">
                <div class="card-body p-3">
                    <div class="d-flex justify-content-between align-items-start mb-2">
                        <div>
                            <a href={marketUrl} class="text-decoration-none">
                                <h6 class="mb-0 fw-bold">{get #title market}</h6>
                                <small class="text-muted">{get #name asset}</small>
                            </a>
                        </div>
                        <div class="text-end">
                            <span class={typeColor}>{typeText} {show qty}</span>
                            <div class="small text-muted">{timeStr} {dateStr}</div>
                        </div>
                    </div>

                    <div class="row text-center small border-top border-bottom py-2">
                        <div class="col-4 border-end">
                            <div class="text-muted" style="font-size: 0.7rem;">Amount</div>
                            <div class="fw-medium">
                                {amountPrefix}€{formatMoneyAmount (abs cashFlow)}
                            </div>
                        </div>
                        <div class="col-4 border-end">
                            <div class="text-muted" style="font-size: 0.7rem;">P&L</div>
                            <div class={pnlClass}>
                                {pnlSign}€{formatMoneyAmount (abs realizedPnL)}
                            </div>
                        </div>
                        <div class="col-4">
                            <div class="text-muted" style="font-size: 0.7rem;">Price Impact</div>
                            <div class="fw-medium">{priceImpact}</div>
                        </div>
                    </div>
                </div>
            </div>
        </div>
    |]

formatMoneyAmount :: Integer -> Text
formatMoneyAmount cents =
    let euros = fromIntegral cents / 100 :: Double
    in pack (printf "%.2f" euros)

renderTxnPagination :: Int -> Int -> Html
renderTxnPagination currentPage totalPages =
    if totalPages <= 1
    then [hsx||]
    else [hsx|
        <nav aria-label="Transaction pagination" class="mt-3">
            <ul class="pagination pagination-sm justify-content-center">
                {renderPrev currentPage}
                {renderPages currentPage totalPages}
                {renderNext currentPage totalPages}
            </ul>
        </nav>
    |]
    where
        renderPrev cp = if cp <= 1
            then [hsx|<li class="page-item disabled"><span class="page-link">←</span></li>|]
            else [hsx|<li class="page-item"><a class="page-link" href={DashboardTransactionsAction (Just (cp - 1))}>←</a></li>|]
        renderNext cp tp = if cp >= tp
            then [hsx|<li class="page-item disabled"><span class="page-link">→</span></li>|]
            else [hsx|<li class="page-item"><a class="page-link" href={DashboardTransactionsAction (Just (cp + 1))}>→</a></li>|]
        renderPages cp tp = forEach [1..tp] $ \n ->
            if n == cp
            then [hsx|<li class="page-item active"><span class="page-link">{show n}</span></li>|]
            else [hsx|<li class="page-item"><a class="page-link" href={DashboardTransactionsAction (Just n)}>{show n}</a></li>|]
