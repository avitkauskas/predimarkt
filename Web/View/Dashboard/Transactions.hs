{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Web.View.Dashboard.Transactions where

import Admin.Controller.Prelude (render)
import Application.Helper.View (formatPricePercent)
import Data.Text (pack)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Database.PostgreSQL.Simple.TypeInfo.Static (money)
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

        timestampStr = formatTime defaultTimeLocale "%F %R" (get #createdAt txn)

        priceBefore = get #priceBefore txn
        priceAfter = get #priceAfter txn
        priceImpact = formatPricePercent priceBefore <> " → " <> formatPricePercent priceAfter

        nextAction = if isBuy then Just "buy" else Just "sell"
        marketUrl = ShowMarketAction market.id (Just asset.id) nextAction

        typeText = if isBuy then "bought" else "sold" :: Text
        typeColor = if isBuy then "text-success" else "text-danger" :: Text
        pnlText = if realizedPnL /= 0
            then formatMoneySigned realizedPnL
            else "--"
        moneyClass :: Integer -> Text
        moneyClass money = case money of
             n | n > 0 -> "text-success fw-bold"
               | n < 0 -> "text-danger fw-bold"
               | otherwise -> "text-muted fw-medium"
    in [hsx|
        <div class="col-12">
            <div class="card shadow-sm">
                <div class="card-body px-3 py-2">
                    <div class="d-flex justify-content-between align-items-start mb-2">
                        <div>
                            <a href={marketUrl} class="text-decoration-none">
                                <span class="h6 mb-0 fw-bold">{get #title market}</span> -
                                <span class="text-muted">{get #name asset}</span>
                            </a>
                        </div>
                    </div>

                    <div class="row text-center small border-top pt-2">
                        <div class="col-3 border-end">
                            <div class="small text-muted fw-medium"
                                 style="font-size: 0.7rem;">
                                {renderTime txn.createdAt}
                            </div>
                            <div class={typeColor <> " fw-bold"}>{typeText} {show qty}</div>
                        </div>
                        <div class="col-3 border-end">
                            <div class="text-muted" style="font-size: 0.7rem;">Cash Flow</div>
                            <div class={moneyClass cashFlow}>
                                {formatMoneySigned cashFlow}
                            </div>
                        </div>
                        <div class="col-3 border-end">
                            <div class="text-muted" style="font-size: 0.7rem;">Realized P&L</div>
                            <div class={moneyClass realizedPnL}>
                                {pnlText}
                            </div>
                        </div>
                        <div class="col-3">
                            <div class="text-muted" style="font-size: 0.7rem;">Probability Impact</div>
                            <div class="fw-medium">{priceImpact}</div>
                        </div>
                    </div>
                </div>
            </div>
        </div>
    |]

renderTxnPagination :: Int -> Int -> Html
renderTxnPagination currentPage totalPages =
    renderSmartPagination currentPage totalPages "Transaction pagination"
        (\pageNum -> pathTo (DashboardTransactionsAction (Just pageNum)))
