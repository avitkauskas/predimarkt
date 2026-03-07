{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Web.View.Dashboard.Transactions where

import Admin.Controller.Prelude
import Application.Domain.Types (Quantity (Quantity))
import Application.Helper.View (formatMoney, formatPricePercent)
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
    , wallet                  :: Wallet
    , searchFilter            :: Maybe Text
    }

instance View TransactionsView where
    html TransactionsView { .. } = dashboardLayout [hsx|
        <div class="container-fluid ps-2">
            <div class="d-flex justify-content-between align-items-center mb-1">
                <h5>Transactions</h5>
                <div class="text-end me-1">
                    Cash Balance: <span class="fw-bold">{formatMoney wallet.amount}</span>
                </div>
            </div>
            <div class="mb-3">
                {renderSearchForm searchFilter}
            </div>
            {renderTransactionsContent transactionsWithDetails currentPage totalPages searchFilter}
        </div>
    |]

renderSearchForm :: Maybe Text -> Html
renderSearchForm searchFilter = [hsx|
    <div class="d-flex" id="transactions-search-form-container">
        <form class="w-100 position-relative"
              action={DashboardTransactionsAction Nothing Nothing}
              method="GET"
              data-auto-submit-delay="300">
            <i class="bi bi-search text-muted position-absolute"
               style="left: 12px; top: 50%; transform: translateY(-50%); z-index: 3;">
            </i>
            <input type="search"
                       id="transactions-search-input"
                       class="form-control"
                       name="search"
                       value={fromMaybe "" searchFilter}
                       placeholder="Search transactions by market or asset..."
                       aria-label="Search transactions"
                       style="padding-left: 36px;">
        </form>
    </div>
|]

renderTransactionsContent :: (?context :: ControllerContext) => [TransactionWithDetails] -> Int -> Int -> Maybe Text -> Html
renderTransactionsContent [] _ _ Nothing = [hsx|
    <div class="alert alert-info">
        No transactions found. Start trading to see your history here.
    </div>
|]
renderTransactionsContent [] _ _ (Just _) = [hsx|
    <div class="alert alert-info">
        No transactions match your search. Try a different search term.
    </div>
|]
renderTransactionsContent txns currentPage totalPages searchFilter = [hsx|
    <div class="row g-3">
        {forEach txns renderTransactionCard}
    </div>
    <div>
        {renderTxnPagination currentPage totalPages searchFilter}
    </div>
|]

renderTransactionCard :: (?context :: ControllerContext) => TransactionWithDetails -> Html
renderTransactionCard twd =
    let txn = get #transaction twd
        asset = get #assetId txn
        market = get #marketId txn

        qty = get #quantity txn
        isBuy = qty > 0
        absQty = abs qty
        cashFlow = get #cashFlow txn

        timestampStr = formatTime defaultTimeLocale "%F %R" (get #createdAt txn)

        priceBefore = get #priceBefore txn
        priceAfter = get #priceAfter txn
        priceImpact = formatPricePercent priceBefore <> " → " <> formatPricePercent priceAfter

        nextAction = if isBuy then Just "buy" else Just "sell"
        marketUrl = ShowMarketAction market.id (Just asset.id) nextAction Nothing Nothing Nothing Nothing Nothing Nothing

        typeText = if isBuy then "bought" else "sold" :: Text
        typeColor = if isBuy then "text-success" else "text-danger" :: Text
        moneyClass :: Integer -> Text
        moneyClass money = case money of
             n | n > 0 -> "text-success fw-bold"
               | n < 0 -> "text-danger fw-bold"
               | otherwise -> "text-muted fw-medium"
    in [hsx|
        <div class="col-12">
            <div class="card shadow-sm">
                <div class="card-body px-3 py-1">
                    <div class="d-flex justify-content-between mb-2 overflow-x-auto scroll-no-bar">
                        <div>
                            <a href={marketUrl} class="text-decoration-none">
                                <span class="h6 mb-0 fw-bold">{get #title market}</span> -
                                <span class="text-muted">{get #name asset}</span>
                            </a>
                        </div>
                    </div>

                    <div class="overflow-x-auto scroll-no-bar">
                        <div class="d-flex justify-content-start align-items-end border-top gap-5 pt-2 flex-nowrap">
                            <div class="flex-shrink-0">
                                <div class="text-muted text-nowrap" style="font-size: 0.7rem;">Data & Time</div>
                                <div class="text-muted fw-medium text-nowrap" style="font-size: 0.9rem;">
                                    {renderTime txn.createdAt}
                                </div>
                            </div>
                            <div class="flex-shrink-0 text-center">
                                <div class="text-muted text-nowrap" style="font-size: 0.7rem;">Type & Quantity</div>
                                <div class={typeColor <> " fw-bold text-nowrap"}>{typeText} {show absQty}</div>
                            </div>
                            <div class="flex-shrink-0 text-center">
                                <div class="text-muted text-nowrap" style="font-size: 0.7rem;">Cash Flow</div>
                                <div class={moneyClass cashFlow <> " text-nowrap"}>
                                    {formatMoneySigned cashFlow}
                                </div>
                            </div>
                            <div class="flex-shrink-0 text-center">
                                <div class="text-muted text-nowrap" style="font-size: 0.7rem;">Probability Impact</div>
                                <div class="fw-medium text-nowrap">
                                    {priceImpact}
                                </div>
                            </div>
                        </div>
                    </div>
                </div>
            </div>
        </div>
    |]

renderTxnPagination :: Int -> Int -> Maybe Text -> Html
renderTxnPagination currentPage totalPages searchFilter =
    renderSmartPagination currentPage totalPages "Transaction pagination"
        (\pageNum -> pathTo (DashboardTransactionsAction (Just pageNum) searchFilter))
