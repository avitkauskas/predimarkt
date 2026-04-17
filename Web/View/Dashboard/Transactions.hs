{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Web.View.Dashboard.Transactions where

import Application.Domain.Types (Quantity (Quantity))
import Application.Helper.Formatting (formatPricePercent)
import Application.Helper.QueryParams (normalizePageParam)
import Data.Text (pack)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Text.Printf (printf)
import Web.View.Dashboard.PortfolioSummary
import Web.View.Prelude

data TransactionWithDetails = TransactionWithDetails
    { transaction :: Include' ["marketId", "assetId"] Transaction
    }

data TransactionsView = TransactionsView
    { transactionsWithDetails :: [TransactionWithDetails]
    , currentPage             :: Int
    , totalPages              :: Int
    , wallet                  :: Wallet
    , positionsValue          :: Integer
    , totalValue              :: Integer
    , searchFilter            :: Maybe Text
    , typeFilter              :: Maybe Text
    }

instance View TransactionsView where
    html TransactionsView { .. } = dashboardLayout [hsx|
        <div>
            <div class="d-flex justify-content-between align-items-baseline gap-2 mb-1">
                <h5>Transactions</h5>
                {renderPortfolioSummary wallet.amount positionsValue totalValue}
            </div>
            <div class="mb-3">
                {renderSearchFormWithType searchFilter typeFilter}
            </div>
            {renderTransactionsContent transactionsWithDetails currentPage totalPages searchFilter typeFilter}
        </div>
    |]

renderSearchFormWithType :: Maybe Text -> Maybe Text -> Html
renderSearchFormWithType searchFilter mType = [hsx|
    <div class="d-flex align-items-center gap-3">
        <div class="flex-grow-1" style="min-width: 0;">
            {renderSearchForm searchFilter mType}
        </div>
        {renderTypeDropdownForm searchFilter mType}
    </div>
|]

renderSearchForm :: Maybe Text -> Maybe Text -> Html
renderSearchForm searchFilter mType = [hsx|
    <div class="d-flex position-relative" id="transactions-search-form-container">
        <form class="w-100 position-relative"
              action={DashboardTransactionsAction Nothing Nothing mType}
              method="GET"
              data-auto-submit-delay="800">
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

renderTypeHiddenInput :: Text -> Html
renderTypeHiddenInput typeValue = [hsx|
    <input type="hidden" name="type" value={typeValue} />
|]

renderTypeDropdownForm :: Maybe Text -> Maybe Text -> Html
renderTypeDropdownForm searchFilter mType = [hsx|
    <form class="d-flex align-items-center gap-2 flex-shrink-0"
          action={DashboardTransactionsAction Nothing Nothing mType}
          method="GET">
        {forEach (maybeToList searchFilter) renderSearchHiddenInput}
        <div class="d-inline-flex align-items-center gap-2 rounded border border-secondary-subtle ps-2 pe-0 text-body-secondary bg-transparent"
             style="min-width: 133px; padding-top: 0.36rem; padding-bottom: 0.36rem;">
            <i class="bi bi-filter-right"></i>
            <select id="transactions-type-filter"
                    name="type"
                    class="form-select form-select-sm flex-grow-1 border-0 bg-transparent text-body-secondary shadow-none py-0 ps-0 pe-4"
                    aria-label="Filter transactions by type"
                    onchange="window.visitGetFormWithTurbolinks(this.form)">
                {renderTypeOption mType Nothing}
                {renderTypeOption mType (Just "buy")}
                {renderTypeOption mType (Just "sell")}
            </select>
        </div>
    </form>
|]

renderSearchHiddenInput :: Text -> Html
renderSearchHiddenInput searchQuery = [hsx|
    <input type="hidden" name="search" value={searchQuery} />
|]

renderTypeOption :: Maybe Text -> Maybe Text -> Html
renderTypeOption mActive mOption = [hsx|
    <option value={fromMaybe "" mOption}
            selected={mActive == mOption}>
        {typeOptionLabel mOption}
    </option>
|]

typeOptionLabel :: Maybe Text -> Text
typeOptionLabel Nothing       = "All"
typeOptionLabel (Just "buy")  = "Buy"
typeOptionLabel (Just "sell") = "Sell"
typeOptionLabel (Just x)      = x

renderTransactionsContent :: (?context :: ControllerContext) => [TransactionWithDetails] -> Int -> Int -> Maybe Text -> Maybe Text -> Html
renderTransactionsContent [] _ _ Nothing Nothing = [hsx|
    <div class="alert alert-info">
        No transactions found. Start trading to see your history here.
    </div>
|]
renderTransactionsContent [] _ _ _ _ = [hsx|
    <div class="alert alert-info">
        No transactions match your filters.
    </div>
|]
renderTransactionsContent txns currentPage totalPages searchFilter mType = [hsx|
    <div class="row g-3">
        {forEach txns (renderTransactionCard currentBackToPath)}
    </div>
    <div>
        {renderTxnPagination currentPage totalPages searchFilter mType}
    </div>
|]
    where
        currentBackToPath = pathTo (DashboardTransactionsAction (normalizePageParam currentPage) searchFilter mType)

renderTransactionCard :: (?context :: ControllerContext) => Text -> TransactionWithDetails -> Html
renderTransactionCard backToPath twd =
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
        marketUrl = ShowMarketAction market.id (Just asset.id) nextAction Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing (Just backToPath)

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
                                <div class="text-muted text-nowrap" style="font-size: 0.7rem;">Date & Time</div>
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

renderTxnPagination :: Int -> Int -> Maybe Text -> Maybe Text -> Html
renderTxnPagination currentPage totalPages searchFilter mType =
    renderSmartPagination currentPage totalPages "Transaction pagination"
        (\pageNum -> pathTo (DashboardTransactionsAction (Just pageNum) searchFilter mType))
