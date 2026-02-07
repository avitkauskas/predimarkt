module Web.View.Dashboard.Transactions where

import Application.Helper.View (formatMoney)
import Data.Time.Format (defaultTimeLocale, formatTime)
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
        <div class="h-100">
            <h3>My Transactions</h3>
            <div class="table-responsive">
                <table class="table table-sm table-responsive table-hover table-borderless">
                    <thead>
                        <tr>
                            <th>Date & Time</th>
                            <th>Market & Asset</th>
                            <th class="text-center">Type</th>
                            <th class="text-end">Qty</th>
                            <th class="text-end">Money</th>
                        </tr>
                    </thead>
                    <tbody>
                        {forEach transactionsWithDetails renderTransaction}
                    </tbody>
                </table>
            </div>
            {renderTxnPagination currentPage totalPages}
        </div>
    |]

renderTransaction :: (?context :: ControllerContext) => TransactionWithDetails -> Html
renderTransaction TransactionWithDetails { .. } =
    let txn = transaction
        asset = txn.assetId
        market = txn.marketId
        txnType = case txn.side of
            "long"  -> "buy" :: Text
            "short" -> "sell" :: Text
            _       -> txn.side
        typeClass = if txn.side == "long" then "text-center text-success" else "text-center text-danger" :: Text
        quantity = abs txn.quantity
        money = formatMoney $ abs txn.cashFlow
        timeStr = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" txn.createdAt
    in [hsx|
        <tr class="small">
            <td class="text-nowrap">{timeStr}</td>
            <td class="text-nowrap">{market.title} - {asset.name}</td>
            <td class={typeClass}>{txnType}</td>
            <td class="text-end">{show quantity}</td>
            <td class="text-end">{money}</td>
        </tr>
    |]

renderTxnPagination :: Int -> Int -> Html
renderTxnPagination currentPage totalPages =
    if totalPages <= 1
    then [hsx||]
    else [hsx|
        <nav aria-label="Transaction pagination">
            <ul class="pagination pagination-sm justify-content-end">
                {renderPrevButton}
                {forEach [1..totalPages] renderPageButton}
                {renderNextButton}
            </ul>
        </nav>
    |]
    where
        renderPrevButton =
            if currentPage <= 1
            then [hsx|
                <li class="page-item disabled">
                    <span class="page-link">Previous</span>
                </li>
            |]
            else [hsx|
                <li class="page-item">
                    <a class="page-link" href={DashboardTransactionsAction (Just (currentPage - 1))}>Previous</a>
                </li>
            |]

        renderNextButton =
            if currentPage >= totalPages
            then [hsx|
                <li class="page-item disabled">
                    <span class="page-link">Next</span>
                </li>
            |]
            else [hsx|
                <li class="page-item">
                    <a class="page-link" href={DashboardTransactionsAction (Just (currentPage + 1))}>Next</a>
                </li>
            |]

        renderPageButton pageNum =
            if pageNum == currentPage
            then [hsx|
                <li class="page-item active">
                    <span class="page-link">{show pageNum}</span>
                </li>
            |]
            else [hsx|
                <li class="page-item">
                    <a class="page-link" href={DashboardTransactionsAction (Just pageNum)}>{show pageNum}</a>
                </li>
            |]
