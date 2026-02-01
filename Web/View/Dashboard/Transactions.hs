module Web.View.Dashboard.Transactions where

import Data.Time.Format (defaultTimeLocale, formatTime)
import Web.Types.Money
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
                <table class="table table-hover table-borderless">
                    <thead>
                        <tr>
                            <th>Time</th>
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
        txnType = if txn.quantity > 0 then "buy" else "sell" :: Text
        typeClass = if txn.quantity > 0 then "text-center text-success" else "text-center text-danger" :: Text
        quantity = abs txn.quantity
        money = formatMoney $ moneyFromCents txn.amountCents
        timeStr = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" txn.createdAt
    in [hsx|
        <tr>
            <td>{timeStr}</td>
            <td>{market.title} - {asset.name}</td>
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
            <ul class="pagination justify-content-center">
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
