module Web.View.Dashboard.Wallets where

import Application.Helper.View (formatMoney)
import Web.View.Prelude

data WalletsView = WalletsView { wallet :: Wallet }

instance View WalletsView where
    html WalletsView { wallet } = dashboardLayout [hsx|
        <div class="h-100">
            <h3>Balance</h3>
            <p class="fs-3">{formatMoney wallet.amount}</p>
        </div>
    |]