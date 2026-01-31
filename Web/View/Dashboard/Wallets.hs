module Web.View.Dashboard.Wallets where

import Web.View.Prelude
import Web.Types.Money

data WalletsView = WalletsView { wallet :: Wallet }

instance View WalletsView where
    html WalletsView { wallet } = dashboardLayout [hsx|
        <div class="h-100">
            <h3>Balance</h3>
            <p class="fs-3">{formatMoney (moneyFromCents wallet.balanceCents)}</p>
        </div>
    |]