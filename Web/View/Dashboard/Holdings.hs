module Web.View.Dashboard.Holdings where

import Web.View.Prelude

data HoldingsView = HoldingsView

instance View HoldingsView where
    html HoldingsView = dashboardLayout [hsx|
        <div class="h-100">
            <h3>My Holdings</h3>
            <p class="text-muted">You don't have any holdings yet. (This is a stub).</p>
        </div>
    |]
