module Web.View.Markets.Refund where

import Web.View.Prelude

data RefundView = RefundView { market :: Market }

instance View RefundView where
    html RefundView { .. } = [hsx|
        <div class="py-3" style="max-width: 800px; margin: 0 auto;">
            <div class="card shadow-sm">
                <div class="card-header text-muted py-2">
                    <span class="ms-2">Refund Market</span>
                </div>
                <div class="card-body p-4">
                    <header>
                        <button class="btn btn-outline-secondary back-button mb-3 ms-2"
                                onclick="history.back()"
                                type="button">
                            ←
                        </button>
                        <span class="h3 fw-bold mb-3 ms-2">{market.title}</span>
                    </header>

                    <div class="alert alert-warning mb-4 mx-2" role="alert">
                        <strong>Refunding this market:</strong><br/>
                        All users will receive back their net position amounts.<br/>
                        This will reverse all transactions and change the market status to "Refunded".
                    </div>

                    <div class="d-flex gap-2 ms-2">
                        <form method="POST" action={RefundMarketAction market.id} class="d-inline">
                            <button type="submit" class="btn btn-danger">Confirm Refund</button>
                        </form>
                        <a href="javascript:history.back()" class="btn btn-outline-secondary">Cancel</a>
                    </div>
                </div>
            </div>
        </div>
    |]
