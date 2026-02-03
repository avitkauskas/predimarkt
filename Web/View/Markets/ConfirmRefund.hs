module Web.View.Markets.ConfirmRefund where

import Web.View.Prelude

data ConfirmRefundView = ConfirmRefundView
    { market :: Market
    }

instance View ConfirmRefundView where
    html ConfirmRefundView { .. } = [hsx|
        <div class="py-3" style="max-width: 800px; margin: 0 auto;">
            <div class="card shadow-sm">
                <div class="card-header text-muted py-2">
                    <span class="ms-2">Refund Market</span>
                </div>
                <div class="card-body p-4">
                    <header class="mb-4">
                        <button class="btn btn-outline-secondary back-button mb-3"
                                onclick="history.back()"
                                type="button"
                                title="Go back">
                            ←
                        </button>
                        <span class="h3 fw-bold mb-3 ms-2">{market.title}</span>
                        <p class="text-muted ms-2">{market.description}</p>
                    </header>

                    <div class="alert alert-warning ms-2" role="alert">
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
