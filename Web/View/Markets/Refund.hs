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
                        <div class="d-flex align-items-start gap-2 ms-2 mb-3">
                            <button
                                onclick="history.back()"
                                class="btn btn-outline-secondary back-button flex-shrink-0">
                                <i class="bi bi-chevron-left"></i>
                            </button>
                            <div class="flex-grow-1 ms-1" style="padding-top: 0.29rem;">
                                <span class="h4 fw-semibold">
                                    {market.title}
                                </span>
                            </div>
                        </div>
                    </header>

                    <div class="alert alert-warning mb-4 mx-2" role="alert">
                        <div class="fw-semibold mb-2">
                            Refunding the market
                        </div>
                        <div class="mb-2">
                            All transactions will be reversed, and users will receive their net position amounts.
                        </div>
                        <div class="text-danger fw-medium">
                            This action cannot be undone!
                        </div>
                    </div>

                    <div class="d-flex gap-2 ms-2">
                        {refundForm}
                        <a href="javascript:history.back()" class="btn btn-outline-secondary">Cancel</a>
                    </div>
                </div>
            </div>
        </div>
    |]
      where
        refundForm = renderPostForm (pathTo (RefundMarketAction market.id)) [("class", "d-inline")] [hsx|
            <button type="submit" class="btn btn-danger">Confirm Refund</button>
        |]
