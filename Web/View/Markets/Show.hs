module Web.View.Markets.Show where
import Web.View.Prelude
import Text.Printf (printf)

data ShowView = ShowView { market :: Include' ["assets", "categoryId"] Market }

instance View ShowView where
    html ShowView { .. } = [hsx|
        <div class="py-3">
            <div class="card shadow-sm">
                <div class={classes [("card-header", True), ("text-muted", True), ("small", True), ("d-flex", True), ("justify-content-between", True), ("align-items-center", True), ("py-2", True), (headerClass, True)]}>
                    <span class="ms-2">{market.categoryId.name}</span>
                    <div class="me-2">
                        {statusBadge}
                    </div>
                </div>
                <div class="card-body p-4">
                    <header class="mb-4">
                        <h1 class="h3 fw-bold mb-3 ms-2">{market.title}</h1>
                        <div class="market-description-box p-3">
                            <p class="text-muted mb-0">{market.description}</p>
                        </div>
                    </header>

                    <div class="assets-list border-top mt-4">
                        {forEach market.assets renderAsset}
                    </div>
                </div>
            </div>
        </div>
    |]
        where
            headerClass = marketStatusHeaderClasses market.status
            statusBadge =
                when (market.status /= MarketStatusOpen)
                    [hsx|<span>{marketStatusLabel market.status}</span>|]
            
            lmsrState = precompute market.beta market.assets

            renderAsset :: Asset -> Html
            renderAsset asset = [hsx|
                <div class={classes ["py-3 border-bottom", (assetStatusClasses asset.status, True)]}
                     id={"asset-" <> show asset.id}>
                    <div class="d-flex justify-content-between align-items-center">
                        <div class="asset-info ms-3">
                            <div class="fw-semibold fs-5">{asset.name}</div>
                        </div>
                        <div class="asset-actions d-flex align-items-center gap-2">
                            {priceDisplay}
                            {renderStatus asset.status}
                            {when (isTradable) buySellButtons}
                        </div>
                    </div>

                    {when (isTradable) buySellForms}
                </div>
            |]
                where
                    isTradable = market.status == MarketStatusOpen && asset.status == AssetStatusOpen
                    
                    assetPrice :: Double
                    assetPrice = price asset.id lmsrState

                    priceDisplay = [hsx|
                        <div class="text-center fw-medium" style="width: 80px; font-variant-numeric: tabular-nums;">
                            {printf "%.2f" assetPrice :: String}
                        </div>
                    |]

                    buySellButtons = [hsx|
                        <div class="btn-group shadow-sm" style="width: 140px">
                            <button class="btn btn-soft-success btn-sm fw-bold w-50" 
                                    type="button" 
                                    onclick={toggleForm (show asset.id) "buy"}>
                                Buy
                            </button>
                            <button class="btn btn-soft-danger btn-sm fw-bold w-50" 
                                    type="button" 
                                    onclick={toggleForm (show asset.id) "sell"}>
                                Sell
                            </button>
                        </div>
                    |]

                    buySellForms = [hsx|
                        <div id={"buy-form-" <> show asset.id} class="asset-form-container d-none mt-3">
                            <div class="d-flex justify-content-end align-items-center gap-3">
                                <div class="text-muted small">Number of shares to BUY</div>
                                <div class="d-flex align-items-center gap-2">
                                    <input type="number" step="10" min="0" 
                                           class="form-control text-start" 
                                           style="width: 80px" value="10" />
                                    <button class="btn btn-primary fw-bold" style="width: 140px">BUY</button>
                                </div>
                            </div>
                        </div>

                        <div id={"sell-form-" <> show asset.id} class="asset-form-container d-none mt-3">
                            <div class="d-flex justify-content-end align-items-center gap-3">
                                <div class="text-muted small">Number of shares to SELL</div>
                                <div class="d-flex align-items-center gap-2">
                                    <input type="number" step="10" min="0" 
                                           class="form-control text-start" 
                                           style="width: 80px" value="10" />
                                    <button class="btn btn-primary fw-bold" style="width: 140px">SELL</button>
                                </div>
                            </div>
                        </div>
                    |]

                    renderStatus :: AssetStatus -> Html
                    renderStatus AssetStatusOpen = [hsx||]
                    renderStatus status = [hsx|
                        <div class="text-center small" style="width: 140px;">
                            {assetStatusLabel status}
                        </div>
                    |]

                    toggleForm :: Text -> Text -> Text
                    toggleForm id type' = "window.toggleAssetForm('" <> id <> "', '" <> type' <> "')"
