module Web.View.Markets.Show where
import Web.View.Prelude

data ShowView = ShowView { market :: Include "assets" Market }

instance View ShowView where
    html ShowView { .. } = [hsx|
        <div class="market-container py-3">
            <div class="card shadow-sm">
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
            renderAsset :: Asset -> Html
            renderAsset asset = [hsx|
                <div class={classes [("asset-row", True), ("py-3", True), ("border-bottom", True), (assetStatusClasses asset.status, True)]} id={"asset-" <> show asset.id}>
                    <div class="d-flex justify-content-between align-items-center">
                        <div class="asset-info ms-3">
                            <div class="fw-semibold fs-5">{asset.name}</div>
                        </div>
                        <div class="asset-actions d-flex align-items-center gap-3">
                            {renderStatus asset.status}
                            {when (isTradable) buySellButtons}
                        </div>
                    </div>

                    {when (isTradable) buySellForms}
                </div>
            |]
                where
                    isTradable = market.status == MarketStatusOpen && asset.status == AssetStatusOpen

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
                                <div class="text-muted small">Enter the quantity of shares to buy</div>
                                <div class="d-flex align-items-center gap-2">
                                    <input type="number" step="10" min="0" 
                                           class="form-control form-control-sm text-start" 
                                           style="width: 80px" />
                                    <button class="btn btn-orange btn-sm text-white fw-bold" style="width: 140px">Transact</button>
                                </div>
                            </div>
                        </div>

                        <div id={"sell-form-" <> show asset.id} class="asset-form-container d-none mt-3">
                            <div class="d-flex justify-content-end align-items-center gap-3">
                                <div class="text-muted small">Enter the quantity of shares to sell</div>
                                <div class="d-flex align-items-center gap-2">
                                    <input type="number" step="10" min="0" 
                                           class="form-control form-control-sm text-start" 
                                           style="width: 80px" />
                                    <button class="btn btn-orange btn-sm text-white fw-bold" style="width: 140px">Transact</button>
                                </div>
                            </div>
                        </div>
                    |]

                    renderStatus :: AssetStatus -> Html
                    renderStatus AssetStatusOpen = [hsx||]
                    renderStatus status = [hsx|
                        <div class="text-center text-secondary small" style="width: 140px; letter-spacing: 0.05em;">
                            {assetStatusLabel status}
                        </div>
                    |]

                    toggleForm :: Text -> Text -> Text
                    toggleForm id type' = "window.toggleAssetForm('" <> id <> "', '" <> type' <> "')"

