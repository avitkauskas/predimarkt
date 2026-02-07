module Web.View.Markets.Show where
import qualified Domain.LMSR as LMSR
import Text.Printf (printf)
import Web.View.Prelude

data ShowView = ShowView
    { market         :: Include' ["assets", "categoryId"] Market
    , tradingAssetId :: Maybe (Id Asset)
    , tradingAction  :: Maybe Text
    }

instance View ShowView where
    html ShowView { .. } = [hsx|
        <div class="py-3" style="max-width: 800px; margin: 0 auto;">
            <div class="card shadow-sm">
                <div class={classes ["card-header text-muted d-flex justify-content-between align-items-center py-2", (headerClass, True)]}>
                    <span class="ms-2">{market.categoryId.name}</span>
                    <div class="me-2">
                        {statusBadge}
                    </div>
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

            lmsrState = LMSR.precompute market.beta [(a.symbol, a.quantity) | a <- market.assets]

            renderAsset :: Asset -> Html
            renderAsset asset = [hsx|
                <div class="py-3 border-bottom"
                     id={"asset-" <> show asset.id}>
                    <div class="d-flex justify-content-between align-items-center">
                        <div class="asset-info ms-3">
                            <div class="fw-semibold fs-5">{asset.name}</div>
                        </div>
                        <div class="asset-actions d-flex align-items-center gap-2">
                            {priceDisplay}
                            {when (isTradable) buySellButtons}
                        </div>
                    </div>

                    {when (isTradable) buySellForms}
                </div>
            |]
                where
                    isTradable = market.status == MarketStatusOpen
                    isBuyFormOpen = tradingAssetId == Just asset.id && tradingAction == Just "buy"
                    isSellFormOpen = tradingAssetId == Just asset.id && tradingAction == Just "sell"

                    assetPrice :: Double
                    assetPrice = LMSR.price asset.symbol lmsrState

                    assetSum :: Double
                    assetSum = LMSR.sumItem asset.symbol lmsrState

                    assetTotal :: Double
                    assetTotal = LMSR.sumTotal lmsrState

                    priceDisplay = [hsx|
                        <div class="text-end fw-medium pe-4" style="width: 100px;">
                            €{printf "%.4f" assetPrice :: String}
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
                        <div id={"buy-form-" <> show asset.id}
                             class={classes ["mt-3", ("d-none", not isBuyFormOpen)]}>
                            <form action={TradeAssetAction asset.id} method="POST">
                                <input type="hidden" name="type" value="buy" />
                                <div class="d-flex flex-column align-items-end gap-2">
                                    <div class="d-flex align-items-center gap-3">
                                        <div class="input-group" style="width: 160px">
                                            <span class="input-group-text text-muted">shares</span>
                                            <input type="number" name="quantity"
                                                   value="10" step="1" min="0"
                                                   class="form-control"
                                                   autofocus={isBuyFormOpen}
                                                   oninput="updateBuyInfo(this)"
                                                   data-info-id={"buy-info-" <> show asset.id}
                                                   data-a={show (assetSum / assetTotal)}
                                                   data-beta={show market.beta} />
                                        </div>
                                        <button type="submit" class="btn btn-primary fw-bold"
                                                style="width: 140px">BUY</button>
                                    </div>
                                    <div id={"buy-info-" <> show asset.id}
                                         class="trade-info-container text-end w-100 d-none"
                                         style="max-width: 320px;">
                                    </div>
                                </div>
                            </form>
                        </div>

                        <div id={"sell-form-" <> show asset.id}
                             class={classes ["mt-3", ("d-none", not isSellFormOpen)]}>
                            <form action={TradeAssetAction asset.id} method="POST">
                                <input type="hidden" name="type" value="sell" />
                                <div class="d-flex flex-column align-items-end gap-2">
                                    <div class="d-flex align-items-center gap-3">
                                        <div class="input-group" style="width: 160px">
                                            <span class="input-group-text text-muted">shares</span>
                                            <input type="number" name="quantity"
                                                   value="10" step="1" min="0"
                                                   class="form-control"
                                                   autofocus={isSellFormOpen}
                                                   oninput="updateSellInfo(this)"
                                                   data-info-id={"sell-info-" <> show asset.id}
                                                   data-a={show (assetSum / assetTotal)}
                                                   data-beta={show market.beta} />
                                        </div>
                                        <button type="submit" class="btn btn-primary fw-bold"
                                                style="width: 140px">SELL</button>
                                    </div>
                                    <div id={"sell-info-" <> show asset.id}
                                         class="trade-info-container text-end w-100 d-none"
                                         style="max-width: 320px;">
                                    </div>
                                </div>
                            </form>
                        </div>
                    |]

                    toggleForm :: Text -> Text -> Text
                    toggleForm id type' = "window.toggleAssetForm('" <> id <> "', '" <> type' <> "')"
