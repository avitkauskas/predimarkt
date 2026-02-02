module Web.View.Markets.Resolve where
import Web.View.Prelude

data ResolveView = ResolveView 
    { market :: Market
    , assets :: [Asset]
    }

instance View ResolveView where
    html ResolveView { .. } = [hsx|
        <div class="py-3" style="max-width: 800px; margin: 0 auto;">
            <div class="card shadow-sm">
                <div class="card-header text-muted py-2">
                    <span class="ms-2">Resolve Market</span>
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

                    <div class="alert alert-info ms-2" role="alert">
                        <strong>Select the outcome:</strong> Choose which asset represents what actually happened. 
                        All user positions will be settled based on this selection.
                    </div>

                    <form method="POST" action={ResolveMarketAction market.id} class="ms-2">
                        <div class="mb-4">
                            <label class="form-label fw-semibold">Select Winning Asset</label>
                            <div class="assets-selection">
                                {forEach assets renderAssetOption}
                            </div>
                        </div>

                        <div class="d-flex gap-2">
                            <button type="submit" class="btn btn-primary">Resolve Market</button>
                            <a href={ShowMarketAction market.id Nothing Nothing} class="btn btn-outline-secondary">Cancel</a>
                        </div>
                    </form>
                </div>
            </div>
        </div>
    |]

renderAssetOption :: Asset -> Html
renderAssetOption asset = [hsx|
    <div class="form-check p-3 border rounded mb-3">
        <input 
            class="form-check-input" 
            type="radio" 
            name="outcomeAssetId" 
            id={"asset-" <> show asset.id}
            value={show asset.id}
            required>
        <label class="form-check-label fw-semibold" for={"asset-" <> show asset.id}>
            {asset.name}
        </label>
    </div>
|]
