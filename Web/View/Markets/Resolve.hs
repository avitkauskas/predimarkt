module Web.View.Markets.Resolve where

import Web.View.Prelude

data ResolveView = ResolveView
    { market :: Market
    , assets :: [Asset]
    , backTo :: Text
    }

instance View ResolveView where
    html ResolveView { .. } = [hsx|
        <div class="py-3" style="max-width: 800px; margin: 0 auto;">
            <div class="card shadow-sm">
                <div class="card-header text-muted py-2">
                    <span class="ms-2">Resolve Market</span>
                </div>
                <div class="card-body p-4">
                    <header>
                        <div class="d-flex align-items-start gap-2 ms-2 mb-3">
                            <a href={backTo}
                                class="btn btn-outline-secondary back-button flex-shrink-0"
                                aria-label="Go back">
                                <i class="bi bi-chevron-left"></i>
                            </a>
                            <div class="flex-grow-1 ms-1" style="padding-top: 0.29rem;">
                                <span class="h4 fw-semibold">
                                    {market.title}
                                </span>
                            </div>
                        </div>
                    </header>

                    <div class="alert alert-info ms-2" role="alert">
                        <div class="fw-semibold mb-2">
                            Select outcome
                        </div>
                        <div class="mb-1">
                            Choose the asset that reflects what actually happened.
                        </div>
                        <div class="mb-2">
                            All positions will be settled based on your selection.
                        </div>
                        <div class="text-danger fw-medium">
                            This action cannot be undone!
                        </div>
                    </div>

                    {resolveForm}
                </div>
            </div>
        </div>
    |]
      where
        resolveForm = renderPostForm (pathTo (ResolveMarketAction market.id)) [("class", "ms-2")] [hsx|
            <div class="mb-4">
                <label class="form-label fw-semibold">Select Winning Asset</label>
                <div>
                    {forEach assets renderAssetOption}
                </div>
            </div>

            <div class="d-flex gap-2">
                <button type="submit" class="btn btn-primary">Resolve Market</button>
                <a href={backTo} class="btn btn-outline-secondary">Cancel</a>
            </div>
        |]

renderAssetOption :: Asset -> Html
renderAssetOption asset = [hsx|
    <div class="form-check py-3 px-5 border rounded mb-3">
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
