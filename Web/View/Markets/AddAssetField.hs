module Web.View.Markets.AddAssetField (renderAssetRow) where
import Web.View.Prelude

data AddAssetFieldView = AddAssetFieldView { asset :: Asset }

instance View AddAssetFieldView where
    html AddAssetFieldView { .. } = renderAssetRow asset

renderAssetRow :: Asset -> Html
renderAssetRow asset = [hsx|
    <div class="row mb-2 asset-row">
        <input type="hidden" name="asset_id" value={asset.id}/>
        <div class="col">
            <input
                type="text"
                name="asset_name"
                value={asset.name}
                class="form-control"
                placeholder="Name"
            />
        </div>
        <div class="col-auto">
            <input
                type="text"
                name="asset_symbol"
                value={asset.symbol}
                class="form-control"
                size="6"
                placeholder="Symbol"
            />
        </div>
        <div class="col-auto d-flex align-items-center">
            <a href="#" class="text-danger delete-asset-btn" onclick="if(document.querySelectorAll('.asset-row').length > 2) { this.closest('.asset-row').remove(); } else { alert('Market must have at least 2 assets.'); } return false;">
                Delete
            </a>
        </div>
    </div>
|]
