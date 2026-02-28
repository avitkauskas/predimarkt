module Web.View.Assets.New where
import Web.View.Prelude

renderAssetRow :: Asset -> Integer -> Html
renderAssetRow asset beta = [hsx|
    <div class="row mb-2 asset-row flex-nowrap" style="min-width: 600px" data-beta={show beta}>
        <input type="hidden" name="asset_id" value={asset.id}/>
        <div class="col" style="max-width: 375px">
            <input
                type="text"
                name="asset_name"
                value={asset.name}
                class="form-control"
                onkeydown="if(event.key === 'Enter') { event.target.form?.elements[event.target.tabIndex]?.focus(); }"
            />
        </div>
        <div class="col" style="max-width: 120px">
            <input
                type="text"
                name="asset_symbol"
                value={asset.symbol}
                class="form-control"
                maxlength="6"
                onkeydown="if(event.key === 'Enter') { event.target.form?.elements[event.target.tabIndex]?.focus(); }"
            />
        </div>
        <div class="col" style="max-width: 80px">
            <span class="form-control-plaintext text-center asset-percentage text-muted">--</span>
        </div>
        <div class="col" style="max-width: 120px">
             <input
                type="number"
                name="asset_quantity"
                value={show asset.quantity}
                class="form-control asset-quantity"
                step="10"
                onkeydown="if(event.key === 'Enter') { event.target.form?.elements[event.target.tabIndex]?.focus(); }"
            />
        </div>
        <div class="col d-flex align-items-center justify-content-end" style="max-width: 40px">
            <a href="#"
               class="text-danger"
               onclick="deleteAssetRow(this); return false;">
                <i class="bi bi-x-lg"></i>
            </a>
        </div>
    </div>
|]
