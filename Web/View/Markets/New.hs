module Web.View.Markets.New where
import Web.View.Prelude


instance CanSelect Category where
    type SelectValue Category = Id Category
    selectValue = get #id
    selectLabel = get #name

data NewView = NewView { market :: Market, assets :: [Asset], categories :: [Category] }

instance View NewView where
    html NewView { .. } = [hsx|
        <h1>New Market</h1>
        {renderForm market assets categories}
    |]

renderForm :: Market -> [Asset] -> [Category] -> Html
renderForm market assets categories = formFor market [hsx|
    {(textField #title)}
    {(textField #description)}
    {(selectField #categoryId categories)}
    {(dateTimeField #closedAt) { fieldClass = "my-flatpickr" }}
    <div class="mt-4">
        <h3>Initial Assets</h3>
        {forEachWithIndex assets renderAssetField}
    </div>
    {submitButton}
|]
    where
        renderAssetField (index, asset) = [hsx|
            <div class="row mb-3">
                <div class="col">
                    <label class="form-label">Name</label>
                    <input type="text" name={"asset_name_" <> show index} value={asset.name} class="form-control" placeholder="e.g. Yes" required />
                </div>
                <div class="col">
                    <label class="form-label">Label</label>
                    <input type="text" name={"asset_label_" <> show index} value={asset.label_} class="form-control" placeholder="e.g. Yes" required />
                </div>
            </div>
        |]

        -- closedAtField = [hsx|
        --     <div class="mb-3" id="form-group-market_closedAt">
        --         <label for="market_closedAt" class="form-label">Will close at</label>
        --         <input
        --             type="text"
        --             id="market_closedAt"
        --             name="closedAt"
        --             value={show (get #closedAt market)}
        --             class="form-control js-flatpickr"
        --             data-enable-time="true"
        --             />
        --     </div>
        -- |]
