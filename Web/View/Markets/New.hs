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
    {(dateTimeField #closedAt) {
        additionalAttributes =
            [ ("data-alt-format", "Y-m-d H:i")
            , ("data-month-selector-type", "static")
            ]
    }}
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
