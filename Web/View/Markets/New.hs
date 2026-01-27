module Web.View.Markets.New where
import Web.View.Prelude

instance CanSelect Category where
    type SelectValue Category = Id Category
    selectValue = get #id
    selectLabel = get #name

data NewView = NewView { market :: Market, assets :: [Asset], categories :: [Category] }

instance View NewView where
    html NewView { .. } = dashboardLayout [hsx|
        <div class="row pt-2">
            <div class="col-12 col-xl-9">
                <div class="card shadow-sm">
                    <div class="card-body px-4 px-md-5 py-2 py-md-4">
                        <h3 class="mb-4">New Market</h3>
                        {renderForm market assets categories}
                    </div>
                </div>
            </div>
        </div>
    |]

renderForm :: Market -> [Asset] -> [Category] -> Html
renderForm market assets categories = formFor market [hsx|
    {(textField #title)}
    {(textareaField #description)}
    {(selectField #categoryId categories)}
    {(dateTimeField #closedAt) {
        fieldLabel = "Closing time",
        additionalAttributes =
            [ ("data-alt-format", "Y-m-d H:i")
            , ("data-month-selector-type", "static")
            , ("data-allow-input", "true")
            ]
    }}
    <div class="mb-4">
        <div class="row">
            <div class="col form-label">
                Asset Name
            </div>
            <div class="col-auto form-label" style="width: 100px">
                Symbol
            </div>
        </div>
        {forEach assets renderAsset}
    </div>
    {submitButton}
|]
    where
        renderAsset :: Asset -> Html
        renderAsset asset = [hsx|
            <div class="row mb-2">
                <input type="hidden" name="asset_id" value={asset.id}/>
                <div class="col">
                    <input
                        type="text"
                        name="asset_name"
                        value={asset.name}
                        class="form-control"
                    />
                </div>
                <div class="col-auto">
                    <input
                        type="text"
                        name="asset_symbol"
                        value={asset.symbol}
                        class="form-control"
                        size="6"
                    />
                </div>
            </div>
        |]

