module Web.View.Markets.Edit where
import Web.View.Prelude
import Web.View.Assets.New

instance CanSelect Category where
    type SelectValue Category = Id Category
    selectValue = get #id
    selectLabel = get #name

data EditView = EditView { market :: Market, assets :: [Asset], categories :: [Category] }

instance View EditView where
    html EditView { .. } = dashboardLayout [hsx|
        <div class="row pt-2">
            <div class="col-12 col-xl-9">
                <div class="card shadow-sm">
                    <div class="card-body px-4 px-md-5 py-2 py-md-4">
                        <h3 class="mb-4">Edit Market</h3>
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
            <div class="col-auto">
            </div>
        </div>
        <div id="assets-list">
            {forEach assets renderAssetRow}
        </div>
         <div class="mt-2">
            <a href="#" 
               class="btn btn-outline-primary btn-sm"
               hx-get={NewAssetAction}
               hx-target="#assets-list"
               hx-swap="beforeend">
                Add Asset
            </a>
        </div>
    </div>
    {submitButton}
|]
