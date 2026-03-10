module Web.View.Markets.New where
import Web.View.Assets.New
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
                        {renderFlashMessages}
                        {renderForm market assets categories}
                    </div>
                </div>
            </div>
        </div>
    |]

renderForm :: Market -> [Asset] -> [Category] -> Html
renderForm market assets categories = formFor market [hsx|
    {(textField #title)}
    {(textareaField #description) {
        fieldLabel = "Rules & Description",
        additionalAttributes = [("rows", "3")]
    }}
    <div class="row">
        <div class="col-12 col-md-6">
            {(selectField #categoryId categories)}
        </div>
        <div class="col-12 col-md-6">
            {(dateTimeField #closedAt) {
                fieldLabel = "Closing time",
                additionalAttributes =
                    [ ("data-alt-format", "Y-m-d H:i")
                    , ("data-month-selector-type", "static")
                    , ("data-allow-input", "true")
                    ]
            }}
        </div>
    </div>
    <div class="mb-4">
        <div class="overflow-auto">
            <div class="row flex-nowrap" style="min-width: 600px">
                <div class="col form-label" style="max-width: 375px">
                    Asset Name
                </div>
                <div class="col form-label" style="max-width: 120px">
                    Symbol
                </div>
                <div class="col form-label text-center" style="max-width: 80px">
                    Prob
                </div>
                <div class="col form-label" style="max-width: 120px">
                    Quantity
                </div>
                <div class="col" style="max-width: 40px">
                </div>
            </div>
            <div id="assets-list" data-beta="300">
                {forEach assets (\asset -> renderAssetRow asset 300)}
            </div>
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
    <div class="d-flex gap-2">
        {submitButton}
        <a href={DashboardMarketsAction (Just MarketStatusDraft)}
           class="btn btn-outline-secondary">
            Cancel
        </a>
    </div>
|]
