module Web.View.Markets.Edit where
import Unsafe.Coerce
import Web.View.Prelude

instance CanSelect Category where
    type SelectValue Category = Id Category
    selectValue = get #id
    selectLabel = get #name

data EditView = EditView { market :: Market, assets :: [Asset], categories :: [Category] }

instance View EditView where
    html EditView { .. } = [hsx|
        {breadcrumb}
        <h1>Edit Market</h1>
        {renderForm market assets categories}
    |]
        where
            breadcrumb = renderBreadcrumb
                [ breadcrumbLink "Markets" MarketsAction
                , breadcrumbText "Edit Market"
                ]

renderForm :: Market -> [Asset] -> [Category] -> Html
renderForm market assets categories = formFor market [hsx|
    {(textField #title)}
    {(textField #description)}
    {(selectField #categoryId categories)}
    {(dateTimeField #closedAt) {
        fieldLabel = "Closing time",
        additionalAttributes =
            [ ("data-alt-format", "Y-m-d H:i")
            , ("data-month-selector-type", "static")
            ]
    }}
    <div class="mt-4">
        <h3>Assets</h3>
        <div class="row form-label">
            <div class="col">
                Asset Name
            </div>
            <div class="col">
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
                <input type="hidden" name="asset_id" value={tshow (get #id asset)}/>
                <div class="col">
                    <input
                        type="text"
                        name="asset_name"
                        value={get #name asset}
                        class="form-control"
                    />
                </div>
                <div class="col">
                    <input
                        type="text"
                        name="asset_symbol"
                        value={get #symbol asset}
                        class="form-control"
                    />
                </div>
            </div>
        |]
