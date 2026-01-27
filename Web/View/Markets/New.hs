module Web.View.Markets.New where
import Web.View.Prelude

instance CanSelect Category where
    type SelectValue Category = Id Category
    selectValue = get #id
    selectLabel = get #name

data NewView = NewView { market :: Market, assets :: [Asset], categories :: [Category] }

instance View NewView where
    html NewView { .. } = [hsx|
        {breadcrumb}
        <h1>New Market</h1>
        {renderForm market assets categories}
    |]
        where
            breadcrumb = renderBreadcrumb
                [ breadcrumbLink "Markets" MarketsAction
                , breadcrumbText "New Market"
                ]

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
            ]
    }}
    <div class="mt-4">
        <h3>Initial Assets</h3>
        {forEach assets renderAsset}
    </div>
    {submitButton}
|]
    where
        renderAsset :: Asset -> Html
        renderAsset asset = [hsx|
            <div class="row mb-3">
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

