module Web.View.Markets.New where
import Web.View.Prelude


instance CanSelect Category where
    type SelectValue Category = Id Category
    selectValue = get #id
    selectLabel = get #name

data NewView = NewView { market :: Market, categories :: [Category] }

instance View NewView where
    html NewView { .. } = [hsx|
        {breadcrumb}
        <h1>New Market</h1>
        {renderForm market categories}
    |]
        where
            breadcrumb = renderBreadcrumb
                [ breadcrumbLink "Markets" MarketsAction
                , breadcrumbText "New Market"
                ]

renderForm :: Market -> [Category] -> Html
renderForm market categories = formFor market [hsx|
    {(textField #title)}
    {(textField #description)}
    {(selectField #categoryId categories)}
    {(dateTimeField #closedAt)}
    {submitButton}

|]
