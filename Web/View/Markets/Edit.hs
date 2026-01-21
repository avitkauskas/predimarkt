module Web.View.Markets.Edit where
import Web.View.Prelude

instance CanSelect Category where
    type SelectValue Category = Id Category
    selectValue = get #id
    selectLabel = get #name

data EditView = EditView { market :: Market, categories :: [Category] }

instance View EditView where
    html EditView { .. } = [hsx|
        {breadcrumb}
        <h1>Edit Market</h1>
        {renderForm market categories}
    |]
        where
            breadcrumb = renderBreadcrumb
                [ breadcrumbLink "Markets" MarketsAction
                , breadcrumbText "Edit Market"
                ]

renderForm :: Market -> [Category] -> Html
renderForm market categories = formFor market [hsx|
    {(textField #title)}
    {(textField #description)}
    {(selectField #categoryId categories)}
    {(dateTimeField #closedAt)}
    {submitButton}

|]
