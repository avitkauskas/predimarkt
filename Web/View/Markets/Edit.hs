module Web.View.Markets.Edit where
import Web.View.Prelude

data EditView = EditView { market :: Market }

instance View EditView where
    html EditView { .. } = [hsx|
        {breadcrumb}
        <h1>Edit Market</h1>
        {renderForm market}
    |]
        where
            breadcrumb = renderBreadcrumb
                [ breadcrumbLink "Markets" MarketsAction
                , breadcrumbText "Edit Market"
                ]

renderForm :: Market -> Html
renderForm market = formFor market [hsx|
    {(textField #userId)}
    {(textField #title)}
    {(textField #slug)}
    {(textField #description)}
    {(textField #categoryId)}
    {(textField #beta)}
    {(textField #status)}
    {(textField #closedAt)}
    {submitButton}

|]
