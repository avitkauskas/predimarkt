module Web.View.Markets.New where
import Web.View.Prelude

data NewView = NewView { market :: Market }

instance View NewView where
    html NewView { .. } = [hsx|
        {breadcrumb}
        <h1>New Market</h1>
        {renderForm market}
    |]
        where
            breadcrumb = renderBreadcrumb
                [ breadcrumbLink "Markets" MarketsAction
                , breadcrumbText "New Market"
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
