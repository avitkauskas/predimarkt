module Admin.View.Categories.New where
import Admin.View.Prelude

data NewView = NewView { category :: Category }

instance View NewView where
    html NewView { .. } = [hsx|
        {breadcrumb}
        <h1>New Category</h1>
        {renderForm category}
    |]
        where
            breadcrumb = renderBreadcrumb
                [ breadcrumbLinkExternal "Dashboard" "/admin/"
                , breadcrumbLink "Categories" CategoriesAction
                , breadcrumbText "New Category"
                ]

renderForm :: Category -> Html
renderForm category = formFor category [hsx|
    {(textField #name)}
    {(textField #slug)}
    {submitButton}

|]
