module Admin.View.Categories.Edit where
import Admin.View.Prelude

data EditView = EditView { category :: Category }

instance View EditView where
    html EditView { .. } = [hsx|
        {breadcrumb}
        <h1>Edit Category</h1>
        {renderForm category}
    |]
        where
            breadcrumb = renderBreadcrumb
                [ breadcrumbLinkExternal "Dashboard" "/admin/"
                , breadcrumbLink "Categories" CategoriesAction
                , breadcrumbText "Edit Category"
                ]

renderForm :: Category -> Html
renderForm category = formFor category [hsx|
    {(textField #name)}
    {(textField #slug)}
    {submitButton}

|]
