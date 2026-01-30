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
                [ breadcrumbLink "Dashboard" DashboardAction
                , breadcrumbLink "Categories" CategoriesAction
                , breadcrumbText "New Category"
                ]

renderForm :: Category -> Html
renderForm category = formFor category [hsx|
    {(textField #name)}
    {(textField #slug)}
    {(numberField #sortIdx) { fieldLabel = "Sort Index", additionalAttributes = [("step", "10")] }}
    {submitButton}

|]
