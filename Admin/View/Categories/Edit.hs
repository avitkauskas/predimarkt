module Admin.View.Categories.Edit where
import Admin.View.Prelude
import Data.Attoparsec.ByteString.Char8

data EditView = EditView { category :: Category }

instance View EditView where
    html EditView { .. } = [hsx|
        {breadcrumb}
        <h1>Edit Category</h1>
        {renderForm category}
    |]
        where
            breadcrumb = renderBreadcrumb
                [ breadcrumbLink "Dashboard" DashboardAction
                , breadcrumbLink "Categories" CategoriesAction
                , breadcrumbText "Edit Category"
                ]

renderForm :: Category -> Html
renderForm category = formFor category [hsx|
    {(textField #name)}
    {(textField #slug)}
    {(numberField #sortIdx) { fieldLabel = "Sort Index", additionalAttributes = [("step", "10")] }}
    {submitButton}

|]
