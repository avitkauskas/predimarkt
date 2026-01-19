{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module Web.View.Categories.New where
import Web.View.Prelude

data NewView = NewView { category :: Category }

instance View NewView where
    html NewView { .. } = [hsx|
        {breadcrumb}
        <h1>New Category</h1>
        {renderForm category}
    |]
        where
            breadcrumb = renderBreadcrumb
                [ breadcrumbLink "Categories" CategoriesAction
                , breadcrumbText "New Category"
                ]

renderForm :: Category -> Html
renderForm category = formFor category [hsx|
    {(textField #name)}
    {(textField #slug)}
    {submitButton}

|]
