{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module Web.View.Categories.Show where
import Web.View.Prelude

data ShowView = ShowView { category :: Category }

instance View ShowView where
    html ShowView { .. } = [hsx|
        {breadcrumb}
        <h1>Show Category</h1>
        <p>{category}</p>

    |]
        where
            breadcrumb = renderBreadcrumb
                            [ breadcrumbLink "Categories" CategoriesAction
                            , breadcrumbText "Show Category"
                            ]
