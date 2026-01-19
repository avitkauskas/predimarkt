{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module Web.View.Categories.Index where
import Web.View.Prelude

data IndexView = IndexView { categories :: [Category] }

instance View IndexView where
    html IndexView { .. } = [hsx|
        {breadcrumb}

        <h1>Index<a href={pathTo NewCategoryAction} class="btn btn-primary ms-4">+ New</a></h1>
        <div class="table-responsive">
            <table class="table">
                <thead>
                    <tr>
                        <th>Category</th>
                        <th></th>
                        <th></th>
                        <th></th>
                    </tr>
                </thead>
                <tbody>{forEach categories renderCategory}</tbody>
            </table>

        </div>
    |]
        where
            breadcrumb = renderBreadcrumb
                [ breadcrumbLink "Categories" CategoriesAction
                ]

renderCategory :: Category -> Html
renderCategory category = [hsx|
    <tr>
        <td>{category}</td>
        <td><a href={ShowCategoryAction category.id}>Show</a></td>
        <td><a href={EditCategoryAction category.id} class="text-muted">Edit</a></td>
        <td><a href={DeleteCategoryAction category.id} class="js-delete text-muted">Delete</a></td>
    </tr>
|]
