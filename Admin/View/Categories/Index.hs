module Admin.View.Categories.Index where
import Admin.View.Prelude

data IndexView = IndexView { categories :: [Category] }

instance View IndexView where
    html IndexView { .. } = [hsx|
        {breadcrumb}
        <h3>Categories <a href={NewCategoryAction} class="btn btn-primary ms-4">+ New</a></h3>
        <div class="table-responsive">
            <table class="table">
                <thead>
                    <tr>
                        <th>Name</th>
                        <th>Slug</th>
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
                [ breadcrumbLink "Dashboard" DashboardAction
                , breadcrumbText "Categories"
                ]

renderCategory :: Category -> Html
renderCategory category = [hsx|
    <tr>
        <td>{category.name}</td>
        <td>{category.slug}</td>
        <td><a href={EditCategoryAction category.id} class="text-muted">Edit</a></td>
        <td><a href={DeleteCategoryAction category.id} class="js-delete text-muted">Delete</a></td>
    </tr>
|]
