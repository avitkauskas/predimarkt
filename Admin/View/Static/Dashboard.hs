module Admin.View.Static.Dashboard where
import Admin.View.Prelude

data DashboardView = DashboardView

instance View DashboardView where
    html DashboardView = [hsx|
        <h2 style="margin-top: 0; margin-bottom: 1rem; font-weight: 600; font-size: 1rem">
            Welcome to the Admin Area
        </h2>
        <div>
            <a href={CategoriesAction}>Categories</a>
        </div>
|]
