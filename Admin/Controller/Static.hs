module Admin.Controller.Static where
import Admin.Controller.Prelude
import Admin.View.Static.Dashboard

instance Controller StaticController where
    beforeAction = notFoundWhen (isNothing currentAdminOrNothing)

    action DashboardAction = render DashboardView
