module Admin.Controller.Categories where

import Admin.Controller.Prelude
import Admin.View.Categories.Edit
import Admin.View.Categories.Index
import Admin.View.Categories.New

instance Controller CategoriesController where
    beforeAction = notFoundWhen (isNothing currentAdminOrNothing)

    action CategoriesAction = do
        categories <- query @Category |> fetch
        render IndexView { .. }

    action NewCategoryAction = do
        let category = newRecord
        render NewView { .. }

    action EditCategoryAction { categoryId } = do
        category <- fetch categoryId
        render EditView { .. }

    action UpdateCategoryAction { categoryId } = do
        category <- fetch categoryId
        category
            |> buildCategory
            |> ifValid \case
                Left category -> render EditView { .. }
                Right category -> do
                    category <- category |> updateRecord
                    setSuccessMessage "Category updated"
                    redirectTo EditCategoryAction { .. }

    action CreateCategoryAction = do
        let category = newRecord @Category
        category
            |> buildCategory
            |> ifValid \case
                Left category -> render NewView { .. }
                Right category -> do
                    category <- category |> createRecord
                    setSuccessMessage "Category created"
                    redirectTo CategoriesAction

    action DeleteCategoryAction { categoryId } = do
        category <- fetch categoryId
        deleteRecord category
        setSuccessMessage "Category deleted"
        redirectTo CategoriesAction

buildCategory category = category
    |> fill @'["name", "slug"]
