module Admin.Controller.Admins where

import Admin.Controller.Prelude
import Admin.View.Admins.Edit
import qualified Data.Text as Text

instance Controller AdminsController where
    beforeAction = ensureIsAdmin

    action EditAdminAction{adminId} = do
        accessDeniedUnless (adminId == currentAdminId)

        existing_admin <- fetch adminId :: IO Admin
        let admin = existing_admin |> set #passwordHash ""
        let passwordConfirmation = ""
        render EditView{..}

    action UpdateAdminAction{adminId} = do
        accessDeniedUnless (adminId == currentAdminId)

        admin <- fetch adminId
        let originalPasswordHash = admin.passwordHash
        let passwordConfirmation = param @Text "passwordConfirmation"

        admin
            |> fill @["email", "passwordHash"]
            |> validateField #passwordHash (isEqual passwordConfirmation
                |> withCustomErrorMessage "Passwords don't match")
            |> validateField #email isEmail
            |> validateIsUniqueCaseInsensitive #email
            >>= ifValid \case
                Left admin -> render EditView{..}
                Right admin -> do
                    hashed <-
                        if admin.passwordHash == "" || admin.passwordHash == originalPasswordHash
                            then pure originalPasswordHash
                            else hashPassword admin.passwordHash
                    admin <- admin
                        |> set #passwordHash hashed
                        |> updateRecord
                    setSuccessMessage "Profile updated"
                    redirectTo EditAdminAction{..}

    action DeleteAdminAction{adminId} = do
        accessDeniedUnless (adminId == currentAdminId)

        admin <- fetch adminId
        logout admin
        deleteRecord admin
        setSuccessMessage "Admin deleted"
        redirectToPath "/"
