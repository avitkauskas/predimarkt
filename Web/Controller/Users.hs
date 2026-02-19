module Web.Controller.Users where

import Web.Controller.Prelude
import Web.View.Users.Edit

instance Controller UsersController where
    action EditUserAction{userId} = do
        ensureIsUser
        accessDeniedUnless (userId == currentUserId)

        user <- fetch userId
        render EditView{..}

    action UpdateUserAction{userId} = do
        ensureIsUser
        accessDeniedUnless (userId == currentUserId)

        user <- fetch userId
        user
            |> fill @'["nickname"]
            |> validateIsUniqueCaseInsensitive #nickname
            >>= ifValid \case
                Left user -> render EditView{..}
                Right user -> do
                    _ <- user |> updateRecord
                    setSuccessMessage "Profile updated"
                    redirectTo EditUserAction{..}

    action DeleteUserAction{userId} = do
        ensureIsUser
        accessDeniedUnless (userId == currentUserId)

        user <- fetch userId
        logout user
        deleteRecord user
        setSuccessMessage "Your account has been deleted"
        redirectToPath "/"
