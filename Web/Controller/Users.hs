module Web.Controller.Users where

import Data.Functor (void)
import Web.Controller.Prelude
import Web.View.Users.Edit

instance Controller UsersController where
    action EditUserAction{userId} = do
        ensureIsUser
        accessDeniedUnless (userId == currentUserId)

        user <- fetch userId
        passkeys <- fetchUserPasskeys user.id
        render EditView{..}

    action UpdateUserAction{userId} = do
        ensureIsUser
        accessDeniedUnless (userId == currentUserId)

        user <- fetch userId
        passkeys <- fetchUserPasskeys user.id
        user
            |> fill @'["nickname"]
            |> validateIsUniqueCaseInsensitive #nickname
            >>= ifValid \case
                Left user -> render EditView{..}
                Right user -> do
                    void $ user |> updateRecord
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

fetchUserPasskeys :: (?modelContext :: ModelContext) => Id User -> IO [Passkey]
fetchUserPasskeys userId =
    query @Passkey
        |> filterWhere (#userId, userId)
        |> orderBy #createdAt
        |> fetch
