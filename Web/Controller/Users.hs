module Web.Controller.Users where

import Data.Functor (void)
import Web.Controller.Prelude
import Web.View.Users.DeleteAccount
import Web.View.Users.DeletePasskey
import Web.View.Users.Edit

instance Controller UsersController where
    action EditUserAction{userId} = do
        let resolvedUserId = if userId == def then param @(Id User) "userId" else userId
        ensureIsUser
        accessDeniedUnless (resolvedUserId == currentUserId)

        user <- fetch resolvedUserId
        passkeys <- fetchUserPasskeys user.id
        render EditView{..}

    action UpdateUserAction{userId} = do
        let resolvedUserId = if userId == def then param @(Id User) "userId" else userId
        ensureIsUser
        accessDeniedUnless (resolvedUserId == currentUserId)

        user <- fetch resolvedUserId
        passkeys <- fetchUserPasskeys user.id
        user
            |> fill @'["nickname"]
            |> validateIsUniqueCaseInsensitive #nickname
            >>= ifValid \case
                Left user -> render EditView{..}
                Right user -> do
                    void $ user |> updateRecord
                    setSuccessMessage "Profile updated"
                    redirectTo EditUserAction { userId = resolvedUserId }

    action ConfirmDeletePasskeyAction{passkeyId} = do
        let resolvedPasskeyId = if passkeyId == def then param @(Id Passkey) "passkeyId" else passkeyId
        ensureIsUser

        passkey <- fetch resolvedPasskeyId
        accessDeniedUnless (passkey.userId == currentUserId)
        passkeyCount <- fetchPasskeyCount currentUserId
        if passkeyCount <= 1
            then do
                setErrorMessage "Cannot remove your only passkey."
                redirectTo EditUserAction { userId = currentUserId }
            else do
                setModal DeletePasskeyView { .. }
                jumpToAction EditUserAction { userId = currentUserId }

    action ConfirmDeleteUserAction{userId} = do
        let resolvedUserId = if userId == def then param @(Id User) "userId" else userId
        ensureIsUser
        accessDeniedUnless (resolvedUserId == currentUserId)

        user <- fetch resolvedUserId
        setModal DeleteAccountView { .. }
        jumpToAction EditUserAction { userId = resolvedUserId }

    action DeleteUserAction{userId} = do
        let resolvedUserId = if userId == def then param @(Id User) "userId" else userId
        ensureIsUser
        accessDeniedUnless (resolvedUserId == currentUserId)

        user <- fetch resolvedUserId
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
