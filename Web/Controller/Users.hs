module Web.Controller.Users where

import qualified Data.Text as Text
import Web.Controller.Prelude
import Web.View.Users.Edit
import Web.View.Users.New

instance Controller UsersController where
    action NewUserAction = do
        case currentUserOrNothing of
            Just _ -> do
                setErrorMessage "You are already logged in"
                redirectToPath "/"
            Nothing -> do
                let user = newRecord
                let passwordConfirmation = ""
                render NewView{..}

    action CreateUserAction = do
        case currentUserOrNothing of
            Just _ -> do
                setErrorMessage "You are already logged in"
                redirectToPath "/"
            Nothing -> do
                let user = newRecord @User
                let passwordConfirmation = param @Text "passwordConfirmation"
                user
                    |> fill @["email", "passwordHash"]
                    |> validateField #passwordHash (isEqual passwordConfirmation
                        |> withCustomErrorMessage "Passwords don't match")
                    |> validateField #passwordHash nonEmpty
                    |> validateField #email isEmail
                    |> validateIsUniqueCaseInsensitive #email
                    >>= ifValid \case
                        Left user -> render NewView{..}
                        Right user -> do
                            hashed <- hashPassword user.passwordHash
                            let baseNickname = Text.takeWhile (/= '@') user.email

                            let findUniqueNickname :: Text -> Int -> IO Text
                                findUniqueNickname base count = do
                                    let candidate = if count == 0 then base else base <> "." <> tshow count
                                    checkedUser <- user
                                        |> set #nickname candidate
                                        |> validateIsUniqueCaseInsensitive #nickname
                                    case getValidationFailure #nickname checkedUser of
                                        Just _ -> findUniqueNickname base (count + 1)
                                        Nothing -> pure candidate

                            uniqueNickname <- findUniqueNickname baseNickname 0

                            user <- withTransaction do
                                createdUser <- user
                                    |> set #passwordHash hashed
                                    |> set #nickname uniqueNickname
                                    |> createRecord

                                newRecord @Wallet
                                    |> set #userId createdUser.id
                                    |> createRecord

                                pure createdUser

                            login user
                            setSuccessMessage "Account created successfully. You are logged in now."
                            redirectToPath "/"

    action EditUserAction{userId} = do
        ensureIsUser
        accessDeniedUnless (userId == currentUserId)

        existing_user <- fetch userId
        let user = existing_user |> set #passwordHash ""
        let passwordConfirmation = ""
        render EditView{..}

    action UpdateUserAction{userId} = do
        ensureIsUser
        accessDeniedUnless (userId == currentUserId)

        user <- fetch userId
        let originalPasswordHash = user.passwordHash
        let passwordConfirmation = param @Text "passwordConfirmation"

        user
            |> fill @["email", "nickname", "passwordHash"]
            |> validateField #passwordHash (isEqual passwordConfirmation
                |> withCustomErrorMessage "Passwords don't match")
            |> validateField #email isEmail
            |> validateIsUniqueCaseInsensitive #email
            >>= validateIsUniqueCaseInsensitive #nickname
            >>= ifValid \case
                Left user -> render EditView{..}
                Right user -> do
                    hashed <-
                        if user.passwordHash == "" || user.passwordHash == originalPasswordHash
                            then pure originalPasswordHash
                            else hashPassword user.passwordHash
                    user <- user
                        |> set #passwordHash hashed
                        |> updateRecord
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
