module Web.Controller.Users where

import Web.Controller.Prelude
import Web.View.Users.Edit
import Web.View.Users.New
import qualified Data.Text as Text

instance Controller UsersController where
    -- Create account page - only accessible if not logged in
    action NewUserAction = do
        case currentUserOrNothing of
            Just _ -> do
                setErrorMessage "You are already logged in"
                redirectToPath "/"
            Nothing -> do
                let user = newRecord
                let passwordConfirmation = ""
                render NewView { .. }

    -- Create account submit - only accessible if not logged in
    action CreateUserAction = do
        case currentUserOrNothing of
            Just _ -> do
                setErrorMessage "You are already logged in"
                redirectToPath "/"
            Nothing -> do
                let user = newRecord @User
                -- The value from the password confirmation input field.
                let passwordConfirmation = param @Text "passwordConfirmation"
                user
                    |> fill @["email", "passwordHash"]
                    -- We ensure that the error message doesn't include
                    -- the entered password.
                    |> validateField #passwordHash (isEqual passwordConfirmation |> withCustomErrorMessage "Passwords don't match")
                    |> validateField #passwordHash nonEmpty
                    |> validateField #email isEmail
                    -- After this validation, since it's operation on the IO, we'll need to use >>=.
                    |> validateIsUniqueCaseInsensitive #email
                    >>= ifValid \case
                        Left user -> render NewView { .. }
                        Right user -> do
                            hashed <- hashPassword user.passwordHash
                            let baseNickname = Text.takeWhile (/= '@') user.email

                            -- Helper to find unique nickname
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

                            user <- user
                                |> set #passwordHash hashed
                                |> set #nickname uniqueNickname
                                |> createRecord
                            -- setSuccessMessage "Account created successfully. Please log in."
                            -- redirectTo NewSessionAction
                            login user
                            setSuccessMessage "Account created successfully. You are logged in now."
                            redirectToPath "/"

    -- Edit profile - only accessible if logged in and for own profile only
    action EditUserAction { userId } = do
        ensureIsUser
        accessDeniedUnless (userId == currentUserId)
        existing_user <- fetch userId
        let user = existing_user { passwordHash = "" }
        let passwordConfirmation = ""
        render EditView { .. }

    -- Update profile - only accessible if logged in and for own profile only
    action UpdateUserAction { userId } = do
        ensureIsUser
        accessDeniedUnless (userId == currentUserId)
        user <- fetch userId
        let originalPasswordHash = user.passwordHash
        -- The value from the password confirmation input field.
        let passwordConfirmation = param @Text "passwordConfirmation"
        user
            |> fill @["email", "nickname", "passwordHash"]
            -- We only validate the email field isn't empty, as the password
            -- can remain empty. We ensure that the error message doesn't include
            -- the entered password.
            |> validateField #passwordHash (isEqual passwordConfirmation |> withCustomErrorMessage "Passwords don't match")
            |> validateField #email isEmail
            -- After this validation, since it's operation on the IO, we'll need to use >>=.
            |> validateIsUniqueCaseInsensitive #email
            >>= validateIsUniqueCaseInsensitive #nickname
            >>= ifValid \case
                Left user -> render EditView { .. }
                Right user -> do
                    -- If the password hash is empty, then the user did not
                    -- change the password. So, we set the password hash to
                    -- the original password hash.
                    hashed <-
                      if user.passwordHash == "" || user.passwordHash == originalPasswordHash
                            then pure originalPasswordHash
                            else hashPassword user.passwordHash

                    user <- user
                        |> set #passwordHash hashed
                        |> updateRecord
                    setSuccessMessage "Profile updated"
                    redirectTo EditUserAction { .. }

    -- Delete account - only accessible if logged in and for own account only
    action DeleteUserAction { userId } = do
        ensureIsUser
        accessDeniedUnless (userId == currentUserId)
        user <- fetch userId
        logout user
        deleteRecord user
        setSuccessMessage "Your account has been deleted"
        redirectToPath "/"
