module Web.Controller.Auth where

import Application.Helper.WorkOS
import qualified Data.Text as Text
import IHP.ControllerPrelude
import Web.Controller.Prelude

instance Controller AuthController where
    action LoginAction = do
        let url = workOSAuthUrl workOSClientId
        redirectToUrl url

    action WorkOSCallbackAction = do
        let code = param @Text "code"
        let ipAddress = "127.0.0.1"
        let userAgent = "Mozilla/5.0"
        authResponse <- liftIO $ authenticateUser code ipAddress userAgent
        let workosUser = user authResponse
            userWorkosId = workosId workosUser
            userEmail = workosEmail workosUser

        existingUser <- query @User
            |> filterWhere (#workosUserId, userWorkosId)
            |> fetchOneOrNothing

        user <- case existingUser of
            Just u -> pure u
            Nothing -> do
                let baseNickname = Text.takeWhile (/= '@') userEmail

                let findUniqueNickname :: Text -> Int -> IO Text
                    findUniqueNickname base count = do
                        let candidate = if count == 0 then base else base <> "." <> tshow count
                        checkedUser <- newRecord @User
                            |> set #nickname candidate
                            |> validateIsUniqueCaseInsensitive #nickname
                        case getValidationFailure #nickname checkedUser of
                            Just _  -> findUniqueNickname base (count + 1)
                            Nothing -> pure candidate

                uniqueNickname <- findUniqueNickname baseNickname 0

                withTransaction do
                    createdUser <- newRecord @User
                        |> set #workosUserId userWorkosId
                        |> set #email userEmail
                        |> set #nickname uniqueNickname
                        |> createRecord

                    newRecord @Wallet
                        |> set #userId createdUser.id
                        |> createRecord

                    pure createdUser

        login user
        setSuccessMessage "Logged in successfully"
        redirectToPath "/"
