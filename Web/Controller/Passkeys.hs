module Web.Controller.Passkeys where

import qualified Data.Aeson as Aeson
import Data.Aeson.Types (parseMaybe)
import IHP.ControllerPrelude
import Network.HTTP.Types (status200)
import Network.Wai (responseLBS)
import Web.Controller.Prelude

instance Controller PasskeysController where
    beforeAction = ensureIsUser

    action UpdatePasskeyNameAction { passkeyId } = do
        passkey <- fetch passkeyId
        putStrLn "DEBUG 1: passkey fetched"
        accessDeniedUnless (passkey.userId == currentUserId)
        putStrLn "DEBUG 2: access check passed"
        rawBody <- getRequestBody
        putStrLn $ "DEBUG 3: rawBody = " <> show rawBody
        let decoded = Aeson.decode rawBody :: Maybe Aeson.Value
        putStrLn $ "DEBUG 4: decoded = " <> show decoded
        let newNameMaybe = case decoded of
                Just (Aeson.Object obj) -> parseMaybe (Aeson..: "name") obj
                _                       -> Nothing
        putStrLn $ "DEBUG 5: newNameMaybe = " <> show (newNameMaybe :: Maybe Text)
        case newNameMaybe of
            Just newName -> do
                putStrLn $ "DEBUG 6: updating name to " <> show newName
                updateRecordDiscardResult (passkey |> set #name newName)
                putStrLn "DEBUG 7: update done"
            Nothing -> putStrLn "DEBUG 6b: skipping update"
        putStrLn "DEBUG 8: about to respondAndExit"
        respondAndExit $ responseLBS status200
            [("Content-Type", "application/json")] (Aeson.encode (Aeson.object ["ok" Aeson..= True]))

    action DeletePasskeyAction { passkeyId } = do
        passkey <- fetch passkeyId
        accessDeniedUnless (passkey.userId == currentUserId)
        passkeyCount <- fetchPasskeyCount currentUserId
        if passkeyCount <= 1
            then do
                setErrorMessage "Cannot remove your only passkey."
                redirectTo EditUserAction { userId = currentUserId }
            else do
                deleteRecord passkey
                setSuccessMessage "Passkey removed."
                redirectTo EditUserAction { userId = currentUserId }

