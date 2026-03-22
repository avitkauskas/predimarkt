module Web.Controller.Passkeys where

import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import IHP.ControllerPrelude
import Web.Controller.Prelude

instance Controller PasskeysController where
    beforeAction = ensureIsUser

    action UpdatePasskeyNameAction { passkeyId } = do
        let resolvedPasskeyId = if passkeyId == def then param @(Id Passkey) "passkeyId" else passkeyId
        passkey <- fetch resolvedPasskeyId
        accessDeniedUnless (passkey.userId == currentUserId)
        let newName = Text.strip (param @Text "name")

        passkey
            |> set #name newName
            |> updateRecord

        renderJson (Aeson.object ["ok" Aeson..= True, "name" Aeson..= newName])

    action DeletePasskeyAction { passkeyId } = do
        let resolvedPasskeyId = if passkeyId == def then param @(Id Passkey) "passkeyId" else passkeyId
        passkey <- fetch resolvedPasskeyId
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
