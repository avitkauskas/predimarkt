module Web.View.Users.DeletePasskey where

import Web.View.Prelude

data DeletePasskeyView = DeletePasskeyView
    { passkey :: Passkey
    }

instance View DeletePasskeyView where
    html DeletePasskeyView { .. } =
        renderConfirmationModal
            "Remove Passkey"
            (pathTo EditUserAction { userId = passkey.userId })
            (pathTo DeletePasskeyAction { passkeyId = passkey.id })
            "Remove Passkey"
            "btn btn-danger"
            [hsx|
                <p class="mb-0">
                    Remove passkey <strong>{passkey.name}</strong>? You will need another passkey
                    or a new registration to log in.
                </p>
            |]
