module Web.View.Users.DeleteAccount where

import Web.View.Prelude

data DeleteAccountView = DeleteAccountView
    { user :: User
    }

instance View DeleteAccountView where
    html DeleteAccountView { .. } =
        renderConfirmationModal
            "Delete Account"
            (pathTo EditUserAction { userId = user.id })
            (pathTo DeleteUserAction { userId = user.id })
            "Delete Account"
            "btn btn-danger"
            [hsx|
                <p class="mb-0">
                    This will permanently delete your account and remove all associated data.
                    This action cannot be undone.
                </p>
            |]
