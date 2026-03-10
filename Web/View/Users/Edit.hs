module Web.View.Users.Edit (EditView(..), renderForm) where

import Web.View.Prelude

data EditView = EditView
    { user         :: User
    , passkeyCount :: Int
    }

instance View EditView where
    html EditView { .. } = dashboardLayout [hsx|
        <div id="users-edit">
            <div class="d-flex align-items-center">
                <div class="w-100">
                    <div style="max-width: 500px" class="mb-5">
                        <h5>Profile</h5>
                        {renderForm user}

                        <hr class="my-4" />

                        <div class="card mb-4">
                            <div class="card-body">
                                <h5 class="card-title mb-2">Passkeys</h5>
                                <p class="card-text text-muted mb-3">
                                    {passkeySummary passkeyCount}
                                </p>
                                <div id="profile-passkey-status" class="alert d-none mb-3" role="alert"></div>
                                <div class="js-passkey-register"
                                     data-begin-url={pathTo BeginPasskeyRegistrationAction}
                                     data-finish-url={pathTo FinishPasskeyRegistrationAction}
                                     data-status-id="profile-passkey-status">
                                    <button type="button" class="btn btn-outline-primary js-passkey-register-button">
                                        <i class="bi bi-key me-2"></i>
                                        {if passkeyCount == 0 then "Set up a passkey" else "Add another passkey" :: Text}
                                    </button>
                                </div>
                            </div>
                        </div>

                        <hr class="my-4" />
                        <div class="card border-danger">
                            <div class="card-body">
                                <h5 class="card-title text-danger">Danger Zone</h5>
                                <p class="card-text text-muted">Once you delete your account, there is no way back.<br/> Please be certain.</p>
                                <a href={DeleteUserAction (get #id user)}
                                   class="btn btn-outline-danger js-delete"
                                   data-confirm="Are you sure you want to delete your account? This action cannot be undone.">
                                    Delete My Account
                                </a>
                            </div>
                        </div>
                    </div>
                </div>
            </div>
        </div>
    |]

renderForm :: User -> Html
renderForm user = formFor user [hsx|
    {(textField #nickname) {required = True}}
    {submitButton {label = "Save Changes"}}
|]

passkeySummary :: Int -> Text
passkeySummary 0 = "You have not registered a passkey yet. Add one now so you can log in without an email or password."
passkeySummary 1 = "You currently have 1 passkey registered."
passkeySummary count = "You currently have " <> tshow count <> " passkeys registered."
