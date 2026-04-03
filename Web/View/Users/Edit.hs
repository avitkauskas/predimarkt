module Web.View.Users.Edit (EditView(..), renderForm) where

import Web.View.Prelude

data EditView = EditView
    { user     :: User
    , passkeys :: [Passkey]
    }

instance View EditView where
    html EditView { .. } = dashboardLayout [hsx|
        <div id="users-edit">
            <div class="d-flex align-items-center">
                <div class="w-100">
                    <div style="max-width: 600px" class="mb-2">
                        <h5>Profile</h5>
                        {renderForm user}

                        <hr class="my-4" />

                        <div class="card mb-4">
                            <div class="card-body">
                                <h5 class="card-title mb-3">Passkeys</h5>
                                {renderPasskeyTable passkeys}
                                <div id="profile-passkey-status" class="alert d-none mb-3" role="alert"></div>
                                <div class="js-passkey-register mt-3"
                                     data-begin-url={pathTo BeginPasskeyRegistrationAction}
                                     data-finish-url={pathTo FinishPasskeyRegistrationAction}
                                     data-status-id="profile-passkey-status"
                                     data-success-redirect={pathTo (EditUserAction user.id)}>
                                    <button type="button" class="btn btn-outline-primary js-passkey-register-button">
                                        <i class="bi bi-key me-2"></i>
                                        {if null passkeys then "Set up a passkey" else "Add another passkey" :: Text}
                                    </button>
                                </div>
                            </div>
                        </div>

                        <hr class="my-4" />
                        <div class="card border-danger">
                            <div class="card-body">
                                <h5 class="card-title text-danger">Danger Zone</h5>
                                <p class="card-text text-muted">Once you delete your account, there is no way back.<br/> Please be certain.</p>
                                <a href={ConfirmDeleteUserAction (get #id user)}
                                   class="btn btn-outline-danger">
                                    Delete My Account
                                </a>
                            </div>
                        </div>
                    </div>
                </div>
            </div>
        </div>
    |]

renderPasskeyTable :: [Passkey] -> Html
renderPasskeyTable [] = [hsx|
    <p class="text-muted small mb-0">
        You have no passkeys yet. Add one to log in without a password.
    </p>
|]
renderPasskeyTable passkeys = [hsx|
    <div class="table-responsive">
        <table class="table table-sm align-middle mb-0">
            <thead>
                <tr>
                    <th class="fw-normal text-muted small ps-4">Name</th>
                    <th class="fw-normal text-muted small">Created</th>
                    <th class="fw-normal text-muted small text-nowrap">Last used</th>
                    <th></th>
                </tr>
            </thead>
            <tbody>
                {forEach passkeys (renderPasskeyRow (length passkeys == 1))}
            </tbody>
        </table>
    </div>
|]

renderPasskeyRow :: Bool -> Passkey -> Html
renderPasskeyRow isOnly passkey = [hsx|
    <tr>
        <td class="ps-0" style="min-width: 160px;">
            <div class="d-flex align-items-center passkey-name-row border rounded px-1"
                 style="border-color: transparent !important;">
                <button type="button"
                        class="passkey-edit-btn btn btn-link p-0 me-1 text-muted"
                        style="font-size: 0.75rem; line-height: 1;"
                        title="Rename">
                    <i class="bi bi-pencil"></i>
                </button>
                <input type="text"
                       class="passkey-name-input border-0 bg-transparent p-0 flex-grow-1"
                       style="outline: none; font-size: inherit; min-width: 0; cursor: default;"
                       value={passkey.name}
                       readonly=""
                       data-update-url={pathTo (UpdatePasskeyNameAction passkey.id)}
                       aria-label="Passkey name" />
            </div>
        </td>
        <td class="text-muted small text-nowrap">{renderTime passkey.createdAt}</td>
        <td class="text-muted small text-nowrap">{timeAgo passkey.lastUsedAt}</td>
        <td class="text-end">
            {renderDeleteButton isOnly passkey}
        </td>
    </tr>
|]

renderDeleteButton :: Bool -> Passkey -> Html
renderDeleteButton True _ = [hsx|
    <button type="button"
            class="btn btn-sm btn-link text-muted p-0"
            disabled
            title="Cannot remove your only passkey">
        <i class="bi bi-trash"></i>
    </button>
|]
renderDeleteButton False passkey = [hsx|
    <a href={ConfirmDeletePasskeyAction passkey.id}
       class="btn btn-sm btn-link text-danger p-0"
       title="Remove passkey">
        <i class="bi bi-trash"></i>
    </a>
|]

renderForm :: User -> Html
renderForm user = formFor user [hsx|
    {(textField #nickname) {required = True}}
    {submitButton {label = "Save Changes"}}
|]
