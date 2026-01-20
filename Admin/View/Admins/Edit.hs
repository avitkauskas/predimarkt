module Admin.View.Admins.Edit where
import Admin.View.Prelude

data EditView = EditView { admin :: Admin, passwordConfirmation :: Text }

instance View EditView where
    html EditView { .. } = [hsx|
        <div class="h-100" id="admins-edit">
            <div class="d-flex align-items-center">
                <div class="w-100">
                    <div style="max-width: 400px" class="mx-auto mb-5">
                        <h1>Edit Admin</h1>
                        {renderForm admin passwordConfirmation}

                        <hr class="my-4" />

                        <div class="card border-danger">
                            <div class="card-body">
                                <h5 class="card-title text-danger">Danger Zone</h5>
                                <p class="card-text text-muted">Once you delete your admin account, there is no going back. Please be certain.</p>
                                <a href={DeleteAdminAction (get #id admin)}
                                   class="btn btn-outline-danger js-delete"
                                   data-confirm="Are you sure you want to delete your admin account? This action cannot be undone.">
                                    Delete My Admin Account
                                </a>
                            </div>
                        </div>
                    </div>
                </div>
            </div>
        </div>
    |]

renderForm :: Admin -> Text -> Html
renderForm admin passwordConfirmation = formFor admin [hsx|
    {(textField #email) {required = True}}
    {(passwordField #passwordHash) {fieldLabel = "New Password (leave blank to keep current)"}}
    <div class="mb-3" id="form-group-admin_passwordConfirmation">
        <label for="admin_passwordConfirmation" class="form-label">Confirm New Password</label>
        <input type="password" name="passwordConfirmation" id="admin_passwordConfirmation" value={passwordConfirmation} class="form-control" />
    </div>
    {submitButton {label = "Save Changes"}}
|]
