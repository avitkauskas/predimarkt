module Web.View.Users.Edit (EditView(..), renderForm) where
import Web.View.Prelude

data EditView = EditView { user :: User, passwordConfirmation :: Text }

instance View EditView where
    html EditView { .. } = dashboardLayout [hsx|
        <div class="h-100" id="users-edit">
            <div class="d-flex align-items-center">
                <div class="w-100">
                    <div style="max-width: 400px" class="mb-5">
                        <h3>Edit Profile</h3>
                        {renderForm user passwordConfirmation}

                        <hr class="my-4" />

                        <div class="card border-danger">
                            <div class="card-body">
                                <h5 class="card-title text-danger">Danger Zone</h5>
                                <p class="card-text text-muted">Once you delete your account, there is no going back. Please be certain.</p>
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

renderForm :: User -> Text -> Html
renderForm user passwordConfirmation = formFor user [hsx|
    {(textField #email) {required = True}}
    {(textField #nickname) {required = True}}
    {(passwordField #passwordHash) {fieldLabel = "New Password (leave blank to keep current)"}}
    <div class="mb-3" id="form-group-user_passwordConfirmation">
        <label for="user_passwordConfirmation" class="form-label">Confirm New Password</label>
        <input type="password" name="passwordConfirmation" id="user_passwordConfirmation" value={passwordConfirmation} class="form-control" />
    </div>
    {submitButton {label = "Save Changes"}}
|]
