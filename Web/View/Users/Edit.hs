module Web.View.Users.Edit (EditView(..), renderForm) where

import Web.View.Prelude

data EditView = EditView { user :: User }

instance View EditView where
    html EditView { .. } = dashboardLayout [hsx|
        <div class="h-100" id="users-edit">
            <div class="d-flex align-items-center">
                <div class="w-100">
                    <div style="max-width: 500px" class="mb-5">
                        <h3>Edit Profile</h3>
                        {renderFlashMessages}
                        {renderForm user}
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
    <div class="mb-3">
        <label class="form-label">Email</label>
        <input type="email" class="form-control" value={user.email} disabled />
    </div>
    {(textField #nickname) {required = True}}
    {submitButton {label = "Save Changes"}}
|]
