module Web.View.Users.New where
import Web.View.Prelude

data NewView = NewView { user :: User, passwordConfirmation :: Text }

instance View NewView where
    html NewView { .. } = [hsx|
        <div id="users-new" class="pt-4 pb-4">
            <div class="container">
                <div class="row justify-content-center">
                    <div class="col-12 col-md-8 col-xl-5">
                        <div class="card shadow-sm border-1">
                            <div class="card-body p-4 p-md-5">
                                <div class="text-center mb-4">
                                    <h1 class="h3 fw-bold mb-2">Create Account</h1>
                                    <p class="text-muted mb-0">Start predicting market outcomes</p>
                                </div>
                                {renderForm user passwordConfirmation}
                                <hr class="my-4"/>
                                <div class="text-center">
                                    <p class="text-muted mb-0">
                                        Already have an account? <br class="d-block d-sm-none"/>
                                        <a href={NewSessionAction} class="fw-semibold text-decoration-none">Login here</a>
                                    </p>
                                </div>
                            </div>
                        </div>
                    </div>
                </div>
            </div>
        </div>
    |]

renderForm :: User -> Text -> Html
renderForm user passwordConfirmation = formFor user [hsx|
    {(textField #email) {required = True, autofocus = True}}
    {(passwordField #passwordHash) {fieldLabel = "Password", required = True}}
    <div class="mb-3" id="form-group-user_passwordConfirmation">
        <label for="user_passwordConfirmation" class="form-label">Password confirmation</label>
        <input type="password" name="passwordConfirmation" id="user_passwordConfirmation" value={passwordConfirmation} required="required" class="form-control" />
    </div>
    <div class="d-grid gap-2 mt-4">
        {submitButton {label = "Create Account", buttonClass = "btn-primary btn-lg"}}
    </div>
|]
