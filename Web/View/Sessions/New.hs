module Web.View.Sessions.New where
import IHP.AuthSupport.View.Sessions.New
import Web.View.Prelude

instance View (NewView User) where
    html NewView { .. } = [hsx|
        <div id="sessions-new" class="pt-4 pb-4">
            <div class="container">
                <div class="row justify-content-center">
                    <div class="col-12 col-md-8 col-xl-5">
                        <div class="card shadow-sm border-1">
                            <div class="card-body p-4 p-md-5">
                                <div class="text-center mb-4">
                                    <h1 class="h3 fw-bold mb-2">Welcome Back</h1>
                                    <p class="text-muted mb-0">Login to your account</p>
                                </div>
                                {renderForm user}
                                <hr class="my-4"/>
                                <div class="text-center">
                                    <p class="text-muted mb-0">
                                        Don't have an account? <br class="d-block d-sm-none"/>
                                        <a href={NewUserAction} class="fw-semibold text-decoration-none">Create one here</a>
                                    </p>
                                </div>
                            </div>
                        </div>
                    </div>
                </div>
            </div>
        </div>
    |]

renderForm :: User -> Html
renderForm user = formFor' user (pathTo CreateSessionAction) [hsx|
    {(textField #email) {required = True, autofocus = True, fieldName = "email"}}
    {(passwordField #passwordHash) {required = True, fieldName = "password", fieldLabel = "Password"}}
    <div class="d-grid gap-2 mt-4">
        {submitButton {label = "Login", buttonClass = "btn-primary btn-lg"}}
    </div>
|]
