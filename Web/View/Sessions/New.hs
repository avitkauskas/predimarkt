module Web.View.Sessions.New where

import IHP.ViewPrelude
import Web.Routes ()
import Web.Types

data NewView = NewView

instance View NewView where
    html NewView { .. } = [hsx|
        <div class="container mt-5">
            <div class="row justify-content-center">
                <div class="col-md-7 col-lg-5">
                    <div class="card shadow">
                        <div class="card-body p-4">
                            <div class="text-center mb-4">
                                <h4 class="mb-3">Continue with a passkey</h4>
                                <p class="text-muted mb-0">
                                    Sign in with the passkey already on your phone, laptop,
                                    password manager, or hardware key.
                                </p>
                            </div>

                            <div class="js-passkey-login"
                                 data-begin-url={pathTo BeginPasskeyAuthenticationAction}
                                 data-finish-url={pathTo FinishPasskeyAuthenticationAction}
                                 data-status-id="passkey-auth-status"
                                 data-success-redirect="/">
                                <button type="button" class="btn btn-success btn-lg w-100 js-passkey-login-button">
                                    <i class="bi bi-box-arrow-in-right me-2"></i>
                                    Log in with a passkey
                                </button>
                            </div>

                            <div class="d-flex align-items-center my-4 text-muted">
                                <hr class="flex-grow-1" />
                                <span class="px-3 small text-uppercase">or</span>
                                <hr class="flex-grow-1" />
                            </div>

                            <div id="passkey-auth-status" class="alert d-none mb-3" role="alert"></div>

                            <div class="js-passkey-register"
                                 data-begin-url={pathTo BeginPasskeyRegistrationAction}
                                 data-finish-url={pathTo FinishPasskeyRegistrationAction}
                                 data-status-id="passkey-auth-status"
                                 data-success-redirect="/"
                                 data-nickname-input-id="passkey-signup-nickname">
                                <!-- <label class="form-label" for="passkey-signup-nickname">Choose a nickname</label> -->
                                <input id="passkey-signup-nickname"
                                       type="text"
                                       class="form-control mb-3"
                                       maxlength="50"
                                       placeholder="enter your chosen nickname" />
                                <button type="button" class="btn btn-primary btn-lg w-100 js-passkey-register-button">
                                    <i class="bi bi-person-plus me-2"></i>
                                    Create account with a passkey
                                </button>
                            </div>

                            <p class="text-muted small text-center mt-4 mb-0">
                                No email, password, or username required.
                            </p>
                        </div>
                    </div>
                </div>
            </div>
        </div>
    |]
