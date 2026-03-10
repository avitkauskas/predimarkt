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
                        <div class="card-body p-4 text-center">
                            <h4 class="mb-3">Login</h4>
                            <p class="text-muted mb-4">
                                We use WorkOS for secure authentication.<br/>
                                Please log in to continue.
                            </p>
                            <a href={WorkOSLoginAction}
                               class="btn btn-primary btn-lg w-100"
                               data-turbolinks="false">
                                <i class="bi bi-box-arrow-in-right me-2"></i>
                                Log in with WorkOS
                            </a>
                        </div>
                    </div>
                </div>
            </div>
        </div>
    |]
