module Web.View.Layout (defaultLayout, dashboardLayout, Html) where

import Application.Helper.View
import Generated.Types
import IHP.Environment
import IHP.ViewPrelude
import Web.Routes
import Web.Types

defaultLayout :: Html -> Html
defaultLayout inner = [hsx|
        <!DOCTYPE html>
        <html lang="en">
            <head>
                {metaTags}
                {stylesheets}
                {scripts}
                <title>{pageTitleOrDefault "Predimarkt"}</title>
            </head>
            <body>
                <div class="container-xxl mt-1">
                    {navbar}
                    <div class="container-xxl mt-1">
                        {renderFlashMessages}
                        {inner}
                    </div>
                </div>
            </body>
        </html>
    |]

navbar :: Html
navbar = [hsx|
        <nav class="navbar navbar-expand-lg bg-body">
            <div class="container-fluid">
                <a class="navbar-brand fw-bold gradient-text fs-3 ms-2"
                   href="/">Predimarkt</a>
                <div class="d-flex d-lg-none ms-auto align-items-center gap-1">
                    <button
                        aria-controls="navbar-collapse"
                        aria-expanded="false"
                        aria-label="Toggle navigation"
                        class="navbar-toggler-custom"
                        data-bs-target="#navbar-collapse"
                        data-bs-toggle="collapse"
                        type="button"
                    >
                        <svg class="hamburger-icon" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                            <path
                                stroke-linecap="round"
                                stroke-linejoin="round"
                                stroke-width="2" d="M4 6h16M4 12h16M4 18h16"
                            />
                        </svg>
                    </button>
                    {renderThemeToggle}
                </div>
                <div class="collapse navbar-collapse" id="navbar-collapse">
                    <ul class="navbar-nav ms-auto">
                        {navItems}
                        <li class="nav-item d-none d-lg-flex align-items-center">{renderThemeToggle}</li>
                    </ul>
                </div>
            </div>
        </nav>
    |]
    where
        navItems :: Html
        navItems = maybe loggedOutNav loggedInNav currentUserOrNothing

        loggedOutNav :: Html
        loggedOutNav = [hsx|
            <li class="nav-item"><a class="nav-link" href={NewSessionAction}>Login</a></li>
        |]

        loggedInNav :: User -> Html
        loggedInNav user = [hsx|
            <li class="nav-item"><a class="nav-link" href={DashboardHoldingsAction}>Dashboard</a></li>
            <li class="nav-item dropdown">
                <a aria-expanded="false"
                   class="nav-link dropdown-toggle"
                   data-bs-toggle="dropdown"
                   href="#" role="button"
                >Account</a>
                <ul class="dropdown-menu dropdown-menu-end" style="min-width:auto;">
                    <li><h6 class="dropdown-header">{user.nickname}</h6></li>
                    <li><hr class="dropdown-divider" /></li>
                    <li><a  class="dropdown-item" href={DashboardWalletsAction}>Balance</a></li>
                    <li><a  class="dropdown-item" href={DashboardHoldingsAction}>Holdings</a></li>
                    <li><a  class="dropdown-item" href={DashboardTransactionsAction Nothing}>Transactions</a></li>
                    <li><a  class="dropdown-item" href={DashboardMarketsAction (Just MarketStatusOpen)}>Markets</a></li>
                    <li><hr class="dropdown-divider" /></li>
                    <li><a  class="dropdown-item" href={EditUserAction user.id}>Profile</a></li>
                    <li><hr class="dropdown-divider" /></li>
                    <li>
                        <a class="dropdown-item js-delete js-delete-no-confirm"
                           href={DeleteSessionAction}
                        >Logout</a>
                    </li>
                </ul>
            </li>
        |]

dashboardLayout :: Html -> Html
dashboardLayout inner = [hsx|
    <div class="container-fluid mt-2">
        <div class="row">
            <div class="col-md-3 col-lg-2 d-md-block sidebar collapse">
                <div class="position-sticky">
                    <div class="mb-2">
                        <h5>Dashboard</h5>
                    </div>
                    <ul class="nav flex-column">
                        <li class="nav-item">
                            <a class={classes
                                    ["nav-link",
                                    ("active", isActivePath (pathTo DashboardWalletsAction))]}
                               href={DashboardWalletsAction}>
                              Balance
                            </a>
                        </li>
                        <li class="nav-item">
                            <a class={classes
                                    ["nav-link",
                                    ("active", isActivePath (pathTo DashboardHoldingsAction))]}
                               href={DashboardHoldingsAction}>
                              Positions
                            </a>
                        </li>
                        <li class="nav-item">
                            <a class={classes
                                    ["nav-link",
                                    ("active", isActivePath (pathTo (DashboardTransactionsAction Nothing)))]}
                               href={DashboardTransactionsAction Nothing}>
                              Transactions
                            </a>
                        </li>
                        <li class="nav-item">
                            <a class={classes
                                    ["nav-link",
                                    ("active", any (\s ->
                                        isActivePath (pathTo (DashboardMarketsAction s))
                                    ) (Nothing : map Just allEnumValues))]}
                               href={DashboardMarketsAction (Just MarketStatusOpen)}>
                              Markets
                            </a>
                        </li>
                        <li class="nav-item">
                            <a class={classes
                                    ["nav-link",
                                    ("active", isActivePath (pathTo (EditUserAction currentUserId)))]}
                                href={EditUserAction currentUserId}>
                              Profile
                            </a>
                        </li>
                    </ul>
                </div>
            </div>

            <main class="col-md-9 ms-sm-auto col-lg-10 px-md-2">
                {inner}
            </main>
        </div>
    </div>
|]

renderThemeToggle :: Html
renderThemeToggle = [hsx|
        <button
            type="button"
            class="btn btn-link p-1 text-body text-decoration-none lh-1"
            onclick="toggleTheme()"
            aria-label="Toggle dark/light mode"
        >
            <span data-theme-icon></span>
        </button>
    |]

-- The 'assetPath' function used below appends a `?v=SOME_VERSION` to the static assets in production
-- This is useful to avoid users having old CSS and JS files in their browser cache once a new version is deployed
-- See https://ihp.digitallyinduced.com/Guide/assets.html for more details

stylesheets :: Html
stylesheets = [hsx|
        <link rel="stylesheet" href="https://cdn.jsdelivr.net/npm/bootstrap-icons@1.13.1/font/bootstrap-icons.min.css"/>
        <link rel="stylesheet" href={assetPath "/vendor/bootstrap-5.3.8/bootstrap.min.css"}/>
        <link rel="stylesheet" href={assetPath "/vendor/flatpickr.min.css"}/>
        <link rel="stylesheet" href={assetPath "/app.css"}/>
    |]

scripts :: Html
scripts = [hsx|
        {when isDevelopment devScripts}
        <script src={assetPath "/vendor/jquery-3.6.0.slim.min.js"}></script>
        <script src={assetPath "/vendor/timeago.js"}></script>
        <script src={assetPath "/vendor/popper-2.11.6.min.js"}></script>
        <script src={assetPath "/vendor/bootstrap-5.3.8/bootstrap.min.js"}></script>
        <script src={assetPath "/vendor/htmx-4.0.0-alpha6/htmx.min.js"}></script>
        <script src={assetPath "/vendor/flatpickr.js"}></script>
        <script src={assetPath "/vendor/morphdom-umd.min.js"}></script>
        <script src={assetPath "/vendor/turbolinks.js"}></script>
        <script src={assetPath "/vendor/turbolinksInstantClick.js"}></script>
        <script src={assetPath "/vendor/turbolinksMorphdom.js"}></script>
        <script src={assetPath "/helpers.js"}></script>
        <script src={assetPath "/ihp-auto-refresh.js"}></script>
        <script src={assetPath "/theme-toggle.js"}></script>
        <script src={assetPath "/app.js"}></script>
    |]

devScripts :: Html
devScripts = [hsx|
        <script id="livereload-script" src={assetPath "/livereload.js"} data-ws={liveReloadWebsocketUrl}></script>
    |]

metaTags :: Html
metaTags = [hsx|
        <meta charset="utf-8"/>
        <meta name="viewport" content="width=device-width, initial-scale=1, shrink-to-fit=no"/>
        <meta property="og:title" content="App"/>
        <meta property="og:type" content="website"/>
        <meta property="og:url" content="TODO"/>
        <meta property="og:description" content="TODO"/>
        {autoRefreshMeta}
    |]
