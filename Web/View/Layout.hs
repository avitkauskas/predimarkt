module Web.View.Layout (defaultLayout, dashboardLayout, contentPageLayout, Html) where

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
            <body class="d-flex flex-column min-vh-100">
                <div class="flex-grow-1">
                    <div class="container-xxl mt-1">
                        {navbar}
                        {renderFlashToasts}
                        <div class="container-xxl mt-0">
                            {inner}
                            {modal}
                        </div>
                    </div>
                </div>
                {siteFooter}
            </body>
        </html>
    |]

navbar :: (?request :: Request) => Html
navbar = [hsx|
        <nav class="navbar navbar-expand-md bg-body">
            <div class="container-fluid">
                <a class="navbar-brand fw-bold gradient-text fs-3 ms-2"
                   href="/">Predimarkt
                </a>
                <span class="navbar-text d-none d-lg-block fs-4 gradient-subtitle">
                    =| educational play-money prediction markets
                </span>
                <div class="d-flex d-md-none ms-auto align-items-center gap-1">
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
                        <li class="nav-item d-none d-md-flex align-items-center">{renderThemeToggle}</li>
                    </ul>
                </div>
            </div>
        </nav>
    |]
    where
        navItems :: Html
        navItems = maybe loggedOutNav loggedInNav currentUserOrNothing

        leaderboardNavItem :: Html
        leaderboardNavItem = [hsx|<li class="nav-item"><a class="nav-link" href={LeaderboardAction}>Leaderboard</a></li>|]

        loggedOutNav :: Html
        loggedOutNav = [hsx|
            {leaderboardNavItem}
            <li class="nav-item"><a class="nav-link" href={LoginAction}>Login</a></li>
        |]

        loggedInNav :: User -> Html
        loggedInNav user = [hsx|
            {leaderboardNavItem}
            <li class="nav-item"><a class="nav-link" href={DashboardPositionsAction Nothing Nothing}>Dashboard</a></li>
            <li class="nav-item dropdown">
                <a aria-expanded="false"
                   class="nav-link dropdown-toggle"
                   data-bs-toggle="dropdown"
                   href="#" role="button"
                >Account</a>
                <ul class="dropdown-menu dropdown-menu-end" style="min-width:auto;">
                    <li><h6 class="dropdown-header">{user.nickname}</h6></li>
                    <li><hr class="dropdown-divider" /></li>
                    <li><a  class="dropdown-item" href={DashboardPositionsAction Nothing Nothing}>Positions</a></li>
                    <li><a  class="dropdown-item" href={DashboardTransactionsAction Nothing Nothing}>Transactions</a></li>
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
    <div class="mt-2">
        <div class="d-flex">
            <div class="dashboard-sidebar d-none d-md-block">
                <div class="position-sticky">
                    <div class="dashboard-sidebar-title ps-3">
                        <h5 class="mb-0">Dashboard</h5>
                    </div>
                    <ul class="nav flex-column">
                        <li class="nav-item">
                            <a class={classes
                                    ["nav-link",
                                    ("active", isActivePath (pathTo (DashboardPositionsAction Nothing Nothing)))]}
                               href={DashboardPositionsAction Nothing Nothing}>
                              <i class="bi bi-wallet"></i>Positions
                            </a>
                        </li>
                        <li class="nav-item">
                            <a class={classes
                                    ["nav-link",
                                    ("active", isActivePath (pathTo (DashboardTransactionsAction Nothing Nothing)))]}
                               href={DashboardTransactionsAction Nothing Nothing}>
                              <i class="bi bi-arrow-left-right"></i>Transactions
                            </a>
                        </li>
                        <li class="nav-item">
                            <a class={classes
                                    ["nav-link",
                                    ("active", any (\s ->
                                        isActivePath (pathTo (DashboardMarketsAction s))
                                    ) (Nothing : map Just allEnumValues))]}
                               href={DashboardMarketsAction (Just MarketStatusOpen)}>
                              <i class="bi bi-cart3"></i>Markets
                            </a>
                        </li>
                        <li class="nav-item">
                            <a class={classes
                                    ["nav-link",
                                    ("active", isActivePath (pathTo (EditUserAction currentUserId)))]}
                                href={EditUserAction currentUserId}>
                              <i class="bi bi-person"></i>Profile
                            </a>
                        </li>
                    </ul>
                </div>
            </div>

            <main class="dashboard-content ms-3">
                {inner}
            </main>
        </div>
    </div>
|]

contentPageLayout :: Text -> Text -> Html -> Html
contentPageLayout heading intro inner = [hsx|
    <div class="py-3 py-lg-4">
        <div class="card shadow-sm">
            <div class="card-body p-4 p-md-5">
                <header class="mb-4">
                    <h1 class="h2 mb-3">{heading}</h1>
                    <p class="lead">{intro}</p>
                </header>
                <div>
                    {inner}
                </div>
            </div>
        </div>
    </div>
|]

siteFooter :: Html
siteFooter = [hsx|
    <footer class="site-footer pt-3 pb-2">
        <div class="container-xxl">
            <div class="site-footer-inner">
                <div class="row gx-4">
                    <div class="col-6 col-sm-4 col-lg-2">
                        <div class="site-footer-brand gradient-text">Predimarkt</div>
                            <div style="font-size: 0.8rem;">&copy; 2026 Predimarkt</div>
                    </div>
                    <div class="col-6 col-sm-4 col-lg-2">
                        <ul class="list-unstyled">
                            <li class="text-secondary" style="font-size: 0.85rem; margin-top: 0.2rem;">Contact</li>
                            <li><a class="site-footer-link" href="mailto:info@predimarkt.eu">info@predimarkt.eu</a></li>
                        </ul>
                    </div>
                    <div class="col-6 col-sm-4 col-lg-2">
                        <ul class="list-unstyled">
                            <li><a class="site-footer-link" data-turbolinks="false" href={AboutAction}>About Us</a></li>
                            <li><a class="site-footer-link" data-turbolinks="false" href={HowItWorksAction}>How It Works</a></li>
                        </ul>
                    </div>
                    <div class="col-6 col-sm-4 col-lg-2">
                        <ul class="list-unstyled">
                            <li><a class="site-footer-link" data-turbolinks="false" href={CommunityRulesAction}>Community Rules</a></li>
                            <li><a class="site-footer-link" data-turbolinks="false" href={ModerationPolicyAction}>Moderation Policy</a></li>
                        </ul>
                    </div>
                    <div class="col-6 col-sm-4 col-lg-2">
                        <ul class="list-unstyled">
                            <li><a class="site-footer-link" data-turbolinks="false" href={LegalNoticeAction}>Legal Notice</a></li>
                            <li><a class="site-footer-link" data-turbolinks="false" href={TermsAction}>Terms of Service</a></li>
                        </ul>
                    </div>
                    <div class="col-6 col-sm-4 col-lg-2">
                        <ul class="list-unstyled">
                            <li><a class="site-footer-link" data-turbolinks="false" href={PrivacyPolicyAction}>Privacy Policy</a></li>
                            <li><a class="site-footer-link" data-turbolinks="false" href={CookiePolicyAction}>Cookie Policy</a></li>
                        </ul>
                    </div>
                </div>
            </div>
        </div>
    </footer>
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
        <link rel="icon" type="image/x-icon" href="/favicon.ico">
        <link rel="stylesheet" href="https://cdn.jsdelivr.net/npm/bootstrap-icons@1.13.1/font/bootstrap-icons.min.css"/>
        <link rel="stylesheet" href={assetPath "/vendor/bootstrap.min.css"}/>
        <link rel="stylesheet" href={assetPath "/vendor/flatpickr.min.css"}/>
        <link rel="stylesheet" href={assetPath "/app.css"}/>
    |]

scripts :: Html
scripts = [hsx|
        {when isDevelopment devScripts}
        <script src={assetPath "/vendor/jquery-3.6.0.slim.min.js"}></script>
        <script src={assetPath "/vendor/timeago.js"}></script>
        <script src={assetPath "/vendor/popper-2.11.6.min.js"}></script>
        <script src={assetPath "/vendor/bootstrap.min.js"}></script>
        <script src="https://cdn.jsdelivr.net/npm/htmx.org@4.0.0-alpha7/dist/htmx.min.js"></script>
        <script src={assetPath "/vendor/flatpickr.js"}></script>
        <script src={assetPath "/vendor/morphdom-umd.min.js"}></script>
        <script src={assetPath "/vendor/turbolinks.js"}></script>
        <script src={assetPath "/vendor/turbolinksInstantClick.js"}></script>
        <script src={assetPath "/vendor/turbolinksMorphdom.js"}></script>
        <script src={assetPath "/helpers.js"}></script>
        <script src={assetPath "/ihp-auto-refresh.js"}></script>
        <script src={assetPath "/theme-toggle.js"}></script>
        <script src={assetPath "/app.js"}></script>
        <script src="https://unpkg.com/lightweight-charts/dist/lightweight-charts.standalone.production.js"></script>
    |]

devScripts :: Html
devScripts = [hsx|
        <script id="livereload-script" src={assetPath "/livereload.js"} data-ws={liveReloadWebsocketUrl}></script>
    |]

metaTags :: Html
metaTags = [hsx|
        <meta charset="utf-8"/>
        <meta name="viewport" content="width=device-width, initial-scale=1, shrink-to-fit=no"/>
        <meta property="og:title" content="Predimarkt"/>
        <meta property="og:type" content="website"/>
        <meta property="og:url" content="https://predimarkt.eu"/>
        <meta property="og:description" content="Educational prediction markets"/>
        {autoRefreshMeta}
    |]
