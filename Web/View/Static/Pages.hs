module Web.View.Static.Pages where

import Web.View.Prelude

lastUpdated :: Html
lastUpdated = [hsx|
    <p class="content-page-muted small mt-3 mb-0">Last updated: 11 March 2026</p>
|]

data AboutView = AboutView
data HowItWorksView = HowItWorksView
data CommunityRulesView = CommunityRulesView
data TermsView = TermsView
data PrivacyPolicyView = PrivacyPolicyView
data CookiePolicyView = CookiePolicyView
data LegalNoticeView = LegalNoticeView

instance View AboutView where
    html AboutView = contentPageLayout "About Predimarkt"
        (Just "Predimarkt is a privacy-conscious play-money prediction market platform operated from Vilnius, Lithuania.")
        [hsx|
            <p>
                Predimarkt lets people create and participate in public
                prediction markets using fictional balances rather than real
                money. The platform is intended for educational,
                informational, and entertainment purposes.
            </p>
            <p>
                Predimarkt is not a brokerage, exchange, gambling service, or
                investment advisory product. No deposits, withdrawals, or
                real-world payouts are offered through the platform.
            </p>
            <p>
                A core design goal of Predimarkt is to know as little as
                reasonably possible about its users. Accounts use passkeys
                instead of email-based registration, and the service is built
                without advertising, analytics, or marketing features.
            </p>
        |]

instance View HowItWorksView where
    html HowItWorksView = contentPageLayout "How It Works"
        (Just "Predimarkt uses play money and public forecasting to explore probability, judgment, and fair market resolution.")
        [hsx|
            <h2>Play-money markets</h2>
            <p>
                Markets on Predimarkt use fictional balances only. Activity on
                the platform does not create any right to real money, financial
                return, or ownership in any real-world asset.
            </p>

            <h2>User-created markets</h2>
            <p>
                Users may create public markets and take positions in them.
                Market creators should write clear market questions, define the
                intended meaning carefully, and avoid ambiguity wherever
                possible.
            </p>

            <h2>Public discussion</h2>
            <p>
                Markets may include public discussion or comments. Users are
                expected to communicate politely, stay on topic, and contribute
                constructively.
            </p>

            <h2>Resolution and refunds</h2>
            <p>
                The creator of a market is responsible for resolving it fairly,
                in good faith, and in a timely manner. If a market cannot be
                continued properly or cannot be resolved fairly, it should be
                refunded or otherwise handled responsibly.
            </p>

            <h2>No investment advice</h2>
            <p>
                Predimarkt does not provide investment advice and does not
                encourage participation in real-money financial markets. The
                platform is for play-money forecasting only.
            </p>
        |]

instance View CommunityRulesView where
    html CommunityRulesView = contentPageLayout "Community Rules"
        (Just "Predimarkt aims to host thoughtful, civil, and responsible forecasting rather than hostility, abuse, or harmful speculation.")
        [hsx|
            {lastUpdated}

            <h2>Be respectful</h2>
            <p>
                Public messages should be polite, relevant, and respectful.
                Harassment, intimidation, hateful conduct, and deliberately
                disruptive behaviour are not allowed.
            </p>

            <h2>No violent or harmful markets</h2>
            <p>
                Predimarkt does not permit markets or messages that promote,
                celebrate, encourage, or normalize violence, assassination,
                death threats, intimidation, or other serious harm. This rule
                applies even though the platform uses play money and offers no
                financial gain.
            </p>

            <h2>No unlawful or abusive use</h2>
            <ul>
                <li>No criminal, fraudulent, or abusive content.</li>
                <li>No impersonation, doxxing, or invasion of privacy.</li>
                <li>No spam, manipulation, or deceptive activity.</li>
                <li>No markets or comments that the operator considers unsafe or inappropriate.</li>
            </ul>

            <h2>Market creator responsibilities</h2>
            <ul>
                <li>Create markets in clear, good-faith terms.</li>
                <li>Resolve markets fairly and on time where possible.</li>
                <li>Refund markets if proper continuation or resolution is not possible.</li>
                <li>Cooperate with moderation if problems arise.</li>
            </ul>

            <h2>Moderation</h2>
            <p>
                Predimarkt may remove content, limit features, close markets,
                suspend accounts, or terminate access at any time at the
                operator's discretion.
            </p>
        |]

instance View TermsView where
    html TermsView = contentPageLayout "Terms of Service"
        (Just "These Terms govern access to and use of Predimarkt. By using the service, you agree to them.")
        [hsx|
            {lastUpdated}

            <h2>Eligibility</h2>
            <p>
                You must be at least 18 years old to use Predimarkt.
            </p>

            <h2>Nature of the service</h2>
            <p>
                Predimarkt is a play-money forecasting platform for
                educational, informational, and entertainment purposes only. It
                does not provide real-money trading, gambling payouts,
                brokerage services, or investment advice.
            </p>

            <h2>Accounts and access</h2>
            <p>
                Access is provided through passkey-based authentication. You
                are responsible for the devices and authentication methods used
                to access your account.
            </p>

            <h2>User content and market responsibility</h2>
            <p>
                Users may create markets and post public messages. You remain
                responsible for the content you publish and for ensuring that it
                is lawful, respectful, and consistent with the purpose of the
                platform. If you create a market, you are expected to resolve it
                fairly and in good faith, or refund it where proper resolution
                is not possible.
            </p>

            <h2>Prohibited conduct</h2>
            <ul>
                <li>No violence-promoting, threatening, or abusive content.</li>
                <li>No unlawful, fraudulent, or deceptive use of the service.</li>
                <li>No harassment, hate, intimidation, or privacy violations.</li>
                <li>No content or behaviour the operator considers harmful or inappropriate.</li>
            </ul>

            <h2>Moderation and enforcement</h2>
            <p>
                Predimarkt reserves the right, at its sole discretion, to
                remove content, close markets, refuse service, suspend users, or
                terminate accounts at any time, with or without prior notice.
            </p>

            <h2>No warranty</h2>
            <p>
                The service is provided on an "as is" and "as available"
                basis. Availability, continuity, and correctness are not
                guaranteed.
            </p>

            <h2>Limitation of liability</h2>
            <p>
                To the fullest extent permitted by applicable law, the operator
                shall not be liable for indirect, incidental, consequential, or
                special damages arising from use of Predimarkt. Nothing in these
                Terms excludes liability where exclusion is not permitted by law.
            </p>

            <h2>Changes and governing law</h2>
            <p>
                These Terms may be updated from time to time. They are governed
                by the laws of Lithuania, subject to any mandatory rights that
                apply under European Union law.
            </p>
        |]

instance View PrivacyPolicyView where
    html PrivacyPolicyView = contentPageLayout "Privacy Policy"
        (Just "Predimarkt is designed to collect and retain as little personal data as reasonably possible.")
        [hsx|
            {lastUpdated}

            <h2>Privacy-first design</h2>
            <p>
                Predimarkt intentionally avoids advertising, analytics, and
                email-based account registration. The service is operated as a
                non-commercial project without a marketing purpose, and a core
                goal is to know as little as possible about the people who use
                it.
            </p>

            <h2>Data stored within the application</h2>
            <ul>
                <li>Passkey-related authentication data needed to support login.</li>
                <li>Your current nickname.</li>
                <li>Markets you create.</li>
                <li>Trades and transaction history.</li>
                <li>Public comments or chat messages you post.</li>
                <li>Operational timestamps and related records needed for the service to function.</li>
            </ul>

            <h2>Data that is not intentionally collected</h2>
            <p>
                Predimarkt does not intentionally require user email addresses
                for normal account use and does not intentionally operate
                advertising analytics, marketing trackers, or user profiling.
                Predimarkt does not sell personal data.
            </p>

            <h2>Technical processing and infrastructure</h2>
            <p>
                Even where the application itself is designed to minimize data
                collection, limited technical request data may still be processed
                by infrastructure involved in delivering the service. This can
                include hosting and asset delivery providers used to make the
                website available and secure.
            </p>
            <p>
                Current infrastructure may include European hosting through
                Hetzner and third-party asset delivery providers used by
                frontend dependencies.
            </p>

            <h2>Why data is processed</h2>
            <ul>
                <li>To authenticate users and maintain account access.</li>
                <li>To store markets, transactions, and public discussion.</li>
                <li>To operate, secure, and maintain the platform.</li>
                <li>To comply with applicable legal obligations where necessary.</li>
            </ul>

            <h2>Legal basis</h2>
            <p>
                Where EU or EEA law applies, processing may be based on the
                performance of the service, legitimate interests in operating
                and securing the platform, and compliance with legal
                obligations.
            </p>

            <h2>Retention</h2>
            <p>
                Predimarkt seeks to retain only the information reasonably
                necessary to operate the platform and preserve its records.
                Public content, market records, and transaction history may be
                retained as part of the service history. Infrastructure providers
                may retain limited technical records in accordance with their own
                operational and legal requirements.
            </p>

            <h2>Your rights</h2>
            <p>
                If GDPR applies to you, you may have rights of access,
                rectification, erasure, restriction, objection, and complaint to
                a supervisory authority, subject to applicable limitations.
            </p>

            <h2>Contact</h2>
            <p>
                Privacy-related questions may be sent to
                <a href="mailto:info@predimarkt.eu">info@predimarkt.eu</a>.
            </p>
        |]

instance View CookiePolicyView where
    html CookiePolicyView = contentPageLayout "Cookie Policy"
        (Just "Predimarkt currently aims to use only technically necessary cookies and similar browser storage for core functionality.")
        [hsx|
            {lastUpdated}

            <h2>Necessary cookies and storage</h2>
            <p>
                Predimarkt may use a session cookie or equivalent session
                mechanism required for authentication, passkey flows, and other
                core site functionality.
            </p>

            <h2>Browser storage currently used</h2>
            <ul>
                <li><strong>Local storage</strong> may be used to remember the selected light or dark theme.</li>
                <li><strong>Session storage</strong> may be used for temporary market chat interface state such as scroll position and composer revision handling.</li>
            </ul>

            <h2>No analytics or marketing cookies</h2>
            <p>
                Predimarkt does not intentionally use analytics cookies,
                advertising cookies, or marketing trackers.
            </p>

            <h2>No consent banner at present</h2>
            <p>
                Because Predimarkt is currently intended to use only
                technically necessary cookies and similar browser storage, the
                site does not currently display a cookie consent banner. If
                non-essential cookies or tracking technologies are introduced in
                the future, this policy and any required consent mechanisms may
                be updated.
            </p>

            <h2>Third-party requests</h2>
            <p>
                Some frontend assets may currently be loaded from third-party
                content delivery services. Those requests can involve technical
                connection data as part of ordinary web delivery, even where
                Predimarkt itself does not use cookies for marketing or
                analytics.
            </p>
        |]

instance View LegalNoticeView where
    html LegalNoticeView = contentPageLayout "Legal Notice"
        (Just "Operator and legal contact information for Predimarkt.")
        [hsx|
            {lastUpdated}

            <h2>Operator</h2>
            <p>
                Alvydas Vitkauskas<br/>
                Vilnius, Lithuania<br/>
                <a href="mailto:info@predimarkt.eu">info@predimarkt.eu</a>
            </p>

            <h2>Project character</h2>
            <p>
                Predimarkt is operated as a private, non-commercial,
                privacy-conscious project. It is not presented as a real-money
                trading platform and does not provide investment advice.
            </p>

            <h2>Third-party components and attribution</h2>
            <p>
                Predimarkt uses Lightweight Charts™ for chart visualisation.
                Where the component displays attribution, logo, or a link, that
                is included for licence and attribution purposes.
            </p>
            <p>
                Attribution notice: TradingView Lightweight Charts™ Copyright
                (c) 2025 TradingView, Inc.
                <a href="https://www.tradingview.com/"
                   rel="noreferrer noopener"
                   target="_blank">https://www.tradingview.com/</a>
            </p>
            <p>
                Predimarkt is not affiliated with, endorsed by, or associated
                with TradingView. References to Lightweight Charts™ or any chart
                attribution do not constitute promotion of TradingView products,
                participation in real-money markets, or investment advice.
            </p>

            <h2>Contact</h2>
            <p>
                General, legal, and privacy-related inquiries may be sent to
                <a href="mailto:info@predimarkt.eu">info@predimarkt.eu</a>.
            </p>
        |]
