module Web.View.Static.Pages where

import Text.Blaze.Html4.FrameSet (h2)
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
data ModerationPolicyView = ModerationPolicyView
data LegalNoticeView = LegalNoticeView

instance View AboutView where
    html AboutView = contentPageLayout "About Predimarkt"
        (Just "Predimarkt is a play-money prediction market platform operated from Vilnius, Lithuania.")
        [hsx|
            <p>
                Predimarkt is a public forecasting platform where people can
                create and participate in prediction markets using fictional
                balances rather than real money. Markets represent questions
                about future events and allow participants to express
                probabilistic beliefs through trading with play money.
            </p>

            <p>
                The platform is intended primarily for educational,
                informational, and entertainment purposes. It aims to encourage
                careful reasoning about uncertainty, probability, and public
                information.
            </p>

            <p>
                Predimarkt is not a brokerage, financial exchange, gambling
                operator, or investment advisory service. The platform does
                not accept deposits, does not provide withdrawals, and does
                not offer any real-world financial payouts.
            </p>

            <p>
                Predimarkt does not permit wagering or staking of anything
                of real-world value.
            </p>

            <p>
                A central design goal of Predimarkt is user data minimisation.
                Accounts use passkey-based authentication and do not require
                email addresses or other identifying personal information.
                The service is operated without advertising, behavioural
                tracking, or marketing analytics.
            </p>

            <p>
                Predimarkt is operated as a private, non-commercial project
                from Vilnius, Lithuania. It is intended as an experimental
                and educational environment for exploring forecasting and
                prediction markets while respecting user privacy.
            </p>
        |]

instance View HowItWorksView where
    html HowItWorksView = contentPageLayout "How It Works"
        (Just "Predimarkt uses play money and public forecasting to explore probability, judgment, and fair market resolution.")
        [hsx|
            <h2>Play-money markets</h2>
            <p>
                All activity on Predimarkt uses fictional balances only.
                No deposits, withdrawals, or transfers of real-world value
                are possible. Participation does not create any financial
                rights or claims.
            </p>

            <h2>Prediction markets</h2>
            <p>
                A prediction market represents a question about a future
                event. Participants trade between possible outcomes using
                play money, and the resulting prices reflect collective
                expectations about the likelihood of those outcomes.
            </p>

            <h2>User-created markets</h2>
            <p>
                Registered users may create public markets. Market creators
                are expected to define questions clearly, specify the
                resolution criteria in advance, and avoid ambiguity
                wherever possible.
            </p>

            <h2>Market resolution</h2>
            <p>
                The creator of a market is responsible for resolving it in
                good faith and according to the stated rules. Resolution
                should be based on publicly verifiable information whenever
                possible.
            </p>

            <p>
                If a market cannot reasonably be resolved or continued,
                it should be refunded. The platform operator may intervene
                to close, refund, or remove markets when necessary to
                maintain the integrity of the service.
            </p>

            <h2>Public discussion</h2>
            <p>
                Markets may include public discussion. Participants are
                encouraged to share information, reasoning, and evidence
                that may help others understand the event being forecast.
            </p>

            <h2>No financial advice</h2>
            <p>
                Predimarkt does not provide financial, investment, or
                trading advice. The platform is intended for educational
                forecasting only and should not be interpreted as guidance
                for real-world financial decisions.
            </p>
        |]

instance View CommunityRulesView where
    html CommunityRulesView = contentPageLayout "Community Rules"
        (Just "Predimarkt aims to host thoughtful, civil, and responsible forecasting.")
        [hsx|
            {lastUpdated}

            <h2>Purpose of the community</h2>
            <p>
                Predimarkt is intended to support constructive discussion
                about uncertain future events. Participants should aim to
                contribute thoughtful forecasts, evidence, and respectful
                dialogue.
            </p>

            <h2>Be respectful</h2>
            <p>
                Communication on the platform should remain civil,
                relevant, and respectful. Harassment, intimidation,
                hateful conduct, or persistent disruption are not allowed.
            </p>

            <h2>Prohibited markets</h2>
            <p>
                Markets must not promote or celebrate violence,
                physical harm, or criminal activity. In particular,
                markets about assassination, death speculation,
                violent acts, or similar harmful scenarios are not
                permitted.
            </p>

            <h2>Markets concerning individuals</h2>
            <p>
                Markets that speculate about private individuals,
                health, death, criminal accusations, or other sensitive
                personal matters may be removed.
            </p>

            <p>
                Markets about public figures or public events may be
                permitted when framed in a responsible and non-abusive
                manner.
            </p>

            <h2>Responsible forecasting</h2>
            <p>
                Markets about public events, politics, economics,
                science, or culture may be permitted when framed
                responsibly and without targeting individuals in a
                harmful or abusive manner.
            </p>

            <h2>No unlawful or abusive use</h2>
            <ul>
                <li>No fraudulent or deceptive activity.</li>
                <li>No impersonation or privacy violations.</li>
                <li>No spam, manipulation, or artificial market activity.</li>
                <li>No content that is unlawful under applicable law.</li>
            </ul>

            <h2>Market creator responsibilities</h2>
            <ul>
                <li>Create markets in clear and good-faith terms.</li>
                <li>Provide transparent resolution criteria.</li>
                <li>Resolve markets fairly and in a timely manner.</li>
                <li>Refund markets when proper resolution is not possible.</li>
            </ul>

            <h2>Good-faith interpretation of markets</h2>
            <p>
                Market questions should be interpreted in good faith according to
                their stated description, the context in which they were created,
                and the reasonable expectations of participants.
            </p>

            <p>
                Where ambiguity exists, interpretation may consider the intent of
                the market creator and the understanding that a reasonable
                participant would have had at the time the market was active.
            </p>

            <h2>Abandoned markets</h2>
            <p>
                If a market creator becomes inactive or deletes their
                account before resolving a market, the platform operator
                may resolve, close, or refund the market in order to
                protect participants.
            </p>

            <h2>Platform intervention and market integrity</h2>
            <p>
                To maintain the integrity and educational purpose of the
                platform, the operator may intervene in markets when
                necessary.
            </p>

            <p>Such intervention may include:</p>
            <ul>
                <li>closing markets</li>
                <li>refunding trades</li>
                <li>removing markets</li>
                <li>restricting participation</li>
                <li>suspending accounts involved in abuse</li>
            </ul>

            <p>
                Intervention decisions are made in good faith to preserve
                the fairness and reliability of the platform.
            </p>

            <h2>Moderation</h2>
            <p>
                Predimarkt may remove content, close markets, limit
                functionality, or suspend accounts when necessary to
                maintain the safety and integrity of the platform.
            </p>
        |]

instance View TermsView where
    html TermsView = contentPageLayout "Terms of Service"
        (Just "These Terms govern access to and use of Predimarkt.")
        [hsx|
            {lastUpdated}

            <h2>Eligibility</h2>
            <p>
                Predimarkt is intended for individuals aged 18 years
                or older. The platform does not verify age, but by
                using the service you represent that you meet this
                requirement.
            </p>

            <h2>Nature of the service</h2>
            <p>
                Predimarkt is a play-money forecasting platform
                provided for educational, informational, and
                entertainment purposes only.
            </p>

            <p>
                The platform does not provide financial services,
                brokerage services, gambling payouts, or investment
                advice. No real-money transactions are supported.
            </p>

            <h2>Educational and experimental environment</h2>
            <p>
                Predimarkt is intended as an experimental and educational
                environment for exploring forecasting and probabilistic reasoning.
            </p>

            <p>
                Market prices reflect the behaviour of participants within a
                play-money simulation and are not intended to represent
                authoritative predictions about real-world events.
            </p>

            <h2>No reliance</h2>
            <p>
                Information presented on Predimarkt, including market prices,
                probabilities, comments, and discussions, reflects the activity
                and opinions of users.
            </p>

            <p>
                Such information should not be interpreted as factual statements,
                professional advice, or reliable forecasts. Users should not rely
                on the platform when making financial, legal, political, or other
                real-world decisions.
            </p>

            <h2>Accounts and authentication</h2>
            <p>
                Accounts are accessed using passkey-based
                authentication. Users are responsible for maintaining
                control of their authentication devices and passkeys.
            </p>

            <p>
                If access credentials are lost, account recovery may
                not be possible. Users should ensure they maintain
                secure access to their authentication methods.
            </p>

            <h2>Account access responsibility</h2>
            <p>
                Accounts are accessed using passkey authentication controlled
                by the user.
            </p>

            <p>
                If a user loses access to their authentication credentials,
                account recovery may not be possible. The platform does not
                guarantee the ability to restore lost accounts.
            </p>

            <h2>User content</h2>
            <p>
                Users may create markets and post public messages.
                You remain responsible for the legality and accuracy
                of the content you publish.
            </p>

            <h2>Market resolution</h2>
            <p>
                Market creators are responsible for resolving their
                markets fairly and according to the stated rules.
                The platform operator may close, refund, or remove
                markets where necessary to maintain platform
                integrity.
            </p>

            <h2>Resolution sources</h2>
            <p>
                Where possible, market resolution should rely on publicly
                available and verifiable information sources.
            </p>

            <p>
                The platform operator does not independently verify all
                resolution decisions and cannot guarantee the accuracy of
                information used by market creators.
            </p>

            <h2>User responsibility for markets</h2>
            <p>
                Users who create markets are responsible for defining the
                resolution criteria and resolving their markets in good faith.
            </p>

            <p>
                The platform operator may intervene to close, refund, or
                remove markets where necessary, but does not guarantee
                the correctness of user-generated markets.
            </p>

            <h2>Prohibited conduct</h2>
            <ul>
                <li>Violent, threatening, or abusive content.</li>
                <li>Fraudulent or unlawful activity.</li>
                <li>Harassment or privacy violations.</li>
                <li>Manipulation of markets or platform systems.</li>
            </ul>

            <h2>Service availability</h2>
            <p>
                The service is provided on an “as is” and “as available”
                basis. Availability and correctness are not guaranteed.
            </p>

            <h2>Limitation of liability</h2>
            <p>
                To the fullest extent permitted by applicable law,
                the operator shall not be liable for indirect,
                incidental, or consequential damages arising from
                use of the service.
            </p>

            <h2>Governing law</h2>
            <p>
                These Terms are governed by the laws of Lithuania,
                subject to mandatory rights under European Union law.
            </p>
        |]

instance View PrivacyPolicyView where
    html PrivacyPolicyView = contentPageLayout "Privacy Policy"
        (Just "Predimarkt is designed to collect and retain as little personal data as reasonably possible.")
        [hsx|
            {lastUpdated}

            <h2>Privacy-first design</h2>
            <p>
                Predimarkt is intentionally designed to minimise
                personal data collection. The service does not require
                email addresses and does not use advertising trackers
                or behavioural analytics.
            </p>

            <h2>Data stored by the application</h2>
            <ul>
                <li>Passkey authentication records.</li>
                <li>Your chosen nickname.</li>
                <li>Markets you create.</li>
                <li>Play-money trades and transaction history.</li>
                <li>Public comments posted by your account.</li>
                <li>Operational timestamps required for platform operation.</li>
            </ul>

            <h2>Data not intentionally collected</h2>
            <p>
                Predimarkt does not require email addresses, real
                names, or other identifying personal information for
                normal account use.
            </p>

            <p>
                The service does not sell personal data and does not
                operate advertising or behavioural profiling systems.
            </p>

            <h2>Infrastructure and technical processing</h2>
            <p>
                Like most websites, limited technical information
                such as IP addresses may be processed by hosting
                providers or content delivery networks in order to
                deliver the website securely.
            </p>

            <p>
                Predimarkt is currently hosted in the European Union
                using infrastructure providers such as Hetzner. Some
                frontend assets may be delivered through third-party
                content delivery networks.
            </p>

            <h2>Legal basis</h2>
            <p>
                Where the General Data Protection Regulation (GDPR)
                applies, processing is based on the legitimate
                interest of operating and securing the platform and
                on providing the service requested by the user.
            </p>

            <h2>Account deletion</h2>
            <p>
                Users may delete their accounts at any time. Account
                deletion removes associated trades, comments, and
                leaderboard records from the platform.
            </p>

            <p>
                Some market records may remain where necessary to
                preserve the integrity and continuity of existing
                markets.
            </p>

            <h2>Your rights</h2>
            <p>
                Depending on your jurisdiction, you may have rights
                under data protection law including access,
                rectification, erasure, restriction, and the right
                to lodge a complaint with a supervisory authority.
            </p>

            <h2>Contact</h2>
            <p>
                Privacy inquiries may be sent to
                <a href="mailto:info@predimarkt.eu">info@predimarkt.eu</a>.
            </p>
        |]

instance View CookiePolicyView where
    html CookiePolicyView = contentPageLayout "Cookie Policy"
        (Just "Predimarkt aims to use only technically necessary cookies and minimal browser storage required for core functionality.")
        [hsx|
            {lastUpdated}

            <h2>Overview</h2>
            <p>
                Predimarkt is designed to minimise tracking and data
                collection. The platform does not use advertising
                cookies, behavioural tracking, or analytics systems.
            </p>

            <p>
                Only limited technical mechanisms required for the
                operation of the website may be used, such as session
                cookies and browser storage for interface preferences.
            </p>

            <h2>Technically necessary cookies</h2>
            <p>
                Predimarkt may use a session cookie or equivalent
                session mechanism required for authentication,
                passkey login flows, and other essential site
                functionality.
            </p>

            <p>
                These cookies are considered technically necessary
                for the operation of the service and therefore do not
                require user consent under applicable European
                ePrivacy rules.
            </p>

            <h2>Browser storage</h2>
            <p>
                Some features may use browser storage that remains
                on the user's device.
            </p>

            <ul>
                <li>
                    <strong>Local storage</strong> may be used to
                    remember interface preferences such as the
                    selected light or dark theme.
                </li>
                <li>
                    <strong>Session storage</strong> may be used for
                    temporary interface state, for example message
                    composer drafts or scroll position in market
                    discussions.
                </li>
            </ul>

            <p>
                These storage mechanisms are controlled by the
                user's browser and are not used for cross-site
                tracking.
            </p>

            <h2>No analytics or marketing cookies</h2>
            <p>
                Predimarkt does not intentionally use analytics
                cookies, advertising technologies, marketing
                trackers, or user profiling systems.
            </p>

            <h2>No consent banner</h2>
            <p>
                Because Predimarkt currently uses only technically
                necessary cookies and limited browser storage for
                core functionality, the site does not display a
                cookie consent banner.
            </p>

            <p>
                If non-essential cookies or tracking technologies
                are introduced in the future, this policy and any
                required consent mechanisms will be updated.
            </p>

            <h2>Third-party technical requests</h2>
            <p>
                Some frontend assets may be delivered through
                third-party content delivery networks such as
                JSDelivr or unpkg. These requests are a normal
                part of web delivery and may involve the
                transmission of technical connection data such
                as IP addresses to those providers.
            </p>

            <h2>Managing cookies</h2>
            <p>
                Most web browsers allow users to control or disable
                cookies through browser settings. Please note that
                disabling cookies or browser storage may affect the
                ability of Predimarkt to provide login and other
                core functionality.
            </p>
        |]

instance View LegalNoticeView where
    html LegalNoticeView = contentPageLayout "Legal Notice"
        (Just "Operator and contact information for Predimarkt.")
        [hsx|
            {lastUpdated}

            <h2>Operator</h2>
            <p>
                Predimarkt is operated by:<br/>
                Alvydas Vitkauskas<br/>
                Vilnius, Lithuania
            </p>

            <p>
                Contact:
                <a href="mailto:info@predimarkt.eu">info@predimarkt.eu</a>
            </p>

            <h2>Project character</h2>
            <p>
                Predimarkt is a private, non-commercial project
                intended for educational exploration of forecasting
                and prediction markets.
            </p>

            <h2>Third-party components</h2>
            <p>
                Predimarkt uses TradingViews Lightweight Charts™ for market visualisation.
                <br/>Predimarkt is not affiliated with or endorsed by TradingView.
            </p>
        |]

instance View ModerationPolicyView where
    html ModerationPolicyView = contentPageLayout "Content Moderation & Complaints"
        (Just "How Predimarkt handles content moderation and user reports.")
        [hsx|
            {lastUpdated}

            <h2>Purpose</h2>
            <p>
                Predimarkt allows users to create markets and post
                public discussion messages. In order to maintain a
                constructive and safe environment, certain forms of
                content may be moderated or removed.
            </p>

            <p>
                This page explains how content moderation and
                complaints are handled on the platform.
            </p>

            <h2>Types of content on the platform</h2>
            <p>
                Most content on Predimarkt is created directly by
                users. This includes:
            </p>

            <ul>
                <li>Prediction market questions and descriptions</li>
                <li>Play-money trading activity</li>
                <li>Public comments or discussion messages</li>
            </ul>

            <p>
                Users are responsible for the content they publish
                and are expected to follow the Community Rules.
            </p>

            <h2>Content that may be moderated</h2>
            <p>
                The platform may remove or restrict content that
                violates the Community Rules or applicable law.
                This may include content that:
            </p>

            <ul>
                <li>Promotes or celebrates violence or harm</li>
                <li>Contains harassment, hate, or intimidation</li>
                <li>Invades privacy or impersonates others</li>
                <li>Is fraudulent, deceptive, or unlawful</li>
                <li>Disrupts the intended educational purpose of the platform</li>
            </ul>

            <h2>Moderation actions</h2>
            <p>
                When necessary to maintain the integrity of the
                platform, the operator may take actions such as:
            </p>

            <ul>
                <li>Removing comments or markets</li>
                <li>Closing or refunding markets</li>
                <li>Restricting user participation</li>
                <li>Suspending or terminating accounts</li>
            </ul>

            <p>
                Moderation decisions are made in good faith based on
                the available information and the goals of the
                platform.
            </p>

            <h2>Reporting content</h2>
            <p>
                If you believe that content on Predimarkt violates
                the Community Rules or applicable law, you may report
                it by contacting the operator.
            </p>

            <p>
                Reports should include a link to the relevant market
                or comment and a short explanation of the issue.
            </p>

            <p>
                Reports may be sent to:
                <a href="mailto:info@predimarkt.eu">info@predimarkt.eu</a>
            </p>

            <h2>Complaints and questions</h2>
            <p>
                If you believe moderation action has been taken in
                error, you may contact the operator using the same
                address. The platform will review reasonable
                requests where possible.
            </p>

            <h2>Project scale</h2>
            <p>
                Predimarkt is operated as a small independent
                project. Moderation is performed manually by the
                operator and response times may vary.
            </p>
        |]
