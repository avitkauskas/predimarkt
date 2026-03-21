module Web.View.Static.Pages where

import IHP.Prelude
import Web.View.Prelude

lastUpdated :: Html
lastUpdated =
    [hsx|
    <p class="small text-muted mt-4 mb-0">Last updated: 2026-03-15</p>
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
    html AboutView =
        contentPageLayout
            "About Predimarkt"
            "Predimarkt is a play-money prediction market platform for curious minds."
            [hsx|
            <p>
                It's a place to learn what prediction markets are, how they work in practice, and
                how they help reveal people's beliefs about the likelihood of future events. Here
                you can explore questions about the future by creating and participating in your
                own markets, observing how probabilities shift over time, and gaining insight into
                how collective expectations form and evolve.
            </p>

            <h5>What is a prediction market?</h5>
            <p>
                A prediction market is a system where people forecast outcomes by trading on what
                they think will happen. It works in a way that's similar to a stock market:
                instead of buying shares in companies, you're buying and selling "shares" in
                possible outcomes. The price of those shares reflects how likely the crowd thinks
                something is to happen.
            </p>

            <p>
                Instead of simply saying "I think this will happen," you back your view with a
                decision — buying or selling based on your belief. Prices then adjust as people
                trade, forming a real-time, crowd-driven probability. Over time, this process
                often produces accurate forecasts — not because any single person is right, but
                because many perspectives combine into one signal.
            </p>

            <h5>How is this different from betting?</h5>
            <p>
                At a glance, it may look similar — but the purpose and mechanics are quite
                different. Prediction markets combine estimating probabilities with taking on
                risk, where potential rewards are tied to how accurate your judgments are. The
                focus is on:
            </p>

            <ul>
                <li>reasoning under uncertainty</li>
                <li>reacting to new information</li>
                <li>improving your judgment over time</li>
            </ul>

            <p>
                Unlike traditional betting, where you place a bet and wait for the outcome,
                prediction markets are dynamic. You can adjust your position over time — buying
                more shares or selling the ones you hold as new information becomes available.
                This turns participation into an ongoing process of updating beliefs, where both
                risk and reward reflect how well you anticipate what happens next.
            </p>

            <p>
                On Predimarkt, all of this happens using play money. There's no real-world value at
                stake — you can't earn real money, but you also can't lose it. This keeps things
                safe and accessible, allowing you to focus on learning, experimenting, and
                improving your judgment rather than worrying about financial risk.
            </p>

            <h5>Learn by doing</h5>
            <p>
                Predimarkt is designed as a hands-on way to understand:
            </p>

            <ul>
                <li>probability and uncertainty</li>
                <li>how "crowd wisdom" emerges</li>
                <li>how information influences expectations</li>
            </ul>

            <p>
                You don't need any prior experience. Just pick a question, make your prediction,
                and see how it evolves.
            </p>

            <h5>Simple by design</h5>
            <p>
                Predimarkt is a non-commercial, educational, independently run project.
            </p>

            <p>
                The platform is intentionally lightweight:
            </p>

            <ul>
                <li>no real money</li>
                <li>no data collection</li>
                <li>no ads</li>
            </ul>

            <p>
                You can get started with a secure passkey and a nickname — no email required. This
                keeps things simple and minimizes the personal information you share. It also means
                we won't contact you with emails. The trade-off is that the platform can't send
                notifications or help recover your account if you lose your passkey.
            </p>

            <h5>What Predimarkt is — and isn't</h5>
            <p>
                Predimarkt is:
            </p>

            <ul>
                <li>an educational and exploratory platform</li>
                <li>a space for curiosity, discussion, and informed guessing</li>
            </ul>

            <p>
            Predimarkt is <strong>not</strong>:
            </p>

            <ul>
                <li>a gambling service</li>
                <li>a financial exchange</li>
                <li>an investment or advisory platform</li>
            </ul>

            <p>
                Nothing on the site should be interpreted as financial advice or a real-world
                opportunity.
            </p>

            <h5>Join the experiment</h5>
            <p>
                Predimarkt is an open invitation to think a bit more carefully about the future.
            </p>

            <p>
                Create markets. Trade on your beliefs. See how your expectations compare with
                everyone else.
            </p>
        |]

instance View HowItWorksView where
    html HowItWorksView =
        contentPageLayout
            "How It Works"
            "Predimarkt is a space for exploring forecasts through interactive markets."
            [hsx|
            <h5>Play-money system</h5>

            <p>
                All activity on Predimarkt uses fictional balances. You start with an initial
                balance and use it to trade on events. Everything runs on play money, allowing you
                to focus on reasoning, information, and judgment without financial risk. There are
                no deposits, withdrawals, or real-world rewards. You can't earn real money — but
                you also can't lose it. Your balance simply reflects how accurate your predictions
                have been over time. Balances may go negative. This allows you to fully
                participate in markets, including taking positions that may later turn out to be
                incorrect.
            </p>

            <h5>Prediction markets</h5>

            <p>
                Each market represents a question about a future event. It includes a set of
                possible outcomes (assets), and each outcome has a price that reflects its current
                estimated probability. By trading — buying or selling outcomes — participants
                collectively shape these probabilities. As new information becomes available, users
                make new trades and prices update to reflect changing expectations.
            </p>

            <h5>Trading</h5>

            <p>
                Trading is continuous and flexible. You can act at any time:
            </p>

            <ul>
                <li>Buy an outcome if you believe it is more likely than the current price
                    suggests</li>
                <li>Sell an outcome if you believe it is less likely than the current price
                    suggests</li>
            </ul>

            <p>
                In binary markets, buying "Yes" is equivalent to selling "No," and vice versa —
                though the framing may differ in how you think about the trade.
            </p>

            <p>
                When buying, you spend money now and profit if the outcome happens. But you can
                sell at any time to cut losses or take profits before resolution. This allows you
                to adjust your position as the situation evolves. You can also short sell (sell
                assets that you don't own yet). In this case, you receive money immediately, and
                this is your maximum possible gain — but if you're wrong, you may have to pay back
                more later.
            </p>

            <p>
                Predimarkt uses an automated market maker based on the logarithmic market scoring
                rule. This means:
            </p>

            <ul>
                <li>You can always trade instantly — there are no bids or asks, and liquidity is
                    always available</li>
                <li>Prices adjust smoothly with each trade, so the total cost is not simply "price
                    × quantity"</li>
                <li>The price you pay varies within a trade: earlier shares are cheaper, and later
                    shares reflect the updated probability</li>
            </ul>

            <h5>Creating a market</h5>

            <p>
                Anyone can create a market. With that comes responsibility.
                A well-created market should be:
            </p>

            <ul>
                <li>Clear — the title should be short, informative, and unambiguous</li>
                <li>Well-defined — the description must explain exactly how the outcome will be
                    determined</li>
                <li>Verifiable — resolution should rely on publicly available information whenever
                    possible</li>
            </ul>

            <p>
                When creating a market, you should:
            </p>

            <ul>
                <li>Write a clear title</li>
                <li>Provide rules and description, including definitions and resolution criteria</li>
                <li>Optionally include references to sources</li>
                <li>Define the possible outcomes (assets)</li>
                <li>Assign short, unique symbols for each outcome (used in charts)</li>
                <li>Set initial probabilities to reflect your best estimate at the time</li>
            </ul>

            <p>
                Setting reasonable initial probabilities is important — large inaccuracies can
                create unfair advantages for early traders. Creators are also expected to avoid
                immediately taking large positions in their own market to exploit initial pricing.
            </p>

            <h5>Responsibilities of market creators</h5>

            <p>
                Market creators act as stewards of their markets. They are expected to:
            </p>

            <ul>
                <li>Treat participants fairly and respectfully</li>
                <li>Keep rules clear and consistent</li>
                <li>Update descriptions if clarification is needed (without changing the core meaning)</li>
                <li>Monitor the event and resolve the market promptly once the outcome is known</li>
            </ul>

            <p>
                Resolution is simple but critical:
            </p>

            <ul>
                <li>You select the winning outcome</li>
                <li>All payouts are handled automatically</li>
            </ul>

            <p>
                This action is final and cannot be reversed, so it must be done carefully. Always
                verify the result before resolving.
            </p>

            <h5>Refunds</h5>

            <p>
                If a market cannot be resolved fairly — due to ambiguity, missing information, or
                unforeseen circumstances — it should be refunded. Refunding returns all invested
                funds to participants and is handled automatically by the system. Refunds should be
                used only as a last resort, when proper resolution is not possible.
            </p>

            <h5>Leaderboard and goals</h5>

            <p>
                The leaderboard highlights the top participants, based on performance adjusted over
                time. It accounts for factors like consistency and duration, rather than just total
                accumulated gains. However, the goal of Predimarkt is not to "win" the leaderboard.
                There are no material rewards. The real value comes from improving your judgment,
                learning how to reason under uncertainty, and understanding how information shapes
                expectations.
            </p>

            <h5>Final note</h5>

            <p>
                Predimarkt is an educational platform. It does not provide financial or investment
                advice, and nothing on the platform should be interpreted as guidance for real-world
                decisions.
            </p>
        |]

instance View CommunityRulesView where
    html CommunityRulesView =
        contentPageLayout
            "Community Rules"
            "Predimarkt aims to host thoughtful, civil, and responsible forecasting."
            [hsx|
            <h5>Purpose of the community</h5>

            <p>
                Our community exists to learn about future events, improve judgment, and enjoy the
                process of prediction — not to maximize play-money gains. Predimarkt supports
                constructive discussion about uncertain future events. Participants should focus on
                providing thoughtful forecasts, relevant evidence, and respectful dialogue that
                helps everyone understand the events being traded.
            </p>

            <h5>Be respectful</h5>

            <p>
                Communication on the platform must remain civil, relevant, and respectful.
                Harassment, intimidation, hateful conduct, or persistent disruption are not
                allowed.
            </p>

            <h5>Prohibited markets</h5>

            <ul>
                <li>
                    Markets promoting or celebrating violence, physical harm, or criminal activity
                    are strictly prohibited. This includes assassination, death speculation,
                    violent acts, or similar harmful scenarios.
                </li>
                <li>
                    Markets concerning private individuals, personal health, death, criminal
                    accusations, or other sensitive personal matters may be removed.
                </li>
                <li>
                    Markets about public figures or public events are allowed only when framed
                    responsibly and without abusive targeting.
                </li>
            </ul>

            <h5>Responsible forecasting</h5>

            <p>
                Markets about public events, politics, economics, science, or culture are welcome
                when presented responsibly. Participants should avoid creating or supporting
                markets designed to manipulate play money outcomes or exploit other users,
                including coordinated trades to take advantage of improperly set initial
                probabilities.
            </p>

            <h5>Market creation and trading ethics</h5>

            <p>
                Market creators should set clear, accurate initial probabilities and avoid
                immediately taking large positions in their own markets. Users should not
                cooperate to exploit new markets for initial gains — the goal is to learn about
                the event, not to earn more play money. Good-faith participation and accurate
                initial settings help maintain fairness and educational value for all users.
            </p>

            <h5>No unlawful or abusive use</h5>

            <ul>
                <li>No fraudulent or deceptive activity</li>
                <li>No impersonation or privacy violations</li>
                <li>No spam, market manipulation, or artificial trading activity</li>
                <li>No content that violates applicable laws</li>
            </ul>

            <h5>Good-faith interpretation of markets</h5>

            <p>
                Market questions should be interpreted in good faith, according to their stated
                description, the context of creation, and the reasonable expectations of
                participants. When ambiguity exists, interpretation may consider the intent of the
                market creator and what a reasonable participant would have understood while the
                market was active.
            </p>

            <h5>Abandoned markets</h5>

            <p>
                If a market creator becomes inactive or deletes their account before resolving a
                market, the platform operator may resolve, close, or refund the market to protect
                participants.
            </p>

            <h5>Platform intervention and market integrity</h5>

            <p>
                To maintain fairness and the educational purpose of Predimarkt, the operator may
                intervene when necessary. This may include:
            </p>

            <ul>
                <li>Closing markets</li>
                <li>Refunding trades</li>
                <li>Removing markets</li>
                <li>Restricting participation</li>
                <li>Suspending accounts involved in abuse</li>
            </ul>

            <p>
                Intervention decisions are made in good faith to preserve the integrity,
                reliability, and learning focus of the platform.
            </p>

            <h5>Moderation</h5>

            <p>
                Predimarkt may remove content, close markets, limit functionality, or suspend
                accounts when necessary to maintain the safety and fairness of the community.
            </p>
        |]

instance View ModerationPolicyView where
    html ModerationPolicyView =
        contentPageLayout
            "Content Moderation & Complaints"
            "How Predimarkt handles content moderation and user reports."
            [hsx|
            <h5>Purpose</h5>

            <p>
                Our goal is to maintain a safe, constructive, and educational environment while
                allowing users to create markets and engage in public discussion. Predimarkt
                allows users to create markets and post discussion messages. To ensure the
                platform remains productive and safe for all participants, certain types of
                content may be moderated or removed. Moderation is designed to protect users,
                maintain fairness, and support the educational focus of the platform.
            </p>

            <h5>Types of content on the platform</h5>

            <p>
                Most content on Predimarkt is created directly by users. This includes:
            </p>

            <ul>
                <li>Prediction market questions and descriptions</li>
                <li>Play-money trading activity</li>
                <li>Public comments and discussion messages</li>
            </ul>

            <p>
                Users are responsible for the content they publish and are expected to follow the
                Community Rules. All content should support respectful discussion and constructive
                forecasting.
            </p>

            <h5>Content that may be moderated</h5>

            <p>
                The platform may remove or restrict content that violates Community Rules or
                applicable law. Examples include content that:
            </p>

            <ul>
                <li>Promotes or celebrates violence, harm, or illegal activity</li>
                <li>Contains harassment, intimidation, or hate speech</li>
                <li>Invades privacy, impersonates others, or misrepresents identity</li>
                <li>Is fraudulent, deceptive, or otherwise unlawful</li>
                <li>Disrupts the intended educational purpose of the platform</li>
            </ul>

            <h5>Moderation actions</h5>

            <p>
                To maintain fairness and integrity, the operator may take the following actions:
            </p>

            <ul>
                <li>Removing or editing comments, posts, or market descriptions</li>
                <li>Closing or refunding markets that violate rules or cannot be resolved
                    fairly</li>
                <li>Restricting participation in certain markets or platform features</li>
                <li>Suspending or terminating accounts that repeatedly violate rules or abuse
                    the system</li>
            </ul>

            <p>
                All moderation decisions are made in good faith based on the available information
                and the goals of the platform.
            </p>

            <h5>Reporting content</h5>

            <p>
                Users who believe content violates Community Rules or applicable law may report it
                to the operator. Reports should include:
            </p>

            <ul>
                <li>A link to the relevant market, comment, or post</li>
                <li>A brief explanation of the issue</li>
            </ul>

            <p>
                Reports may be sent to:
                <a href="mailto:info@predimarkt.eu">
                    info@predimarkt.eu
                </a>
            </p>

            <h5>Complaints and questions</h5>

            <p>
                If you believe a moderation action has been taken in error, you may contact the
                operator using the same address. The platform will review reasonable requests and
                provide a response when possible.
            </p>

            <h5>Project scale</h5>

            <p>
                Predimarkt is a small, independent project. Moderation is performed manually by the
                operator, and response times may vary. Participants are asked to be patient and
                considerate of the limitations of a small team.
            </p>
        |]

instance View LegalNoticeView where
    html LegalNoticeView =
        contentPageLayout
            "Legal Notice"
            "Operator and contact information for Predimarkt."
            [hsx|
            <h5>Operator</h5>

            <p>
                Predimarkt is operated by Alvydas Vitkauskas, based in Vilnius, Lithuania.<br/>
                Contact: <a href="mailto:info@predimarkt.eu">info@predimarkt.eu</a>
            </p>

            <h5>Project Character</h5>

            <p>
                Predimarkt is a private, non-commercial project designed for educational purposes,
                focused on the exploration of forecasting and prediction markets. The platform is
                provided "as is" for learning and informational use only.
            </p>

            <h5>Third-Party Components</h5>

            <p>
                Predimarkt uses TradingView's Lightweight Charts™ for market visualization. The
                platform is not affiliated with, endorsed by, or officially connected to
                TradingView.
            </p>
        |]

instance View TermsView where
    html TermsView =
        contentPageLayout
            "Terms of Service"
            "By using the platform, you agree to comply with these Terms."
            [hsx|
            <h5>Eligibility</h5>

            <p>
                Predimarkt is intended for individuals aged 18 years or older. The platform does
                not verify age, but by using the service, you represent that you meet this
                requirement.
            </p>

            <h5>Nature of the service</h5>

            <p>
                Predimarkt is a play-money forecasting platform provided for educational,
                informational, and entertainment purposes only. The platform does not provide
                financial services, brokerage services, gambling payouts, or investment advice. No
                real-money transactions are supported.
            </p>

            <h5>Educational and experimental environment</h5>

            <p>
                Predimarkt is an experimental environment for exploring forecasting and
                probabilistic reasoning. Market prices reflect the behavior of participants within
                a play-money simulation and are not authoritative predictions about real-world
                events.
            </p>

            <h5>No reliance</h5>

            <p>
                Information on Predimarkt, including market prices, probabilities, comments, and
                discussions, reflects the activity and opinions of users. Such information should
                not be interpreted as factual statements, professional advice, or reliable
                forecasts. Users should not rely on the platform for financial, legal, political,
                or other real-world decisions.
            </p>

            <h5>Accounts and authentication</h5>

            <p>
                Accounts are accessed using passkey-based authentication. Users are responsible for
                maintaining secure control of their authentication devices and passkeys. If
                credentials are lost, account recovery may not be possible.
            </p>

            <h5>User content</h5>

            <p>
                Users may create markets and post public messages. You remain responsible for the
                legality and accuracy of the content you publish.
            </p>

            <h5>Market resolution</h5>

            <p>
                Market creators are responsible for resolving markets fairly and according to the
                stated rules. Where possible, resolutions should rely on publicly available and
                verifiable information. The platform operator does not independently verify all
                resolutions and cannot guarantee the accuracy of user-provided information.
            </p>

            <p>
                The operator may intervene to close, refund, or remove markets to maintain
                platform integrity, but does not guarantee the correctness of user-generated
                markets.
            </p>

            <h5>Prohibited conduct</h5>

            <ul>
                <li>Violent, threatening, or abusive content</li>
                <li>Fraudulent or unlawful activity</li>
                <li>Harassment or privacy violations</li>
                <li>Manipulation of markets or platform systems</li>
            </ul>

            <h5>Service availability</h5>

            <p>
                The service is provided on an "as is" and "as available" basis. Availability,
                correctness, and uninterrupted access are not guaranteed.
            </p>

            <h5>Limitation of liability</h5>

            <p>
                To the fullest extent permitted by applicable law, the operator shall not be liable
                for indirect, incidental, or consequential damages arising from the use of the
                service.
            </p>

            <h5>Governing law</h5>

            <p>
                These Terms are governed by the laws of Lithuania, subject to mandatory rights
                under European Union law.
            </p>

            {lastUpdated}
        |]

instance View PrivacyPolicyView where
    html PrivacyPolicyView =
        contentPageLayout
            "Privacy Policy"
            "This policy explains what information is collected, how it is used, and your rights as a user."
            [hsx|
            <h5>Privacy-first design</h5>

            <p>
                Predimarkt is intentionally designed to minimize personal data collection. The
                platform does not require email addresses and does not use advertising trackers or
                behavioral analytics.
            </p>

            <h5>Data stored by the application</h5>

            <ul>
                <li>Passkey authentication records</li>
                <li>Your chosen nickname</li>
                <li>Markets you create</li>
                <li>Play-money trades and transaction history</li>
                <li>Public comments posted by your account</li>
                <li>Operational timestamps required for platform operation</li>
            </ul>

            <h5>Data not intentionally collected</h5>

            <p>
                Predimarkt does not require email addresses, real names, or other identifying
                personal information for normal account use. The platform does not sell personal
                data and does not operate advertising or behavioral profiling systems.
            </p>

            <h5>Infrastructure and technical processing</h5>

            <p>
                Like most websites, limited technical information such as IP addresses may be
                processed by hosting providers or content delivery networks to deliver the website
                securely. Predimarkt is hosted in the European Union using providers such as
                Hetzner.
            </p>

            <h5>Legal basis</h5>

            <p>
                Where the General Data Protection Regulation (GDPR) applies, processing is based on
                the legitimate interest of operating and securing the platform, and on fulfilling
                the service requested by the user.
            </p>

            <h5>Account deletion</h5>

            <p>
                Users may delete their accounts at any time. Account deletion removes associated
                trades, comments, and leaderboard records. Some market records may remain when
                necessary to preserve the integrity and continuity of existing markets.
            </p>

            <h5>Your rights</h5>

            <p>
                Depending on your jurisdiction, you may have rights under applicable data
                protection law, including access, rectification, erasure, restriction, and the
                right to lodge a complaint with a supervisory authority.
            </p>

            <h5>Contact</h5>

            <p>
                Privacy inquiries may be sent to
                <a href="mailto:info@predimarkt.eu">info@predimarkt.eu</a>.
            </p>

            {lastUpdated}
        |]

instance View CookiePolicyView where
    html CookiePolicyView =
        contentPageLayout
            "Cookie Policy"
            "This policy explains how cookies and browser storage are used and how you can manage them."
            [hsx|
            <h5>Overview</h5>

            <p>
                Predimarkt aims to use only technically necessary cookies and minimal browser
                storage required for core functionality. Predimarkt is designed to minimize
                tracking and data collection. The platform does not use advertising cookies,
                behavioral tracking, or analytics systems. Only limited technical mechanisms
                required for the operation of the website may be used, such as session cookies and
                browser storage for interface preferences.
            </p>

            <h5>Technically necessary cookies</h5>

            <p>
                Predimarkt may use session cookies or equivalent mechanisms required for
                authentication, passkey login flows, and other essential site functionality. These
                cookies are considered technically necessary and do not require user consent under
                applicable European ePrivacy rules.
            </p>

            <h5>Browser storage</h5>

            <p>
                Some features may use browser storage that remains on your device:
            </p>

            <ul>
                <li>Local storage may remember interface preferences such as the selected light or
                    dark theme</li>
                <li>Session storage may temporarily store interface state, for example message
                    drafts or scroll position in market discussions</li>
            </ul>

            <p>
                These storage mechanisms are controlled by your browser and are not used for
                cross-site tracking.
            </p>

            <h5>No analytics or marketing cookies</h5>

            <p>
                Predimarkt does not intentionally use analytics cookies, advertising technologies,
                marketing trackers, or user profiling systems.
            </p>

            <h5>No consent banner</h5>

            <p>
                Because Predimarkt currently uses only technically necessary cookies and limited
                browser storage for core functionality, the site does not display a cookie consent
                banner. If non-essential cookies or tracking technologies are introduced in the
                future, this policy and any required consent mechanisms will be updated.
            </p>

            <h5>Third-party technical requests</h5>

            <p>
                Some frontend assets may be delivered through third-party content delivery networks
                such as JSDelivr or unpkg. These requests are part of normal web delivery and may
                involve transmission of technical connection data, such as IP addresses, to those
                providers.
            </p>

            <h5>Managing cookies</h5>

            <p>
                Most web browsers allow users to control or disable cookies through browser
                settings. Please note that disabling cookies or browser storage may affect
                Predimarkt's ability to provide login and other core functionality.
            </p>

            {lastUpdated}
        |]
