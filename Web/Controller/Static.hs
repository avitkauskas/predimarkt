module Web.Controller.Static where

import Web.Controller.Prelude
import Web.View.Static.Pages

instance Controller StaticController where
    action AboutAction          = render AboutView
    action HowItWorksAction     = render HowItWorksView
    action CommunityRulesAction = render CommunityRulesView
    action TermsAction          = render TermsView
    action PrivacyPolicyAction  = render PrivacyPolicyView
    action CookiePolicyAction   = render CookiePolicyView
    action LegalNoticeAction    = render LegalNoticeView
