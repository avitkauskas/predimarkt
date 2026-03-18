module Web.Controller.Static where

import Web.Controller.Prelude
import Web.View.Layout (withFooterLayout)
import Web.View.Static.Pages

instance Controller StaticController where
    action AboutAction = do
        setLayout withFooterLayout
        render AboutView
    action HowItWorksAction = do
        setLayout withFooterLayout
        render HowItWorksView
    action CommunityRulesAction = do
        setLayout withFooterLayout
        render CommunityRulesView
    action TermsAction = do
        setLayout withFooterLayout
        render TermsView
    action PrivacyPolicyAction = do
        setLayout withFooterLayout
        render PrivacyPolicyView
    action CookiePolicyAction = do
        setLayout withFooterLayout
        render CookiePolicyView
    action ModerationPolicyAction = do
        setLayout withFooterLayout
        render ModerationPolicyView
    action LegalNoticeAction = do
        setLayout withFooterLayout
        render LegalNoticeView
