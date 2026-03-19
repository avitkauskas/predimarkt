module Application.Helper.View
    ( module Application.Helper.View
    ) where

import Application.Helper.Text (textParagraphs)
import qualified Data.List as List
import qualified Data.Text as Text
import Generated.Enums
import Generated.Types
import IHP.ViewPrelude
import Network.Wai.Middleware.FlashMessages (FlashMessage (ErrorFlashMessage, SuccessFlashMessage))

isPathPrefix :: (?request :: Request) => Text -> Bool
isPathPrefix prefix =
    let pathSegments = pathInfo theRequest
        fullPath = Text.intercalate "/" pathSegments
        prefixStripped = Text.stripPrefix "/" prefix
        actualPrefix = fromMaybe prefix prefixStripped
    in Text.isPrefixOf actualPrefix fullPath

marketStatusLabel :: MarketStatus -> Text
marketStatusLabel = \case
    MarketStatusDraft -> "draft"
    MarketStatusOpen -> "open"
    MarketStatusClosed -> "closed"
    MarketStatusResolved -> "resolved"
    MarketStatusRefunded -> "refunded"

marketStatusClasses :: MarketStatus -> Text
marketStatusClasses = \case
    MarketStatusClosed   -> "market-status-closed-body"
    MarketStatusResolved -> "market-status-resolved-body"
    MarketStatusRefunded -> "market-status-refunded-body"
    _                    -> ""

marketStatusHeaderClasses :: MarketStatus -> Text
marketStatusHeaderClasses = \case
    MarketStatusClosed   -> "market-status-closed-header"
    MarketStatusResolved -> "market-status-resolved-header"
    MarketStatusRefunded -> "market-status-refunded-header"
    _                    -> ""

marketStatusFooterClasses :: MarketStatus -> Text
marketStatusFooterClasses = \case
    MarketStatusClosed   -> "market-status-closed-footer"
    MarketStatusResolved -> "market-status-resolved-footer"
    MarketStatusRefunded -> "market-status-refunded-footer"
    _                    -> ""

renderTime :: UTCTime -> Html
renderTime time =
    [hsx|
        <span class="local-time" data-time={tshow time}></span>
    |]

renderFlashToasts :: (?request :: Request) => Html
renderFlashToasts =
    case theFlashMessages of
        [] -> [hsx||]
        flashMessages -> [hsx|
            <div id="flash-toast-container"
                 class="toast-container position-fixed top-0 start-50 translate-middle-x mt-5 p-3"
                 aria-atomic="true">
                {forEach flashMessages renderFlashToast}
            </div>
        |]

renderFlashToast :: FlashMessage -> Html
renderFlashToast flashMessage = [hsx|
    <div class={classes ["toast align-items-center", (flashToastClass flashMessage, True)]}
         role={flashToastRole flashMessage}
         aria-live={flashToastLive flashMessage}
         aria-atomic="true"
         data-auto-show-toast="true"
         data-bs-autohide="true"
         data-bs-delay="5000">
        <div class="d-flex">
            <div class="toast-body">
                {flashToastMessage flashMessage}
            </div>
            <button type="button"
                    class="btn-close me-2 m-auto"
                    data-bs-dismiss="toast"
                    aria-label="Close">
            </button>
        </div>
    </div>
|]

flashToastClass :: FlashMessage -> Text
flashToastClass = \case
    SuccessFlashMessage _ ->
        "bg-success-subtle border border-success-subtle text-success-emphasis"
    ErrorFlashMessage _ ->
        "bg-danger-subtle border border-danger-subtle text-danger-emphasis"

flashToastMessage :: FlashMessage -> Text
flashToastMessage = \case
    SuccessFlashMessage message -> message
    ErrorFlashMessage message -> message

flashToastRole :: FlashMessage -> Text
flashToastRole = \case
    SuccessFlashMessage _ -> "status"
    ErrorFlashMessage _ -> "alert"

flashToastLive :: FlashMessage -> Text
flashToastLive = \case
    SuccessFlashMessage _ -> "polite"
    ErrorFlashMessage _ -> "assertive"

renderTextParagraphs :: Text -> Html
renderTextParagraphs text =
    case textParagraphs text of
        [] -> [hsx||]
        paragraphs -> [hsx|
            <div class="d-flex flex-column gap-3">
                {mconcat (map renderParagraph paragraphs)}
            </div>
        |]
  where
    renderParagraph :: Text -> Html
    renderParagraph paragraph = [hsx|
        <p class="mb-0" style="overflow-wrap: anywhere;">
            {renderParagraphLines (Text.lines paragraph)}
        </p>
    |]

    renderParagraphLines :: [Text] -> Html
    renderParagraphLines =
        mconcat
            . List.intersperse [hsx|<br />|]
            . map renderParagraphLine

    renderParagraphLine :: Text -> Html
    renderParagraphLine line = [hsx|{line}|]
