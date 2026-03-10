module Application.Helper.View
    ( module Application.Helper.View
    ) where

import Data.List (intercalate)
import qualified Data.List as List
import Data.Text (pack, unpack)
import qualified Data.Text as Text
import Generated.Enums
import Generated.Types
import IHP.ViewPrelude
import Network.Wai.Middleware.FlashMessages (FlashMessage (ErrorFlashMessage, SuccessFlashMessage))
import Text.Printf (printf)

-- Market Status Helpers

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

-- | Time formatting helper
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

textParagraphs :: Text -> [Text]
textParagraphs =
    go [] []
        . Text.lines
        . Text.replace "\r" "\n"
        . Text.replace "\r\n" "\n"
  where
    go :: [Text] -> [Text] -> [Text] -> [Text]
    go paragraphs currentParagraph remainingLines =
        case remainingLines of
            [] -> reverse (finishParagraph paragraphs currentParagraph)
            line : rest
                | Text.strip line == "" ->
                    go (finishParagraph paragraphs currentParagraph) [] rest
                | otherwise ->
                    go paragraphs (currentParagraph <> [line]) rest

    finishParagraph :: [Text] -> [Text] -> [Text]
    finishParagraph paragraphs [] = paragraphs
    finishParagraph paragraphs currentParagraph =
        Text.strip (Text.intercalate "\n" currentParagraph) : paragraphs

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

-- Price Formatting Helpers

-- | Format price as percentage with 2 decimals (e.g., "23.45%")
formatPricePercent :: Double -> Text
formatPricePercent price =
    pack $ printf "%.1f%%" (price * 100)

-- | Format price as decimal with 4 decimals (e.g., "0.2345")
formatPriceDecimal :: Double -> Text
formatPriceDecimal price =
    pack $ printf "%.4f" price

-- | Format price as rounded percentage (e.g., "23%")
formatPriceRounded :: Double -> Text
formatPriceRounded price =
    pack $ printf "%d%%" (round (price * 100) :: Int)

-- Money Formatting Helpers

formatMoney :: Integral a => a -> Text
formatMoney cents =
    let absCents = abs (fromIntegral cents) :: Integer
        euros = fromIntegral absCents / 100 :: Double
        formatted = printf "%.2f" euros
        (intPart, decPart) = break (== '.') formatted
        intWithSeps = reverse . Data.List.intercalate "'" . chunksOf3 . reverse $ intPart
        sign = if cents < 0 then "-" else ""
        -- The ⲙﾱﾓ𐌼𐓀ƤṖⱣႴᵩ are possible placeholders for the currency symbol
    in sign <> pack (intWithSeps ++ decPart)
  where
    chunksOf3 [] = []
    chunksOf3 xs = take 3 xs : chunksOf3 (drop 3 xs)

-- | Format cents as signed money (e.g., "+10.23" or "-5.00")
formatMoneySigned :: Integral a => a -> Text
formatMoneySigned cents
    | cents > 0 = "+" <> formatMoney cents
    | otherwise = formatMoney cents

-- | Format money for closed positions (shows "--" for zero values)
formatMoneyOrDash :: Integral a => a -> Text
formatMoneyOrDash cents
    | cents == 0 = "--"
    | otherwise = formatMoney cents

-- | Format integer with thousand separators (e.g., "1'234'567")
formatWithSep :: Integral a => a -> Text
formatWithSep n =
    let str = unpack (show (abs (toInteger n)))
        withSeps = reverse . Data.List.intercalate "'" . chunksOf3 . reverse $ str
        signed = if n < 0 then "-" <> pack withSeps else pack withSeps
    in signed
  where
    chunksOf3 [] = []
    chunksOf3 xs = take 3 xs : chunksOf3 (drop 3 xs)

-- Pagination Helpers

-- | Data type representing a pagination link item
data PaginationItem
    = PageNumber Int
    | Ellipsis Int
    deriving (Eq, Show)

-- | Generate the list of pagination items for smart pagination
-- Shows: [1,2,...,window,...,last-1,last]
-- When on early pages: [1..8], ellipsis, [last-1,last]
-- When on late pages: [1,2], ellipsis, [last-7..last]
-- When in middle: [1,2], ellipsis, window around current, ellipsis, [last-1,last]
-- Uses 2 pages for initial/final blocks and 2 pages on each side of current
generatePaginationItems :: Int -> Int -> [PaginationItem]
generatePaginationItems currentPage totalPages
    | totalPages <= 11 = map PageNumber [1..totalPages]
    | currentPage < 7 =
        let firstEllipsisPage = (8 + totalPages - 1) `div` 2
        in map PageNumber [1..8] ++ [Ellipsis firstEllipsisPage] ++ map PageNumber [totalPages - 1, totalPages]
    | currentPage > totalPages - 6 =
        let lastEllipsisPage = (2 + totalPages - 7) `div` 2
        in map PageNumber [1, 2] ++ [Ellipsis lastEllipsisPage] ++ map PageNumber [totalPages - 7..totalPages]
    | otherwise =
        let firstPages = [1, 2]
            lastPages = [totalPages - 1, totalPages]
            windowStart = max 3 (currentPage - 2)
            windowEnd = min (totalPages - 2) (currentPage + 2)
            middlePages = [windowStart..windowEnd]
            firstEllipsisMid = (2 + windowStart) `div` 2
            lastEllipsisMid = (windowEnd + totalPages - 1) `div` 2
        in concat [
            map PageNumber firstPages,
            [Ellipsis firstEllipsisMid],
            map PageNumber middlePages,
            [Ellipsis lastEllipsisMid],
            map PageNumber lastPages
           ]

-- | Render a full pagination component with prev/next buttons
-- Takes the current page, total pages, aria label, and a function to generate page URLs
renderSmartPagination
    :: (?context :: ControllerContext)
    => Int
    -> Int
    -> Text
    -> (Int -> Text)
    -> Html
renderSmartPagination currentPage totalPages ariaLabel pageUrlFn =
    if totalPages <= 1
    then [hsx||]
    else [hsx|
        <nav aria-label={ariaLabel} class="mt-3">
            <ul class="pagination pagination-sm justify-content-center mb-0"
                style="--bs-pagination-bg: transparent; --bs-pagination-focus-box-shadow: none;">
                {renderPrev}
                {renderPages}
                {renderNext}
            </ul>
        </nav>
    |]
    where
        renderPrev = if currentPage <= 1
            then [hsx|
                    <li class="page-item disabled prevent-select">
                        <span class="page-link">←</span>
                    </li>
            |]
            else [hsx|
                    <li class="page-item">
                        <a class="page-link prevent-select" style="box-shadow: none;"
                           href={pageUrlFn (currentPage - 1)}>
                           ←
                        </a>
                    </li>
            |]
        renderNext = if currentPage >= totalPages
            then [hsx|
                    <li class="page-item disabled prevent-select">
                        <span class="page-link">→</span>
                    </li>
            |]
            else [hsx|
                    <li class="page-item">
                        <a class="page-link prevent-select" style="box-shadow: none;"
                           href={pageUrlFn (currentPage + 1)}>
                           →
                        </a>
                    </li>
            |]
        renderPages = mconcat $ map renderPageLink (generatePaginationItems currentPage totalPages)

        renderPageLink :: PaginationItem -> Html
        renderPageLink (Ellipsis midPage) = [hsx|
                <li class="page-item">
                    <a class="page-link prevent-select" style="box-shadow: none;"
                       href={pageUrlFn midPage}>
                        …
                    </a>
                </li>
        |]
        renderPageLink (PageNumber n) =
            if n == currentPage
            then [hsx|
                    <li class="page-item active prevent-select">
                        <span class="page-link">{show n}</span>
                    </li>
            |]
            else [hsx|
                    <li class="page-item">
                        <a class="page-link prevent-select" style="box-shadow: none;"
                           href={pageUrlFn n}>
                            {show n}
                        </a>
                    </li>
            |]
