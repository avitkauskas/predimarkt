module Application.Helper.Pagination where

import IHP.ViewPrelude

data PaginationItem
    = PageNumber Int
    | Ellipsis Int
    deriving (Eq, Show)

generatePaginationItems :: Int -> Int -> [PaginationItem]
generatePaginationItems currentPage totalPages
    | totalPages < 1 = []
    | currentPage < 1 || currentPage > totalPages = []
    | totalPages <= 11 = map PageNumber [1 .. totalPages]
    | currentPage < 7 =
        let firstEllipsisPage = (8 + totalPages - 1) `div` 2
        in map PageNumber [1 .. 8]
            ++ [Ellipsis firstEllipsisPage]
            ++ map PageNumber [totalPages - 1, totalPages]
    | currentPage > totalPages - 6 =
        let lastEllipsisPage = (2 + totalPages - 7) `div` 2
        in map PageNumber [1, 2]
            ++ [Ellipsis lastEllipsisPage]
            ++ map PageNumber [totalPages - 7 .. totalPages]
    | otherwise =
        let firstPages = [1, 2]
            lastPages = [totalPages - 1, totalPages]
            windowStart = max 3 (currentPage - 2)
            windowEnd = min (totalPages - 2) (currentPage + 2)
            middlePages = [windowStart .. windowEnd]
            firstEllipsisMid = (2 + windowStart) `div` 2
            lastEllipsisMid = (windowEnd + totalPages - 1) `div` 2
        in concat
            [ map PageNumber firstPages
            , [Ellipsis firstEllipsisMid]
            , map PageNumber middlePages
            , [Ellipsis lastEllipsisMid]
            , map PageNumber lastPages
            ]

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
    renderPrev =
        if currentPage <= 1
        then [hsx|
            <li class="page-item disabled">
                <span class="page-link">←</span>
            </li>
        |]
        else [hsx|
            <li class="page-item">
                <a class="page-link" href={pageUrlFn (currentPage - 1)}>←</a>
            </li>
        |]

    renderNext =
        if currentPage >= totalPages
        then [hsx|
            <li class="page-item disabled">
                <span class="page-link">→</span>
            </li>
        |]
        else [hsx|
            <li class="page-item">
                <a class="page-link" href={pageUrlFn (currentPage + 1)}>→</a>
            </li>
        |]

    renderPages = mconcat $ map renderPageLink
        (generatePaginationItems currentPage totalPages)

    renderPageLink = \case
        PageNumber pageNum
            | pageNum == currentPage -> [hsx|
                <li class="page-item active" aria-current="page">
                    <span class="page-link">{pageNum}</span>
                </li>
            |]
            | otherwise -> [hsx|
                <li class="page-item">
                    <a class="page-link" href={pageUrlFn pageNum}>{pageNum}</a>
                </li>
            |]
        Ellipsis targetPage -> [hsx|
            <li class="page-item">
                <a class="page-link" href={pageUrlFn targetPage} aria-label={"Jump to page " <> tshow targetPage}>
                    ...
                </a>
            </li>
        |]
