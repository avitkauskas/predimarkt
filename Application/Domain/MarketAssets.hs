module Application.Domain.MarketAssets where

import Data.List (sortBy)
import Generated.Types
import IHP.Prelude

sortAssetsForDisplay :: [Asset] -> [Asset]
sortAssetsForDisplay [] = []
sortAssetsForDisplay assets =
    case assets of
        [a, b]
            | a.name == "Yes" && b.name == "No" -> assets
            | a.name == "No" && b.name == "Yes" -> [b, a]
        _ -> sortBy (\a b -> compare (b.quantity) (a.quantity)) assets
