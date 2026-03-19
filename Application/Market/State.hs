module Application.Market.State
    ( buildMarketState
    , parseMarketState
    ) where

import Application.Domain.Types
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as AesonTypes
import qualified Data.Map.Strict as M
import qualified Data.UUID as UUID
import Generated.Types
import IHP.Prelude
import Unsafe.Coerce (unsafeCoerce)

buildMarketState :: M.Map (Id Asset) Quantity -> [(Text, Int)]
buildMarketState qtyMap =
    [ (cs (show assetId), fromIntegral qty)
    | (assetId, Quantity qty) <- M.toList qtyMap
    ]

parseMarketState :: Aeson.Value -> Maybe (M.Map (Id Asset) Quantity)
parseMarketState value = case Aeson.fromJSON value of
    AesonTypes.Success (obj :: [(Text, Int)]) -> Just $ M.fromList
        [ (unsafeCoerce uid, Quantity (fromIntegral qty))
        | (key, qty) <- obj
        , Just uid <- [UUID.fromText key]
        ]
    AesonTypes.Error _ -> Nothing
