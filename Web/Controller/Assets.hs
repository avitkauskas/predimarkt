module Web.Controller.Assets where

import Web.Controller.Prelude
import Web.View.Assets.New

instance Controller AssetsController where
    action NewAssetAction = do
        let asset = newRecord @Asset
        respondHtml $ renderAssetRow asset 300
