module Config where

import Generated.Types
import IHP.Environment
import IHP.FrameworkConfig
import IHP.LoginSupport.Middleware
import IHP.Prelude
import Web.Types

config :: ConfigBuilder
config = do
    option $ AuthMiddleware (authMiddleware @User)
