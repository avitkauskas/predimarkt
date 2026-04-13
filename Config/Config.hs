module Config where

import IHP.Environment
import IHP.FrameworkConfig
import IHP.LoginSupport.Middleware
import IHP.Prelude
import Generated.Types
import Web.Types

config :: ConfigBuilder
config = do
    let userAuth = IHP.LoginSupport.Middleware.authMiddleware @User
    option $ AuthMiddleware userAuth
