module Admin.Types where

import Generated.Types
import IHP.LoginSupport.Types
import IHP.ModelSupport
import IHP.Prelude

data AdminApplication = AdminApplication
    deriving (Eq, Show)

data StaticController = WelcomeAction
    deriving (Eq, Show, Data)

data SessionsController
    = NewSessionAction
    | CreateSessionAction
    | DeleteSessionAction
    deriving (Eq, Show, Data)

instance HasNewSessionUrl Admin where
    newSessionUrl _ = "/admin/NewSession"

type instance CurrentAdminRecord = Admin

data AdminsController
    = EditAdminAction {adminId :: !(Id Admin)}
    | UpdateAdminAction {adminId :: !(Id Admin)}
    | DeleteAdminAction {adminId :: !(Id Admin)}
    deriving (Eq, Show, Data)
