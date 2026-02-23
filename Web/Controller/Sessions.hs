module Web.Controller.Sessions where

import qualified IHP.AuthSupport.Controller.Sessions as Sessions
import Web.Controller.Prelude

instance Controller SessionsController where
    action DeleteSessionAction = do
        case currentUserOrNothing of
            Just user -> logout user
            Nothing   -> pure ()
        setSuccessMessage "Logged out successfully"
        redirectToPath "/"

instance Sessions.SessionsControllerConfig User
