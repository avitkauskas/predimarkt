module Web.Controller.Sessions where

import qualified IHP.AuthSupport.Controller.Sessions as Sessions
import Web.Controller.Prelude

instance Controller SessionsController where
    action DeleteSessionAction = do
        case currentUserOrNothing of
            Just user -> do
                currentDateTime <- liftIO getCurrentTime
                user
                    |> set #loggedOutAt currentDateTime
                    |> updateRecord
                logout user
            Nothing   -> pure ()
        setSuccessMessage "Logged out successfully"
        redirectToPath "/"

instance Sessions.SessionsControllerConfig User
