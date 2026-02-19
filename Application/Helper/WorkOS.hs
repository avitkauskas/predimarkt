module Application.Helper.WorkOS where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import IHP.Prelude
import qualified Network.HTTP.Simple as Http
import System.Environment (lookupEnv)
import System.IO.Unsafe (unsafePerformIO)

getEnvVar :: String -> Text
getEnvVar key = cs $ fromMaybe "" $ unsafePerformIO (lookupEnv key)

workOSClientId :: Text
workOSClientId = getEnvVar "WORKOS_CLIENT_ID"

workOSApiKey :: Text
workOSApiKey = getEnvVar "WORKOS_API_KEY"

workOSAuthUrl :: Text -> Text
workOSAuthUrl clientId = "https://api.workos.com/user_management/authorize?" <> Text.intercalate "&"
    [ "client_id=" <> clientId
    , "redirect_uri=http://localhost:8000/auth/callback"
    , "response_type=code"
    , "provider=authkit"
    , "scope=openid+profile+email"
    ]

data WorkOSUser = WorkOSUser
    { workosId             :: Text
    , workosEmail          :: Text
    , workosFirstName      :: Maybe Text
    , workosLastName       :: Maybe Text
    , workosProfilePicture :: Maybe Text
    } deriving (Show)

data WorkOSAuthResponse = WorkOSAuthResponse
    { user         :: WorkOSUser
    , accessToken  :: Text
    , refreshToken :: Text
    } deriving (Show)

authenticateUser :: Text -> Text -> Text -> IO WorkOSAuthResponse
authenticateUser code ipAddress userAgent = do
    initReq <- Http.parseRequest "https://api.workos.com/user_management/authenticate"
    let body = Aeson.object
            [ "client_id" Aeson..= workOSClientId
            , "client_secret" Aeson..= workOSApiKey
            , "grant_type" Aeson..= ("authorization_code" :: Text)
            , "code" Aeson..= code
            , "ip_address" Aeson..= ipAddress
            , "user_agent" Aeson..= userAgent
            ]
        req = Http.setRequestMethod "POST" $
            Http.setRequestHeaders
                [ ("Content-Type", "application/json")
                , ("Accept", "application/json")
                ]
            $ Http.setRequestBodyLBS (Aeson.encode body)
            initReq
    response <- Http.httpLbs req
    let respBody = Http.getResponseBody response
    let respText = Text.decodeUtf8 (LazyByteString.toStrict respBody)
    case Aeson.eitherDecode respBody of
        Left err -> error $ "Failed to authenticate: " <> cs err <> ". Response: " <> respText
        Right authResponse -> pure authResponse

instance Aeson.FromJSON WorkOSUser where
    parseJSON = Aeson.withObject "WorkOSUser" $ \obj -> do
        workosId <- obj Aeson..: "id"
        workosEmail <- obj Aeson..: "email"
        workosFirstName <- obj Aeson..:? "first_name"
        workosLastName <- obj Aeson..:? "last_name"
        workosProfilePicture <- obj Aeson..:? "profile_picture_url"
        pure WorkOSUser { .. }

instance Aeson.FromJSON WorkOSAuthResponse where
    parseJSON = Aeson.withObject "WorkOSAuthResponse" $ \obj -> do
        userObj <- obj Aeson..: "user"
        workosId <- userObj Aeson..: "id"
        workosEmail <- userObj Aeson..: "email"
        workosFirstName <- userObj Aeson..:? "first_name"
        workosLastName <- userObj Aeson..:? "last_name"
        workosProfilePicture <- userObj Aeson..:? "profile_picture_url"
        let user = WorkOSUser { .. }
        accessToken <- obj Aeson..: "access_token"
        refreshToken <- obj Aeson..: "refresh_token"
        pure WorkOSAuthResponse { .. }
