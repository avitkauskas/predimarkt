module Web.Controller.Auth where

import Application.Helper.Passkeys
import qualified Crypto.WebAuthn.Encoding.WebAuthnJson as WebAuthnJson
import Crypto.WebAuthn.Model.Types
import Crypto.WebAuthn.Operation.Authentication
import Crypto.WebAuthn.Operation.CredentialEntry (CredentialEntry (..))
import Crypto.WebAuthn.Operation.Registration
import qualified Data.Aeson as Aeson
import Data.Hourglass (timeConvert)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text as Text
import Data.Time.Clock (getCurrentTime)
import qualified Data.UUID as UUID
import qualified Data.Validation as Validation
import Database.PostgreSQL.Simple.Types (Binary (Binary))
import IHP.ControllerPrelude
import Network.HTTP.Types.Status (Status, status400, status409, status422)
import Web.Controller.Prelude
import Web.View.Sessions.New

instance Controller AuthController where
    action LoginAction = do
        when (isJust (currentUserOrNothing @User)) do
            redirectToPath "/"

        render NewView { .. }

    action BeginPasskeyRegistrationAction = do
        payloadResult <- parseJsonBody @BeginPasskeyRegistrationPayload
        payload <- case payloadResult of
            Left errorMessage -> jsonError status400 errorMessage
            Right payload     -> pure payload

        (pendingUserId, nickname, excludeCredentials) <- case currentUserOrNothing @User of
            Just user -> do
                existingPasskeys <- query @Passkey
                    |> filterWhere (#userId, user.id)
                    |> fetch
                pure (user.id, user.nickname, map passkeyCredentialDescriptor existingPasskeys)
            Nothing -> do
                nickname <- requireAvailableNickname payload.nickname
                userUuid <- sqlQueryScalar "SELECT uuidv7()" ()
                pure (Id userUuid, nickname, [])

        challenge <- liftIO generateChallenge
        setSession registrationChallengeSessionKey (unChallenge challenge)
        setSession registrationNicknameSessionKey nickname
        setSession registrationUserIdSessionKey (inputValue pendingUserId)

        renderJson
            (WebAuthnJson.wjEncodeCredentialOptionsRegistration (registrationCredentialOptions challenge pendingUserId nickname excludeCredentials))

    action FinishPasskeyRegistrationAction = do
        challenge <- sessionChallenge registrationChallengeSessionKey
        pendingUserId <- sessionUserId registrationUserIdSessionKey
        pendingNickname <- sessionText registrationNicknameSessionKey

        credentialResult <- parseJsonBody @WebAuthnJson.WJCredentialRegistration
        credentialPayload <- case credentialResult of
            Left errorMessage -> do
                clearRegistrationSession
                jsonError status400 errorMessage
            Right payload -> pure payload

        credential <- case WebAuthnJson.wjDecodeCredentialRegistration credentialPayload of
            Left errorMessage -> do
                clearRegistrationSession
                jsonError status422 errorMessage
            Right credential -> pure credential

        currentDateTime <- liftIO (timeConvert <$> getCurrentTime)
        maybeExistingUser <- case currentUserOrNothing @User of
            Just user -> do
                accessDeniedUnless (user.id == pendingUserId)
                pure (Just user)
            Nothing -> pure Nothing

        existingPasskeys <- case maybeExistingUser of
            Just user -> query @Passkey
                |> filterWhere (#userId, user.id)
                |> fetch
            Nothing -> pure []

        let verification = verifyRegistrationResponse
                allowedOrigins
                rpIdHashFromRequest
                mempty
                currentDateTime
                (registrationCredentialOptions challenge pendingUserId pendingNickname (map passkeyCredentialDescriptor existingPasskeys))
                credential

        clearRegistrationSession

        registrationResult <- case verification of
            Validation.Failure errors -> jsonError status422 (validationErrors errors)
            Validation.Success registrationResult -> pure registrationResult

        let entry = rrEntry registrationResult
            credentialId = unCredentialId entry.ceCredentialId

        credentialAlreadyExists <- query @Passkey
            |> filterWhere (#credentialId, Binary credentialId)
            |> fetchExists

        when credentialAlreadyExists do
            jsonError status409 "This passkey is already registered."

        case maybeExistingUser of
            Just user -> do
                _ <- createPasskeyRecord user.id entry
                setSuccessMessage "Passkey added successfully"
                renderJson (Aeson.object ["ok" Aeson..= True, "message" Aeson..= ("Passkey added successfully" :: Text)])
            Nothing -> do
                nicknameInUse <- nicknameAlreadyExists pendingNickname
                when nicknameInUse do
                    jsonError status409 "That nickname is no longer available."

                user <- withTransaction do
                    createdUser <- newRecord @User
                        |> set #id pendingUserId
                        |> set #nickname pendingNickname
                        |> createRecord

                    _ <- newRecord @Wallet
                        |> set #userId createdUser.id
                        |> createRecord

                    _ <- createPasskeyRecord createdUser.id entry

                    pure createdUser

                login user
                setSuccessMessage "Account created successfully"
                renderJson (Aeson.object ["ok" Aeson..= True, "redirectTo" Aeson..= ("/" :: Text)])

    action BeginPasskeyAuthenticationAction = do
        challenge <- liftIO generateChallenge
        setSession authenticationChallengeSessionKey (unChallenge challenge)
        renderJson
            (WebAuthnJson.wjEncodeCredentialOptionsAuthentication (authenticationCredentialOptions challenge))

    action FinishPasskeyAuthenticationAction = do
        challenge <- sessionChallenge authenticationChallengeSessionKey

        credentialResult <- parseJsonBody @WebAuthnJson.WJCredentialAuthentication
        credentialPayload <- case credentialResult of
            Left errorMessage -> do
                clearAuthenticationSession
                jsonError status400 errorMessage
            Right payload -> pure payload

        credential <- case WebAuthnJson.wjDecodeCredentialAuthentication credentialPayload of
            Left errorMessage -> do
                clearAuthenticationSession
                jsonError status422 errorMessage
            Right credential -> pure credential

        clearAuthenticationSession

        let CredentialId credentialId = cIdentifier credential
        passkey <- query @Passkey
            |> filterWhere (#credentialId, Binary credentialId)
            |> fetchOneOrNothing
            >>= maybe (jsonError status422 "No account matched that passkey.") pure

        user <- fetch passkey.userId

        let verification = verifyAuthenticationResponse
                allowedOrigins
                rpIdHashFromRequest
                (Just (userHandleForUserId user.id))
                (credentialEntryForPasskey passkey)
                (authenticationCredentialOptions challenge)
                credential

        authenticationResult <- case verification of
            Validation.Failure errors -> jsonError status422 (validationErrors errors)
            Validation.Success result -> pure result

        case arSignatureCounterResult authenticationResult of
            SignatureCounterPotentiallyCloned ->
                jsonError status422 "This passkey could not be verified safely. Please use a different passkey."
            SignatureCounterUpdated newSignCount -> do
                _ <- passkey
                    |> set #signCount (fromIntegral (unSignatureCounter newSignCount))
                    |> updateRecord
                pure ()
            SignatureCounterZero -> pure ()

        login user
        currentDateTime <- liftIO (timeConvert <$> getCurrentTime)
        _ <- user
            |> set #loggedInAt currentDateTime
            |> updateRecord
        _ <- passkey
            |> set #lastUsedAt currentDateTime
            |> updateRecord
        setSuccessMessage "Logged in successfully"
        renderJson (Aeson.object ["ok" Aeson..= True, "redirectTo" Aeson..= ("/" :: Text)])

registrationChallengeSessionKey :: ByteString
registrationChallengeSessionKey = "passkey-registration-challenge"

registrationNicknameSessionKey :: ByteString
registrationNicknameSessionKey = "passkey-registration-nickname"

registrationUserIdSessionKey :: ByteString
registrationUserIdSessionKey = "passkey-registration-user-id"

authenticationChallengeSessionKey :: ByteString
authenticationChallengeSessionKey = "passkey-authentication-challenge"

data BeginPasskeyRegistrationPayload = BeginPasskeyRegistrationPayload
    { nickname :: Maybe Text
    }

instance Aeson.FromJSON BeginPasskeyRegistrationPayload where
    parseJSON = Aeson.withObject "BeginPasskeyRegistrationPayload" $ \object -> do
        nickname <- object Aeson..:? "nickname"
        pure BeginPasskeyRegistrationPayload { .. }

parseJsonBody :: (?request :: Request, Aeson.FromJSON payload) => IO (Either Text payload)
parseJsonBody = do
    jsonValue <- requestBodyJSON
    pure $ case Aeson.fromJSON jsonValue of
        Aeson.Error errorMessage -> Left (cs errorMessage)
        Aeson.Success payload    -> Right payload

requireAvailableNickname :: (?request :: Request, ?modelContext :: ModelContext) => Maybe Text -> IO Text
requireAvailableNickname maybeNickname = do
    nickname <- case Text.strip <$> maybeNickname of
        Just nickname | not (Text.null nickname) -> pure nickname
        _ -> jsonError status422 "Please choose a nickname."

    nicknameInUse <- nicknameAlreadyExists nickname
    when nicknameInUse do
        jsonError status422 "That nickname is already taken."

    pure nickname

nicknameAlreadyExists :: (?modelContext :: ModelContext) => Text -> IO Bool
nicknameAlreadyExists nickname = do
    checkedUser <- newRecord @User
        |> set #nickname nickname
        |> validateIsUniqueCaseInsensitive #nickname
    pure (isJust (getValidationFailure #nickname checkedUser))

sessionChallenge :: (?request :: Request) => ByteString -> IO Challenge
sessionChallenge sessionKey = do
    maybeChallenge <- getSession @ByteString sessionKey
    case maybeChallenge of
        Just challenge -> pure (Challenge challenge)
        Nothing -> jsonError status422 "This passkey request has expired. Please try again."

sessionText :: (?request :: Request) => ByteString -> IO Text
sessionText sessionKey = do
    maybeValue <- getSession @Text sessionKey
    case maybeValue of
        Just value -> pure value
        Nothing -> jsonError status422 "This passkey request has expired. Please try again."

sessionUserId :: (?request :: Request) => ByteString -> IO (Id User)
sessionUserId sessionKey = do
    userIdText <- sessionText sessionKey
    case UUID.fromText userIdText of
        Just userId -> pure (Id userId)
        Nothing -> jsonError status422 "The pending passkey registration is invalid. Please try again."

createPasskeyRecord :: (?modelContext :: ModelContext) => Id User -> CredentialEntry -> IO Passkey
createPasskeyRecord userId entry =
    newRecord @Passkey
        |> set #userId userId
        |> set #credentialId (Binary (unCredentialId entry.ceCredentialId))
        |> set #publicKey (Binary (unPublicKeyBytes entry.cePublicKeyBytes))
        |> set #signCount (fromIntegral (unSignatureCounter entry.ceSignCounter))
        |> createRecord

clearRegistrationSession :: (?request :: Request) => IO ()
clearRegistrationSession = do
    deleteSession registrationChallengeSessionKey
    deleteSession registrationNicknameSessionKey
    deleteSession registrationUserIdSessionKey

clearAuthenticationSession :: (?request :: Request) => IO ()
clearAuthenticationSession =
    deleteSession authenticationChallengeSessionKey

jsonError :: (?request :: Request) => Status -> Text -> IO a
jsonError statusCode errorMessage =
    renderJsonWithStatusCode statusCode (Aeson.object ["error" Aeson..= errorMessage])
        >> error "jsonError should have exited the controller"

validationErrors :: Show error => NonEmpty.NonEmpty error -> Text
validationErrors errors = Text.intercalate "; " (map (cs . show) (NonEmpty.toList errors))
