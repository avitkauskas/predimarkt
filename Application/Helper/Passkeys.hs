{-# LANGUAGE PackageImports #-}

module Application.Helper.Passkeys
    ( allowedOrigins
    , authenticationCredentialOptions
    , credentialEntryForPasskey
    , passkeyCredentialDescriptor
    , passkeyRelyingPartyName
    , registrationCredentialOptions
    , rpIdTextFromHost
    , rpIdTextFromRequest
    , rpIdHashFromRequest
    , userHandleForUserId
    ) where

import qualified "crypton" Crypto.Hash as Hash
import Crypto.WebAuthn.Cose.SignAlg
import Crypto.WebAuthn.Model.Kinds (CeremonyKind (Authentication, Registration))
import Crypto.WebAuthn.Model.Types
import Crypto.WebAuthn.Operation.CredentialEntry
import qualified Data.ByteString as ByteString
import qualified Data.CaseInsensitive as CI
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Database.PostgreSQL.Simple.Types (Binary (Binary))
import Generated.Types
import IHP.InputValue (inputValue)
import IHP.Prelude
import Network.Wai (Request, isSecure, requestHeaderHost, requestHeaders)

passkeyRelyingPartyName :: RelyingPartyName
passkeyRelyingPartyName = RelyingPartyName "Predimarkt"

rpIdTextFromHost :: Text -> Text
rpIdTextFromHost = Text.takeWhile (\char -> char /= ':' && char /= '/')

userHandleForUserId :: Id User -> UserHandle
userHandleForUserId userId = UserHandle (cs (inputValue userId))

rpIdTextFromRequest :: (?request :: Request) => Text
rpIdTextFromRequest = rpIdTextFromHost requestHostText

rpIdHashFromRequest :: (?request :: Request) => RpIdHash
rpIdHashFromRequest = RpIdHash (Hash.hash (Text.encodeUtf8 rpIdTextFromRequest))

allowedOrigins :: (?request :: Request) => NonEmpty Origin
allowedOrigins = Origin (requestSchemeText <> "://" <> requestHostText) :| []

registrationCredentialOptions ::
    (?request :: Request) =>
    Challenge ->
    Id User ->
    Text ->
    [CredentialDescriptor] ->
    CredentialOptions 'Registration
registrationCredentialOptions challenge userId nickname excludeCredentials =
    CredentialOptionsRegistration
        { corRp = CredentialRpEntity
            { creId = Just (RpId rpIdTextFromRequest)
            , creName = passkeyRelyingPartyName
            }
        , corUser = CredentialUserEntity
            { cueId = userHandleForUserId userId
            , cueDisplayName = UserAccountDisplayName nickname
            , cueName = UserAccountName nickname
            }
        , corChallenge = challenge
        , corPubKeyCredParams =
            [ CredentialParameters CredentialTypePublicKey (CoseSignAlgECDSA CoseHashAlgECDSASHA256)
            , CredentialParameters CredentialTypePublicKey CoseSignAlgEdDSA
            , CredentialParameters CredentialTypePublicKey (CoseSignAlgRSA CoseHashAlgRSASHA256)
            ]
        , corTimeout = Just (Timeout 60000)
        , corExcludeCredentials = excludeCredentials
        , corAuthenticatorSelection = Just AuthenticatorSelectionCriteria
            { ascAuthenticatorAttachment = Nothing
            , ascResidentKey = ResidentKeyRequirementPreferred
            , ascUserVerification = UserVerificationRequirementPreferred
            }
        , corAttestation = AttestationConveyancePreferenceNone
        , corExtensions = Nothing
        }

authenticationCredentialOptions ::
    (?request :: Request) =>
    Challenge ->
    CredentialOptions 'Authentication
authenticationCredentialOptions challenge =
    CredentialOptionsAuthentication
        { coaChallenge = challenge
        , coaTimeout = Just (Timeout 60000)
        , coaRpId = Just (RpId rpIdTextFromRequest)
        , coaAllowCredentials = []
        , coaUserVerification = UserVerificationRequirementPreferred
        , coaExtensions = Nothing
        }

passkeyCredentialDescriptor :: Passkey -> CredentialDescriptor
passkeyCredentialDescriptor Passkey { credentialId = Binary credentialId } =
    CredentialDescriptor
        { cdTyp = CredentialTypePublicKey
        , cdId = CredentialId credentialId
        , cdTransports = Nothing
        }

credentialEntryForPasskey :: Passkey -> CredentialEntry
credentialEntryForPasskey Passkey
    { userId
    , credentialId = Binary credentialId
    , publicKey = Binary publicKey
    , signCount
    } = CredentialEntry
        { ceCredentialId = CredentialId credentialId
        , ceUserHandle = userHandleForUserId userId
        , cePublicKeyBytes = PublicKeyBytes publicKey
        , ceSignCounter = fromIntegral signCount
        , ceTransports = []
        }

requestSchemeText :: (?request :: Request) => Text
requestSchemeText =
    fromMaybe fallbackScheme (headerValue "x-forwarded-proto")
    where
        fallbackScheme = if isSecure ?request then "https" else "http"

requestHostText :: (?request :: Request) => Text
requestHostText =
    fromMaybe fallbackHost (headerValue "x-forwarded-host" <|> headerHost)
    where
        fallbackHost = "localhost:8000"
        headerHost = cs <$> requestHeaderHost ?request

headerValue :: (?request :: Request) => ByteString.ByteString -> Maybe Text
headerValue headerName =
    cs . snd <$> find matchesHeader (requestHeaders ?request)
    where
        matchesHeader (name, _) = CI.foldedCase name == headerName
