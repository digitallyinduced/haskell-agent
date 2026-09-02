{-# LANGUAGE ScopedTypeVariables #-}

-- | Stateless OAuth protocol support for Gmail and Microsoft mailboxes.
--
-- Callers own OAuth state, PKCE verifier storage, credentials, and account
-- persistence.  Provider responses and exceptions are never included in
-- returned errors.
module Agent.Mail.OAuth
    ( MailOAuthClient(..)
    , MailOAuthToken(..)
    , mailOAuthAuthorizationUrl
    , mailOAuthPkceChallenge
    , mailOAuthScopes
    , exchangeMailOAuthCode
    , refreshMailOAuthToken
    , resolveMailOAuthMailbox
    ) where

import Agent.Mail.Types (MailProvider(..))
import Control.Applicative ((<|>))
import Control.Exception.Safe (tryAny)
import Crypto.Hash (Digest, SHA256, hash)
import Data.Aeson ((.:), (.:?))
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (parseMaybe)
import qualified Data.ByteArray as ByteArray
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64.URL as Base64URL
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import Network.HTTP.Client
    ( BodyReader
    , Manager
    , RequestBody(..)
    , brRead
    , checkResponse
    , method
    , parseRequest
    , redirectCount
    , requestBody
    , requestHeaders
    , responseBody
    , responseStatus
    , responseTimeout
    , responseTimeoutMicro
    , withResponse
    )
import Network.HTTP.Types
    ( Status
    , hAuthorization
    , hContentType
    , statusCode
    , statusIsSuccessful
    )
import Network.HTTP.Types.URI (renderSimpleQuery)

data MailOAuthClient = MailOAuthClient
    { mailOAuthClientProvider :: !MailProvider
    , mailOAuthClientId :: !Text
    , mailOAuthClientSecret :: !(Maybe Text)
    , mailOAuthClientRedirectUri :: !Text
    }
    deriving (Eq)

instance Show MailOAuthClient where
    show client =
        "MailOAuthClient { mailOAuthClientProvider = "
            <> show client.mailOAuthClientProvider
            <> ", mailOAuthClientId = "
            <> show client.mailOAuthClientId
            <> ", mailOAuthClientSecret = <redacted>"
            <> ", mailOAuthClientRedirectUri = "
            <> show client.mailOAuthClientRedirectUri
            <> " }"

data MailOAuthToken = MailOAuthToken
    { mailOAuthTokenAccessToken :: !Text
    , mailOAuthTokenRefreshToken :: !(Maybe Text)
    , mailOAuthTokenExpiresIn :: !Int
    , mailOAuthTokenScopes :: ![Text]
    }
    deriving (Eq)

instance Show MailOAuthToken where
    show token =
        "MailOAuthToken { mailOAuthTokenAccessToken = <redacted>"
            <> ", mailOAuthTokenRefreshToken = <redacted>"
            <> ", mailOAuthTokenExpiresIn = "
            <> show token.mailOAuthTokenExpiresIn
            <> ", mailOAuthTokenScopes = "
            <> show token.mailOAuthTokenScopes
            <> " }"

data OAuthTokenResponse = OAuthTokenResponse
    { tokenAccessToken :: !Text
    , tokenRefreshToken :: !Text
    , tokenExpiresIn :: !Int
    , tokenScope :: !Text
    }

instance Aeson.FromJSON OAuthTokenResponse where
    parseJSON = Aeson.withObject "Mail OAuth token" \value ->
        OAuthTokenResponse
            <$> value .: "access_token"
            <*> value .:? "refresh_token" Aeson..!= ""
            <*> value .:? "expires_in" Aeson..!= 3600
            <*> value .:? "scope" Aeson..!= ""

mailOAuthAuthorizationUrl
    :: MailOAuthClient
    -> Text
    -> Text
    -> Either Text Text
mailOAuthAuthorizationUrl client state verifier
    | client.mailOAuthClientProvider == ImapProvider =
        Left "Custom IMAP accounts do not use OAuth."
    | Text.null (Text.strip client.mailOAuthClientId)
        || Text.null (Text.strip client.mailOAuthClientRedirectUri)
        || Text.null (Text.strip state)
        || Text.null (Text.strip verifier) =
            Left "Mail OAuth configuration is incomplete."
    | otherwise =
        Right $
            authorizationEndpoint client.mailOAuthClientProvider
                <> "?"
                <> TextEncoding.decodeUtf8
                    ( renderSimpleQuery
                        False
                        [ ("response_type", "code")
                        , ( "client_id"
                          , TextEncoding.encodeUtf8
                                client.mailOAuthClientId
                          )
                        , ( "redirect_uri"
                          , TextEncoding.encodeUtf8
                                client.mailOAuthClientRedirectUri
                          )
                        , ( "scope"
                          , TextEncoding.encodeUtf8
                                (mailOAuthScopes
                                    client.mailOAuthClientProvider)
                          )
                        , ("state", TextEncoding.encodeUtf8 state)
                        , ( "code_challenge"
                          , TextEncoding.encodeUtf8
                                (mailOAuthPkceChallenge verifier)
                          )
                        , ("code_challenge_method", "S256")
                        ]
                    )
                <> providerExtras client.mailOAuthClientProvider

mailOAuthPkceChallenge :: Text -> Text
mailOAuthPkceChallenge verifier =
    TextEncoding.decodeUtf8
        ( Base64URL.encodeUnpadded
            ( ByteArray.convert
                ( hash (TextEncoding.encodeUtf8 verifier)
                    :: Digest SHA256
                )
            )
        )

mailOAuthScopes :: MailProvider -> Text
mailOAuthScopes = \case
    GmailProvider ->
        "openid email https://www.googleapis.com/auth/gmail.readonly "
            <> "https://www.googleapis.com/auth/gmail.compose"
    MicrosoftProvider ->
        "openid profile offline_access User.Read Mail.ReadWrite"
    ImapProvider -> ""

exchangeMailOAuthCode
    :: Manager
    -> MailOAuthClient
    -> Text
    -> Text
    -> IO (Either Text MailOAuthToken)
exchangeMailOAuthCode manager client code verifier =
    exchangeToken manager client $
        [ ("grant_type", "authorization_code")
        , ("code", code)
        , ("code_verifier", verifier)
        , ("redirect_uri", client.mailOAuthClientRedirectUri)
        ]

refreshMailOAuthToken
    :: Manager
    -> MailOAuthClient
    -> Text
    -> IO (Either Text MailOAuthToken)
refreshMailOAuthToken manager client refreshToken =
    exchangeToken manager client
        [ ("grant_type", "refresh_token")
        , ("refresh_token", refreshToken)
        ]

exchangeToken
    :: Manager
    -> MailOAuthClient
    -> [(Text, Text)]
    -> IO (Either Text MailOAuthToken)
exchangeToken manager client fields
    | client.mailOAuthClientProvider == ImapProvider =
        pure (Left "Custom IMAP accounts do not use OAuth.")
    | otherwise = do
        parsed <-
            tryAny
                ( parseRequest
                    ( Text.unpack
                        (tokenEndpoint
                            client.mailOAuthClientProvider)
                    )
                )
        case parsed of
            Left _ -> pure (Left oauthUnavailable)
            Right initial -> do
                attempted <- tryAny $
                    withResponse
                        initial
                            { method = "POST"
                            , requestHeaders =
                                [ ( hContentType
                                  , "application/x-www-form-urlencoded"
                                  )
                                ]
                            , requestBody =
                                RequestBodyBS
                                    ( renderSimpleQuery
                                        False
                                        [ ( TextEncoding.encodeUtf8 key
                                          , TextEncoding.encodeUtf8 value
                                          )
                                        | (key, value) <-
                                            tokenFields client fields
                                        ]
                                    )
                            , redirectCount = 0
                            , responseTimeout =
                                responseTimeoutMicro
                                    oauthHttpTimeoutMicros
                            , checkResponse = \_ _ -> pure ()
                            }
                        manager
                        \response -> do
                            body <-
                                readOAuthBody response.responseBody
                            pure (response.responseStatus, body)
                pure case attempted of
                    Left _ -> Left oauthUnavailable
                    Right (status, body)
                        | not (statusIsSuccessful status) ->
                            Left (oauthTokenStatusError status)
                        | otherwise -> do
                            bytes <- body
                            (token :: OAuthTokenResponse) <-
                                either
                                    (const (Left invalidTokenResponse))
                                    Right
                                    (Aeson.eitherDecodeStrict' bytes)
                            if Text.null
                                (Text.strip token.tokenAccessToken)
                                then Left invalidTokenResponse
                                else Right MailOAuthToken
                                    { mailOAuthTokenAccessToken =
                                        token.tokenAccessToken
                                    , mailOAuthTokenRefreshToken =
                                        nonEmpty token.tokenRefreshToken
                                    , mailOAuthTokenExpiresIn =
                                        max 1 token.tokenExpiresIn
                                    , mailOAuthTokenScopes =
                                        case Text.words token.tokenScope of
                                            [] ->
                                                Text.words
                                                    (mailOAuthScopes
                                                        client.mailOAuthClientProvider)
                                            scopes -> scopes
                                    }

tokenFields
    :: MailOAuthClient
    -> [(Text, Text)]
    -> [(Text, Text)]
tokenFields client fields =
    [ ("client_id", client.mailOAuthClientId) ]
        <> catMaybes
            [ ("client_secret",)
                <$> (client.mailOAuthClientSecret >>= nonEmpty)
            ]
        <> fields

resolveMailOAuthMailbox
    :: Manager
    -> MailProvider
    -> Text
    -> IO (Either Text (Text, Text))
resolveMailOAuthMailbox manager provider accessToken =
    case provider of
        GmailProvider ->
            getJson
                manager
                accessToken
                "https://gmail.googleapis.com/gmail/v1/users/me/profile"
                >>= \case
                    Left err -> pure (Left err)
                    Right value ->
                        pure $
                            maybe
                                (Left invalidMailboxIdentity)
                                Right
                                ( parseMaybe
                                    ( Aeson.withObject
                                        "Gmail profile"
                                        ( \profile ->
                                            (, "")
                                                <$> profile .: "emailAddress"
                                        )
                                    )
                                    value
                                )
        MicrosoftProvider -> do
            identity <-
                getJson
                    manager
                    accessToken
                    "https://graph.microsoft.com/v1.0/me?$select=mail,userPrincipalName,displayName"
            mailboxProbe <-
                getJson
                    manager
                    accessToken
                    "https://graph.microsoft.com/v1.0/me/messages?$top=1&$select=id"
            pure do
                _ <- mailboxProbe
                value <- identity
                maybe
                    (Left invalidMailboxIdentity)
                    Right
                    ( parseMaybe
                        ( Aeson.withObject
                            "Microsoft profile"
                            ( \profile -> do
                                mail <- profile .:? "mail"
                                principal <-
                                    profile .:? "userPrincipalName"
                                label <-
                                    profile .:? "displayName"
                                        Aeson..!= ""
                                address <-
                                    maybe
                                        (fail "missing mailbox identity")
                                        pure
                                        ( (mail >>= nonEmpty)
                                            <|> (principal >>= nonEmpty)
                                        )
                                pure (address, label)
                            )
                        )
                        value
                    )
        ImapProvider ->
            pure (Left "Custom IMAP accounts do not use OAuth.")

getJson
    :: Manager
    -> Text
    -> Text
    -> IO (Either Text Aeson.Value)
getJson manager accessToken rawUrl = do
    parsed <- tryAny (parseRequest (Text.unpack rawUrl))
    case parsed of
        Left _ -> pure (Left oauthUnavailable)
        Right initial -> do
            attempted <- tryAny $
                withResponse
                    initial
                        { requestHeaders =
                            [ ( hAuthorization
                              , "Bearer "
                                    <> TextEncoding.encodeUtf8
                                        accessToken
                              )
                            ]
                        , redirectCount = 0
                        , responseTimeout =
                            responseTimeoutMicro
                                oauthHttpTimeoutMicros
                        , checkResponse = \_ _ -> pure ()
                        }
                    manager
                    \response -> do
                        body <- readOAuthBody response.responseBody
                        pure (response.responseStatus, body)
            pure case attempted of
                Left _ -> Left oauthUnavailable
                Right (status, body)
                    | not (statusIsSuccessful status) ->
                        Left invalidMailboxIdentity
                    | otherwise -> do
                        bytes <- body
                        either
                            (const (Left invalidMailboxIdentity))
                            Right
                            (Aeson.eitherDecodeStrict' bytes)

authorizationEndpoint :: MailProvider -> Text
authorizationEndpoint = \case
    GmailProvider ->
        "https://accounts.google.com/o/oauth2/v2/auth"
    MicrosoftProvider ->
        "https://login.microsoftonline.com/common/oauth2/v2.0/authorize"
    ImapProvider -> ""

tokenEndpoint :: MailProvider -> Text
tokenEndpoint = \case
    GmailProvider -> "https://oauth2.googleapis.com/token"
    MicrosoftProvider ->
        "https://login.microsoftonline.com/common/oauth2/v2.0/token"
    ImapProvider -> ""

providerExtras :: MailProvider -> Text
providerExtras GmailProvider =
    "&access_type=offline&prompt=consent"
providerExtras MicrosoftProvider = ""
providerExtras ImapProvider = ""

readOAuthBody
    :: BodyReader
    -> IO (Either Text BS.ByteString)
readOAuthBody = go [] 0
  where
    go chunks size reader =
        brRead reader >>= \chunk ->
            if BS.null chunk
                then pure (Right (BS.concat (reverse chunks)))
                else
                    let size' = size + BS.length chunk
                    in if size' > oauthMaximumResponseBytes
                        then pure
                            (Left
                                "Mail OAuth provider returned an oversized response."
                            )
                        else go (chunk : chunks) size' reader

oauthTokenStatusError :: Status -> Text
oauthTokenStatusError status
    | statusCode status == 400
        || statusCode status == 401
        || statusCode status == 403 =
            "Mail OAuth credentials were rejected and the account must be reconnected."
    | statusCode status == 429
        || statusCode status >= 500 =
            oauthUnavailable
    | otherwise =
            "Mail OAuth authorization failed. Please try again."

nonEmpty :: Text -> Maybe Text
nonEmpty value
    | Text.null (Text.strip value) = Nothing
    | otherwise = Just value

oauthUnavailable, invalidTokenResponse, invalidMailboxIdentity :: Text
oauthUnavailable =
    "The mail OAuth provider is temporarily unavailable. Please try again."
invalidTokenResponse =
    "Mail OAuth provider returned an invalid token response."
invalidMailboxIdentity =
    "Mail provider did not return a usable mailbox identity."

oauthHttpTimeoutMicros, oauthMaximumResponseBytes :: Int
oauthHttpTimeoutMicros = 30 * 1_000_000
oauthMaximumResponseBytes = 1024 * 1024
