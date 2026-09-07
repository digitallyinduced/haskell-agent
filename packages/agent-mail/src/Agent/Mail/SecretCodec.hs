-- | Explicit at-rest serialization for mail credentials.
--
-- 'MailSecret' intentionally has no ambient Aeson instances: generic logging
-- and diagnostics must not be able to serialize access tokens or passwords by
-- accident. Persistence layers must opt in by calling these functions only at
-- an encrypted or owner-only storage boundary.
module Agent.Mail.SecretCodec
    ( mailSecretStorageValue
    , parseMailSecretStorageValue
    ) where

import Agent.Mail.Types (MailSecret(..))
import Data.Aeson
    ( Value
    , object
    , withObject
    , (.:)
    , (.:?)
    , (.!=)
    , (.=)
    )
import Data.Aeson.Types (Parser)
import Data.Text (Text)

mailSecretStorageValue :: MailSecret -> Value
mailSecretStorageValue = \case
    MailOAuthSecret
        { mailSecretAccountId
        , mailOAuthAccessToken
        , mailOAuthRefreshToken
        , mailOAuthExpiresAt
        , mailOAuthScopes
        } ->
            object
                [ "id" .= mailSecretAccountId
                , "kind" .= ("oauth" :: Text)
                , "access_token" .= mailOAuthAccessToken
                , "refresh_token" .= mailOAuthRefreshToken
                , "expires_at" .= mailOAuthExpiresAt
                , "scopes" .= mailOAuthScopes
                ]
    MailImapSecret
        { mailSecretAccountId
        , mailImapPassword
        } ->
            object
                [ "id" .= mailSecretAccountId
                , "kind" .= ("imap_password" :: Text)
                , "password" .= mailImapPassword
                ]

parseMailSecretStorageValue :: Value -> Parser MailSecret
parseMailSecretStorageValue = withObject "MailSecret" \value -> do
    kind <- value .: "kind"
    case (kind :: Text) of
        "oauth" ->
            MailOAuthSecret
                <$> value .: "id"
                <*> value .: "access_token"
                <*> value .:? "refresh_token"
                <*> value .:? "expires_at"
                <*> value .:? "scopes" .!= []
        "imap_password" ->
            MailImapSecret
                <$> value .: "id"
                <*> value .: "password"
        _ -> fail "unknown mail secret kind"
