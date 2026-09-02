-- | Local custom-IMAP account connection backed by the standalone store.
module Agent.CLI.Mail.Imap
    ( connectMailImapAccount
    , verifyMailImapCredentials
    , withMailImapConnection
    ) where

import Agent.CLI.Mail.Store
import Agent.Mail.Imap
    ( verifyMailImapCredentials
    , withMailImapConnection
    )
import Data.List (find)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Clock (getCurrentTime)

-- | Validate and persist a custom account. Persistence occurs only after a
-- successful authenticated LIST probe.
connectMailImapAccount
    :: Text
    -> Text
    -> MailImapSettings
    -> Text
    -> IO (Either Text Text)
connectMailImapAccount rawEmail rawLabel settings password =
    case
        ( normalizeMailEmail rawEmail
        , validateMailImapSettings settings
        )
    of
        (Left err, _) -> pure (Left err)
        (_, Left err) -> pure (Left err)
        (Right email, Right ()) ->
            verifyMailImapCredentials settings password >>= \case
                Left err -> pure (Left err)
                Right () ->
                    loadMailAccounts >>= \case
                        Left err -> pure (Left err)
                        Right accounts -> do
                            now <- getCurrentTime
                            let prior =
                                    find (sameMailbox email) accounts
                            accountId <-
                                maybe
                                    (newMailAccountId ImapProvider)
                                    (pure . (.mailAccountId))
                                    prior
                            let account = MailAccount
                                    { mailAccountId = accountId
                                    , mailAccountProvider = ImapProvider
                                    , mailAccountEmail = email
                                    , mailAccountLabel =
                                        if Text.null
                                            (Text.strip rawLabel)
                                            then email
                                            else Text.take 256
                                                (Text.strip rawLabel)
                                    , mailAccountEnabled = True
                                    , mailAccountState = MailConnected
                                    , mailAccountImapSettings =
                                        Just settings
                                    , mailAccountOAuthClientId =
                                        Nothing
                                    , mailAccountCreatedAt =
                                        maybe
                                            now
                                            (.mailAccountCreatedAt)
                                            prior
                                    , mailAccountUpdatedAt = now
                                    , mailAccountLastVerifiedAt =
                                        Just now
                                    , mailAccountLastErrorCode =
                                        Nothing
                                    }
                                credential =
                                    MailImapSecret
                                        accountId
                                        password
                            upsertMailAccount
                                account
                                credential
                                >>= \case
                                    Left err -> pure (Left err)
                                    Right () ->
                                        pure (Right accountId)
  where
    sameMailbox email account =
        account.mailAccountProvider == ImapProvider
            && Text.toCaseFold
                account.mailAccountEmail
                == email
