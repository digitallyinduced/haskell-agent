-- | Telegram configuration and command types.
module Agent.Telegram.Types.Config
    ( TelegramApprovalMode(..)
    , TelegramConfig(..)
    , TelegramSetupOptions(..)
    , defaultTelegramSetupOptions
    , defaultTelegramWorkerCount
    , maximumTelegramWorkerCount
    , TelegramCommand(..)
    , TelegramUsersCommand(..)
    ) where

import Agent.Provider (Provider, parseProvider, providerSlug)
import Data.Aeson
    ( FromJSON(..)
    , ToJSON(..)
    , object
    , withObject
    , (.:)
    , (.:?)
    , (.!=)
    , (.=)
    )
import qualified Data.Aeson as Aeson
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text

data TelegramApprovalMode
    = TelegramApprovalPrompt
    | TelegramApprovalDeny
    | TelegramApprovalYolo
    deriving (Eq, Show)

instance ToJSON TelegramApprovalMode where
    toJSON = \case
        TelegramApprovalPrompt -> "prompt"
        TelegramApprovalDeny -> "deny"
        TelegramApprovalYolo -> "yolo"

instance FromJSON TelegramApprovalMode where
    parseJSON = Aeson.withText "TelegramApprovalMode" \case
        "prompt" -> pure TelegramApprovalPrompt
        "deny" -> pure TelegramApprovalDeny
        "yolo" -> pure TelegramApprovalYolo
        other -> fail ("unknown Telegram approval mode: " <> Text.unpack other)

data TelegramConfig = TelegramConfig
    { telegramProvider :: !Provider
    , telegramModel :: !(Maybe Text)
    , telegramCwd :: !FilePath
    , telegramEffort :: !(Maybe Text)
    , telegramApprovalMode :: !TelegramApprovalMode
    , telegramAllowedUsers :: !(Set Integer)
    , telegramRespondToAllGroupMessages :: !Bool
    , telegramWorkerCount :: !Int
    } deriving (Eq, Show)

defaultTelegramWorkerCount :: Int
defaultTelegramWorkerCount = 8

maximumTelegramWorkerCount :: Int
maximumTelegramWorkerCount = 64

instance ToJSON TelegramConfig where
    toJSON config = object
        [ "provider" .= providerSlug config.telegramProvider
        , "model" .= config.telegramModel
        , "cwd" .= config.telegramCwd
        , "effort" .= config.telegramEffort
        , "approvalMode" .= config.telegramApprovalMode
        , "allowedUsers" .= Set.toList config.telegramAllowedUsers
        , "respondToAllGroupMessages"
            .= config.telegramRespondToAllGroupMessages
        , "workers" .= config.telegramWorkerCount
        ]

instance FromJSON TelegramConfig where
    parseJSON = withObject "TelegramConfig" \o -> do
        providerText <- o .: "provider"
        telegramProvider <- maybe
            (fail ("unknown provider: " <> Text.unpack providerText))
            pure
            (parseProvider providerText)
        legacyYolo <- o .:? "yolo" .!= False
        approvalMode <- o .:? "approvalMode" .!=
            (if legacyYolo
                then TelegramApprovalYolo
                else TelegramApprovalPrompt)
        workerCount <- o .:? "workers" .!= defaultTelegramWorkerCount
        if workerCount < 1 || workerCount > maximumTelegramWorkerCount
            then fail
                ("workers must be between 1 and "
                    <> show maximumTelegramWorkerCount)
            else pure ()
        TelegramConfig
            <$> pure telegramProvider
            <*> o .:? "model"
            <*> o .: "cwd"
            <*> o .:? "effort"
            <*> pure approvalMode
            <*> (Set.fromList <$> o .: "allowedUsers")
            <*> (o .:? "respondToAllGroupMessages" .!= False)
            <*> pure workerCount

data TelegramSetupOptions = TelegramSetupOptions
    { setupProvider :: !(Maybe Provider)
    , setupModel :: !(Maybe Text)
    , setupCwd :: !(Maybe FilePath)
    , setupEffort :: !(Maybe Text)
    , setupApprovalMode :: !TelegramApprovalMode
    , setupAllowedUsers :: ![Integer]
    , setupRespondToAllGroupMessages :: !Bool
    , setupWorkerCount :: !Int
    , setupStart :: !Bool
    } deriving (Eq, Show)

defaultTelegramSetupOptions :: TelegramSetupOptions
defaultTelegramSetupOptions = TelegramSetupOptions
    { setupProvider = Nothing
    , setupModel = Nothing
    , setupCwd = Nothing
    , setupEffort = Nothing
    , setupApprovalMode = TelegramApprovalPrompt
    , setupAllowedUsers = []
    , setupRespondToAllGroupMessages = False
    , setupWorkerCount = defaultTelegramWorkerCount
    , setupStart = False
    }

data TelegramCommand
    = TelegramSetup !TelegramSetupOptions
    | TelegramRun
    | TelegramStart
    | TelegramStop
    | TelegramStatus
    | TelegramUsers !TelegramUsersCommand
    | TelegramHelp
    | TelegramVersion
    deriving (Eq, Show)

data TelegramUsersCommand
    = TelegramUsersList
    | TelegramUsersAdd !Integer
    | TelegramUsersRemove !Integer
    deriving (Eq, Show)
