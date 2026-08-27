module Agent.CLI.Login.Types
    ( AccountBilling(..)
    , AccountUsage(..)
    , LoginAccount(..)
    , LoginAction(..)
    , LoginState(..)
    , UsageState(..)
    , UsageWindow(..)
    , applyLoginKey
    , initialLoginState
    ) where

import Agent.CLI.CredentialStore (ManagedAuthKind)
import Agent.CLI.Picker (PickerKey(..))
import Agent.Provider (Provider)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)

data AccountBilling
    = SubscriptionBilling !(Maybe Text)
    | ApiCreditsBilling
    deriving (Eq, Show)

data UsageWindow = UsageWindow
    { windowName :: !Text
    , usedPercent :: !Int
    , windowSeconds :: !Int
    , resetsAt :: !UTCTime
    }
    deriving (Eq, Show)

data AccountUsage = AccountUsage
    { usagePlan :: !(Maybe Text)
    , usageWindows :: ![UsageWindow]
    , creditsRemaining :: !(Maybe Text)
    , creditsUsed :: !(Maybe Text)
    }
    deriving (Eq, Show)

data UsageState
    = UsageNotChecked
    | UsageAvailable !AccountUsage
    | UsageUnavailable !Text
    deriving (Eq, Show)

data LoginAccount = LoginAccount
    { loginManagedId :: !(Maybe Text)
    , loginProvider :: !Provider
    , loginAccountId :: !Text
    , loginLabel :: !Text
    , loginBilling :: !AccountBilling
    , loginSource :: !Text
    , loginUsage :: !UsageState
    , loginAccessToken :: !Text
    , loginAuthKind :: !ManagedAuthKind
    , loginSecretPayload :: !Text
    , loginEnabled :: !Bool
    }
    deriving (Eq)

instance Show LoginAccount where
    show account =
        "LoginAccount { loginProvider = "
            <> show account.loginProvider
            <> ", loginAccountId = "
            <> show account.loginAccountId
            <> ", loginLabel = "
            <> show account.loginLabel
            <> ", loginBilling = "
            <> show account.loginBilling
            <> ", loginSource = "
            <> show account.loginSource
            <> ", loginUsage = "
            <> show account.loginUsage
            <> ", loginAccessToken = <redacted> }"

data LoginState = LoginState
    { loginAccounts :: ![LoginAccount]
    , loginIndex :: !Int
    }
    deriving (Eq, Show)

data LoginAction
    = LoginClose
    | LoginRefresh !Int
    | LoginAdd
    | LoginToggle !Int
    | LoginDelete !Int
    | LoginImport !Int
    deriving (Eq, Show)

initialLoginState :: [LoginAccount] -> LoginState
initialLoginState accounts = LoginState
    { loginAccounts = accounts
    , loginIndex = 0
    }

applyLoginKey :: PickerKey -> LoginState -> Either LoginAction LoginState
applyLoginKey key state = case key of
    PickerKeyCancel -> Left LoginClose
    PickerKeyConfirm -> refresh
    PickerKeyChar 'r' -> refresh
    PickerKeyChar 'R' -> refresh
    PickerKeyChar 'a' -> Left LoginAdd
    PickerKeyChar 'A' -> Left LoginAdd
    PickerKeyChar 'e' -> selected LoginToggle
    PickerKeyChar 'E' -> selected LoginToggle
    PickerKeyChar 'd' -> selected LoginDelete
    PickerKeyChar 'D' -> selected LoginDelete
    PickerKeyChar 'i' -> selected LoginImport
    PickerKeyChar 'I' -> selected LoginImport
    PickerKeyUp -> Right (move (-1))
    PickerKeyDown -> Right (move 1)
    _ -> Right state
  where
    count = length state.loginAccounts
    move delta
        | count == 0 = state { loginIndex = 0 }
        | otherwise =
            state
                { loginIndex = (state.loginIndex + delta) `mod` count
                }
    refresh
        | count == 0 = Left LoginClose
        | otherwise = Left (LoginRefresh (min (count - 1) state.loginIndex))
    selected constructor
        | count == 0 = Right state
        | otherwise = Left (constructor (min (count - 1) state.loginIndex))
