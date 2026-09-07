-- | Interactive credential dashboard for @/login@.
--
-- The implementation is split into internal modules by responsibility. This
-- facade intentionally preserves the complete public API.
module Agent.CLI.Login
    ( AccountBilling(..)
    , AccountUsage(..)
    , DevicePollReadiness(..)
    , DevicePollSchedule
    , GatewayLoginFlow(..)
    , LoginAccount(..)
    , LoginAction(..)
    , LoginState(..)
    , UsageState(..)
    , UsageWindow(..)
    , applyLoginKey
    , connectProviderAccount
    , advanceDevicePollSchedule
    , devicePollReadiness
    , discoverLoginAccounts
    , discoverSelectableLoginAccounts
    , formatLoginAccounts
    , formatCurrencyAmount
    , initialDevicePollSchedule
    , initialLoginState
    , loginAccountActionRows
    , loginAccountDetail
    , launchBrowserCommand
    , loginAccountSelectionId
    , loginDashboardRows
    , refreshLoginAccount
    , renderLoginFrame
    , runFullscreenLoginManager
    , runLoginManager
    , selectGatewayLoginFlow
    , storeConnectedCredential
    ) where

import Agent.CLI.Login.Internal.Accounts
    ( discoverLoginAccounts
    , discoverSelectableLoginAccounts
    , formatCurrencyAmount
    , loginAccountSelectionId
    , refreshLoginAccount
    )
import Agent.CLI.Login.Internal.Browser (launchBrowserCommand)
import Agent.CLI.Login.Internal.Dashboard
    ( loginAccountActionRows
    , loginAccountDetail
    , loginDashboardRows
    )
import Agent.CLI.Login.Internal.Device
    ( DevicePollReadiness(..)
    , DevicePollSchedule
    , advanceDevicePollSchedule
    , devicePollReadiness
    , initialDevicePollSchedule
    )
import Agent.CLI.Login.Internal.Formatting
    ( formatLoginAccounts
    , renderLoginFrame
    )
import Agent.CLI.Login.Internal.Gateway
    ( GatewayLoginFlow(..)
    , selectGatewayLoginFlow
    )
import Agent.CLI.Login.Internal.Manager
    ( runFullscreenLoginManager
    , runLoginManager
    )
import Agent.CLI.Login.Internal.ProviderConnection
    ( connectProviderAccount
    , storeConnectedCredential
    )
import Agent.CLI.Login.Types
    ( AccountBilling(..)
    , AccountUsage(..)
    , LoginAccount(..)
    , LoginAction(..)
    , LoginState(..)
    , UsageState(..)
    , UsageWindow(..)
    , applyLoginKey
    , initialLoginState
    )
