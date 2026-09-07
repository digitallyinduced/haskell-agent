-- | Native bridge composition root and compatibility facade for boundary tests.
-- FFI implementations live with the private owner of their state and lifetime.
module Agent.CLI.MacOS.Bridge
    ( BrowserCancelCallback
    , BrowserCompletion
    , nextAvailableBrowserPendingId
    , BrowserCallback
    , BrowserHost(..)
    , BrowserRegistration(..)
    , browserCommandABI
    , browserOutputCapacity
    , browserStatusMessage
    , browserToolsWhenEnabled
    , composeNativeTools
    , invokeBrowserCommand
    , mailABISynchronousValidationSmoke
    , RepositoryCheckHandle(..)
    , ha_repository_check_destroy
    , TurnStart(..)
    , nativeExceptionMessage
    , nativeRequestRequiresGatewayLock
    , nativeSessionRouteMatchesBoundary
    , nativeTurnRouteMatchesBoundary
    , nativeTurnArguments
    , NativeInteractionResolution(..)
    , PendingInteraction(..)
    , cancelPendingInteractions
    , discardStagedTurn
    , discardStagedTurnById
    , emitBoundaryChecked
    , invokeGatewayCallbackOnce
    , resolvePendingInteraction
    , turnStartCleanupId
    ) where

import Agent.CLI.MacOS.AccountBridge ()
import Agent.CLI.MacOS.BrowserBridge
import Agent.CLI.MacOS.DatabaseBrowseBridge ()
import Agent.CLI.MacOS.EngineHandle ()
import Agent.CLI.MacOS.EngineHostRegistration ()
import Agent.CLI.MacOS.EngineStaging ()
import Agent.CLI.MacOS.EngineSubmission ()
import Agent.CLI.MacOS.GatewayBridge (invokeGatewayCallbackOnce)
import Agent.CLI.MacOS.InteractionState
import Agent.CLI.MacOS.LearnedSkillsBridge ()
import Agent.CLI.MacOS.MailBridge (mailABISynchronousValidationSmoke)
import Agent.CLI.MacOS.NativeGatewayBoundary
import Agent.CLI.MacOS.NativeRequest
import Agent.CLI.MacOS.NativeRequestHandler (nativeRequestRequiresGatewayLock)
import Agent.CLI.MacOS.RepositoryChecks
import Agent.CLI.MacOS.RepositoryDeliveryBridge ()
import Agent.CLI.MacOS.RepositoryReviewBridge ()
import Agent.CLI.MacOS.ResourceAdmin ()
import Agent.CLI.MacOS.SessionTransferBridge ()
import Agent.CLI.MacOS.TurnExecution (composeNativeTools, nativeExceptionMessage)
import Agent.CLI.MacOS.TurnInputs (nativeTurnArguments)
import Agent.CLI.MacOS.TurnState (discardStagedTurn, discardStagedTurnById)
