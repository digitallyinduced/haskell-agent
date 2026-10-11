-- | Provider-native child conversations, retained across follow-up turns
-- without converting the transcript to the neutral loop representation.
module Agent.Subagents.Native
    ( NativeConversations
    , newNativeConversations
    , nativeConversation
    , NativeBackendConfig(..)
    , nativeTranscriptBackend
    ) where

import Agent.Error (ApiError)
import qualified Agent.Loop as Agent
import Agent.Loop.Native (NativeCodec(..))
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)

-- | Scoped to the owning registry; drop the store after closing the registry.
-- Each child must have at most one active turn, as enforced by SubagentRegistry.
newtype NativeConversations key item =
    NativeConversations (IORef (Map key (IORef [item])))

newNativeConversations :: IO (NativeConversations key item)
newNativeConversations = NativeConversations <$> newIORef Map.empty

nativeConversation :: Ord key => NativeConversations key item -> key -> IO (IORef [item])
nativeConversation (NativeConversations conversations) identifier = do
    fresh <- newIORef []
    atomicModifyIORef' conversations \known -> case Map.lookup identifier known of
        Just existing -> (known, existing)
        Nothing -> (Map.insert identifier fresh known, fresh)

data NativeBackendConfig item response = NativeBackendConfig
    { codec :: !(NativeCodec item response)
    , continuationNamespace :: !Text
    , prepareReplay :: !([item] -> [item])
    , submit :: !(Maybe Text -> [item] -> IO (Either ApiError response))
    -- | Record usage or persist the response before it enters retained history.
    , recordResponse :: !(response -> IO ())
    }

-- | A new connection starts with no continuation and replays native history.
-- Later requests on that connection send only new inputs. Failed submissions
-- and failed recording callbacks leave the retained conversation unchanged.
nativeTranscriptBackend :: NativeBackendConfig item response -> IORef [item] -> Agent.Backend
nativeTranscriptBackend config conversation =
    Agent.backendWithCallbacks \snapshot previous inputs _ -> do
        history <- readIORef conversation
        let native = config.codec
            newItems =
                [native.completedToolItem result | Agent.CompletedTool result <- inputs]
                    <> native.auxiliaryInputItems inputs
            requestItems = maybe (config.prepareReplay history) (const []) previous <> newItems
        config.submit previous requestItems >>= \case
            Left err -> pure (Left err)
            Right response -> do
                config.recordResponse response
                writeIORef conversation (history <> newItems <> native.responseItems response)
                pure $ Right Agent.BackendResult
                    { backendOutput = native.responseTurnOutput response
                    , backendState = Agent.advanceBackendSnapshot snapshot (native.responseToolItems response)
                        (Just (Agent.BackendContinuation config.continuationNamespace (native.responseIdentifier response)))
                    }
