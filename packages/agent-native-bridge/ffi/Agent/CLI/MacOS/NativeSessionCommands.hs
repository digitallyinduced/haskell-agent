-- | Session mutations and conversation searches executed by the supervisor.
module Agent.CLI.MacOS.NativeSessionCommands
    ( runSessionMutation, sendSessionMutationFailure
    , runConversationSearch, sendSearchFailure
    ) where

import Agent.CLI.MacOS.EngineCallbacks
import Agent.CLI.MacOS.EngineState (SessionMutation(..))
import Agent.CLI.MacOS.EngineStore
import Agent.CLI.MacOS.Marshalling (withText)
import Agent.CLI.MacOS.NativeGatewayBoundary
import Agent.CLI.GatewayClient (withGatewayCredentialLease)
import Agent.CLI.ModelConfig (organizationGatewayConnectionId)
import Agent.CLI.Session (renameSession, deleteSession, setSessionArchived)
import Agent.Store.Postgres (ManagedPostgresConfig, Store, trustedPool)
import Agent.Store.Postgres.Session
    ( NativeConversationSearchResult(..), searchNativeConversationsForBoundary )
import Agent.Store.Types (renderStoreError)
import Control.Concurrent.MVar (MVar)
import Control.Exception.Safe (tryAny)
import qualified Data.ByteString as BS
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Word (Word8)
import Foreign.C.Types (CInt, CSize)
import Foreign.Ptr (FunPtr, Ptr, nullPtr, castPtr)
import System.OsPath (OsPath)

runSessionMutation
    :: ManagedPostgresConfig
    -> MVar (Maybe Store)
    -> OsPath
    -> SessionMutation
    -> FunPtr SessionResultCallback
    -> Ptr ()
    -> IO ()
runSessionMutation config store root mutation callback context = do
    outcome <- tryAny do
        activeStore <- acquireStore config store
        let pool = trustedPool activeStore
            sessionId = case mutation of
                SessionRename identifier _ -> identifier
                SessionDelete identifier -> identifier
                SessionArchive identifier _ -> identifier
        withNativeSessionBoundary
            pool root sessionId \gatewayIdentity _ ->
                case mutation of
                    SessionRename _ title -> do
                        result <- renameSession pool root sessionId title
                        pure (gatewayIdentity <$ result)
                    SessionDelete _ -> do
                        result <- deleteSession pool root sessionId
                        pure (gatewayIdentity <$ result)
                    SessionArchive _ archived -> do
                        result <-
                            setSessionArchived
                                pool root sessionId archived
                        pure (gatewayIdentity <$ result)
    case outcome of
        Left exception ->
            sendSessionMutationFailure
                callback context (Text.pack (show exception))
        Right (Left err) ->
            sendSessionMutationFailure callback context err
        Right (Right gatewayIdentity) ->
            emitForNativeGatewayBoundary gatewayIdentity
                (invokeSessionResultCallback callback context 0 nullPtr 0)
                >>= \case
                    Left err ->
                        sendSessionMutationFailure callback context err
                    Right () -> pure ()

sendSessionMutationFailure
    :: FunPtr SessionResultCallback -> Ptr () -> Text -> IO ()
sendSessionMutationFailure callback context message =
    withText message \errorPtr errorLength ->
        invokeSessionResultCallback callback context (-1) errorPtr errorLength

runConversationSearch
    :: ManagedPostgresConfig
    -> MVar (Maybe Store)
    -> Maybe Text
    -> Text
    -> Int
    -> FunPtr SearchCallback
    -> Ptr ()
    -> IO ()
runConversationSearch
        config store gatewayIdentity query limit callback context = do
    outcome <- tryAny do
        activeStore <- acquireStore config store
        searchNativeConversationsForBoundary
            (trustedPool activeStore)
            organizationGatewayConnectionId
            gatewayIdentity
            query
            limit
    case outcome of
        Left exception ->
            sendSearchFailure callback context (Text.pack (show exception))
        Right (Left err) ->
            sendSearchFailure callback context (renderStoreError err)
        Right (Right results) ->
            emitSearchResultsForBoundary
                gatewayIdentity callback context results >>= \case
                    Left err -> sendSearchFailure callback context err
                    Right () -> pure ()

emitSearchResultsForBoundary
    :: Maybe Text
    -> FunPtr SearchCallback
    -> Ptr ()
    -> [NativeConversationSearchResult]
    -> IO (Either Text ())
emitSearchResultsForBoundary gatewayIdentity callback context =
    emitBoundaryChecked
        withGatewayCredentialLease
        (ensureNativeGatewayIdentity gatewayIdentity)
        (sendSearchResult callback context)
        (invokeSearchCallback callback context
            1 nullPtr 0 nullPtr 0 nullPtr 0 nullPtr 0 nullPtr 0
            0 0 (-1) 0 0 nullPtr 0 nullPtr 0 0 nullPtr 0)

sendSearchFailure :: FunPtr SearchCallback -> Ptr () -> Text -> IO ()
sendSearchFailure callback context message =
    withTextBytes message \errorPointer errorLength ->
        invokeSearchCallback callback context
            (-1) nullPtr 0 nullPtr 0 nullPtr 0 nullPtr 0 nullPtr 0
            0 0 (-1) 0 0 nullPtr 0 nullPtr 0 0
            errorPointer errorLength

sendSearchResult
    :: FunPtr SearchCallback
    -> Ptr ()
    -> NativeConversationSearchResult
    -> IO ()
sendSearchResult callback context result =
    withTextBytes result.nativeSearchSessionId \sessionPointer sessionLength ->
    withTextBytes result.nativeSearchTitle \titlePointer titleLength ->
    withTextBytes result.nativeSearchCwd \cwdPointer cwdLength ->
    withTextBytes result.nativeSearchProvider \providerPointer providerLength ->
    withTextBytes result.nativeSearchModel \modelPointer modelLength ->
    withMaybeTextBytes result.nativeSearchUserText \userPointer userLength ->
    withMaybeTextBytes result.nativeSearchAssistantText
        \assistantPointer assistantLength ->
            invokeSearchCallback callback context
                0
                sessionPointer sessionLength
                titlePointer titleLength
                cwdPointer cwdLength
                providerPointer providerLength
                modelPointer modelLength
                (epochMilliseconds result.nativeSearchUpdatedAt)
                (if result.nativeSearchArchived then 1 else 0)
                (fromMaybe (-1) result.nativeSearchTurnIndex)
                (maybe 0 epochMilliseconds result.nativeSearchOccurredAt)
                (searchRoleCode result.nativeSearchRole)
                userPointer userLength
                assistantPointer assistantLength
                (realToFrac result.nativeSearchRank)
                nullPtr 0

withTextBytes :: Text -> (Ptr Word8 -> CSize -> IO a) -> IO a
withTextBytes value action =
    BS.useAsCStringLen (TextEncoding.encodeUtf8 value) \(pointer, length) ->
        action (castPtr pointer) (fromIntegral length)

withMaybeTextBytes
    :: Maybe Text
    -> (Ptr Word8 -> CSize -> IO a)
    -> IO a
withMaybeTextBytes Nothing action = action nullPtr 0
withMaybeTextBytes (Just value) action = withTextBytes value action

epochMilliseconds :: UTCTime -> Int64
epochMilliseconds =
    floor . (* 1000) . utcTimeToPOSIXSeconds

searchRoleCode :: Maybe Text -> CInt
searchRoleCode = \case
    Just "user" -> 1
    Just "assistant" -> 2
    _ -> 0
