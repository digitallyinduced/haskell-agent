{-# LANGUAGE ForeignFunctionInterface #-}

-- | Standalone session history and transfer endpoints. Each asynchronous call
-- owns its store bracket and keeps borrowed callback buffers invocation-scoped.
module Agent.CLI.MacOS.SessionTransferBridge () where

import Agent.CLI.MacOS.Marshalling
import Agent.CLI.MacOS.NativeGatewayBoundary
import Agent.CLI.GatewayClient (withGatewayCredentialLease)
import Agent.CLI.Models (validateResumedGatewayBoundary)
import Agent.CLI.Session
    ( SessionMeta(..), SessionTurn(..), SessionTurnPage(..)
    , SessionTransfer(..), SessionTransferEnvelope(..), TranscriptEffect(..)
    , forkSessionAtTurn, importSessionTransferRemapped
    , loadSessionHistoryTurnsAround, sessionsRoot, streamSessionTransfer
    )
import Agent.CLI.SessionAdmin (managedPostgresConfigForHome)
import Agent.Loop (TokenUsage(..))
import Agent.Store.Postgres (openStore, closeStore, trustedPool)
import Agent.Store.Postgres.Connection (StorePool)
import Agent.Store.Types (renderStoreError)
import Control.Concurrent (forkIO)
import Control.Exception.Safe (SomeException, bracket, throwString, tryAny)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import Data.Word (Word8)
import Foreign.C.String (CString)
import Foreign.C.Types (CInt(..), CSize(..), CLLong(..))
import Foreign.Ptr (FunPtr, Ptr, castPtr, nullPtr, nullFunPtr)
import System.Directory.OsPath (getHomeDirectory)
import System.OsPath (OsPath)

-- Session page status is 0 for a turn, 1 for completion, and -1 for failure.
-- Text buffers are callback-scoped UTF-8.
type SessionTurnCallback =
    Ptr () -> CInt -> Int64
    -> CString -> CSize -- occurred at
    -> CString -> CSize -- user
    -> CString -> CSize -- assistant
    -> CString -> CSize -- turn error
    -> CString -> CSize -- response id
    -> CString -> CSize -- transcript effect
    -> CString -> CSize -- provider-extensible response items JSON
    -> CLLong -> CLLong -> CLLong -- usage; -1 means absent
    -> CInt -> CInt -- has older/newer on completion
    -> CString -> CSize -- error
    -> IO ()

-- Transfer result status is 0 for success and -1 for failure.
type SessionTransferResultCallback =
    Ptr () -> CInt -> CString -> CSize -> CString -> CSize -> IO ()

-- Export status is 0 for a chunk, 1 for completion, and -1 for failure.
type SessionExportCallback =
    Ptr () -> CInt -> Ptr Word8 -> CSize -> CString -> CSize -> IO ()

foreign import ccall "dynamic"
    invokeSessionTransferResultCallback
        :: FunPtr SessionTransferResultCallback -> SessionTransferResultCallback

foreign import ccall "dynamic"
    invokeSessionTurnCallback
        :: FunPtr SessionTurnCallback -> SessionTurnCallback

foreign import ccall "dynamic"
    invokeSessionExportCallback
        :: FunPtr SessionExportCallback -> SessionExportCallback

foreign export ccall ha_session_load_around
    :: Ptr Word8 -> CSize -> Int64 -> CInt
    -> FunPtr SessionTurnCallback -> Ptr () -> IO CInt

foreign export ccall ha_session_fork
    :: Ptr Word8 -> CSize -> Int64
    -> FunPtr SessionTransferResultCallback -> Ptr () -> IO CInt

foreign export ccall ha_session_export
    :: Ptr Word8 -> CSize
    -> FunPtr SessionExportCallback -> Ptr () -> IO CInt

foreign export ccall ha_session_import
    :: Ptr Word8 -> CSize
    -> FunPtr SessionTransferResultCallback -> Ptr () -> IO CInt

ha_session_load_around
    :: Ptr Word8 -> CSize -> Int64 -> CInt
    -> FunPtr SessionTurnCallback -> Ptr () -> IO CInt
ha_session_load_around sessionBytes (CSize sessionLength) center radius
        callback context
    | callback == nullFunPtr = pure 2
    | sessionBytes == nullPtr || sessionLength == 0 = pure 2
    | toInteger sessionLength > toInteger (maxBound :: Int) = pure 2
    | center < 0 || radius < 0 = pure 2
    | otherwise = do
        decoded <- tryAny (decodeUtf8Input sessionBytes sessionLength)
        case decoded of
            Left _ -> pure 3
            Right (Left ()) -> pure 2
            Right (Right sessionId) -> do
                _ <- forkIO do
                    result <- tryAny $ withNativeSessionStore \pool root ->
                        withNativeGatewayBoundary \gatewayIdentity ->
                            validateNativeSessionBoundary
                                pool root gatewayIdentity sessionId >>= \case
                                    Left err -> pure (Left err)
                                    Right _ ->
                                        loadSessionHistoryTurnsAround
                                            pool
                                            root
                                            sessionId
                                            center
                                            (fromIntegral radius) >>= \case
                                                Left err -> pure (Left err)
                                                Right page ->
                                                    pure
                                                        (Right
                                                            ( gatewayIdentity
                                                            , page
                                                            ))
                    case result of
                        Left exception ->
                            sessionTurnFailure callback context
                                (Text.pack (show exception))
                        Right (Left err) ->
                            sessionTurnFailure callback context err
                        Right (Right (gatewayIdentity, page)) ->
                            emitSessionPageForBoundary
                                gatewayIdentity callback context page >>= \case
                                    Left err ->
                                        sessionTurnFailure
                                            callback context err
                                    Right () -> pure ()
                pure 0

ha_session_fork
    :: Ptr Word8 -> CSize -> Int64
    -> FunPtr SessionTransferResultCallback -> Ptr () -> IO CInt
ha_session_fork sessionBytes (CSize sessionLength) throughIndex callback context
    | callback == nullFunPtr = pure 2
    | sessionBytes == nullPtr || sessionLength == 0 || throughIndex < 0 = pure 2
    | toInteger sessionLength > toInteger (maxBound :: Int) = pure 2
    | otherwise = do
        decoded <- tryAny (decodeUtf8Input sessionBytes sessionLength)
        case decoded of
            Left _ -> pure 3
            Right (Left ()) -> pure 2
            Right (Right sessionId) -> do
                _ <- forkIO do
                    result <- tryAny $ withNativeSessionStore \pool root ->
                        withNativeSessionBoundary
                            pool root sessionId \gatewayIdentity _ ->
                                forkSessionAtTurn
                                    pool root sessionId throughIndex >>= \case
                                        Left err -> pure (Left err)
                                        Right forkedSessionId ->
                                            pure
                                                (Right
                                                    ( gatewayIdentity
                                                    , forkedSessionId
                                                    ))
                    completeBoundarySessionResult callback context result
                pure 0

ha_session_export
    :: Ptr Word8 -> CSize
    -> FunPtr SessionExportCallback -> Ptr () -> IO CInt
ha_session_export sessionBytes (CSize sessionLength) callback context
    | callback == nullFunPtr = pure 2
    | sessionBytes == nullPtr || sessionLength == 0 = pure 2
    | toInteger sessionLength > toInteger (maxBound :: Int) = pure 2
    | otherwise = do
        decoded <- tryAny (decodeUtf8Input sessionBytes sessionLength)
        case decoded of
            Left _ -> pure 3
            Right (Left ()) -> pure 2
            Right (Right sessionId) -> do
                _ <- forkIO do
                    result <- tryAny $ withNativeSessionStore \pool root ->
                        withNativeGatewayBoundary \gatewayIdentity ->
                            validateNativeSessionBoundary
                                pool root gatewayIdentity sessionId >>= \case
                                    Left err -> pure (Left err)
                                    Right _ ->
                                        streamSessionTransfer
                                            pool
                                            root
                                            sessionId
                                            (emitSessionExportChunkForBoundary
                                                gatewayIdentity
                                                callback
                                                context) >>= \case
                                                    Left err ->
                                                        pure (Left err)
                                                    Right () ->
                                                        pure
                                                            (Right
                                                                gatewayIdentity)
                    case result of
                        Left exception ->
                            sessionExportFailure callback context
                                (Text.pack (show exception))
                        Right (Left err) ->
                            sessionExportFailure callback context err
                        Right (Right gatewayIdentity) ->
                            emitSessionExportTerminalForBoundary
                                gatewayIdentity callback context >>= \case
                                    Left err ->
                                        sessionExportFailure
                                            callback context err
                                    Right () -> pure ()
                pure 0

ha_session_import
    :: Ptr Word8 -> CSize
    -> FunPtr SessionTransferResultCallback -> Ptr () -> IO CInt
ha_session_import bytes (CSize length) callback context
    | callback == nullFunPtr = pure 2
    | bytes == nullPtr || length == 0 = pure 2
    | toInteger length > 512 * 1024 * 1024 = pure 2
    | otherwise = do
        payload <- BS.packCStringLen (castPtr bytes, fromIntegral length)
        _ <- forkIO do
            result <- tryAny $
                case TextEncoding.decodeUtf8' payload of
                    Left _ ->
                        pure
                            (Left
                                "invalid session transfer: invalid UTF-8")
                    Right _ ->
                        case
                            (Aeson.eitherDecodeStrict' payload
                                :: Either String SessionTransferEnvelope)
                        of
                            Left err ->
                                pure
                                    (Left
                                        ("invalid session transfer: "
                                            <> Text.pack err))
                            Right envelope -> do
                                let meta =
                                        envelope.transferSession.transferMeta
                                withNativeSessionStore \pool root ->
                                    withNativeGatewayBoundary
                                        \gatewayIdentity ->
                                            case
                                                validateResumedGatewayBoundary
                                                    gatewayIdentity
                                                    meta.metaConnection
                                                    meta.metaGatewayIdentity
                                            of
                                                Left err -> pure (Left err)
                                                Right () ->
                                                    importSessionTransferRemapped
                                                        pool
                                                        root
                                                        Nothing
                                                        envelope >>= \case
                                                            Left err ->
                                                                pure
                                                                    (Left
                                                                        err)
                                                            Right
                                                                importedSessionId ->
                                                                    pure
                                                                        (Right
                                                                            ( gatewayIdentity
                                                                            , importedSessionId
                                                                            ))
            completeBoundarySessionResult callback context result
        pure 0

withNativeSessionStore
    :: (StorePool -> OsPath -> IO (Either Text a))
    -> IO (Either Text a)
withNativeSessionStore action = do
    home <- getHomeDirectory
    config <- managedPostgresConfigForHome home
    openStore config >>= \case
        Left err -> pure (Left (renderStoreError err))
        Right opened ->
            bracket (pure opened) closeStore \store ->
                action (trustedPool store) (sessionsRoot home)

emitSessionTurn
    :: FunPtr SessionTurnCallback
    -> Ptr ()
    -> Int64
    -> SessionTurn
    -> IO ()
emitSessionTurn callback context turnIndex turn =
    withText (Text.pack (show turn.turnAt)) \occurred occurredLength ->
    withText turn.turnUserText \user userLength ->
    withOptionalText turn.turnAssistantText \assistant assistantLength ->
    withOptionalText turn.turnError \turnError turnErrorLength ->
    withOptionalText turn.turnResponseId \response responseLength ->
    withText (transcriptEffectName turn.turnEffect) \effect effectLength ->
    BS.useAsCStringLen
        (LBS.toStrict (Aeson.encode turn.turnItems))
        \(items, itemsLength) -> do
            let (inputTokens', outputTokens', cachedTokens') =
                    maybe (-1, -1, -1)
                        (\usage ->
                            ( fromIntegral usage.inputTokens
                            , fromIntegral usage.outputTokens
                            , fromIntegral usage.cachedTokens
                            ))
                        turn.turnUsage
            invokeSessionTurnCallback callback context 0 turnIndex
                occurred occurredLength
                user userLength
                assistant assistantLength
                turnError turnErrorLength
                response responseLength
                effect effectLength
                items (fromIntegral itemsLength)
                inputTokens' outputTokens' cachedTokens'
                0 0 nullPtr 0

emitSessionPageForBoundary
    :: Maybe Text
    -> FunPtr SessionTurnCallback
    -> Ptr ()
    -> SessionTurnPage
    -> IO (Either Text ())
emitSessionPageForBoundary gatewayIdentity callback context page =
    emitBoundaryChecked
        withGatewayCredentialLease
        (ensureNativeGatewayIdentity gatewayIdentity)
        (\(turnIndex, turn) ->
            emitSessionTurn callback context turnIndex turn)
        (sessionTurnTerminal callback context
            page.pageHasOlder page.pageHasNewer)
        page.pageTurns

sessionTurnTerminal
    :: FunPtr SessionTurnCallback -> Ptr () -> Bool -> Bool -> IO ()
sessionTurnTerminal callback context hasOlder hasNewer =
    invokeSessionTurnCallback callback context 1 (-1)
        nullPtr 0 nullPtr 0 nullPtr 0 nullPtr 0 nullPtr 0 nullPtr 0 nullPtr 0
        (-1) (-1) (-1)
        (if hasOlder then 1 else 0)
        (if hasNewer then 1 else 0)
        nullPtr 0

sessionTurnFailure
    :: FunPtr SessionTurnCallback -> Ptr () -> Text -> IO ()
sessionTurnFailure callback context err =
    withText err \errorPointer errorLength ->
        invokeSessionTurnCallback callback context (-1) (-1)
            nullPtr 0 nullPtr 0 nullPtr 0 nullPtr 0 nullPtr 0 nullPtr 0
            nullPtr 0 (-1) (-1) (-1) 0 0 errorPointer errorLength

transcriptEffectName :: TranscriptEffect -> Text
transcriptEffectName = \case
    TranscriptAppend -> "append"
    TranscriptReplace -> "replace"
    TranscriptReset -> "reset"

completeBoundarySessionResult
    :: FunPtr SessionTransferResultCallback
    -> Ptr ()
    -> Either SomeException (Either Text (Maybe Text, Text))
    -> IO ()
completeBoundarySessionResult callback context = \case
    Left exception ->
        emitSessionResult callback context (-1) Nothing
            (Just (Text.pack (show exception)))
    Right (Left err) ->
        emitSessionResult callback context (-1) Nothing (Just err)
    Right (Right (gatewayIdentity, sessionId)) ->
        emitForNativeGatewayBoundary
            gatewayIdentity
            (emitSessionResult callback context 0 (Just sessionId) Nothing)
            >>= \case
                Left err ->
                    emitSessionResult
                        callback context (-1) Nothing (Just err)
                Right () -> pure ()

emitSessionResult
    :: FunPtr SessionTransferResultCallback
    -> Ptr ()
    -> CInt
    -> Maybe Text
    -> Maybe Text
    -> IO ()
emitSessionResult callback context status sessionId err =
    withOptionalText sessionId \sessionPointer sessionLength ->
    withOptionalText err \errorPointer errorLength ->
        invokeSessionTransferResultCallback callback context status
            sessionPointer sessionLength errorPointer errorLength

emitSessionExportChunk
    :: FunPtr SessionExportCallback -> Ptr () -> BS.ByteString -> IO ()
emitSessionExportChunk callback context chunk =
    BS.useAsCStringLen chunk \(pointer, length) ->
        invokeSessionExportCallback callback context 0
            (castPtr pointer) (fromIntegral length) nullPtr 0

emitSessionExportChunkForBoundary
    :: Maybe Text
    -> FunPtr SessionExportCallback
    -> Ptr ()
    -> BS.ByteString
    -> IO ()
emitSessionExportChunkForBoundary
        gatewayIdentity callback context chunk =
    emitForNativeGatewayBoundary
        gatewayIdentity
        (emitSessionExportChunk callback context chunk) >>= \case
            Left _ ->
                throwString
                    "Gateway credentials changed during session export."
            Right () -> pure ()

emitSessionExportTerminalForBoundary
    :: Maybe Text
    -> FunPtr SessionExportCallback
    -> Ptr ()
    -> IO (Either Text ())
emitSessionExportTerminalForBoundary gatewayIdentity callback context =
    emitForNativeGatewayBoundary
        gatewayIdentity
        (invokeSessionExportCallback callback
            context 1 nullPtr 0 nullPtr 0) >>= \case
                Left _ ->
                    pure
                        (Left
                            "Gateway credentials changed during session export.")
                Right () -> pure (Right ())

sessionExportFailure
    :: FunPtr SessionExportCallback -> Ptr () -> Text -> IO ()
sessionExportFailure callback context err =
    withText err \pointer length ->
        invokeSessionExportCallback callback context (-1)
            nullPtr 0 pointer length
