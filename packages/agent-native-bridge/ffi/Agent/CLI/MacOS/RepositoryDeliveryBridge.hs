{-# LANGUAGE ForeignFunctionInterface #-}

-- | Native delivery previews and confirmed push/pull-request operations.
-- Each worker retains its existing single terminal-callback gate.
module Agent.CLI.MacOS.RepositoryDeliveryBridge () where

import Agent.CLI.MacOS.Marshalling (withText)
import Agent.CLI.MacOS.RepositoryInput (copyRequiredTexts)
import Agent.CLI.MacOS.RepositoryWorkers
    ( startRepositoryWorker, tryRepositorySynchronous )
import qualified Agent.CLI.RepositoryDelivery as RepositoryDelivery
import Control.Concurrent (MVar, modifyMVar, newMVar)
import Control.Exception.Safe (mask)
import Control.Monad (when)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word (Word8)
import Foreign (FunPtr, Ptr, nullFunPtr, nullPtr)
import Foreign.C.String (CString)
import Foreign.C.Types (CInt(..), CLLong(..), CSize(..))

type RepositoryDeliveryStatusCallback =
    Ptr () -> CInt
    -> CString -> CSize -> CString -> CSize
    -> CString -> CSize -> CString -> CSize
    -> CString -> CSize -> CString -> CSize
    -> CLLong -> CLLong -> CString -> CSize -> IO ()

type RepositoryPushPreviewCallback =
    Ptr () -> CInt -> CString -> CSize -> CLLong
    -> CString -> CSize -> CString -> CSize
    -> CString -> CSize -> CString -> CSize
    -> CLLong -> CLLong -> CString -> CSize -> IO ()

type RepositoryPushResultCallback =
    Ptr () -> CInt -> CString -> CSize -> CString -> CSize
    -> CString -> CSize -> IO ()

type RepositoryPullRequestPreviewCallback =
    Ptr () -> CInt -> CString -> CSize -> CLLong
    -> CString -> CSize -> CString -> CSize
    -> CString -> CSize -> CString -> CSize
    -> CString -> CSize -> IO ()

type RepositoryPullRequestResultCallback =
    Ptr () -> CInt -> CString -> CSize -> CString -> CSize -> IO ()

foreign import ccall "dynamic"
    invokeRepositoryDeliveryStatusCallback
        :: FunPtr RepositoryDeliveryStatusCallback
        -> RepositoryDeliveryStatusCallback

foreign import ccall "dynamic"
    invokeRepositoryPushPreviewCallback
        :: FunPtr RepositoryPushPreviewCallback
        -> RepositoryPushPreviewCallback

foreign import ccall "dynamic"
    invokeRepositoryPushResultCallback
        :: FunPtr RepositoryPushResultCallback
        -> RepositoryPushResultCallback

foreign import ccall "dynamic"
    invokeRepositoryPullRequestPreviewCallback
        :: FunPtr RepositoryPullRequestPreviewCallback
        -> RepositoryPullRequestPreviewCallback

foreign import ccall "dynamic"
    invokeRepositoryPullRequestResultCallback
        :: FunPtr RepositoryPullRequestResultCallback
        -> RepositoryPullRequestResultCallback

foreign export ccall ha_repository_delivery_status
    :: Ptr Word8 -> CSize -> Ptr Word8 -> CSize
    -> FunPtr RepositoryDeliveryStatusCallback -> Ptr () -> IO CInt

foreign export ccall ha_repository_push_preview
    :: Ptr Word8 -> CSize -> Ptr Word8 -> CSize
    -> FunPtr RepositoryPushPreviewCallback -> Ptr () -> IO CInt

foreign export ccall ha_repository_push_confirm
    :: Ptr Word8 -> CSize -> Ptr Word8 -> CSize
    -> FunPtr RepositoryPushResultCallback -> Ptr () -> IO CInt

foreign export ccall ha_repository_pr_preview
    :: Ptr Word8 -> CSize -> Ptr Word8 -> CSize
    -> Ptr Word8 -> CSize -> Ptr Word8 -> CSize -> Ptr Word8 -> CSize
    -> FunPtr RepositoryPullRequestPreviewCallback -> Ptr () -> IO CInt

foreign export ccall ha_repository_pr_confirm
    :: Ptr Word8 -> CSize -> Ptr Word8 -> CSize
    -> FunPtr RepositoryPullRequestResultCallback -> Ptr () -> IO CInt

ha_repository_delivery_status
    :: Ptr Word8 -> CSize -> Ptr Word8 -> CSize
    -> FunPtr RepositoryDeliveryStatusCallback -> Ptr () -> IO CInt
ha_repository_delivery_status pathBytes pathLength snapshotBytes snapshotLength
    callback context
    | callback == nullFunPtr = pure 1
    | not (deliveryInputValid pathBytes pathLength deliveryPathLimit)
        || not (deliveryInputValid
            snapshotBytes snapshotLength deliveryTokenLimit) = pure 2
    | otherwise =
        copyRequiredTexts
            [(pathBytes, pathLength), (snapshotBytes, snapshotLength)] >>= \case
                Right [path, snapshot] -> do
                    terminal <- newMVar False
                    started <- startRepositoryWorker
                        (emitDeliveryOnce terminal $
                            emitDeliveryStatusFailure callback context (-3)
                                "repository delivery was cancelled") $
                        prepareDeliveryResult terminal
                            (RepositoryDelivery.repositoryDeliveryStatus
                                (Text.unpack path)
                                snapshot)
                            (emitDeliveryStatusFailure callback context (-1)
                                "repository delivery failed")
                            (emitDeliveryStatusFailure callback context)
                            (emitDeliveryStatus callback context)
                    pure (if started then 0 else 3)
                _ -> pure 2

ha_repository_push_preview
    :: Ptr Word8 -> CSize -> Ptr Word8 -> CSize
    -> FunPtr RepositoryPushPreviewCallback -> Ptr () -> IO CInt
ha_repository_push_preview pathBytes pathLength snapshotBytes snapshotLength
    callback context
    | callback == nullFunPtr = pure 1
    | not (deliveryInputValid pathBytes pathLength deliveryPathLimit)
        || not (deliveryInputValid
            snapshotBytes snapshotLength deliveryTokenLimit) = pure 2
    | otherwise =
        copyRequiredTexts
            [(pathBytes, pathLength), (snapshotBytes, snapshotLength)] >>= \case
                Right [path, snapshot] -> do
                    terminal <- newMVar False
                    started <- startRepositoryWorker
                        (emitDeliveryOnce terminal $
                            emitPushPreviewFailure callback context (-3)
                                "repository push preview was cancelled") $
                        prepareDeliveryResult terminal
                            (RepositoryDelivery.previewRepositoryPush
                                (Text.unpack path)
                                snapshot)
                            (emitPushPreviewFailure callback context (-1)
                                "repository push preview failed")
                            (emitPushPreviewFailure callback context)
                            (emitPushPreview callback context)
                    pure (if started then 0 else 3)
                _ -> pure 2

ha_repository_push_confirm
    :: Ptr Word8 -> CSize -> Ptr Word8 -> CSize
    -> FunPtr RepositoryPushResultCallback -> Ptr () -> IO CInt
ha_repository_push_confirm pathBytes pathLength tokenBytes tokenLength
    callback context
    | callback == nullFunPtr = pure 1
    | not (deliveryInputValid pathBytes pathLength deliveryPathLimit)
        || not (deliveryInputValid tokenBytes tokenLength deliveryTokenLimit) =
            pure 2
    | otherwise =
        copyRequiredTexts
            [(pathBytes, pathLength), (tokenBytes, tokenLength)] >>= \case
                Right [path, token] -> do
                    terminal <- newMVar False
                    started <- startRepositoryWorker
                        (emitDeliveryOnce terminal $
                            emitPushResultFailure callback context (-3)
                                "repository push was cancelled") $
                        prepareDeliveryResult terminal
                            (RepositoryDelivery.confirmRepositoryPush
                                (Text.unpack path)
                                token)
                            (emitPushResultFailure callback context (-1)
                                "repository push failed")
                            (emitPushResultFailure callback context)
                            (\status ->
                                        withText status.deliverySnapshotId
                                            \snapshotPtr snapshotSize ->
                                        withText status.deliveryHeadOid
                                            \headPtr headSize ->
                                                invokeRepositoryPushResultCallback
                                                    callback context 0
                                                    snapshotPtr snapshotSize
                                                    headPtr headSize
                                                    nullPtr 0)
                    pure (if started then 0 else 3)
                _ -> pure 2

ha_repository_pr_preview
    :: Ptr Word8 -> CSize -> Ptr Word8 -> CSize
    -> Ptr Word8 -> CSize -> Ptr Word8 -> CSize -> Ptr Word8 -> CSize
    -> FunPtr RepositoryPullRequestPreviewCallback -> Ptr () -> IO CInt
ha_repository_pr_preview pathBytes pathLength snapshotBytes snapshotLength
    baseBytes baseLength titleBytes titleLength bodyBytes bodyLength
    callback context
    | callback == nullFunPtr = pure 1
    | not (deliveryInputValid pathBytes pathLength deliveryPathLimit)
        || not (deliveryInputValid
            snapshotBytes snapshotLength deliveryTokenLimit)
        || not (deliveryInputValid baseBytes baseLength deliveryRefLimit)
        || not (deliveryInputValid titleBytes titleLength deliveryTitleLimit)
        || not (deliveryInputValid bodyBytes bodyLength deliveryBodyLimit) =
            pure 2
    | otherwise =
        copyRequiredTexts
            [ (pathBytes, pathLength)
            , (snapshotBytes, snapshotLength)
            , (baseBytes, baseLength)
            , (titleBytes, titleLength)
            , (bodyBytes, bodyLength)
            ] >>= \case
                Right [path, snapshot, base, title, body] -> do
                    terminal <- newMVar False
                    started <- startRepositoryWorker
                        (emitDeliveryOnce terminal $
                            emitPullRequestPreviewFailure
                                callback context (-3)
                                "pull-request preview was cancelled") $
                        prepareDeliveryResult terminal
                            (RepositoryDelivery.previewPullRequest
                                (Text.unpack path)
                                snapshot base title body)
                            (emitPullRequestPreviewFailure
                                callback context (-1)
                                "pull-request preview failed")
                            (emitPullRequestPreviewFailure callback context)
                            (emitPullRequestPreview callback context)
                    pure (if started then 0 else 3)
                _ -> pure 2

ha_repository_pr_confirm
    :: Ptr Word8 -> CSize -> Ptr Word8 -> CSize
    -> FunPtr RepositoryPullRequestResultCallback -> Ptr () -> IO CInt
ha_repository_pr_confirm pathBytes pathLength tokenBytes tokenLength
    callback context
    | callback == nullFunPtr = pure 1
    | not (deliveryInputValid pathBytes pathLength deliveryPathLimit)
        || not (deliveryInputValid tokenBytes tokenLength deliveryTokenLimit) =
            pure 2
    | otherwise =
        copyRequiredTexts
            [(pathBytes, pathLength), (tokenBytes, tokenLength)] >>= \case
                Right [path, token] -> do
                    terminal <- newMVar False
                    started <- startRepositoryWorker
                        (emitDeliveryOnce terminal $
                            emitPullRequestResultFailure callback context (-3)
                                "pull-request creation was cancelled") $
                        prepareDeliveryResult terminal
                            (RepositoryDelivery.createPullRequest
                                (Text.unpack path)
                                token)
                            (emitPullRequestResultFailure callback context (-1)
                                "pull-request creation failed")
                            (emitPullRequestResultFailure callback context)
                            (\url ->
                                        withText url \urlPtr urlSize ->
                                            invokeRepositoryPullRequestResultCallback
                                                callback context 0
                                                urlPtr urlSize nullPtr 0)
                    pure (if started then 0 else 3)
                _ -> pure 2

emitDeliveryOnce :: MVar Bool -> IO () -> IO ()
emitDeliveryOnce terminal callback =
    mask \_ -> do
        shouldRun <- modifyMVar terminal \completed ->
            pure (True, not completed)
        when shouldRun callback

prepareDeliveryResult
    :: MVar Bool
    -> IO (Either RepositoryDelivery.DeliveryError value)
    -> IO ()
    -> (CInt -> Text -> IO ())
    -> (value -> IO ())
    -> IO (IO ())
prepareDeliveryResult terminal operation unexpectedFailure knownFailure success =
    tryRepositorySynchronous operation >>= \case
        Left _ ->
            pure (emitDeliveryOnce terminal unexpectedFailure)
        Right (Left err) ->
            pure
                (emitDeliveryOnce terminal $
                    knownFailure
                        (deliveryErrorStatus err)
                        (RepositoryDelivery.deliveryErrorText err))
        Right (Right value) ->
            pure (emitDeliveryOnce terminal (success value))

emitDeliveryStatus
    :: FunPtr RepositoryDeliveryStatusCallback
    -> Ptr ()
    -> RepositoryDelivery.DeliveryStatus
    -> IO ()
emitDeliveryStatus callback context status =
    withText status.deliverySnapshotId \snapshotPtr snapshotSize ->
    withText status.deliveryHeadOid \headPtr headSize ->
    withText status.deliveryBranch \branchPtr branchSize ->
    withText status.deliveryRemote \remotePtr remoteSize ->
    withText status.deliveryUpstreamRef \upstreamPtr upstreamSize ->
    withText status.deliveryUpstreamOid \upstreamOidPtr upstreamOidSize ->
        invokeRepositoryDeliveryStatusCallback
            callback context 0
            snapshotPtr snapshotSize headPtr headSize
            branchPtr branchSize remotePtr remoteSize
            upstreamPtr upstreamSize upstreamOidPtr upstreamOidSize
            (fromIntegral status.deliveryAhead)
            (fromIntegral status.deliveryBehind)
            nullPtr 0

emitDeliveryStatusFailure
    :: FunPtr RepositoryDeliveryStatusCallback
    -> Ptr () -> CInt -> Text -> IO ()
emitDeliveryStatusFailure callback context status message =
    withText message \errorPtr errorSize ->
        invokeRepositoryDeliveryStatusCallback
            callback context status
            nullPtr 0 nullPtr 0 nullPtr 0 nullPtr 0
            nullPtr 0 nullPtr 0 0 0 errorPtr errorSize

emitPushPreview
    :: FunPtr RepositoryPushPreviewCallback
    -> Ptr ()
    -> RepositoryDelivery.PushPreview
    -> IO ()
emitPushPreview callback context preview =
    let status = preview.pushPreviewStatus
        expires = fromIntegral
            (floor preview.pushPreviewExpiresAt :: Integer)
    in withText preview.pushPreviewConfirmation \tokenPtr tokenSize ->
    withText status.deliveryHeadOid \headPtr headSize ->
    withText status.deliveryBranch \branchPtr branchSize ->
    withText status.deliveryRemote \remotePtr remoteSize ->
    withText status.deliveryUpstreamRef \upstreamPtr upstreamSize ->
        invokeRepositoryPushPreviewCallback
            callback context 0 tokenPtr tokenSize expires
            headPtr headSize branchPtr branchSize remotePtr remoteSize
            upstreamPtr upstreamSize
            (fromIntegral status.deliveryAhead)
            (fromIntegral status.deliveryBehind)
            nullPtr 0

emitPushPreviewFailure
    :: FunPtr RepositoryPushPreviewCallback
    -> Ptr () -> CInt -> Text -> IO ()
emitPushPreviewFailure callback context status message =
    withText message \errorPtr errorSize ->
        invokeRepositoryPushPreviewCallback
            callback context status nullPtr 0 0
            nullPtr 0 nullPtr 0 nullPtr 0 nullPtr 0
            0 0 errorPtr errorSize

emitPushResultFailure
    :: FunPtr RepositoryPushResultCallback
    -> Ptr () -> CInt -> Text -> IO ()
emitPushResultFailure callback context status message =
    withText message \errorPtr errorSize ->
        invokeRepositoryPushResultCallback
            callback context status
            nullPtr 0 nullPtr 0 errorPtr errorSize

emitPullRequestPreview
    :: FunPtr RepositoryPullRequestPreviewCallback
    -> Ptr ()
    -> RepositoryDelivery.PullRequestPreview
    -> IO ()
emitPullRequestPreview callback context preview =
    let expires = fromIntegral
            (floor preview.pullRequestExpiresAt :: Integer)
    in withText preview.pullRequestConfirmation \tokenPtr tokenSize ->
    withText preview.pullRequestRepository \repositoryPtr repositorySize ->
    withText preview.pullRequestBaseRef \basePtr baseSize ->
    withText preview.pullRequestHeadRef \headPtr headSize ->
    withText preview.pullRequestTitle \titlePtr titleSize ->
        invokeRepositoryPullRequestPreviewCallback
            callback context 0 tokenPtr tokenSize expires
            repositoryPtr repositorySize basePtr baseSize
            headPtr headSize titlePtr titleSize nullPtr 0

emitPullRequestPreviewFailure
    :: FunPtr RepositoryPullRequestPreviewCallback
    -> Ptr () -> CInt -> Text -> IO ()
emitPullRequestPreviewFailure callback context status message =
    withText message \errorPtr errorSize ->
        invokeRepositoryPullRequestPreviewCallback
            callback context status nullPtr 0 0
            nullPtr 0 nullPtr 0 nullPtr 0 nullPtr 0 errorPtr errorSize

emitPullRequestResultFailure
    :: FunPtr RepositoryPullRequestResultCallback
    -> Ptr () -> CInt -> Text -> IO ()
emitPullRequestResultFailure callback context status message =
    withText message \errorPtr errorSize ->
        invokeRepositoryPullRequestResultCallback
            callback context status nullPtr 0 errorPtr errorSize

deliveryErrorStatus :: RepositoryDelivery.DeliveryError -> CInt
deliveryErrorStatus = \case
    RepositoryDelivery.DeliveryStale _ -> -2
    RepositoryDelivery.DeliveryConfirmationRejected _ -> -4
    _ -> -1

deliveryInputValid :: Ptr Word8 -> CSize -> Int -> Bool
deliveryInputValid pointer length limit =
    pointer /= nullPtr
        && length > 0
        && toInteger length <= toInteger limit

deliveryPathLimit, deliveryTokenLimit, deliveryRefLimit :: Int
deliveryPathLimit = 16 * 1024
deliveryTokenLimit = 4 * 1024
deliveryRefLimit = 1024

deliveryTitleLimit, deliveryBodyLimit :: Int
deliveryTitleLimit = 4 * 1024
deliveryBodyLimit = 1024 * 1024
