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
import Control.Exception.Safe (mask, tryAny)
import Agent.CLI.MacOS.NativeGatewayBoundary (withNativeSessionBoundary, validateNativeSessionBoundary)
import Agent.CLI.MacOS.SessionTransferBridge (withNativeSessionStore)
import Agent.CLI.Session
    ( SessionMeta(..), SessionTurnPage(..), isValidSessionId
    , loadSessionHistorySnapshot, loadSessionHistoryTurnsRangeBounded )
import Agent.CLI.Session.PullRequest (sessionTurnPullRequestURL, sessionTurnPullRequestURLs)
import Agent.Store.Postgres.Connection (StorePool)
import Agent.Store.Types (renderStoreError)
import qualified Agent.Store.Postgres.Session as PRStore
import qualified Data.List as PRList
import Data.Maybe (mapMaybe)
import System.OsPath (OsPath, decodeFS)
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

-- Narrow asynchronous read-only PR/CI surface; no JSON crosses the ABI.
type RepositoryPullRequestStatusCallback =
    Ptr () -> CInt -> CLLong -> CString -> CSize -> CInt -> CInt -> IO ()

foreign import ccall "dynamic"
    invokeRepositoryPullRequestStatusCallback
        :: FunPtr RepositoryPullRequestStatusCallback -> RepositoryPullRequestStatusCallback

foreign export ccall ha_repository_pr_status
    :: Ptr Word8 -> CSize -> FunPtr RepositoryPullRequestStatusCallback -> Ptr () -> IO CInt

ha_repository_pr_status
    :: Ptr Word8 -> CSize -> FunPtr RepositoryPullRequestStatusCallback -> Ptr () -> IO CInt
ha_repository_pr_status pathBytes pathLength callback context
    | callback == nullFunPtr = pure 1
    | not (deliveryInputValid pathBytes pathLength deliveryPathLimit) = pure 2
    | otherwise = copyRequiredTexts [(pathBytes, pathLength)] >>= \case
        Right [path] -> do
            terminal <- newMVar False
            let empty status = invokeRepositoryPullRequestStatusCallback callback context status 0 nullPtr 0 0 0
                emit Nothing = empty 1
                emit (Just pr) = withText pr.repositoryPullRequestUrl \url size ->
                    invokeRepositoryPullRequestStatusCallback callback context 0
                        (fromIntegral pr.repositoryPullRequestNumber) url size
                        (fromIntegral pr.repositoryPullRequestState)
                        (fromIntegral pr.repositoryPullRequestCI)
            started <- startRepositoryWorker
                (emitDeliveryOnce terminal (empty (-3))) $
                prepareDeliveryResult terminal
                    (RepositoryDelivery.repositoryPullRequest (Text.unpack path))
                    (empty (-1)) (\_ _ -> empty (-1)) emit
            pure (if started then 0 else 3)
        _ -> pure 2


-- One item callback per PR followed by exactly one terminal callback. All
-- access to session history is subject to the native gateway boundary.
foreign export ccall ha_session_pr_status
    :: Ptr Word8 -> CSize -> FunPtr RepositoryPullRequestStatusCallback -> Ptr () -> IO CInt

ha_session_pr_status
    :: Ptr Word8 -> CSize -> FunPtr RepositoryPullRequestStatusCallback -> Ptr () -> IO CInt
ha_session_pr_status sessionBytes sessionLength callback context
    | callback == nullFunPtr = pure 1
    | not (deliveryInputValid sessionBytes sessionLength 1024) = pure 2
    | otherwise = copyRequiredTexts [(sessionBytes, sessionLength)] >>= \case
        Right [sessionId] | isValidSessionId sessionId -> do
            terminal <- newMVar False
            let finish status = emitDeliveryOnce terminal $
                    invokeRepositoryPullRequestStatusCallback callback context status 0 nullPtr 0 0 0
                emit pr = withText pr.repositoryPullRequestUrl \url size ->
                    invokeRepositoryPullRequestStatusCallback callback context 0
                        (fromIntegral pr.repositoryPullRequestNumber) url size
                        (fromIntegral pr.repositoryPullRequestState) (fromIntegral pr.repositoryPullRequestCI)
            started <- startRepositoryWorker (finish (-3)) do
                result <- tryAny $ withNativeSessionStore \pool root ->
                    withNativeSessionBoundary pool root sessionId \identity meta -> do
                        loaded <- indexSessionPullRequests pool root sessionId
                        case loaded of
                            Left err -> pure (Left err)
                            Right urls -> do
                                -- Use the session cwd for fallback only. URL lookup must
                                -- continue working after a worktree has been removed.
                                cwd <- decodeFS meta.metaCwd
                                prs <- if null urls
                                    then do
                                        RepositoryDelivery.repositoryPullRequest cwd >>= \case
                                            Right pr -> pure (Right (maybe [] pure pr))
                                            Left err -> pure (Left (RepositoryDelivery.deliveryErrorText err))
                                    else Right <$> mapM (resolve cwd) (take 20 urls)
                                validateNativeSessionBoundary pool root identity sessionId >>= \case
                                    Left err -> pure (Left err)
                                    Right _ -> pure prs
                pure $ case result of
                    Right (Right prs) -> mapM_ emit prs >> finish 1
                    _ -> finish (-1)
            pure (if started then 0 else 3)
        _ -> pure 2
  where
    resolve directory url = RepositoryDelivery.pullRequestByURL directory url >>= \case
        Right pr -> pure pr
        Left _ -> pure (RepositoryDelivery.RepositoryPullRequest
            (case reads (Text.unpack (Text.takeWhileEnd (/= '/') url)) of
                [(n, "")] -> n
                _ -> 0) url 0 0)

indexSessionPullRequests :: StorePool -> OsPath -> Text -> IO (Either Text [Text])
indexSessionPullRequests pool root sessionId =
    loadSessionHistorySnapshot pool root sessionId >>= \case
        Left err -> pure (Left err)
        Right (_, _, total) -> PRStore.loadSessionPullRequests pool sessionId >>= \case
            Left err -> pure (Left (renderStoreError err))
            Right cached -> do
                let (cursor, urls) = case cached of
                        Just value@(next, _) | next <= total -> value
                        _ -> (0, [])
                scan total cursor urls >>= \case
                    Left err -> pure (Left err)
                    Right associated -> prioritizeLatest total associated
  where
    -- Old caches contain all associations, but ordered prompt evidence before
    -- newly created PRs. Re-read the latest associated turn even when the
    -- cursor is already current, skipping unrelated turns in bounded pages.
    prioritizeLatest _ [] = pure (Right [])
    prioritizeLatest end urls
        | end <= 0 = pure (Right urls)
        | otherwise =
            let start = max 0 (end - 32)
            in loadSessionHistoryTurnsRangeBounded pool root sessionId start end 32 >>= \case
                Left err -> pure (Left err)
                Right page -> case page.pageTurns of
                    [] -> pure (Left "incomplete PR association history")
                    turns -> case mapMaybe (sessionTurnPullRequestURL . snd) (reverse turns) of
                        url : _ -> pure (Right (PRList.nub (url : urls)))
                        [] -> prioritizeLatest start urls
    scan total cursor urls
        | cursor >= total = pure (Right urls)
        | otherwise = loadSessionHistoryTurnsRangeBounded pool root sessionId cursor total 32 >>= \case
            Left err -> pure (Left err)
            Right page -> case page.pageTurns of
                [] -> pure (Left "incomplete PR association history")
                turns -> do
                    let next = 1 + maximum (map fst turns)
                        discovered = concatMap (sessionTurnPullRequestURLs . snd) (reverse turns)
                        associated = PRList.nub (discovered <> urls)
                    PRStore.saveSessionPullRequests pool sessionId next associated >>= \case
                        Left err -> pure (Left (renderStoreError err))
                        Right () -> scan total next associated
