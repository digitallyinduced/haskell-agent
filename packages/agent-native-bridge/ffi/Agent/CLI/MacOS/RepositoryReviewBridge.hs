{-# LANGUAGE ForeignFunctionInterface #-}

-- | Native repository snapshots, diffs and mutations. RepositoryWorkers owns
-- worker cancellation and the terminal callback gate; all streamed buffers
-- remain scoped to their host callback.
module Agent.CLI.MacOS.RepositoryReviewBridge () where

import Agent.CLI.MacOS.Marshalling
    ( withText, withOptionalText, withNullableText )
import Agent.CLI.MacOS.RepositoryInput (copyRequiredText, copyRequiredTexts)
import Agent.CLI.MacOS.RepositoryWorkers
    ( cancelRepositoryWorkers, startRepositoryWorker, tryRepositorySynchronous )
import qualified Agent.CLI.RepositoryReview as RepositoryReview
import Control.Monad (forM_)
import qualified Data.ByteString as BS
import Data.Char (ord)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word (Word8)
import Foreign (FunPtr, Ptr, castPtr, nullFunPtr, nullPtr, peekByteOff, sizeOf)
import Foreign.C.String (CString)
import Foreign.C.Types (CInt(..), CLLong(..), CSize(..))

type RepositorySnapshotCallback =
    Ptr ()
    -> CString -> CSize -- snapshot id
    -> CString -> CSize -- repository root
    -> CString -> CSize -- HEAD, empty for unborn
    -> CString -> CSize -- index fingerprint
    -> CString -> CSize -- worktree fingerprint
    -> IO ()

type RepositoryFileCallback =
    Ptr ()
    -> CString -> CSize -- path
    -> CString -> CSize -- original path, null when absent
    -> CInt -- index status byte
    -> CInt -- worktree status byte
    -> IO ()

type RepositoryDiffCallback =
    Ptr () -> Ptr Word8 -> CSize -> CInt -> IO ()

type RepositoryHunkCallback =
    Ptr () -> CLLong -> CLLong -> CLLong -> CLLong
    -> CString -> CSize -> IO ()

type RepositoryResultCallback =
    Ptr () -> CInt
    -> CString -> CSize -- current snapshot id on success/stale
    -> CString -> CSize -- error
    -> IO ()

foreign import ccall "dynamic"
    invokeRepositorySnapshotCallback
        :: FunPtr RepositorySnapshotCallback -> RepositorySnapshotCallback

foreign import ccall "dynamic"
    invokeRepositoryFileCallback
        :: FunPtr RepositoryFileCallback -> RepositoryFileCallback

foreign import ccall "dynamic"
    invokeRepositoryDiffCallback
        :: FunPtr RepositoryDiffCallback -> RepositoryDiffCallback

foreign import ccall "dynamic"
    invokeRepositoryHunkCallback
        :: FunPtr RepositoryHunkCallback -> RepositoryHunkCallback

foreign import ccall "dynamic"
    invokeRepositoryResultCallback
        :: FunPtr RepositoryResultCallback -> RepositoryResultCallback

foreign export ccall ha_repository_snapshot
    :: Ptr Word8 -> CSize
    -> FunPtr RepositorySnapshotCallback
    -> FunPtr RepositoryFileCallback
    -> FunPtr RepositoryResultCallback
    -> Ptr () -> IO CInt

foreign export ccall ha_repository_diff
    :: Ptr Word8 -> CSize -> Ptr Word8 -> CSize -> CInt -> Ptr Word8 -> CSize
    -> FunPtr RepositoryDiffCallback
    -> FunPtr RepositoryHunkCallback
    -> FunPtr RepositoryResultCallback
    -> Ptr () -> IO CInt

foreign export ccall ha_repository_apply_path
    :: Ptr Word8 -> CSize -> Ptr Word8 -> CSize -> CInt
    -> Ptr Word8 -> CSize -> FunPtr RepositoryResultCallback
    -> Ptr () -> IO CInt

foreign export ccall ha_repository_apply_hunks
    :: Ptr Word8 -> CSize -> Ptr Word8 -> CSize -> CInt
    -> Ptr Word8 -> CSize -> Ptr CSize -> CSize
    -> FunPtr RepositoryResultCallback
    -> Ptr () -> IO CInt

foreign export ccall ha_repository_commit
    :: Ptr Word8 -> CSize -> Ptr Word8 -> CSize -> Ptr Word8 -> CSize
    -> FunPtr RepositoryResultCallback -> Ptr () -> IO CInt

foreign export ccall ha_repository_cancel_all :: IO ()

ha_repository_snapshot
    :: Ptr Word8 -> CSize
    -> FunPtr RepositorySnapshotCallback
    -> FunPtr RepositoryFileCallback
    -> FunPtr RepositoryResultCallback
    -> Ptr () -> IO CInt
ha_repository_snapshot pathBytes pathLength snapshotCallback fileCallback
    resultCallback context
    | snapshotCallback == nullFunPtr
        || fileCallback == nullFunPtr
        || resultCallback == nullFunPtr = pure 1
    | otherwise =
        copyRequiredText pathBytes pathLength >>= \case
            Left _ -> pure 2
            Right path -> do
                started <- startRepositoryWorker
                    (emitRepositoryCancelled resultCallback context) do
                    tryRepositorySynchronous
                        (RepositoryReview.repositorySnapshot (Text.unpack path))
                        >>= \case
                            Left exception ->
                                pure
                                    (emitRepositoryFailure
                                        resultCallback
                                        context
                                        (Text.pack (show exception)))
                            Right (Left err) ->
                                pure
                                    (emitRepositoryError
                                        resultCallback context err)
                            Right (Right snapshot) -> do
                                streamed <- tryRepositorySynchronous do
                                    withRepositorySnapshot snapshot $
                                        invokeRepositorySnapshotCallback
                                            snapshotCallback
                                            context
                                    forM_ snapshot.snapshotFiles \file ->
                                        withText
                                            (Text.pack file.repositoryFilePath)
                                            \pathPtr pathSize ->
                                        withNullableText
                                            (Text.pack
                                                <$> file.repositoryFileOriginalPath)
                                            \originalPtr originalSize ->
                                                invokeRepositoryFileCallback
                                                    fileCallback
                                                    context
                                                    pathPtr pathSize
                                                    originalPtr originalSize
                                                    (fromIntegral
                                                        (ord
                                                            file.repositoryFileIndexStatus))
                                                    (fromIntegral
                                                        (ord
                                                            file.repositoryFileWorktreeStatus))
                                case streamed of
                                    Left exception ->
                                        pure
                                            (emitRepositoryFailure
                                                resultCallback
                                                context
                                                (Text.pack (show exception)))
                                    Right () ->
                                        pure
                                            (emitRepositorySuccess
                                                resultCallback
                                                context
                                                snapshot.snapshotId)
                pure (if started then 0 else 3)

ha_repository_diff
    :: Ptr Word8 -> CSize -> Ptr Word8 -> CSize -> CInt -> Ptr Word8 -> CSize
    -> FunPtr RepositoryDiffCallback
    -> FunPtr RepositoryHunkCallback
    -> FunPtr RepositoryResultCallback
    -> Ptr () -> IO CInt
ha_repository_diff pathBytes pathLength snapshotBytes snapshotLength
    diffKind fileBytes fileLength diffCallback hunkCallback resultCallback context
    | diffCallback == nullFunPtr
        || hunkCallback == nullFunPtr
        || resultCallback == nullFunPtr = pure 1
    | otherwise =
        copyRequiredTexts
            [ (pathBytes, pathLength)
            , (snapshotBytes, snapshotLength)
            , (fileBytes, fileLength)
            ] >>= \case
                Left _ -> pure 2
                Right [path, expected, file] ->
                    case repositoryDiffKind diffKind of
                        Nothing -> pure 2
                        Just kind -> do
                            started <- startRepositoryWorker
                                (emitRepositoryCancelled resultCallback context) do
                                tryRepositorySynchronous
                                    (RepositoryReview.repositoryDiff
                                        (Text.unpack path)
                                        expected
                                        kind
                                        (Text.unpack file))
                                    >>= \case
                                        Left exception ->
                                            pure
                                                (emitRepositoryFailure
                                                    resultCallback
                                                    context
                                                    (Text.pack (show exception)))
                                        Right (Left err) ->
                                            pure
                                                (emitRepositoryError
                                                    resultCallback context err)
                                        Right (Right diff) -> do
                                            streamed <- tryRepositorySynchronous do
                                                forM_
                                                    (byteStringChunks
                                                        (64 * 1024)
                                                        diff.repositoryDiffPatch)
                                                    \chunk ->
                                                        BS.useAsCStringLen chunk
                                                            \(pointer, length) ->
                                                                invokeRepositoryDiffCallback
                                                                    diffCallback
                                                                    context
                                                                    (castPtr pointer)
                                                                    (fromIntegral length)
                                                                    (if
                                                                        diff.repositoryDiffBinary
                                                                        then 1
                                                                        else 0)
                                                forM_
                                                    diff.repositoryDiffHunks
                                                    \hunk ->
                                                        withText
                                                            hunk.hunkHeader
                                                            \headerPtr
                                                                headerLength ->
                                                                    invokeRepositoryHunkCallback
                                                                        hunkCallback
                                                                        context
                                                                        (fromIntegral hunk.hunkOldStart)
                                                                        (fromIntegral hunk.hunkOldCount)
                                                                        (fromIntegral hunk.hunkNewStart)
                                                                        (fromIntegral hunk.hunkNewCount)
                                                                        headerPtr
                                                                        headerLength
                                            case streamed of
                                                Left exception ->
                                                    pure
                                                        (emitRepositoryFailure
                                                            resultCallback
                                                            context
                                                            (Text.pack
                                                                (show exception)))
                                                Right () ->
                                                    pure
                                                        (emitRepositorySuccess
                                                            resultCallback
                                                            context
                                                            expected)
                            pure (if started then 0 else 3)
                Right _ -> pure 3

ha_repository_apply_path
    :: Ptr Word8 -> CSize -> Ptr Word8 -> CSize -> CInt
    -> Ptr Word8 -> CSize -> FunPtr RepositoryResultCallback
    -> Ptr () -> IO CInt
ha_repository_apply_path pathBytes pathLength snapshotBytes snapshotLength
    operation fileBytes fileLength callback context
    | callback == nullFunPtr = pure 1
    | otherwise =
        copyRequiredTexts
            [ (pathBytes, pathLength)
            , (snapshotBytes, snapshotLength)
            , (fileBytes, fileLength)
            ] >>= \case
                Left _ -> pure 2
                Right [path, expected, file] ->
                    case repositoryPathMutation operation (Text.unpack file) of
                        Nothing -> pure 2
                        Just mutation -> do
                            started <- startRepositoryMutation
                                callback context (Text.unpack path) expected mutation
                            pure (if started then 0 else 3)
                Right _ -> pure 3

ha_repository_apply_hunks
    :: Ptr Word8 -> CSize -> Ptr Word8 -> CSize -> CInt
    -> Ptr Word8 -> CSize -> Ptr CSize -> CSize
    -> FunPtr RepositoryResultCallback
    -> Ptr () -> IO CInt
ha_repository_apply_hunks pathBytes pathLength snapshotBytes snapshotLength
    operation fileBytes fileLength hunkIndices hunkCount callback context
    | callback == nullFunPtr = pure 1
    | hunkIndices == nullPtr || hunkCount == 0 = pure 2
    | hunkCount > 4096 = pure 2
    | fromIntegral hunkCount
        > (maxBound :: Int) `div` sizeOf (undefined :: CSize) = pure 2
    | otherwise =
        copyRequiredTexts
            [ (pathBytes, pathLength)
            , (snapshotBytes, snapshotLength)
            , (fileBytes, fileLength)
            ]
            >>= \case
                Left _ -> pure 2
                Right [path, expected, file] -> do
                    rawIndices <- mapM
                        (\index ->
                            (peekByteOff
                                    hunkIndices
                                    (index * sizeOf (undefined :: CSize))
                                    :: IO CSize))
                        [0 .. fromIntegral hunkCount - 1]
                    if any
                        ((> toInteger (maxBound :: Int)) . toInteger)
                        rawIndices
                        then pure 2
                        else
                            case repositoryHunkMutation
                                operation
                                (Text.unpack file)
                                (map fromIntegral rawIndices) of
                                    Nothing -> pure 2
                                    Just mutation -> do
                                        started <- startRepositoryMutation
                                            callback
                                            context
                                            (Text.unpack path)
                                            expected
                                            mutation
                                        pure (if started then 0 else 3)
                Right _ -> pure 3

ha_repository_commit
    :: Ptr Word8 -> CSize -> Ptr Word8 -> CSize -> Ptr Word8 -> CSize
    -> FunPtr RepositoryResultCallback -> Ptr () -> IO CInt
ha_repository_commit pathBytes pathLength snapshotBytes snapshotLength
    messageBytes messageLength callback context
    | callback == nullFunPtr = pure 1
    | otherwise =
        copyRequiredTexts
            [ (pathBytes, pathLength)
            , (snapshotBytes, snapshotLength)
            , (messageBytes, messageLength)
            ] >>= \case
                Left _ -> pure 2
                Right [path, expected, message] -> do
                    started <- startRepositoryWorker
                        (emitRepositoryCancelled callback context) $
                        prepareRepositoryResult callback context $
                            (RepositoryReview.commitRepository
                                (Text.unpack path)
                                expected
                                message)
                    pure (if started then 0 else 3)
                Right _ -> pure 3

startRepositoryMutation
    :: FunPtr RepositoryResultCallback
    -> Ptr ()
    -> FilePath
    -> Text
    -> RepositoryReview.RepositoryMutation
    -> IO Bool
startRepositoryMutation callback context path expected mutation =
    startRepositoryWorker (emitRepositoryCancelled callback context) $
        prepareRepositoryResult callback context $
            (RepositoryReview.mutateRepository path expected mutation)

prepareRepositoryResult
    :: FunPtr RepositoryResultCallback
    -> Ptr ()
    -> IO
        (Either
            RepositoryReview.RepositoryError
            RepositoryReview.RepositorySnapshot)
    -> IO (IO ())
prepareRepositoryResult callback context action =
    tryRepositorySynchronous action >>= \case
        Left exception ->
            pure
                (emitRepositoryFailure callback context
                    (Text.pack (show exception)))
        Right (Left err) ->
            pure (emitRepositoryError callback context err)
        Right (Right snapshot) ->
            pure
                (emitRepositorySuccess
                    callback context snapshot.snapshotId)

ha_repository_cancel_all :: IO ()
ha_repository_cancel_all = cancelRepositoryWorkers

repositoryPathMutation
    :: CInt -> FilePath -> Maybe RepositoryReview.RepositoryMutation
repositoryPathMutation operation path = case operation of
    0 -> Just (RepositoryReview.StagePath path)
    1 -> Just (RepositoryReview.UnstagePath path)
    2 -> Just (RepositoryReview.RestorePath path)
    _ -> Nothing

repositoryDiffKind
    :: CInt -> Maybe RepositoryReview.RepositoryDiffKind
repositoryDiffKind kind = case kind of
    0 -> Just RepositoryReview.RepositoryWorktreeDiff
    1 -> Just RepositoryReview.RepositoryStagedDiff
    _ -> Nothing

repositoryHunkMutation
    :: CInt
    -> FilePath
    -> [Int]
    -> Maybe RepositoryReview.RepositoryMutation
repositoryHunkMutation operation path hunks = case operation of
    0 -> Just (RepositoryReview.StageHunks path hunks)
    1 -> Just (RepositoryReview.UnstageHunks path hunks)
    2 -> Just (RepositoryReview.RestoreHunks path hunks)
    _ -> Nothing

withRepositorySnapshot
    :: RepositoryReview.RepositorySnapshot
    -> (CString -> CSize -> CString -> CSize -> CString -> CSize
        -> CString -> CSize -> CString -> CSize -> IO value)
    -> IO value
withRepositorySnapshot snapshot action =
    withText snapshot.snapshotId \snapshotPtr snapshotLength ->
    withText (Text.pack snapshot.snapshotRoot) \rootPtr rootLength ->
    withOptionalText snapshot.snapshotHead \headPtr headLength ->
    withText snapshot.snapshotIndexFingerprint \indexPtr indexLength ->
    withText snapshot.snapshotWorktreeFingerprint
        \worktreePtr worktreeLength ->
            action snapshotPtr snapshotLength rootPtr rootLength
                headPtr headLength indexPtr indexLength
                worktreePtr worktreeLength

emitRepositorySuccess
    :: FunPtr RepositoryResultCallback -> Ptr () -> Text -> IO ()
emitRepositorySuccess callback context snapshotId =
    withText snapshotId \snapshotPtr snapshotLength ->
        invokeRepositoryResultCallback callback context 0
            snapshotPtr snapshotLength nullPtr 0

emitRepositoryFailure
    :: FunPtr RepositoryResultCallback -> Ptr () -> Text -> IO ()
emitRepositoryFailure callback context message =
    withText message \errorPtr errorLength ->
        invokeRepositoryResultCallback callback context (-1)
            nullPtr 0 errorPtr errorLength

emitRepositoryCancelled
    :: FunPtr RepositoryResultCallback -> Ptr () -> IO ()
emitRepositoryCancelled callback context =
    withText "cancelled" \errorPtr errorLength ->
        invokeRepositoryResultCallback callback context (-3)
            nullPtr 0 errorPtr errorLength

emitRepositoryError
    :: FunPtr RepositoryResultCallback
    -> Ptr ()
    -> RepositoryReview.RepositoryError
    -> IO ()
emitRepositoryError callback context err =
    case err of
        RepositoryReview.StaleRepositorySnapshot _ actual ->
            withText actual \snapshotPtr snapshotLength ->
            withText (RepositoryReview.repositoryErrorText err)
                \errorPtr errorLength ->
                    invokeRepositoryResultCallback callback context (-2)
                        snapshotPtr snapshotLength errorPtr errorLength
        _ ->
            emitRepositoryFailure
                callback context (RepositoryReview.repositoryErrorText err)

byteStringChunks :: Int -> BS.ByteString -> [BS.ByteString]
byteStringChunks size bytes
    | BS.null bytes = []
    | otherwise =
        let (chunk, remaining) = BS.splitAt size bytes
        in chunk : byteStringChunks size remaining
