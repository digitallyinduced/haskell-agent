-- | Session-scoped speculative file reads for mutating tools. The write
-- itself always waits until consume after approval.
module Agent.Tools.FileSystem.FilePrefetch
    ( FilePrefetch
    , FileCallState(..)
    , PrefetchedFile(..)
    , PathProgress(..)
    , emptyFileCallState
    , newFilePrefetch
    , closeFilePrefetch
    , refreshFileCall
    , consumePrefetchedFile
    , closeFileCall
    , waitForFilePrefetch
    , jsonStringFieldProgress
    ) where

import Agent.OsPath (fromText)
import Agent.Tools.FileSystem.PathPrefix
    ( FileFingerprint(..)
    , PathProgress(..)
    , cancelAndJoin
    , fileFingerprint
    , maxSpeculativeReadBytes
    , maximumConcurrentSpeculativeTasks
    , minimumPredictionPrefix
    , uniqueWorkspaceCandidate
    , workspaceFileIndex
    , jsonStringFieldProgress
    )
import Agent.Tools.IO
    ( readTextFile
    , resolveUnderCwd
    , resolveUnderCwdWithoutAccessRequest
    )
import Agent.Tools.Types (ToolEnv(..))
import Control.Concurrent.Async
    ( Async
    , asyncWithUnmask
    , waitCatch
    )
import Control.Concurrent.MVar
    ( MVar
    , modifyMVar
    , modifyMVar_
    , newMVar
    , readMVar
    )
import Control.Exception (evaluate)
import Control.Exception.Safe (mask)
import Control.Monad (forM_, void)
import Data.Maybe (fromMaybe, isNothing)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import System.OsPath (OsPath, equalFilePath)

data FilePrefetch = FilePrefetch
    { prefetchEnv :: !ToolEnv
    , prefetchState :: !(MVar PrefetchState)
    }

data PrefetchState = PrefetchState
    { prefetchClosed :: !Bool
    , prefetchFiles :: !(Maybe (Set.Set Text))
    , prefetchIndexTask :: !(Maybe (Async ()))
    , prefetchNextKey :: !Int
    , prefetchActive :: !(Map.Map Int (Async (Maybe PrefetchedFile)))
    }

data PrefetchedFile = PrefetchedFile
    { prefetchedPath :: !OsPath
    , prefetchedFingerprint :: !FileFingerprint
    , prefetchedContent :: !Text
    }

data FileCandidate = FileCandidate
    { fileCandidateTarget :: !Text
    , fileCandidateKey :: !Int
    , fileCandidateTask :: !(Async (Maybe PrefetchedFile))
    }

data FileCallState = FileCallState
    { fileCallArguments :: !Text
    , fileCallCandidate :: !(Maybe FileCandidate)
    }

emptyFileCallState :: FileCallState
emptyFileCallState =
    FileCallState
        { fileCallArguments = ""
        , fileCallCandidate = Nothing
        }

newFilePrefetch :: ToolEnv -> IO FilePrefetch
newFilePrefetch env = do
    state <- newMVar PrefetchState
        { prefetchClosed = False
        , prefetchFiles = Nothing
        , prefetchIndexTask = Nothing
        , prefetchNextKey = 0
        , prefetchActive = Map.empty
        }
    pure FilePrefetch { prefetchEnv = env, prefetchState = state }

closeFilePrefetch :: FilePrefetch -> IO ()
closeFilePrefetch prefetch = mask \_ -> do
    (indexTask, active) <-
        modifyMVar prefetch.prefetchState \current ->
            pure
                ( current
                    { prefetchClosed = True
                    , prefetchIndexTask = Nothing
                    , prefetchActive = Map.empty
                    }
                , (current.prefetchIndexTask, Map.elems current.prefetchActive)
                )
    mapM_ cancelAndJoin indexTask
    mapM_ cancelAndJoin active

refreshFileCall
    :: FilePrefetch
    -> FileCallState
    -> Text
    -> Maybe PathProgress
    -> IO FileCallState
refreshFileCall prefetch partial arguments progress = do
    let next = partial { fileCallArguments = arguments }
    startFileIndex prefetch
    refreshed <- refreshFileCandidate prefetch next progress
    pending <- pendingFileIndex prefetch refreshed progress
    case pending of
        Nothing -> pure refreshed
        Just indexTask -> do
            void (waitCatch indexTask)
            refreshFileCandidate prefetch refreshed progress

closeFileCall :: FilePrefetch -> FileCallState -> IO ()
closeFileCall prefetch =
    mapM_ (cancelFileCandidate prefetch) . (.fileCallCandidate)

consumePrefetchedFile
    :: FilePrefetch
    -> Text
    -> FileCallState
    -> IO (Maybe (OsPath, Text))
consumePrefetchedFile prefetch target partial =
    case partial.fileCallCandidate of
        Nothing -> pure Nothing
        Just selected ->
            waitCatch selected.fileCandidateTask >>= \case
                Left _ -> miss selected
                Right Nothing -> miss selected
                Right (Just prefetched) ->
                    resolveUnderCwd
                        prefetch.prefetchEnv
                        (fromText target) >>= \case
                            Left _ -> miss selected
                            Right finalPath
                                | not (equalFilePath finalPath prefetched.prefetchedPath) ->
                                    miss selected
                                | otherwise ->
                                    fileFingerprint finalPath >>= \case
                                        Just current
                                            | current == prefetched.prefetchedFingerprint -> do
                                                releaseFileCandidate prefetch selected
                                                pure $ Just
                                                    ( prefetched.prefetchedPath
                                                    , prefetched.prefetchedContent
                                                    )
                                        _ -> miss selected
  where
    miss selected = do
        cancelFileCandidate prefetch selected
        pure Nothing

waitForFilePrefetch :: FilePrefetch -> IO ()
waitForFilePrefetch prefetch = do
    initial <- readMVar prefetch.prefetchState
    mapM_ (void . waitCatch) initial.prefetchIndexTask
    current <- readMVar prefetch.prefetchState
    mapM_ (void . waitCatch) (Map.elems current.prefetchActive)

refreshFileCandidate
    :: FilePrefetch
    -> FileCallState
    -> Maybe PathProgress
    -> IO FileCallState
refreshFileCandidate prefetch partial progress = mask \_ -> do
    current <- readMVar prefetch.prefetchState
    let desired = desiredFileTarget current.prefetchFiles progress
    case (partial.fileCallCandidate, desired) of
        (Just existing, Just target)
            | existing.fileCandidateTarget == target ->
                pure partial
        (Just existing, Nothing)
            | candidateStillMatches progress existing.fileCandidateTarget ->
                pure partial
        (existing, next) -> do
            forM_ existing (cancelFileCandidate prefetch)
            nextCandidate <-
                case next of
                    Nothing -> pure Nothing
                    Just target -> startFileCandidate prefetch target
            pure partial { fileCallCandidate = nextCandidate }

pendingFileIndex
    :: FilePrefetch
    -> FileCallState
    -> Maybe PathProgress
    -> IO (Maybe (Async ()))
pendingFileIndex prefetch partial progress
    | not (isNothing partial.fileCallCandidate) = pure Nothing
    | otherwise =
        case progress of
            Just (PathPrefix prefix)
                | Text.length prefix >= minimumPredictionPrefix ->
                    (.prefetchIndexTask) <$> readMVar prefetch.prefetchState
            _ -> pure Nothing

desiredFileTarget
    :: Maybe (Set.Set Text)
    -> Maybe PathProgress
    -> Maybe Text
desiredFileTarget _ Nothing = Nothing
desiredFileTarget _ (Just (PathComplete target))
    | Text.null target = Nothing
    | otherwise = Just target
desiredFileTarget files (Just (PathPrefix prefix))
    | Text.length prefix < minimumPredictionPrefix = Nothing
    | otherwise =
        uniqueWorkspaceCandidate prefix (fromMaybe Set.empty files)

candidateStillMatches :: Maybe PathProgress -> Text -> Bool
candidateStillMatches progress candidate =
    case progress of
        Just (PathPrefix prefix) ->
            not (Text.null candidate) && prefix `Text.isPrefixOf` candidate
        Just (PathComplete target) -> target == candidate
        Nothing -> False

startFileIndex :: FilePrefetch -> IO ()
startFileIndex prefetch =
    modifyMVar_ prefetch.prefetchState \current ->
        case (current.prefetchClosed, current.prefetchFiles, current.prefetchIndexTask) of
            (False, Nothing, Nothing) -> do
                worker <- asyncWithUnmask \restore ->
                    restore (workspaceFileIndex prefetch.prefetchEnv)
                        >>= installFileIndex prefetch
                pure current { prefetchIndexTask = Just worker }
            _ -> pure current

installFileIndex :: FilePrefetch -> Set.Set Text -> IO ()
installFileIndex prefetch paths =
    modifyMVar_ prefetch.prefetchState \current ->
        if current.prefetchClosed
            then pure current
            else pure current
                { prefetchFiles = Just paths
                , prefetchIndexTask = Nothing
                }

startFileCandidate
    :: FilePrefetch
    -> Text
    -> IO (Maybe FileCandidate)
startFileCandidate prefetch target = mask \_ -> do
    candidate <-
        modifyMVar prefetch.prefetchState \current ->
            if current.prefetchClosed
                || Map.size current.prefetchActive
                    >= maximumConcurrentSpeculativeTasks
                then pure (current, Nothing)
                else do
                    let key = current.prefetchNextKey
                    worker <-
                        asyncWithUnmask \restore ->
                            restore (prefetchFile prefetch.prefetchEnv target)
                    pure
                        ( current
                            { prefetchNextKey = key + 1
                            , prefetchActive =
                                Map.insert key worker current.prefetchActive
                            }
                        , Just FileCandidate
                            { fileCandidateTarget = target
                            , fileCandidateKey = key
                            , fileCandidateTask = worker
                            }
                        )
    pure candidate

prefetchFile :: ToolEnv -> Text -> IO (Maybe PrefetchedFile)
prefetchFile env target =
    resolveUnderCwdWithoutAccessRequest env (fromText target) >>= \case
        Left _ -> pure Nothing
        Right path -> do
            before <- fileFingerprint path
            case before of
                Just fingerprint@FileFingerprint{fingerprintSize}
                    | fingerprintSize <= maxSpeculativeReadBytes ->
                        readTextFile path >>= \case
                            Left _ -> pure Nothing
                            Right content -> do
                                void (evaluate (Text.length content))
                                after <- fileFingerprint path
                                if after == Just fingerprint
                                    then pure $ Just PrefetchedFile
                                        { prefetchedPath = path
                                        , prefetchedFingerprint = fingerprint
                                        , prefetchedContent = content
                                        }
                                    else pure Nothing
                _ -> pure Nothing

cancelFileCandidate :: FilePrefetch -> FileCandidate -> IO ()
cancelFileCandidate prefetch candidate = do
    releaseFileCandidate prefetch candidate
    cancelAndJoin candidate.fileCandidateTask

releaseFileCandidate :: FilePrefetch -> FileCandidate -> IO ()
releaseFileCandidate prefetch candidate =
    modifyMVar_ prefetch.prefetchState \current ->
        pure current
            { prefetchActive =
                Map.delete candidate.fileCandidateKey current.prefetchActive
            }
