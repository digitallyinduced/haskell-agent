module Agent.CLI.RepositoryDelivery
    ( DeliveryStatus(..)
    , PushPreview(..)
    , PullRequestPreview(..)
    , DeliveryError(..)
    , repositoryDeliveryStatus
    , previewRepositoryPush
    , confirmRepositoryPush
    , previewPullRequest
    , createPullRequest
    , deliveryErrorText
    , validateBranchName
    , validateRemoteName
    ) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.Async (cancel, wait, withAsync)
import Control.Applicative ((<|>))
import Control.Concurrent.MVar
    ( MVar
    , modifyMVar
    , modifyMVar_
    , newMVar
    )
import Control.Exception.Safe
    ( SomeException
    , bracket
    , finally
    , isAsyncException
    , mask
    , onException
    , throwIO
    , tryAny
    )
import Control.Monad (forever, unless, when)
import Crypto.Hash (Digest, SHA1, SHA256, hash)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import Data.Char (isAlphaNum, isHexDigit, isSpace)
import Data.IORef
    ( modifyIORef'
    , newIORef
    , readIORef
    , writeIORef
    )
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import Data.Text.Encoding.Error (lenientDecode)
import Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime)
import Data.Word (Word8, Word64)
import GHC.Clock (getMonotonicTimeNSec)
import System.Directory
    ( canonicalizePath
    , createDirectory
    , doesDirectoryExist
    , getTemporaryDirectory
    , removePathForcibly
    )
import System.Exit (ExitCode(..))
import System.Environment (getEnvironment)
import System.IO
    ( Handle
    , IOMode(ReadMode)
    , hClose
    , openBinaryFile
    )
import System.IO.Unsafe (unsafePerformIO)
import System.FilePath
    ( isAbsolute
    , (</>)
    )
import System.Posix.Files
    ( ownerExecuteMode
    , ownerReadMode
    , ownerWriteMode
    , setFileMode
    , unionFileModes
    )
import System.Posix.Signals
    ( Signal
    , sigKILL
    , sigTERM
    , signalProcessGroup
    )
import System.Posix.Types (ProcessID)
import System.Posix.Temp (mkdtemp)
import System.Process
    ( CreateProcess(..)
    , ProcessHandle
    , StdStream(CreatePipe)
    , createProcess
    , getPid
    , proc
    , terminateProcess
    , waitForProcess
    )
import System.Timeout (timeout)

import Agent.CLI.RepositoryReview
    ( RepositorySnapshot(..)
    , repositorySnapshot
    )
import Agent.CLI.ProcessSecurity
    ( canonicalPathOutside
    , resolveExecutableOutside
    , sanitizeSearchPathOutside
    )

data DeliveryStatus = DeliveryStatus
    { deliverySnapshotId :: !Text
    , deliveryRoot :: !FilePath
    , deliveryHeadOid :: !Text
    , deliveryBranch :: !Text
    , deliveryRemote :: !Text
    , deliveryRemoteFingerprint :: !Text
    , deliveryUpstreamRef :: !Text
    , deliveryUpstreamOid :: !Text
    , deliveryAhead :: !Int
    , deliveryBehind :: !Int
    } deriving (Eq, Show)

data PushPreview = PushPreview
    { pushPreviewStatus :: !DeliveryStatus
    , pushPreviewConfirmation :: !Text
    , pushPreviewExpiresAt :: !POSIXTime
    } deriving (Eq, Show)

data PullRequestPreview = PullRequestPreview
    { pullRequestRepository :: !Text
    , pullRequestBaseRef :: !Text
    , pullRequestHeadRef :: !Text
    , pullRequestTitle :: !Text
    , pullRequestConfirmation :: !Text
    , pullRequestExpiresAt :: !POSIXTime
    } deriving (Eq, Show)

data DeliveryError
    = DeliveryInvalidRequest !Text
    | DeliveryStale !Text
    | DeliveryUnavailable !Text
    | DeliveryCommandFailed !Text
    | DeliveryConfirmationRejected !Text
    deriving (Eq, Show)

data IsolatedGitStorage = IsolatedGitStorage
    { isolatedGitCommonDirectory :: !FilePath
    , isolatedGitObjectDirectory :: !FilePath
    , isolatedGitObjectFormat :: !Text
    }

data CreatedPullRequest = CreatedPullRequest
    { createdPullRequestNumber :: !Int
    , createdPullRequestUrl :: !Text
    , createdPullRequestHeadOid :: !Text
    , createdPullRequestHeadRef :: !Text
    , createdPullRequestHeadRepository :: !Text
    , createdPullRequestBaseRef :: !Text
    , createdPullRequestRepository :: !Text
    , createdPullRequestDraft :: !Bool
    }

data Confirmation
    = PushConfirmation !DeliveryStatus !ValidatedRemote
    | PullRequestConfirmation
        !DeliveryStatus
        !ValidatedRemote
        !Text -- repository
        !Text -- base
        !Text -- title
        !Text -- body

data ValidatedRemote = ValidatedRemote
    { validatedRemoteUrl :: !BS.ByteString
    , validatedRemoteFingerprint :: !Text
    , validatedGitHubRepository :: !(Maybe Text)
    }

instance Eq ValidatedRemote where
    left == right =
        left.validatedRemoteUrl == right.validatedRemoteUrl
            && left.validatedRemoteFingerprint
                == right.validatedRemoteFingerprint
            && left.validatedGitHubRepository
                == right.validatedGitHubRepository

data StoredConfirmation = StoredConfirmation
    { storedRoot :: !FilePath
    , storedDeadlineNanos :: !Word64
    , storedConfirmation :: !Confirmation
    }

data ProcessResult = ProcessResult
    { processExitCode :: !ExitCode
    , processStdout :: !BS.ByteString
    , processStderr :: !BS.ByteString
    , processOutputTruncated :: !Bool
    }

data ProcessFailure
    = ProcessLaunchFailed
    | ProcessTimedOut
    | ProcessOutputExceeded
    deriving (Eq, Show)

repositoryDeliveryStatus
    :: FilePath
    -> Text
    -> IO (Either DeliveryError DeliveryStatus)
repositoryDeliveryStatus requested expected =
    timeout localTimeoutMicros (repositorySnapshot requested) >>= \case
        Nothing ->
            pure
                (Left
                    (DeliveryCommandFailed
                        "repository snapshot verification timed out"))
        Just (Left _) ->
            pure
                (Left
                    (DeliveryCommandFailed
                        "repository state could not be verified"))
        Just (Right snapshot)
            | snapshot.snapshotId /= expected ->
                pure
                    (Left
                        (DeliveryStale
                            "repository state changed"))
            | otherwise -> statusAtSnapshot snapshot

previewRepositoryPush
    :: FilePath
    -> Text
    -> IO (Either DeliveryError PushPreview)
previewRepositoryPush requested expected =
    repositoryDeliveryStatus requested expected >>= \case
        Left err -> pure (Left err)
        Right status ->
            validatedRemoteForStatus status >>= \case
                Left err -> pure (Left err)
                Right remote ->
                    queryRemoteHead status remote >>= \case
                        Left err -> pure (Left err)
                        Right oid
                            | not (remoteMatchesExpected status oid) ->
                                pure
                                    (Left
                                        (DeliveryStale
                                            "the remote branch changed; fetch before previewing a push"))
                            | status.deliveryAhead <= 0 ->
                                pure
                                    (Left
                                        (DeliveryInvalidRequest
                                            "the branch has no commits to push"))
                            | status.deliveryBehind /= 0 ->
                                pure
                                    (Left
                                        (DeliveryInvalidRequest
                                            "the branch is behind its upstream"))
                            | otherwise ->
                                proveFastForward status >>= \case
                                    Left err -> pure (Left err)
                                    Right () -> do
                                        dryRun <- runNetworkGit
                                            status.deliveryRoot
                                            remote
                                            [ "push"
                                            , "--porcelain"
                                            , "--dry-run"
                                            , "--no-verify"
                                            , leaseArgument status
                                            , "--"
                                            , BS8.unpack remote.validatedRemoteUrl
                                            , pushRefspec status
                                            ]
                                            BS.empty
                                            networkTimeoutMicros
                                        case dryRun of
                                            Left err -> pure (Left err)
                                            Right _ -> do
                                                (token, expiresAt) <-
                                                    storeConfirmation
                                                        status.deliveryRoot
                                                        (PushConfirmation status remote)
                                                pure
                                                    (Right
                                                        PushPreview
                                                            { pushPreviewStatus = status
                                                            , pushPreviewConfirmation = token
                                                            , pushPreviewExpiresAt = expiresAt
                                                            })

confirmRepositoryPush
    :: FilePath
    -> Text
    -> IO (Either DeliveryError DeliveryStatus)
confirmRepositoryPush requested token =
    consumeConfirmation requested token >>= \case
        Left err -> pure (Left err)
        Right (PushConfirmation previewed previewedRemote) ->
            repositoryDeliveryStatus
                requested
                previewed.deliverySnapshotId >>= \case
                    Left err -> pure (Left err)
                    Right current
                        | current /= previewed ->
                            pure
                                (Left
                                    (DeliveryStale
                                        "branch or upstream state changed after push preview"))
                        | otherwise ->
                            validatedRemoteForStatus current >>= \case
                                Left err -> pure (Left err)
                                Right currentRemote
                                    | currentRemote /= previewedRemote ->
                                        pure
                                            (Left
                                                (DeliveryStale
                                                    "the remote destination changed after push preview"))
                                    | otherwise ->
                                        proveFastForward current >>= \case
                                            Left err -> pure (Left err)
                                            Right () ->
                                                queryRemoteHead current previewedRemote >>= \case
                                                    Left err -> pure (Left err)
                                                    Right remoteOid
                                                        | not (remoteMatchesExpected current remoteOid) ->
                                                            pure
                                                                (Left
                                                                    (DeliveryStale
                                                                        "the remote branch changed after push preview"))
                                                        | otherwise ->
                                                            revalidateLocalMutation
                                                                current >>= \case
                                                                    Left err -> pure (Left err)
                                                                    Right () ->
                                                                        -- The explicit object ID prevents
                                                                        -- any still-later local ref movement
                                                                        -- from changing what is delivered.
                                                                        runNetworkGit
                                                                            current.deliveryRoot
                                                                            previewedRemote
                                                                            [ "push"
                                                                            , "--porcelain"
                                                                            , "--no-verify"
                                                                            , leaseArgument current
                                                                            , "--"
                                                                            , BS8.unpack previewedRemote.validatedRemoteUrl
                                                                            , pushRefspec current
                                                                            ]
                                                                            BS.empty
                                                                            networkTimeoutMicros >>= \case
                                                                                Left err -> pure (Left err)
                                                                                Right _ ->
                                                                                    refreshPushedStatus
                                                                                        current
                                                                                        previewedRemote
        Right _ ->
            pure
                (Left
                    (DeliveryConfirmationRejected
                        "confirmation token is for a different operation"))

previewPullRequest
    :: FilePath
    -> Text
    -> Text
    -> Text
    -> Text
    -> IO (Either DeliveryError PullRequestPreview)
previewPullRequest requested expected base title body
    | Left err <- validatePullRequestInput base title body =
        pure (Left err)
    | otherwise =
        repositoryDeliveryStatus requested expected >>= \case
            Left err -> pure (Left err)
            Right status
                | status.deliveryAhead /= 0 || status.deliveryBehind /= 0 ->
                    pure
                        (Left
                            (DeliveryInvalidRequest
                                "push the exact branch state before previewing a pull request"))
                | otherwise ->
                    validatedRemoteForStatus status >>= \case
                        Left err -> pure (Left err)
                        Right remote ->
                            queryRemoteHead status remote >>= \case
                                Left err -> pure (Left err)
                                Right remoteOid
                                    | remoteOid /= Just status.deliveryHeadOid ->
                                        pure
                                            (Left
                                                (DeliveryStale
                                                    "the pushed remote branch does not match HEAD"))
                                    | otherwise ->
                                        case remote.validatedGitHubRepository of
                                            Nothing ->
                                                pure
                                                    (Left
                                                        (DeliveryInvalidRequest
                                                            "the delivery remote is not a supported GitHub repository"))
                                            Just repository ->
                                                requireGitHubCli
                                                    status.deliveryRoot
                                                    repository >>= \case
                                                        Left err -> pure (Left err)
                                                        Right () ->
                                                            ensureBaseAndNoOpenPullRequest
                                                                status remote repository base >>= \case
                                                                    Left err -> pure (Left err)
                                                                    Right () -> do
                                                                        (token, expiresAt) <-
                                                                            storeConfirmation
                                                                                status.deliveryRoot
                                                                                (PullRequestConfirmation
                                                                                    status remote repository
                                                                                    base title body)
                                                                        pure
                                                                            (Right
                                                                                PullRequestPreview
                                                                                    { pullRequestRepository =
                                                                                        repository
                                                                                    , pullRequestBaseRef = base
                                                                                    , pullRequestHeadRef =
                                                                                        status.deliveryBranch
                                                                                    , pullRequestTitle = title
                                                                                    , pullRequestConfirmation =
                                                                                        token
                                                                                    , pullRequestExpiresAt =
                                                                                        expiresAt
                                                                                    })

createPullRequest
    :: FilePath
    -> Text
    -> IO (Either DeliveryError Text)
createPullRequest requested token =
    consumeConfirmation requested token >>= \case
        Left err -> pure (Left err)
        Right
            (PullRequestConfirmation
                previewed previewedRemote repository base title body) ->
            repositoryDeliveryStatus
                requested
                previewed.deliverySnapshotId >>= \case
                    Left err -> pure (Left err)
                    Right current
                        | current /= previewed ->
                            pure
                                (Left
                                    (DeliveryStale
                                        "branch or upstream state changed after pull-request preview"))
                        | otherwise ->
                            validatedRemoteForStatus current >>= \case
                                Left err -> pure (Left err)
                                Right currentRemote
                                    | currentRemote /= previewedRemote ->
                                        pure
                                            (Left
                                                (DeliveryStale
                                                    "the remote destination changed after preview"))
                                    | otherwise ->
                                        queryRemoteHead current previewedRemote >>= \case
                                            Left err -> pure (Left err)
                                            Right remoteOid
                                                | remoteOid
                                                    /= Just current.deliveryHeadOid ->
                                                        pure
                                                            (Left
                                                                (DeliveryStale
                                                                    "the pushed remote branch changed after preview"))
                                                | otherwise ->
                                                    requireGitHubCli
                                                        current.deliveryRoot
                                                        repository >>= \case
                                                            Left err -> pure (Left err)
                                                            Right () ->
                                                                ensureBaseAndNoOpenPullRequest
                                                                    current
                                                                    previewedRemote
                                                                    repository
                                                                    base >>= \case
                                                                        Left err ->
                                                                            pure (Left err)
                                                                        Right () ->
                                                                            revalidatePullRequestMutation
                                                                                current
                                                                                previewedRemote >>= \case
                                                                                    Left err ->
                                                                                        pure (Left err)
                                                                                    Right () ->
                                                                                        revalidateLocalMutation
                                                                                            current >>= \case
                                                                                                Left err ->
                                                                                                    pure (Left err)
                                                                                                Right () ->
                                                                                                    createGitHubPullRequest
                                                                                                        current
                                                                                                        repository
                                                                                                        base
                                                                                                        title
                                                                                                        body
        Right _ ->
            pure
                (Left
                    (DeliveryConfirmationRejected
                        "confirmation token is for a different operation"))

statusAtSnapshot
    :: RepositorySnapshot
    -> IO (Either DeliveryError DeliveryStatus)
statusAtSnapshot snapshot =
    case snapshot.snapshotHead of
        Nothing ->
            pure
                (Left
                    (DeliveryInvalidRequest
                        "delivery requires a branch with at least one commit"))
        Just headOid ->
            runGit root ["symbolic-ref", "--quiet", "HEAD"] BS.empty
                localTimeoutMicros >>= \case
                    Left _ ->
                        pure
                            (Left
                                (DeliveryInvalidRequest
                                    "delivery requires a named local branch"))
                    Right branchBytes ->
                        let fullBranch = decodeTrimmed branchBytes
                        in case Text.stripPrefix "refs/heads/" fullBranch of
                            Nothing ->
                                pure
                                    (Left
                                        (DeliveryInvalidRequest
                                            "Git returned an invalid local branch"))
                            Just branch
                                | not (validateBranchName branch) ->
                                    pure
                                        (Left
                                            (DeliveryInvalidRequest
                                                "local branch name is not safe for delivery"))
                                | otherwise ->
                                    readUpstream root fullBranch >>= \case
                                        Left err -> pure (Left err)
                                        Right (remote, upstreamRef) ->
                                            readUpstreamStatus
                                                root headOid branch remote upstreamRef
  where
    root = snapshot.snapshotRoot
    readUpstreamStatus root headOid branch remote upstreamRef =
        runGit root ["rev-parse", "--verify", "@{upstream}"] BS.empty
            localTimeoutMicros >>= \case
                Left _ ->
                    runGit root ["rev-list", "--count", "HEAD"] BS.empty
                        localTimeoutMicros >>= \case
                            Left err -> pure (Left err)
                            Right countBytes ->
                                case reads (BS8.unpack (stripLineEnding countBytes)) of
                                    [(ahead, "")]
                                        | ahead > 0 ->
                                            finishStatus
                                                (zeroObjectId headOid)
                                                ahead
                                                0
                                    _ ->
                                        pure
                                            (Left
                                                (DeliveryCommandFailed
                                                    "Git returned an invalid commit count"))
                Right upstreamBytes ->
                    let upstreamOid = decodeTrimmed upstreamBytes
                    in runGit
                        root
                        [ "rev-list"
                        , "--left-right"
                        , "--count"
                        , "HEAD...@{upstream}"
                        ]
                        BS.empty
                        localTimeoutMicros >>= \case
                            Left err -> pure (Left err)
                            Right counts ->
                                case parseAheadBehind counts of
                                    Nothing ->
                                        pure
                                            (Left
                                                (DeliveryCommandFailed
                                                    "Git returned invalid ahead/behind counts"))
                                    Just (ahead, behind) ->
                                        finishStatus upstreamOid ahead behind
      where
        finishStatus upstreamOid ahead behind =
            readValidatedRemote root headOid remote >>= \case
                Left err -> pure (Left err)
                Right validatedRemote ->
                    pure
                        (Right
                            DeliveryStatus
                                { deliverySnapshotId = snapshot.snapshotId
                                , deliveryRoot = root
                                , deliveryHeadOid = headOid
                                , deliveryBranch = branch
                                , deliveryRemote = remote
                                , deliveryRemoteFingerprint =
                                    validatedRemote.validatedRemoteFingerprint
                                , deliveryUpstreamRef = upstreamRef
                                , deliveryUpstreamOid = upstreamOid
                                , deliveryAhead = ahead
                                , deliveryBehind = behind
                                })

readUpstream
    :: FilePath
    -> Text
    -> IO (Either DeliveryError (Text, Text))
readUpstream root fullBranch =
    runGit
        root
        [ "for-each-ref"
        , "--format=%(upstream:remotename)%00%(upstream:remoteref)"
        , "--count=1"
        , Text.unpack fullBranch
        ]
        BS.empty
        localTimeoutMicros >>= \case
            Left err -> pure (Left err)
            Right output ->
                case BS.split 0 (stripLineEnding output) of
                    [remoteBytes, refBytes] ->
                        let remote = decodeTrimmed remoteBytes
                            upstreamRef = decodeTrimmed refBytes
                        in if validateRemoteName remote
                            && validateFullBranchRef upstreamRef
                            then pure (Right (remote, upstreamRef))
                            else
                                pure
                                    (Left
                                        (DeliveryInvalidRequest
                                            "the branch has no safe push upstream"))
                    _ ->
                        pure
                            (Left
                                (DeliveryInvalidRequest
                                    "the branch has no configured upstream"))

readValidatedRemote
    :: FilePath
    -> Text
    -> Text
    -> IO (Either DeliveryError ValidatedRemote)
readValidatedRemote root headOid remote =
    readLocalConfig >>= \case
        Left err -> pure (Left err)
        Right entries ->
            case (valuesFor "url" entries, valuesFor "pushurl" entries) of
                ([fetchUrl], []) -> finish fetchUrl fetchUrl
                ([fetchUrl], [pushUrl]) -> finish fetchUrl pushUrl
                _ -> invalid
  where
    finish fetchUrl pushUrl
        | fetchUrl == pushUrl
            && validRemoteUrl pushUrl =
                pure do
                    fingerprint <- gitBlobHashForOid headOid pushUrl
                    pure
                        ValidatedRemote
                            { validatedRemoteUrl = pushUrl
                            , validatedRemoteFingerprint = fingerprint
                            , validatedGitHubRepository =
                                githubRepositoryFromUrl pushUrl
                            }
        | otherwise = invalid
    readLocalConfig =
        runGit
            root
            [ "config"
            , "--local"
            , "--no-includes"
            , "--null"
            , "--list"
            ]
            BS.empty
            localTimeoutMicros >>= \case
                Left err -> pure (Left err)
                Right bytes -> pure (parseConfigEntries bytes)
    parseConfigEntries bytes =
        traverse parseEntry
            [ entry
            | entry <- BS.split 0 bytes
            , not (BS.null entry)
            ]
    parseEntry entry =
        case BS8.break (== '\n') entry of
            (key, separatorAndValue)
                | not (BS.null separatorAndValue) ->
                    Right (key, BS.drop 1 separatorAndValue)
            _ ->
                Left
                    (DeliveryInvalidRequest
                        "the local repository configuration is invalid")
    valuesFor field entries =
        [ value
        | (key, value) <- entries
        , key
            == BS8.pack
                ( "remote."
                    <> Text.unpack remote
                    <> "."
                    <> field
                )
        ]
    invalid =
        pure
            (Left
                (DeliveryInvalidRequest
                    "delivery requires one identical fetch and push destination"))

validatedRemoteForStatus
    :: DeliveryStatus
    -> IO (Either DeliveryError ValidatedRemote)
validatedRemoteForStatus status =
    readValidatedRemote
        status.deliveryRoot
        status.deliveryHeadOid
        status.deliveryRemote >>= \case
        Left _ ->
            pure
                (Left
                    (DeliveryStale
                        "the remote destination is no longer valid"))
        Right remote
            | remote.validatedRemoteFingerprint
                /= status.deliveryRemoteFingerprint ->
                    pure
                        (Left
                            (DeliveryStale
                                "the remote destination changed"))
            | otherwise -> pure (Right remote)

gitBlobHashForOid
    :: Text
    -> BS.ByteString
    -> Either DeliveryError Text
gitBlobHashForOid oid bytes =
    let material =
            BS8.pack ("blob " <> show (BS.length bytes) <> "\NUL") <> bytes
    in case Text.length oid of
        40 ->
            Right
                (Text.pack
                    (show (hash material :: Digest SHA1)))
        64 ->
            Right
                (Text.pack
                    (show (hash material :: Digest SHA256)))
        _ ->
            Left
                (DeliveryCommandFailed
                    "Git returned an unsupported object format")

queryRemoteHead
    :: DeliveryStatus
    -> ValidatedRemote
    -> IO (Either DeliveryError (Maybe Text))
queryRemoteHead status remote =
    runNetworkGit
        status.deliveryRoot
        remote
        [ "ls-remote"
        , "--heads"
        , "--"
        , BS8.unpack remote.validatedRemoteUrl
        , Text.unpack status.deliveryUpstreamRef
        ]
        BS.empty
        networkTimeoutMicros >>= \case
            Left _ ->
                pure
                    (Left
                        (DeliveryUnavailable
                            "could not verify the exact remote branch"))
            Right output ->
                case BS8.words output of
                    [] -> pure (Right Nothing)
                    [oidBytes, refBytes]
                        | decodeTrimmed refBytes
                            == status.deliveryUpstreamRef
                            && validObjectId (decodeTrimmed oidBytes) ->
                                pure (Right (Just (decodeTrimmed oidBytes)))
                    _ ->
                        pure
                            (Left
                                (DeliveryUnavailable
                                    "the exact remote branch is unavailable"))

refreshPushedStatus
    :: DeliveryStatus
    -> ValidatedRemote
    -> IO (Either DeliveryError DeliveryStatus)
refreshPushedStatus pushed validatedRemote = do
    -- A successful push does not necessarily update the local remote-tracking
    -- ref. Bind the returned result to the immutable pushed OID instead of
    -- pretending the tracking ref was refreshed.
    remoteResult <- queryRemoteHead
        pushed
            { deliveryUpstreamOid = pushed.deliveryHeadOid }
        validatedRemote
    pure case remoteResult of
        Left err -> Left err
        Right oid
            | oid /= Just pushed.deliveryHeadOid ->
                Left
                    (DeliveryStale
                        "remote verification did not match the pushed commit")
            | otherwise ->
                Right
                    pushed
                        { deliveryUpstreamOid = pushed.deliveryHeadOid
                        , deliveryAhead = 0
                        , deliveryBehind = 0
                        }

revalidateLocalMutation
    :: DeliveryStatus
    -> IO (Either DeliveryError ())
revalidateLocalMutation expected =
    repositoryDeliveryStatus
        expected.deliveryRoot
        expected.deliverySnapshotId >>= \case
            Left err -> pure (Left err)
            Right current
                | current == expected -> pure (Right ())
                | otherwise ->
                    pure
                        (Left
                            (DeliveryStale
                                "repository state changed immediately before delivery"))

revalidatePullRequestMutation
    :: DeliveryStatus
    -> ValidatedRemote
    -> IO (Either DeliveryError ())
revalidatePullRequestMutation expected expectedRemote =
    repositoryDeliveryStatus
        expected.deliveryRoot
        expected.deliverySnapshotId >>= \case
            Left err -> pure (Left err)
            Right current
                | current /= expected ->
                    pure
                        (Left
                            (DeliveryStale
                                "branch or upstream state changed before pull-request creation"))
                | otherwise ->
                    validatedRemoteForStatus current >>= \case
                        Left err -> pure (Left err)
                        Right remote
                            | remote /= expectedRemote ->
                                pure
                                    (Left
                                        (DeliveryStale
                                            "the remote destination changed before pull-request creation"))
                            | otherwise ->
                                queryRemoteHead current expectedRemote >>= \case
                                    Left err -> pure (Left err)
                                    Right oid
                                        | oid /= Just current.deliveryHeadOid ->
                                            pure
                                                (Left
                                                    (DeliveryStale
                                                        "the pushed branch changed before pull-request creation"))
                                        | otherwise -> pure (Right ())

ensureBaseAndNoOpenPullRequest
    :: DeliveryStatus
    -> ValidatedRemote
    -> Text
    -> Text
    -> IO (Either DeliveryError ())
ensureBaseAndNoOpenPullRequest status remote repository base =
    runNetworkGit
        status.deliveryRoot
        remote
        [ "ls-remote"
        , "--exit-code"
        , "--heads"
        , "--"
        , BS8.unpack remote.validatedRemoteUrl
        , "refs/heads/" <> Text.unpack base
        ]
        BS.empty
        networkTimeoutMicros >>= \case
            Left _ ->
                pure
                    (Left
                        (DeliveryInvalidRequest
                            "the pull-request base branch does not exist"))
            Right _ ->
                runGh
                    status.deliveryRoot
                    [ "pr"
                    , "list"
                    , "--repo"
                    , githubRepoArgument repository
                    , "--head"
                    , Text.unpack status.deliveryBranch
                    , "--state"
                    , "open"
                    , "--limit"
                    , "1"
                    , "--json"
                    , "number"
                    ]
                    BS.empty
                    networkTimeoutMicros >>= \case
                        Left err -> pure (Left err)
                        Right output ->
                            case Aeson.decodeStrict' output
                                    :: Maybe [Aeson.Value] of
                                Just [] -> pure (Right ())
                                Just _ ->
                                    pure
                                        (Left
                                            (DeliveryInvalidRequest
                                                "an open pull request already exists for this branch"))
                                Nothing ->
                                    pure
                                        (Left
                                            (DeliveryCommandFailed
                                                "GitHub CLI returned an invalid pull-request response"))

requireGitHubCli
    :: FilePath
    -> Text
    -> IO (Either DeliveryError ())
requireGitHubCli root repository =
    runGh root ["auth", "status", "--hostname", "github.com"]
        BS.empty localTimeoutMicros >>= \case
        Left _ ->
            pure
                (Left
                    (DeliveryUnavailable
                        "GitHub CLI is unavailable or not authenticated"))
        Right _ ->
            runGh
                root
                [ "repo"
                , "view"
                , "--repo"
                , githubRepoArgument repository
                , "--json"
                , "nameWithOwner"
                ]
                BS.empty
                networkTimeoutMicros >>= \case
                    Left _ ->
                        pure
                            (Left
                                (DeliveryUnavailable
                                    "GitHub repository identity is unavailable"))
                    Right output ->
                        case Aeson.decodeStrict' output of
                            Just (RepositoryIdentity returnedRepository)
                                | returnedRepository == repository ->
                                    pure (Right ())
                            _ ->
                                pure
                                    (Left
                                        (DeliveryUnavailable
                                            "GitHub CLI returned an invalid repository identity"))

newtype RepositoryIdentity = RepositoryIdentity Text

githubRepoArgument :: Text -> String
githubRepoArgument repository =
    Text.unpack ("github.com/" <> repository)

pullRequestHeadArgument :: Text -> Text -> String
pullRequestHeadArgument repository branch =
    Text.unpack
        (Text.takeWhile (/= '/') repository <> ":" <> branch)

instance Aeson.FromJSON RepositoryIdentity where
    parseJSON = Aeson.withObject "RepositoryIdentity" \object ->
        RepositoryIdentity <$> object Aeson..: "nameWithOwner"

instance Aeson.FromJSON CreatedPullRequest where
    parseJSON = Aeson.withObject "CreatedPullRequest" \object -> do
        createdPullRequestNumber <- object Aeson..: "number"
        createdPullRequestUrl <- object Aeson..: "html_url"
        headValue <- object Aeson..: "head"
        (createdPullRequestHeadOid, createdPullRequestHeadRef) <-
            Aeson.withObject
                "CreatedPullRequestHead"
                (\headObject ->
                    (,)
                        <$> headObject Aeson..: "sha"
                        <*> headObject Aeson..: "ref")
                headValue
        createdPullRequestHeadRepository <-
            Aeson.withObject
                "CreatedPullRequestHead"
                (\headObject -> do
                    repositoryValue <- headObject Aeson..: "repo"
                    Aeson.withObject
                        "CreatedPullRequestHeadRepository"
                        (\repositoryObject ->
                            repositoryObject Aeson..: "full_name")
                        repositoryValue)
                headValue
        baseValue <- object Aeson..: "base"
        ( createdPullRequestBaseRef
            , createdPullRequestRepository
            ) <-
            Aeson.withObject
                "CreatedPullRequestBase"
                (\baseObject -> do
                    baseRef <- baseObject Aeson..: "ref"
                    repositoryValue <- baseObject Aeson..: "repo"
                    repository <-
                        Aeson.withObject
                            "CreatedPullRequestRepository"
                            (\repositoryObject ->
                                repositoryObject Aeson..: "full_name")
                            repositoryValue
                    pure (baseRef, repository))
                baseValue
        createdPullRequestDraft <- object Aeson..: "draft"
        pure
            CreatedPullRequest
                { createdPullRequestNumber
                , createdPullRequestUrl
                , createdPullRequestHeadOid
                , createdPullRequestHeadRef
                , createdPullRequestHeadRepository
                , createdPullRequestBaseRef
                , createdPullRequestRepository
                , createdPullRequestDraft
                }

createGitHubPullRequest
    :: DeliveryStatus
    -> Text
    -> Text
    -> Text
    -> Text
    -> IO (Either DeliveryError Text)
createGitHubPullRequest status repository base title body =
    runGh
        status.deliveryRoot
        [ "pr"
        , "create"
        , "--draft"
        , "--repo"
        , githubRepoArgument repository
        , "--base"
        , Text.unpack base
        , "--head"
        , pullRequestHeadArgument repository status.deliveryBranch
        , "--title"
        , Text.unpack title
        , "--body-file"
        , "-"
        ]
        (TextEncoding.encodeUtf8 body)
        networkTimeoutMicros >>= \case
            Left err -> pure (Left err)
            Right output ->
                case parsePullRequestUrl repository output of
                    Left err -> pure (Left err)
                    Right url ->
                        case pullRequestNumber repository url of
                            Nothing ->
                                pure
                                    (Left
                                        (DeliveryCommandFailed
                                            "GitHub CLI returned an invalid pull-request number"))
                            Just number ->
                                verifyCreated number url
  where
    verifyCreated :: Int -> Text -> IO (Either DeliveryError Text)
    verifyCreated number url =
        readCreatedPullRequest number >>= \case
            Right created
                | createdMatches True number url created ->
                    markReady number url
                | otherwise -> do
                    closeIfConfirmedDraft number created
                    pure
                        (Left
                            (DeliveryStale
                                "the created pull request did not bind the confirmed commit"))
            _ ->
                pure
                    (Left
                        (DeliveryStale
                            "the created pull request did not bind the confirmed commit"))
    readCreatedPullRequest
        :: Int
        -> IO (Either DeliveryError CreatedPullRequest)
    readCreatedPullRequest number =
        runGh
            status.deliveryRoot
            [ "api"
            , "repos/"
                <> Text.unpack repository
                <> "/pulls/"
                <> show number
            ]
            BS.empty
            networkTimeoutMicros >>= \case
                Left err -> pure (Left err)
                Right output ->
                    case Aeson.decodeStrict' output :: Maybe CreatedPullRequest of
                        Just created -> pure (Right created)
                        _ ->
                            pure
                                (Left
                                    (DeliveryCommandFailed
                                        "GitHub CLI returned invalid pull-request details"))
    createdMatches expectedDraft number url created =
        created.createdPullRequestNumber == number
            && created.createdPullRequestUrl == url
            && created.createdPullRequestHeadOid
                == status.deliveryHeadOid
            && created.createdPullRequestHeadRef
                == status.deliveryBranch
            && created.createdPullRequestHeadRepository == repository
            && created.createdPullRequestBaseRef == base
            && created.createdPullRequestRepository == repository
            && created.createdPullRequestDraft == expectedDraft
    markReady :: Int -> Text -> IO (Either DeliveryError Text)
    markReady number url =
        runGh
            status.deliveryRoot
            [ "pr"
            , "ready"
            , show number
            , "--repo"
            , githubRepoArgument repository
            ]
            BS.empty
            networkTimeoutMicros >>= \case
                Left err -> rejectCreated number err
                Right _ ->
                    readCreatedPullRequest number >>= \case
                        Right created
                            | createdMatches False number url created ->
                                pure (Right url)
                        Right created -> do
                            closeIfConfirmedDraft number created
                            pure
                                (Left
                                    (DeliveryStale
                                        "the ready pull request no longer bound the confirmed commit"))
                        Left _ ->
                            pure
                                (Left
                                    (DeliveryStale
                                        "the ready pull request no longer bound the confirmed commit"))
    rejectCreated :: Int -> DeliveryError -> IO (Either DeliveryError Text)
    rejectCreated number err = do
        readCreatedPullRequest number >>= \case
            Right created -> closeIfConfirmedDraft number created
            Left _ -> pure ()
        pure (Left err)
    closeIfConfirmedDraft expectedNumber created =
        when
            ( created.createdPullRequestNumber == expectedNumber
                && isConfirmedDraft created
            ) do
            _ <- closeCreatedPullRequest
                status.deliveryRoot
                repository
                created.createdPullRequestNumber
            pure ()
    isConfirmedDraft created =
        created.createdPullRequestHeadOid == status.deliveryHeadOid
            && created.createdPullRequestHeadRef == status.deliveryBranch
            && created.createdPullRequestHeadRepository == repository
            && created.createdPullRequestBaseRef == base
            && created.createdPullRequestRepository == repository
            && created.createdPullRequestDraft
            && pullRequestNumber
                repository
                created.createdPullRequestUrl
                == Just created.createdPullRequestNumber

closeCreatedPullRequest :: FilePath -> Text -> Int -> IO (Either DeliveryError ())
closeCreatedPullRequest root repository number =
    runGh
        root
        [ "api"
        , "--method"
        , "PATCH"
        , "repos/"
            <> Text.unpack repository
            <> "/pulls/"
            <> show number
        , "--input"
        , "-"
        ]
        (LBS.toStrict (Aeson.encode (Aeson.object ["state" Aeson..= ("closed" :: Text)])))
        networkTimeoutMicros >>= \case
            Left err -> pure (Left err)
            Right _ -> pure (Right ())

parsePullRequestUrl :: Text -> BS.ByteString -> Either DeliveryError Text
parsePullRequestUrl repository output =
    case filter (not . Text.null)
        (map Text.strip
            (Text.lines
                (TextEncoding.decodeUtf8With lenientDecode output))) of
        [url]
            | Text.length url <= 4096
                && let prefix =
                        "https://github.com/"
                            <> repository
                            <> "/pull/"
                   in prefix `Text.isPrefixOf` url
                        && Text.all isDigit
                            (Text.drop (Text.length prefix) url)
                        && Text.length url > Text.length prefix
                && not (Text.any isControlOrSpace url) ->
                    Right url
        _ ->
            Left
                (DeliveryCommandFailed
                    "GitHub CLI did not return one valid pull-request URL")
  where
    isControlOrSpace character = isSpace character
    isDigit character = character >= '0' && character <= '9'

pullRequestNumber :: Text -> Text -> Maybe Int
pullRequestNumber repository url =
    let prefix = "https://github.com/" <> repository <> "/pull/"
        suffix = Text.drop (Text.length prefix) url
    in case reads (Text.unpack suffix) of
        [(number, "")]
            | number > 0 -> Just number
        _ -> Nothing

validRemoteUrl :: BS.ByteString -> Bool
validRemoteUrl url =
    not (BS.null url)
        && BS.length url <= 4096
        && BS.all (\byte -> byte >= 0x20 && byte /= 0x7f) url
        && BS8.head url /= '-'
        && (validHttps url
            || validSsh url
            || validScpLike url)
  where
    validHttps value =
        "https://" `BS8.isPrefixOf` value
            && validUrlAuthority "https://" value
            && not (authorityContains '@' "https://" value)
            && not (containsQueryOrFragment value)
            && not (authorityContains '%' "https://" value)
    validSsh value =
        "ssh://" `BS8.isPrefixOf` value
            && validSshAuthority value
            && not (containsQueryOrFragment value)
    authorityContains character prefix value =
        BS8.elem character
            (BS8.takeWhile (/= '/') (BS.drop (BS.length prefix) value))
    validSshAuthority value =
        let authority =
                BS8.takeWhile (/= '/') (BS.drop (BS.length "ssh://") value)
            (userinfo, separatorAndHost) = BS8.break (== '@') authority
            host = BS.drop 1 separatorAndHost
        in not (BS.null authority)
            && not (BS8.elem '%' authority)
            && if BS.null separatorAndHost
                then validHostPort authority
                else validUsername userinfo
                    && validHostPort host
                    && not (BS8.elem '@' host)
    validUrlAuthority prefix value =
        validHostPort
            (BS8.takeWhile (/= '/') (BS.drop (BS.length prefix) value))
    validHostPort authority =
        case BS8.break (== ':') authority of
            (host, port)
                | BS.null port -> validHost host
                | otherwise ->
                    validHost host
                        && BS.length port > 1
                        && BS8.all
                            (\character ->
                                character >= '0' && character <= '9')
                            (BS.drop 1 port)
    validHost host =
        not (BS.null host)
            && BS8.all
                (\character ->
                    isAsciiAlphaNumeric character
                        || character `elem` (".-" :: String))
                host
            && BS8.head host /= '.'
            && BS8.last host /= '.'
    validUsername username =
        not (BS.null username)
            && BS8.all
                (\character ->
                    isAsciiAlphaNumeric character
                        || character `elem` ("._-" :: String))
                username
    isAsciiAlphaNumeric character =
        (character >= 'a' && character <= 'z')
            || (character >= 'A' && character <= 'Z')
            || (character >= '0' && character <= '9')
    containsQueryOrFragment value =
        BS8.elem '?' value || BS8.elem '#' value
    validScpLike value =
        case BS8.break (== ':') value of
            (authority, path) ->
                let (username, separatorAndHost) = BS8.break (== '@') authority
                    host = BS.drop 1 separatorAndHost
                in validUsername username
                    && not (BS.null separatorAndHost)
                    && validHost host
                    && not (BS8.elem '@' host)
                    && not (BS8.elem '%' authority)
                    && BS.length path > 1
                    && not (BS8.elem '@' path)
                    && not (containsQueryOrFragment value)

githubRepositoryFromUrl :: BS.ByteString -> Maybe Text
githubRepositoryFromUrl bytes
    | not (BS.all (< 0x80) bytes) = Nothing
    | otherwise =
        let url = Text.pack (BS8.unpack bytes)
        in parseHttps url
            <|> parseSsh url
            <|> parseScp url
  where
    parseHttps url =
        Text.stripPrefix "https://github.com/" url >>= repositoryPath
    parseSsh url =
        (Text.stripPrefix "ssh://git@github.com/" url
            <|> Text.stripPrefix "ssh://github.com/" url)
            >>= repositoryPath
    parseScp url =
        Text.stripPrefix "git@github.com:" url >>= repositoryPath
    repositoryPath path =
        let withoutGit = fromMaybe path (Text.stripSuffix ".git" path)
        in if validateRepositoryName withoutGit
            then Just withoutGit
            else Nothing

remoteMatchesExpected :: DeliveryStatus -> Maybe Text -> Bool
remoteMatchesExpected status =
    (== expectedRemoteHead status)

expectedRemoteHead :: DeliveryStatus -> Maybe Text
expectedRemoteHead status
    | Text.all (== '0') status.deliveryUpstreamOid = Nothing
    | otherwise = Just status.deliveryUpstreamOid

proveFastForward :: DeliveryStatus -> IO (Either DeliveryError ())
proveFastForward status =
    case expectedRemoteHead status of
        Nothing -> pure (Right ())
        Just expected ->
            runGit
                status.deliveryRoot
                [ "merge-base"
                , "--is-ancestor"
                , Text.unpack expected
                , Text.unpack status.deliveryHeadOid
                ]
                BS.empty
                localTimeoutMicros >>= \case
                    Right _ -> pure (Right ())
                    Left _ ->
                        pure
                            (Left
                                (DeliveryInvalidRequest
                                    "the push is not an exact fast-forward"))

leaseArgument :: DeliveryStatus -> String
leaseArgument status =
    "--force-with-lease="
        <> Text.unpack status.deliveryUpstreamRef
        <> ":"
        <> maybe
            (Text.unpack (zeroObjectId status.deliveryHeadOid))
            Text.unpack
            (expectedRemoteHead status)

validatePullRequestInput
    :: Text
    -> Text
    -> Text
    -> Either DeliveryError ()
validatePullRequestInput base title body
    | not (validateBranchName base) =
        Left (DeliveryInvalidRequest "pull-request base branch is invalid")
    | Text.null (Text.strip title) || Text.length title > 512 =
        Left (DeliveryInvalidRequest "pull-request title is invalid")
    | Text.any (== '\NUL') title =
        Left (DeliveryInvalidRequest "pull-request title contains a NUL byte")
    | Text.length body > 1024 * 1024 =
        Left (DeliveryInvalidRequest "pull-request body exceeds 1 MiB")
    | Text.any (== '\NUL') body =
        Left (DeliveryInvalidRequest "pull-request body contains a NUL byte")
    | otherwise = Right ()

validateBranchName :: Text -> Bool
validateBranchName branch =
    validateFullBranchRef ("refs/heads/" <> branch)

validateFullBranchRef :: Text -> Bool
validateFullBranchRef ref =
    not (Text.null ref)
        && Text.length ref <= 1024
        && "refs/heads/" `Text.isPrefixOf` ref
        && not ("/" `Text.isSuffixOf` ref)
        && not ("." `Text.isSuffixOf` ref)
        && not ("." `Text.isPrefixOf` ref)
        && not ("-" `Text.isPrefixOf` Text.drop (Text.length "refs/heads/") ref)
        && not (".." `Text.isInfixOf` ref)
        && not ("@{" `Text.isInfixOf` ref)
        && not ("//" `Text.isInfixOf` ref)
        && Text.all safeRefCharacter ref
        && all validComponent (Text.splitOn "/" ref)
  where
    safeRefCharacter character =
        not (isSpace character)
            && character >= '\x20'
            && character /= '\x7f'
            && character `notElem` ("~^:?*[\\" :: String)
    validComponent component =
        not (Text.null component)
            && not ("." `Text.isPrefixOf` component)
            && component /= "."
            && component /= ".."
            && not (".lock" `Text.isSuffixOf` component)

validateRemoteName :: Text -> Bool
validateRemoteName remote =
    not (Text.null remote)
        && Text.length remote <= 255
        && Text.head remote /= '-'
        && Text.all
            (\character ->
                isAlphaNum character
                    || character `elem` ("._/-" :: String))
            remote
        && not (".." `Text.isInfixOf` remote)
        && not ("//" `Text.isInfixOf` remote)

validateRepositoryName :: Text -> Bool
validateRepositoryName name =
    case Text.splitOn "/" name of
        [owner, repository] ->
            validPart owner && validPart repository
        _ -> False
  where
    validPart value =
        not (Text.null value)
            && value /= "."
            && value /= ".."
            && Text.length value <= 100
            && Text.all
                (\character ->
                    isAlphaNum character
                        || character `elem` ("-._" :: String))
                value

validObjectId :: Text -> Bool
validObjectId oid =
    Text.length oid `elem` [40, 64] && Text.all isHexDigit oid

zeroObjectId :: Text -> Text
zeroObjectId oid = Text.replicate (Text.length oid) "0"

parseAheadBehind :: BS.ByteString -> Maybe (Int, Int)
parseAheadBehind bytes =
    case map (reads . BS8.unpack) (BS8.words bytes) of
        [[ (ahead, "") ], [ (behind, "") ]]
            | ahead >= 0 && behind >= 0 -> Just (ahead, behind)
        _ -> Nothing

pushRefspec :: DeliveryStatus -> String
pushRefspec status =
    Text.unpack status.deliveryHeadOid
        <> ":"
        <> Text.unpack status.deliveryUpstreamRef

storeConfirmation
    :: FilePath
    -> Confirmation
    -> IO (Text, POSIXTime)
storeConfirmation root confirmation = do
    wallNow <- getPOSIXTime
    monotonicNow <- getMonotonicTimeNSec
    token <- randomToken
    let expiresAt = wallNow + confirmationLifetimeSeconds
        deadline = saturatingAdd monotonicNow confirmationLifetimeNanos
    stored <- modifyMVar deliveryConfirmations \confirmations ->
        let active = Map.filter
                (\entry -> entry.storedDeadlineNanos > monotonicNow)
                confirmations
        in if Map.size active >= maxActiveConfirmations
            || Map.member token active
            then pure (active, False)
            else
                pure
                    ( Map.insert
                        token
                        StoredConfirmation
                            { storedRoot = root
                            , storedDeadlineNanos = deadline
                            , storedConfirmation = confirmation
                            }
                        active
                    , True
                    )
    unless stored (fail "repository delivery confirmation capacity exhausted")
    pure (token, expiresAt)

consumeConfirmation
    :: FilePath
    -> Text
    -> IO (Either DeliveryError Confirmation)
consumeConfirmation requested token
    | Text.length token /= 64 || not (Text.all isHexDigit token) =
        pure
            (Left
                (DeliveryConfirmationRejected
                    "confirmation token is invalid"))
    | otherwise = do
        snapshotResult <- timeout localTimeoutMicros
            (repositorySnapshot requested)
        case snapshotResult of
            Nothing ->
                pure
                    (Left
                        (DeliveryCommandFailed
                            "repository snapshot verification timed out"))
            Just (Left _) ->
                pure
                    (Left
                        (DeliveryCommandFailed
                            "repository state could not be verified"))
            Just (Right snapshot) -> do
                monotonicNow <- getMonotonicTimeNSec
                modifyMVar deliveryConfirmations \confirmations ->
                    let pruned = Map.filter
                            (\stored ->
                                stored.storedDeadlineNanos > monotonicNow)
                            confirmations
                    in case Map.lookup token pruned of
                        Nothing ->
                            pure
                                ( pruned
                                , Left
                                    (DeliveryConfirmationRejected
                                        "confirmation token expired or was already used")
                                )
                        Just stored ->
                            let remaining = Map.delete token pruned
                            in if stored.storedRoot /= snapshot.snapshotRoot
                                then
                                    pure
                                        ( remaining
                                        , Left
                                            (DeliveryConfirmationRejected
                                                "confirmation token belongs to another repository")
                                        )
                                else
                                    pure
                                        ( remaining
                                        , Right stored.storedConfirmation
                                        )

randomToken :: IO Text
randomToken =
    bracket
        (openBinaryFile "/dev/urandom" ReadMode)
        hClose
        (\handle -> do
            bytes <- BS.hGet handle 32
            unless (BS.length bytes == 32)
                (fail "could not read confirmation entropy")
            pure (hexEncode bytes))

hexEncode :: BS.ByteString -> Text
hexEncode = Text.pack . concatMap encodeByte . BS.unpack
  where
    alphabet = "0123456789abcdef"
    encodeByte :: Word8 -> String
    encodeByte byte =
        [ alphabet !! fromIntegral (byte `div` 16)
        , alphabet !! fromIntegral (byte `mod` 16)
        ]

runGit
    :: FilePath
    -> [String]
    -> BS.ByteString
    -> Int
    -> IO (Either DeliveryError BS.ByteString)
runGit root arguments input timeoutMicros =
    runCommand
        root
        "git"
        (safeLocalGitArguments <> arguments)
        input
        timeoutMicros >>= \case
        Left ProcessTimedOut ->
            pure (Left (DeliveryCommandFailed "Git command timed out"))
        Left ProcessOutputExceeded ->
            pure (Left (DeliveryCommandFailed "Git command output exceeded its limit"))
        Left ProcessLaunchFailed ->
            pure (Left (DeliveryCommandFailed "Git command could not be started"))
        Right result -> case result.processExitCode of
            ExitSuccess
                | result.processOutputTruncated ->
                    pure
                        (Left
                            (DeliveryCommandFailed
                                "Git command output exceeded its limit"))
                | otherwise -> pure (Right result.processStdout)
            ExitFailure code ->
                pure
                    (Left
                        (DeliveryCommandFailed
                            ("Git command failed with exit code "
                                <> Text.pack (show code))))

safeLocalGitArguments :: [String]
safeLocalGitArguments =
    [ "--no-replace-objects"
    , "-c"
    , "core.hooksPath=/dev/null"
    , "-c"
    , "core.fsmonitor=false"
    , "-c"
    , "credential.helper="
    , "-c"
    , "core.sshCommand=false"
    , "-c"
    , "protocol.ext.allow=never"
    ]

runNetworkGit
    :: FilePath
    -> ValidatedRemote
    -> [String]
    -> BS.ByteString
    -> Int
    -> IO (Either DeliveryError BS.ByteString)
runNetworkGit root remote arguments input timeoutMicros =
    isolatedGitStorage root >>= \case
        Left err -> pure (Left err)
        Right storage -> do
            attempted <- trySynchronous $
                withPrivateTempDirectory
                    [ root
                    , storage.isolatedGitCommonDirectory
                    , storage.isolatedGitObjectDirectory
                    ]
                    "haskell-agent-delivery" \gitDirectory -> do
                    prepareIsolatedGitDirectory
                        gitDirectory
                        storage.isolatedGitObjectFormat
                    overrides <-
                        isolatedGitEnvironment
                            root
                            gitDirectory
                            storage.isolatedGitObjectDirectory
                            remote
                    runCommandWithEnvironment
                        overrides
                        root
                        gitDirectory
                        "git"
                        (safeNetworkGitArguments <> arguments)
                        input
                        timeoutMicros
            case attempted of
                Left _ ->
                    pure
                        (Left
                            (DeliveryCommandFailed
                                "isolated Git command could not be prepared"))
                Right result ->
                    case result of
                        Left ProcessTimedOut ->
                            pure
                                (Left
                                    (DeliveryCommandFailed
                                        "Git command timed out"))
                        Left ProcessOutputExceeded ->
                            pure
                                (Left
                                    (DeliveryCommandFailed
                                        "Git command output exceeded its limit"))
                        Left ProcessLaunchFailed ->
                            pure
                                (Left
                                    (DeliveryCommandFailed
                                        "Git command could not be started"))
                        Right processResult ->
                            case processResult.processExitCode of
                                ExitSuccess
                                    | processResult.processOutputTruncated ->
                                        pure
                                            (Left
                                                (DeliveryCommandFailed
                                                    "Git command output exceeded its limit"))
                                    | otherwise ->
                                        pure (Right processResult.processStdout)
                                ExitFailure code ->
                                    pure
                                        (Left
                                            (DeliveryCommandFailed
                                                ("Git command failed with exit code "
                                                    <> Text.pack (show code))))

isolatedGitStorage
    :: FilePath
    -> IO (Either DeliveryError IsolatedGitStorage)
isolatedGitStorage root =
    resolveGitDirectory
        [ "rev-parse"
        , "--path-format=absolute"
        , "--git-common-dir"
        ] >>= \case
            Left err -> pure (Left err)
            Right commonDirectory ->
                resolveGitDirectory
                    [ "rev-parse"
                    , "--path-format=absolute"
                    , "--git-path"
                    , "objects"
                    ] >>= \case
                        Left err -> pure (Left err)
                        Right objectDirectory ->
                            runGit
                                root
                                ["rev-parse", "--show-object-format"]
                                BS.empty
                                localTimeoutMicros >>= \case
                                    Left err -> pure (Left err)
                                    Right formatBytes ->
                                        let objectFormat =
                                                decodeTrimmed formatBytes
                                        in if
                                            objectFormat
                                                `elem` ["sha1", "sha256"]
                                            then
                                                pure
                                                    (Right
                                                        IsolatedGitStorage
                                                            { isolatedGitCommonDirectory =
                                                                commonDirectory
                                                            , isolatedGitObjectDirectory =
                                                                objectDirectory
                                                            , isolatedGitObjectFormat =
                                                                objectFormat
                                                            })
                                            else
                                                pure
                                                    (Left
                                                        (DeliveryCommandFailed
                                                            "repository object format is unsupported"))
  where
    resolveGitDirectory arguments =
        runGit root arguments BS.empty localTimeoutMicros >>= \case
            Left err -> pure (Left err)
            Right output -> do
                let requested = BS8.unpack (stripLineEnding output)
                resolved <- trySynchronous (canonicalizePath requested)
                case resolved of
                    Right path
                        | isAbsolute path ->
                            doesDirectoryExist path >>= \case
                                True -> pure (Right path)
                                False -> invalid
                    _ -> invalid
    invalid =
        pure
            (Left
                (DeliveryCommandFailed
                    "repository object storage could not be isolated"))

withPrivateTempDirectory
    :: [FilePath]
    -> String
    -> (FilePath -> IO value)
    -> IO value
withPrivateTempDirectory excludedRoots template action = do
    configured <- trySynchronous getTemporaryDirectory
    let fallbackBases = ["/private/tmp", "/tmp", "/var/tmp"]
        candidates =
            either (const fallbackBases) (: fallbackBases) configured
    bracket
        (createInFirstSafeBase candidates)
        removePathForcibly
        action
  where
    createInFirstSafeBase = \case
        [] -> fail "no safe private temporary directory is available"
        candidate : remaining ->
            trySynchronous (createAt candidate) >>= \case
                Right directory -> pure directory
                Left _ -> createInFirstSafeBase remaining
    createAt candidate = do
        checkedBase <- canonicalOutsideAll excludedRoots candidate
            >>= maybe (fail "temporary directory is inside a trust boundary") pure
        directory <- mkdtemp (checkedBase </> template <> ".XXXXXX")
        (do
            setFileMode directory
                (ownerReadMode
                    `unionFileModes` ownerWriteMode
                    `unionFileModes` ownerExecuteMode)
            canonicalOutsideAll excludedRoots directory
                >>= maybe
                    (fail "created temporary directory crossed a trust boundary")
                    pure)
            `onException` removePathForcibly directory

canonicalOutsideAll :: [FilePath] -> FilePath -> IO (Maybe FilePath)
canonicalOutsideAll roots requested =
    go roots requested
  where
    go [] checked = pure (Just checked)
    go (root : remaining) checked =
        canonicalPathOutside root checked >>= \case
            Nothing -> pure Nothing
            Just canonical -> go remaining canonical

prepareIsolatedGitDirectory :: FilePath -> Text -> IO ()
prepareIsolatedGitDirectory directory objectFormat = do
    createDirectory (directory </> "refs")
    createDirectory (directory </> "refs" </> "heads")
    BS.writeFile (directory </> "HEAD") "ref: refs/heads/isolated\n"
    BS8.writeFile
        (directory </> "config")
        (BS8.pack
            (unlines
                ( [ "[core]"
                  , "\trepositoryformatversion = "
                        <> if objectFormat == "sha256" then "1" else "0"
                  , "\tbare = true"
                  ]
                    <> if objectFormat == "sha256"
                        then
                            [ "[extensions]"
                            , "\tobjectformat = sha256"
                            ]
                        else [])))
    setFileMode (directory </> "HEAD")
        (ownerReadMode `unionFileModes` ownerWriteMode)
    setFileMode (directory </> "config")
        (ownerReadMode `unionFileModes` ownerWriteMode)

isolatedGitEnvironment
    :: FilePath
    -> FilePath
    -> FilePath
    -> ValidatedRemote
    -> IO [(String, String)]
isolatedGitEnvironment root gitDirectory objectDirectory remote = do
    helper <- githubCredentialHelper root remote
    let settings =
            [ ("protocol.ext.allow", "never")
            , ("credential.interactive", "false")
            ]
                <> maybe [] (\value -> [("credential.https://github.com.helper", value)]) helper
        indexed =
            concat
                [ [ ("GIT_CONFIG_KEY_" <> show index, key)
                  , ("GIT_CONFIG_VALUE_" <> show index, value)
                  ]
                | (index, (key, value)) <- zip [0 :: Int ..] settings
                ]
    pure
        ( [ ("GIT_DIR", gitDirectory)
          , ("GIT_OBJECT_DIRECTORY", objectDirectory)
          , ("TMPDIR", gitDirectory)
          , ("TMP", gitDirectory)
          , ("TEMP", gitDirectory)
          , ("GIT_CONFIG_GLOBAL", "/dev/null")
          , ("GIT_CONFIG_SYSTEM", "/dev/null")
          , ("GIT_CONFIG_NOSYSTEM", "1")
          , ("GIT_CONFIG_COUNT", show (length settings))
          , ("GIT_ATTR_NOSYSTEM", "1")
          ]
            <> indexed
        )

githubCredentialHelper
    :: FilePath
    -> ValidatedRemote
    -> IO (Maybe String)
githubCredentialHelper root remote
    | "https://github.com/" `BS8.isPrefixOf` remote.validatedRemoteUrl =
        resolveExecutableOutside root "gh" >>= \case
            Left _ -> pure Nothing
            Right executable ->
                pure
                    (Just
                        ("!"
                            <> shellQuoteArgument executable
                            <> " auth git-credential"))
    | otherwise = pure Nothing

shellQuoteArgument :: String -> String
shellQuoteArgument value =
    "'" <> concatMap escape value <> "'"
  where
    escape '\'' = "'\\''"
    escape character = [character]

safeNetworkGitArguments :: [String]
safeNetworkGitArguments =
    [ "--no-replace-objects"
    , "-c"
    , "core.hooksPath=/dev/null"
    , "-c"
    , "core.fsmonitor=false"
    , "-c"
    , "protocol.ext.allow=never"
    ]

runGh
    :: FilePath
    -> [String]
    -> BS.ByteString
    -> Int
    -> IO (Either DeliveryError BS.ByteString)
runGh root arguments input timeoutMicros = do
    isolatedGitStorage root >>= \case
        Left _ ->
            pure
                (Left
                    (DeliveryUnavailable
                        "GitHub CLI command was unavailable"))
        Right storage -> do
            attempted <- trySynchronous $
                withPrivateTempDirectory
                    [ root
                    , storage.isolatedGitCommonDirectory
                    , storage.isolatedGitObjectDirectory
                    ]
                    "haskell-agent-gh" \workingDirectory ->
                        runCommandWithEnvironment
                            [ ("TMPDIR", workingDirectory)
                            , ("TMP", workingDirectory)
                            , ("TEMP", workingDirectory)
                            ]
                            root
                            workingDirectory
                            "gh"
                            arguments
                            input
                            timeoutMicros
            case attempted of
                Left _ ->
                    pure
                        (Left
                            (DeliveryUnavailable
                                "GitHub CLI command was unavailable"))
                Right result -> finish result
  where
    finish = \case
        Left _ ->
            pure
                (Left
                    (DeliveryUnavailable
                        "GitHub CLI command was unavailable"))
        Right result -> case result.processExitCode of
            ExitSuccess
                | result.processOutputTruncated ->
                    pure
                        (Left
                            (DeliveryCommandFailed
                                "GitHub CLI output exceeded its limit"))
                | otherwise -> pure (Right result.processStdout)
            ExitFailure _ ->
                pure
                    (Left
                        (DeliveryUnavailable
                            "GitHub CLI command failed"))

runCommand
    :: FilePath
    -> FilePath
    -> [String]
    -> BS.ByteString
    -> Int
    -> IO (Either ProcessFailure ProcessResult)
runCommand root =
    runCommandWithEnvironment [] root root

runCommandWithEnvironment
    :: [(String, String)]
    -> FilePath
    -> FilePath
    -> FilePath
    -> [String]
    -> BS.ByteString
    -> Int
    -> IO (Either ProcessFailure ProcessResult)
runCommandWithEnvironment
    overrides
    trustRoot
    workingDirectory
    executable
    arguments
    input
    timeoutMicros = do
    launched <- trySynchronous
        (bracket start stop \processData ->
            timeout timeoutMicros (run processData))
    pure case launched of
        Left _ -> Left ProcessLaunchFailed
        Right Nothing -> Left ProcessTimedOut
        Right (Just result) -> result
  where
    start = mask \_ -> do
        resolvedExecutable <-
            resolveExecutableOutside trustRoot executable
                >>= either (fail . Text.unpack) pure
        environment <- applyEnvironmentOverrides overrides
            <$> nonInteractiveEnvironment trustRoot executable
        (maybeInput, maybeOutput, maybeError, process) <-
            createProcess
                (proc resolvedExecutable arguments)
                    { cwd = Just workingDirectory
                    , std_in = CreatePipe
                    , std_out = CreatePipe
                    , std_err = CreatePipe
                    , close_fds = True
                    , create_group = True
                    , env = Just environment
                    }
        case (maybeInput, maybeOutput, maybeError) of
            (Just inputHandle, Just outputHandle, Just errorHandle) -> do
                let closePipes = do
                        closeQuietly inputHandle
                        closeQuietly outputHandle
                        closeQuietly errorHandle
                    cleanupWithoutGroup = do
                        closePipes
                        _ <- tryAny (terminateProcess process)
                        _ <- tryAny (waitForProcess process)
                        pure ()
                processGroup <- getPid process
                    `onException` cleanupWithoutGroup
                completed <- newIORef False
                    `onException` do
                        closePipes
                        terminateProcessGroup sigKILL processGroup process
                        _ <- tryAny (waitForProcess process)
                        pure ()
                pure
                    ( inputHandle
                    , outputHandle
                    , errorHandle
                    , process
                    , processGroup
                    , completed
                    )
            _ -> do
                terminateProcess process
                _ <- waitForProcess process
                fail "could not create command pipes"
    stop
        ( inputHandle
        , outputHandle
        , errorHandle
        , process
        , processGroup
        , completed
        ) = do
        closeQuietly inputHandle
        closeQuietly outputHandle
        closeQuietly errorHandle
        finished <- readIORef completed
        unless finished do
            terminateProcessGroup sigTERM processGroup process
            threadDelay 100_000
            -- A descendant can retain the group and pipes after its leader
            -- exits, so always escalate the captured process group.
            terminateProcessGroup sigKILL processGroup process
        _ <- tryAny (waitForProcess process)
        pure ()
    run
        ( inputHandle
        , outputHandle
        , errorHandle
        , process
        , processGroup
        , completed
        ) =
        withAsync
            (BS.hPut inputHandle input `finally` closeQuietly inputHandle)
            \inputWriter ->
                withAsync (readBounded outputHandle) \outputReader ->
                    withAsync (readBounded errorHandle) \errorReader -> do
                        exitCode <- waitForProcess process
                        inputFinished <- timeout processPipeTeardownMicros
                            (wait inputWriter)
                        case inputFinished of
                            Just () -> pure ()
                            Nothing -> do
                                closeQuietly inputHandle
                                cancel inputWriter
                        let drainReaders =
                                (,) <$> wait outputReader <*> wait errorReader
                        -- Give ordinary buffered output a short chance to
                        -- drain. The captured group is then terminated even
                        -- when EOF already arrived: a descendant may close
                        -- its pipes while continuing to run.
                        naturallyDrained <- timeout
                            processPipeTeardownMicros
                            drainReaders
                        terminateProcessGroup sigTERM processGroup process
                        drainedAfterTerm <- case naturallyDrained of
                            Just values -> pure (Just values)
                            Nothing ->
                                timeout processGroupTermGraceMicros drainReaders
                        -- Always escalate the captured group so a descendant
                        -- cannot outlive a successful leader.
                        terminateProcessGroup sigKILL processGroup process
                        drained <- case drainedAfterTerm of
                            Just values -> pure (Just values)
                            Nothing ->
                                timeout processPipeTeardownMicros drainReaders
                        case drained of
                            Nothing -> do
                                closeQuietly outputHandle
                                closeQuietly errorHandle
                                cancel outputReader
                                cancel errorReader
                                pure (Left ProcessTimedOut)
                            Just
                                ( (output, outputTruncated)
                                , (errors, errorsTruncated)
                                ) -> do
                                    writeIORef completed True
                                    let truncated =
                                            outputTruncated || errorsTruncated
                                    pure
                                        (if truncated
                                            then Left ProcessOutputExceeded
                                            else
                                                Right
                                                    ProcessResult
                                                        { processExitCode =
                                                            exitCode
                                                        , processStdout = output
                                                        , processStderr = errors
                                                        , processOutputTruncated =
                                                            False
                                                        })

applyEnvironmentOverrides
    :: [(String, String)]
    -> [(String, String)]
    -> [(String, String)]
applyEnvironmentOverrides overrides inherited =
    overrides
        <> filter
            (\(name, _) -> name `notElem` map fst overrides)
            inherited

readBounded :: Handle -> IO (BS.ByteString, Bool)
readBounded handle = do
    chunks <- newIORef []
    retained <- newIORef 0
    truncated <- newIORef False
    let drain = do
            chunk <- BS.hGetSome handle (64 * 1024)
            unless (BS.null chunk) do
                current <- readIORef retained
                let room = max 0 (maxProcessOutputBytes - current)
                    kept = BS.take room chunk
                unless (BS.null kept) do
                    modifyIORef' chunks (kept :)
                    writeIORef retained (current + BS.length kept)
                when (BS.length kept < BS.length chunk)
                    (writeIORef truncated True)
                drain
    drain `finally` closeQuietly handle
    bytes <- BS.concat . reverse <$> readIORef chunks
    wasTruncated <- readIORef truncated
    pure (bytes, wasTruncated)

terminateProcessGroup
    :: Signal
    -> Maybe ProcessID
    -> ProcessHandle
    -> IO ()
terminateProcessGroup signal processGroup process = do
    pid <- maybe (getPid process) (pure . Just) processGroup
    case pid of
        Nothing -> do
            _ <- tryAny (terminateProcess process)
            pure ()
        Just processId -> do
            _ <- tryAny (signalProcessGroup signal processId)
            pure ()

stripLineEnding :: BS.ByteString -> BS.ByteString
stripLineEnding = BS8.dropWhileEnd (`elem` ['\r', '\n'])

decodeTrimmed :: BS.ByteString -> Text
decodeTrimmed =
    Text.strip . TextEncoding.decodeUtf8With lenientDecode

closeQuietly :: Handle -> IO ()
closeQuietly handle = do
    _ <- tryAny (hClose handle)
    pure ()

nonInteractiveEnvironment :: FilePath -> FilePath -> IO [(String, String)]
nonInteractiveEnvironment root executable = do
    inherited <- getEnvironment
    let blocked =
            [ "GIT_TERMINAL_PROMPT"
            , "GCM_INTERACTIVE"
            , "GH_PROMPT_DISABLED"
            , "GH_REPO"
            , "GH_HOST"
            , "SSH_ASKPASS_REQUIRE"
            , "GIT_DIR"
            , "GIT_WORK_TREE"
            , "GIT_INDEX_FILE"
            , "GIT_OBJECT_DIRECTORY"
            , "GIT_ALTERNATE_OBJECT_DIRECTORIES"
            , "GIT_COMMON_DIR"
            , "GIT_CONFIG_COUNT"
            , "GIT_CONFIG_KEY_0"
            , "GIT_CONFIG_VALUE_0"
            , "GIT_CONFIG_PARAMETERS"
            , "GIT_CONFIG_GLOBAL"
            , "GIT_CONFIG_SYSTEM"
            , "GIT_CONFIG_NOSYSTEM"
            , "GIT_ATTR_NOSYSTEM"
            , "GIT_CEILING_DIRECTORIES"
            , "GIT_SSH_COMMAND"
            , "GIT_ASKPASS"
            , "GIT_PROXY_COMMAND"
            , "GIT_EXEC_PATH"
            ]
        sanitized = filter
            (\(name, _) ->
                name `notElem` blocked
                    && not ("GIT_CONFIG_KEY_" `prefixOf` name)
                    && not ("GIT_CONFIG_VALUE_" `prefixOf` name))
            inherited
    safePath <-
        case lookup "PATH" sanitized of
            Nothing -> pure Nothing
            Just value -> sanitizeSearchPathOutside root value
    let
        sanitizedWithSafePath =
            case safePath of
                Nothing -> filter ((/= "PATH") . fst) sanitized
                Just value ->
                    ("PATH", value) : filter ((/= "PATH") . fst) sanitized
        retained
            | executable == "git" =
                filter (\(name, _) -> name `elem` gitEnvironmentAllowlist)
                    sanitizedWithSafePath
            | otherwise = sanitizedWithSafePath
    pure
        ( [ ("GIT_TERMINAL_PROMPT", "0")
          , ("GCM_INTERACTIVE", "never")
          , ("GH_PROMPT_DISABLED", "true")
          , ("SSH_ASKPASS_REQUIRE", "never")
          ]
            <> retained
        )
  where
    prefixOf prefix value = take (length prefix) value == prefix
    gitEnvironmentAllowlist =
        [ "PATH"
        , "HOME"
        , "TMPDIR"
        , "TMP"
        , "TEMP"
        , "LANG"
        , "LC_ALL"
        , "LC_CTYPE"
        , "USER"
        , "LOGNAME"
        , "SSH_AUTH_SOCK"
        , "XDG_CONFIG_HOME"
        , "XDG_CONFIG_DIRS"
        , "SSL_CERT_FILE"
        , "SSL_CERT_DIR"
        , "NIX_SSL_CERT_FILE"
        , "TERM"
        ]

deliveryErrorText :: DeliveryError -> Text
deliveryErrorText = \case
    DeliveryInvalidRequest message -> message
    DeliveryStale message -> message
    DeliveryUnavailable message -> message
    DeliveryCommandFailed message -> message
    DeliveryConfirmationRejected message -> message

trySynchronous :: IO value -> IO (Either SomeException value)
trySynchronous action =
    tryAny action >>= \case
        Left exception
            | isAsyncException exception -> throwIO exception
            | otherwise -> pure (Left exception)
        Right value -> pure (Right value)

confirmationLifetimeSeconds :: POSIXTime
confirmationLifetimeSeconds = 10 * 60

confirmationLifetimeNanos :: Word64
confirmationLifetimeNanos = 10 * 60 * 1_000_000_000

saturatingAdd :: Word64 -> Word64 -> Word64
saturatingAdd left right
    | maxBound - left < right = maxBound
    | otherwise = left + right

localTimeoutMicros :: Int
localTimeoutMicros = 15 * 1_000_000

networkTimeoutMicros :: Int
networkTimeoutMicros = 60 * 1_000_000

processPipeTeardownMicros :: Int
processPipeTeardownMicros = 1_000_000

processGroupTermGraceMicros :: Int
processGroupTermGraceMicros = 2_000_000

maxProcessOutputBytes :: Int
maxProcessOutputBytes = 1024 * 1024

maxActiveConfirmations :: Int
maxActiveConfirmations = 1024

{-# NOINLINE deliveryConfirmations #-}
deliveryConfirmations :: MVar (Map Text StoredConfirmation)
deliveryConfirmations = unsafePerformIO do
    confirmations <- newMVar Map.empty
    _ <- forkIO $ forever do
        threadDelay 60_000_000
        monotonicNow <- getMonotonicTimeNSec
        modifyMVar_ confirmations
            (pure . Map.filter
                (\stored -> stored.storedDeadlineNanos > monotonicNow))
    pure confirmations
