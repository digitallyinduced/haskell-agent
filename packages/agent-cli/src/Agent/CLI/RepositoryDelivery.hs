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
import Control.Concurrent.Async (wait, withAsync)
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
    , throwIO
    , tryAny
    )
import Control.Monad (forever, unless, when)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
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
import Data.Word (Word8)
import System.Exit (ExitCode(..))
import System.Environment (getEnvironment)
import System.IO
    ( Handle
    , IOMode(ReadMode)
    , hClose
    , openBinaryFile
    )
import System.IO.Unsafe (unsafePerformIO)
import System.FilePath (isAbsolute)
import System.Posix.Signals
    ( Signal
    , sigKILL
    , sigTERM
    , signalProcessGroup
    )
import System.Process
    ( CreateProcess(..)
    , ProcessHandle
    , StdStream(CreatePipe)
    , createProcess
    , getPid
    , getProcessExitCode
    , proc
    , terminateProcess
    , waitForProcess
    )
import System.Timeout (timeout)

import Agent.CLI.RepositoryReview
    ( RepositorySnapshot(..)
    , repositorySnapshot
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
    , storedExpiresAt :: !POSIXTime
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
                                        dryRun <- runGit
                                            status.deliveryRoot
                                            (remoteCommandPrefix remote
                                            <> [ "push"
                                            , "--porcelain"
                                            , "--dry-run"
                                            , "--no-verify"
                                            , leaseArgument status
                                            , "--"
                                            , BS8.unpack remote.validatedRemoteUrl
                                            , pushRefspec status
                                            ])
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
                                                            runGit
                                                                current.deliveryRoot
                                                                (remoteCommandPrefix previewedRemote
                                                                <> [ "push"
                                                                , "--porcelain"
                                                                , "--no-verify"
                                                                , leaseArgument current
                                                                , "--"
                                                                , BS8.unpack previewedRemote.validatedRemoteUrl
                                                                , pushRefspec current
                                                                ])
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
                                                                                        runGh
                                                                                            current.deliveryRoot
                                                                                            [ "pr"
                                                                                            , "create"
                                                                                            , "--repo"
                                                                                            , Text.unpack repository
                                                                                            , "--base"
                                                                                            , Text.unpack base
                                                                                            , "--head"
                                                                                            , Text.unpack current.deliveryBranch
                                                                                            , "--title"
                                                                                            , Text.unpack title
                                                                                            , "--body-file"
                                                                                            , "-"
                                                                                            ]
                                                                                            (TextEncoding.encodeUtf8 body)
                                                                                            networkTimeoutMicros >>= \case
                                                                                                Left err ->
                                                                                                    pure (Left err)
                                                                                                Right output ->
                                                                                                    pure
                                                                                                        (parsePullRequestUrl
                                                                                                            repository
                                                                                                            output)
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
            readValidatedRemote root remote >>= \case
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
    -> IO (Either DeliveryError ValidatedRemote)
readValidatedRemote root remote =
    readUrls False >>= \case
        Left _ -> invalid
        Right [fetchUrl] ->
            readUrls True >>= \case
                Right [pushUrl]
                    | fetchUrl == pushUrl
                        && validRemoteUrl pushUrl ->
                            runGit
                                root
                                ["hash-object", "--stdin"]
                                pushUrl
                                localTimeoutMicros >>= \case
                                    Left err -> pure (Left err)
                                    Right fingerprint ->
                                        pure
                                            (Right
                                                ValidatedRemote
                                                    { validatedRemoteUrl = pushUrl
                                                    , validatedRemoteFingerprint =
                                                        decodeTrimmed fingerprint
                                                    , validatedGitHubRepository =
                                                        githubRepositoryFromUrl pushUrl
                                                    })
                _ -> invalid
        _ -> invalid
  where
    readUrls push =
        runGit
            root
            ( [ "remote"
              , "get-url"
              ]
                <> (if push then ["--push"] else [])
                <> [ "--all"
                   , Text.unpack remote
                   ]
            )
            BS.empty
            localTimeoutMicros >>= \case
                Left _ -> pure (Left ())
                Right bytes ->
                    pure
                        (Right
                            [ url
                            | url <- map stripLineEnding (BS8.lines bytes)
                            , not (BS.null url)
                            , not (BS.elem 0 url)
                            ])
    invalid =
        pure
            (Left
                (DeliveryInvalidRequest
                    "delivery requires one identical fetch and push destination"))

validatedRemoteForStatus
    :: DeliveryStatus
    -> IO (Either DeliveryError ValidatedRemote)
validatedRemoteForStatus status =
    readValidatedRemote status.deliveryRoot status.deliveryRemote >>= \case
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

queryRemoteHead
    :: DeliveryStatus
    -> ValidatedRemote
    -> IO (Either DeliveryError (Maybe Text))
queryRemoteHead status remote =
    runGit
        status.deliveryRoot
        (remoteCommandPrefix remote
        <> [ "ls-remote"
        , "--heads"
        , "--"
        , BS8.unpack remote.validatedRemoteUrl
        , Text.unpack status.deliveryUpstreamRef
        ])
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
    runGit
        status.deliveryRoot
        (remoteCommandPrefix remote
        <> [ "ls-remote"
        , "--exit-code"
        , "--heads"
        , "--"
        , BS8.unpack remote.validatedRemoteUrl
        , "refs/heads/" <> Text.unpack base
        ])
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
                    , Text.unpack repository
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
                , Text.unpack repository
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

instance Aeson.FromJSON RepositoryIdentity where
    parseJSON = Aeson.withObject "RepositoryIdentity" \object ->
        RepositoryIdentity <$> object Aeson..: "nameWithOwner"

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

validRemoteUrl :: BS.ByteString -> Bool
validRemoteUrl url =
    not (BS.null url)
        && BS.length url <= 4096
        && BS.all (\byte -> byte >= 0x20 && byte /= 0x7f) url
        && BS8.head url /= '-'
        && (isAbsolute (BS8.unpack url)
            || any (`BS8.isPrefixOf` url)
                [ "https://"
                , "ssh://"
                , "git://"
                , "file://"
                ]
            || validScpLike url)
  where
    validScpLike value =
        case BS8.break (== ':') value of
            (host, path) ->
                not (BS.null host)
                    && BS8.elem '@' host
                    && BS.length path > 1

remoteCommandPrefix :: ValidatedRemote -> [String]
remoteCommandPrefix remote =
    [ "-c"
    , "url."
        <> BS8.unpack remote.validatedRemoteUrl
        <> ".insteadOf="
        <> BS8.unpack remote.validatedRemoteUrl
    ]

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
    now <- getPOSIXTime
    token <- randomToken
    let expiresAt = now + confirmationLifetimeSeconds
    stored <- modifyMVar deliveryConfirmations \confirmations ->
        let active = Map.filter
                (\entry -> entry.storedExpiresAt > now)
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
                            , storedExpiresAt = expiresAt
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
                now <- getPOSIXTime
                modifyMVar deliveryConfirmations \confirmations ->
                    let pruned = Map.filter
                            (\stored -> stored.storedExpiresAt > now)
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
    runCommand root "git" arguments input timeoutMicros >>= \case
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

runGh
    :: FilePath
    -> [String]
    -> BS.ByteString
    -> Int
    -> IO (Either DeliveryError BS.ByteString)
runGh root arguments input timeoutMicros =
    runCommand root "gh" arguments input timeoutMicros >>= \case
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
runCommand root executable arguments input timeoutMicros = do
    launched <- trySynchronous
        (bracket start stop \processData ->
            timeout timeoutMicros (run processData))
    pure case launched of
        Left _ -> Left ProcessLaunchFailed
        Right Nothing -> Left ProcessTimedOut
        Right (Just result) -> result
  where
    start = do
        environment <- nonInteractiveEnvironment executable
        (maybeInput, maybeOutput, maybeError, process) <-
            createProcess
                (proc executable arguments)
                    { cwd = Just root
                    , std_in = CreatePipe
                    , std_out = CreatePipe
                    , std_err = CreatePipe
                    , close_fds = True
                    , create_group = True
                    , env = Just environment
                    }
        case (maybeInput, maybeOutput, maybeError) of
            (Just inputHandle, Just outputHandle, Just errorHandle) ->
                pure (inputHandle, outputHandle, errorHandle, process)
            _ -> do
                terminateProcess process
                _ <- waitForProcess process
                fail "could not create command pipes"
    stop (inputHandle, outputHandle, errorHandle, process) = do
        closeQuietly inputHandle
        closeQuietly outputHandle
        closeQuietly errorHandle
        getProcessExitCode process >>= \case
            Just _ -> pure ()
            Nothing -> do
                terminateProcessGroup sigTERM process
                threadDelay 100_000
                getProcessExitCode process >>= \case
                    Just _ -> pure ()
                    Nothing -> terminateProcessGroup sigKILL process
        _ <- tryAny (waitForProcess process)
        pure ()
    run (inputHandle, outputHandle, errorHandle, process) =
        withAsync
            (BS.hPut inputHandle input `finally` closeQuietly inputHandle)
            \inputWriter ->
                withAsync (readBounded outputHandle) \outputReader ->
                    withAsync (readBounded errorHandle) \errorReader -> do
                        exitCode <- waitForProcess process
                        _ <- wait inputWriter
                        (output, outputTruncated) <- wait outputReader
                        (errors, errorsTruncated) <- wait errorReader
                        let truncated = outputTruncated || errorsTruncated
                        pure
                            (if truncated
                                then Left ProcessOutputExceeded
                                else
                                    Right
                                        ProcessResult
                                            { processExitCode = exitCode
                                            , processStdout = output
                                            , processStderr = errors
                                            , processOutputTruncated = False
                                            })

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
    -> ProcessHandle
    -> IO ()
terminateProcessGroup signal process = do
    pid <- getPid process
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

nonInteractiveEnvironment :: FilePath -> IO [(String, String)]
nonInteractiveEnvironment executable = do
    inherited <- getEnvironment
    let blocked =
            [ "GIT_TERMINAL_PROMPT"
            , "GCM_INTERACTIVE"
            , "GH_PROMPT_DISABLED"
            , "GH_REPO"
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
        retained
            | executable == "git" =
                filter (\(name, _) -> name `elem` gitEnvironmentAllowlist)
                    sanitized
            | otherwise = sanitized
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

localTimeoutMicros :: Int
localTimeoutMicros = 15 * 1_000_000

networkTimeoutMicros :: Int
networkTimeoutMicros = 60 * 1_000_000

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
        now <- getPOSIXTime
        modifyMVar_ confirmations
            (pure . Map.filter (\stored -> stored.storedExpiresAt > now))
    pure confirmations
