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

import Agent.CLI.RepositoryDelivery.ConfirmationStore
    ( ConfirmationStore
    , insertConfirmation
    , newConfirmationStore
    , takeConfirmation
    )
import Agent.CLI.RepositoryDelivery.Process
    ( ProcessResult(..)
    , ProcessFailure(..)
    , runCommand
    , runCommandWithEnvironment
    , trySynchronous
    )
import Agent.CLI.RepositoryDelivery.Validation
import Control.Exception.Safe
    ( bracket
    , onException
    )
import Control.Monad (unless, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except
    ( ExceptT(..)
    , runExceptT
    , throwE
    )
import Crypto.Hash (Digest, SHA1, SHA256, hash)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import Data.Char (isHexDigit, isSpace)
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
import System.IO
    ( IOMode(ReadMode)
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
import System.Posix.Temp (mkdtemp)
import System.Timeout (timeout)

import Agent.CLI.RepositoryReview
    ( RepositorySnapshot(..)
    , repositorySnapshot
    )
import Agent.CLI.ProcessSecurity
    ( canonicalPathOutside
    , resolveExecutableOutside
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

type Delivery = ExceptT DeliveryError IO

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
    , storedConfirmation :: !Confirmation
    }

repositoryDeliveryStatus
    :: FilePath
    -> Text
    -> IO (Either DeliveryError DeliveryStatus)
repositoryDeliveryStatus requested expected =
    runExceptT (repositoryDeliveryStatusT requested expected)

repositoryDeliveryStatusT
    :: FilePath
    -> Text
    -> Delivery DeliveryStatus
repositoryDeliveryStatusT requested expected = do
    snapshotResult <-
        liftIO (timeout localTimeoutMicros (repositorySnapshot requested))
    snapshot <- case snapshotResult of
        Nothing ->
            throwE
                (DeliveryCommandFailed
                    "repository snapshot verification timed out")
        Just (Left _) ->
            throwE
                (DeliveryCommandFailed
                    "repository state could not be verified")
        Just (Right snapshot) -> pure snapshot
    when (snapshot.snapshotId /= expected) $
        throwE (DeliveryStale "repository state changed")
    liftDelivery (statusAtSnapshot snapshot)

previewRepositoryPush
    :: FilePath
    -> Text
    -> IO (Either DeliveryError PushPreview)
previewRepositoryPush requested expected =
    runExceptT do
        status <- repositoryDeliveryStatusT requested expected
        remote <- liftDelivery (validatedRemoteForStatus status)
        oid <- liftDelivery (queryRemoteHead status remote)
        unless (remoteMatchesExpected status oid) $
            throwE
                (DeliveryStale
                    "the remote branch changed; fetch before previewing a push")
        when (status.deliveryAhead <= 0) $
            throwE
                (DeliveryInvalidRequest
                    "the branch has no commits to push")
        when (status.deliveryBehind /= 0) $
            throwE
                (DeliveryInvalidRequest
                    "the branch is behind its upstream")
        liftDelivery (proveFastForward status)
        _ <- liftDelivery $
            runNetworkGit
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
        (token, expiresAt) <-
            liftIO
                (storeConfirmation
                    status.deliveryRoot
                    (PushConfirmation status remote))
        pure
            PushPreview
                { pushPreviewStatus = status
                , pushPreviewConfirmation = token
                , pushPreviewExpiresAt = expiresAt
                }

confirmRepositoryPush
    :: FilePath
    -> Text
    -> IO (Either DeliveryError DeliveryStatus)
confirmRepositoryPush requested token =
    runExceptT do
        confirmation <- liftDelivery (consumeConfirmation requested token)
        (previewed, previewedRemote) <- case confirmation of
            PushConfirmation status remote -> pure (status, remote)
            PullRequestConfirmation {} ->
                throwE
                    (DeliveryConfirmationRejected
                        "confirmation token is for a different operation")
        current <-
            repositoryDeliveryStatusT
                requested
                previewed.deliverySnapshotId
        when (current /= previewed) $
            throwE
                (DeliveryStale
                    "branch or upstream state changed after push preview")
        currentRemote <- liftDelivery (validatedRemoteForStatus current)
        when (currentRemote /= previewedRemote) $
            throwE
                (DeliveryStale
                    "the remote destination changed after push preview")
        liftDelivery (proveFastForward current)
        remoteOid <- liftDelivery (queryRemoteHead current previewedRemote)
        unless (remoteMatchesExpected current remoteOid) $
            throwE
                (DeliveryStale
                    "the remote branch changed after push preview")
        liftDelivery (revalidateLocalMutation current)
        -- The explicit object ID prevents any still-later local ref movement
        -- from changing what is delivered.
        _ <- liftDelivery $
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
                networkTimeoutMicros
        liftDelivery (refreshPushedStatus current previewedRemote)

previewPullRequest
    :: FilePath
    -> Text
    -> Text
    -> Text
    -> Text
    -> IO (Either DeliveryError PullRequestPreview)
previewPullRequest requested expected base title body =
    runExceptT do
        either throwE pure (validatePullRequestInput base title body)
        status <- repositoryDeliveryStatusT requested expected
        when (status.deliveryAhead /= 0 || status.deliveryBehind /= 0) $
            throwE
                (DeliveryInvalidRequest
                    "push the exact branch state before previewing a pull request")
        remote <- liftDelivery (validatedRemoteForStatus status)
        remoteOid <- liftDelivery (queryRemoteHead status remote)
        when (remoteOid /= Just status.deliveryHeadOid) $
            throwE
                (DeliveryStale
                    "the pushed remote branch does not match HEAD")
        repository <- case remote.validatedGitHubRepository of
            Nothing ->
                throwE
                    (DeliveryInvalidRequest
                        "the delivery remote is not a supported GitHub repository")
            Just repository -> pure repository
        liftDelivery (requireGitHubCli status.deliveryRoot repository)
        liftDelivery
            (ensureBaseAndNoOpenPullRequest
                status remote repository base)
        (token, expiresAt) <-
            liftIO
                (storeConfirmation
                    status.deliveryRoot
                    (PullRequestConfirmation
                        status remote repository base title body))
        pure
            PullRequestPreview
                { pullRequestRepository = repository
                , pullRequestBaseRef = base
                , pullRequestHeadRef = status.deliveryBranch
                , pullRequestTitle = title
                , pullRequestConfirmation = token
                , pullRequestExpiresAt = expiresAt
                }

createPullRequest
    :: FilePath
    -> Text
    -> IO (Either DeliveryError Text)
createPullRequest requested token =
    runExceptT do
        confirmation <- liftDelivery (consumeConfirmation requested token)
        (previewed, previewedRemote, repository, base, title, body) <-
            case confirmation of
                PullRequestConfirmation
                    status remote repo baseRef prTitle prBody ->
                        pure (status, remote, repo, baseRef, prTitle, prBody)
                PushConfirmation {} ->
                    throwE
                        (DeliveryConfirmationRejected
                            "confirmation token is for a different operation")
        current <-
            repositoryDeliveryStatusT
                requested
                previewed.deliverySnapshotId
        when (current /= previewed) $
            throwE
                (DeliveryStale
                    "branch or upstream state changed after pull-request preview")
        currentRemote <- liftDelivery (validatedRemoteForStatus current)
        when (currentRemote /= previewedRemote) $
            throwE
                (DeliveryStale
                    "the remote destination changed after preview")
        remoteOid <- liftDelivery (queryRemoteHead current previewedRemote)
        when (remoteOid /= Just current.deliveryHeadOid) $
            throwE
                (DeliveryStale
                    "the pushed remote branch changed after preview")
        liftDelivery (requireGitHubCli current.deliveryRoot repository)
        liftDelivery
            (ensureBaseAndNoOpenPullRequest
                current previewedRemote repository base)
        liftDelivery
            (revalidatePullRequestMutation current previewedRemote)
        liftDelivery (revalidateLocalMutation current)
        liftDelivery
            (createGitHubPullRequest current repository base title body)

liftDelivery :: IO (Either DeliveryError value) -> Delivery value
liftDelivery = ExceptT

statusAtSnapshot
    :: RepositorySnapshot
    -> IO (Either DeliveryError DeliveryStatus)
statusAtSnapshot snapshot = runExceptT do
    headOid <- maybe
        (throwE (DeliveryInvalidRequest
            "delivery requires a branch with at least one commit"))
        pure
        snapshot.snapshotHead
    branchBytes <- liftIO
        (runGit root ["symbolic-ref", "--quiet", "HEAD"] BS.empty
            localTimeoutMicros)
        >>= either
            (const (throwE (DeliveryInvalidRequest
                "delivery requires a named local branch")))
            pure
    let fullBranch = decodeTrimmed branchBytes
    branch <- maybe
        (throwE (DeliveryInvalidRequest
            "Git returned an invalid local branch"))
        pure
        (Text.stripPrefix "refs/heads/" fullBranch)
    unless (validateBranchName branch) $
        throwE (DeliveryInvalidRequest
            "local branch name is not safe for delivery")
    (remote, upstreamRef) <- liftDelivery (readUpstream root fullBranch)
    readUpstreamStatus headOid branch remote upstreamRef
  where
    root = snapshot.snapshotRoot
    readUpstreamStatus headOid branch remote upstreamRef = do
        upstream <- liftIO $
            runGit root ["rev-parse", "--verify", "@{upstream}"] BS.empty
                localTimeoutMicros
        case upstream of
            Left _ -> do
                countBytes <- liftDelivery $
                    runGit root ["rev-list", "--count", "HEAD"] BS.empty
                        localTimeoutMicros
                case reads (BS8.unpack (stripLineEnding countBytes)) of
                    [(ahead, "")] | ahead > 0 ->
                        finishStatus (zeroObjectId headOid) ahead 0
                    _ -> throwE (DeliveryCommandFailed
                        "Git returned an invalid commit count")
            Right upstreamBytes -> do
                counts <- liftDelivery $
                    runGit root
                        ["rev-list", "--left-right", "--count", "HEAD...@{upstream}"]
                        BS.empty localTimeoutMicros
                (ahead, behind) <- maybe
                    (throwE (DeliveryCommandFailed
                        "Git returned invalid ahead/behind counts"))
                    pure
                    (parseAheadBehind counts)
                finishStatus (decodeTrimmed upstreamBytes) ahead behind
      where
        finishStatus upstreamOid ahead behind = do
            validatedRemote <- liftDelivery $
                readValidatedRemote root headOid remote
            pure DeliveryStatus
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
                }

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
revalidateLocalMutation expected = runExceptT do
    current <- liftDelivery $
        repositoryDeliveryStatus
            expected.deliveryRoot expected.deliverySnapshotId
    unless (current == expected) $
        throwE (DeliveryStale
            "repository state changed immediately before delivery")

revalidatePullRequestMutation
    :: DeliveryStatus
    -> ValidatedRemote
    -> IO (Either DeliveryError ())
revalidatePullRequestMutation expected expectedRemote = runExceptT do
    current <- liftDelivery $
        repositoryDeliveryStatus
            expected.deliveryRoot expected.deliverySnapshotId
    unless (current == expected) $
        throwE (DeliveryStale
            "branch or upstream state changed before pull-request creation")
    remote <- liftDelivery (validatedRemoteForStatus current)
    unless (remote == expectedRemote) $
        throwE (DeliveryStale
            "the remote destination changed before pull-request creation")
    oid <- liftDelivery (queryRemoteHead current expectedRemote)
    unless (oid == Just current.deliveryHeadOid) $
        throwE (DeliveryStale
            "the pushed branch changed before pull-request creation")

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
    stored <-
        insertConfirmation
            deliveryConfirmations
            maxActiveConfirmations
            token
            deadline
            StoredConfirmation
                { storedRoot = root
                , storedConfirmation = confirmation
                }
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
                takeConfirmation
                    deliveryConfirmations
                    token
                    monotonicNow >>= \case
                        Nothing ->
                            pure
                                (Left
                                    (DeliveryConfirmationRejected
                                        "confirmation token expired or was already used")
                                )
                        Just stored ->
                            if stored.storedRoot /= snapshot.snapshotRoot
                                then
                                    pure
                                        (Left
                                            (DeliveryConfirmationRejected
                                                "confirmation token belongs to another repository")
                                        )
                                else
                                    pure
                                        (Right stored.storedConfirmation)

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

stripLineEnding :: BS.ByteString -> BS.ByteString
stripLineEnding = BS8.dropWhileEnd (`elem` ['\r', '\n'])

decodeTrimmed :: BS.ByteString -> Text
decodeTrimmed =
    Text.strip . TextEncoding.decodeUtf8With lenientDecode

deliveryErrorText :: DeliveryError -> Text
deliveryErrorText = \case
    DeliveryInvalidRequest message -> message
    DeliveryStale message -> message
    DeliveryUnavailable message -> message
    DeliveryCommandFailed message -> message
    DeliveryConfirmationRejected message -> message

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

maxActiveConfirmations :: Int
maxActiveConfirmations = 1024

{-# NOINLINE deliveryConfirmations #-}
deliveryConfirmations :: ConfirmationStore StoredConfirmation
deliveryConfirmations = unsafePerformIO newConfirmationStore
