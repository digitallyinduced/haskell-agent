module Agent.CLI.RepositoryDeliverySpec (spec) where

import Agent.CLI.RepositoryDelivery
import Agent.CLI.RepositoryReview
    ( RepositorySnapshot(..)
    , repositorySnapshot
    )
import System.Timeout (timeout)
import Control.Concurrent (threadDelay)
import Control.Exception.Safe (bracket)
import qualified Data.Text as Text
import System.Directory
    ( createDirectory
    , doesFileExist
    , findExecutable
    , getTemporaryDirectory
    , removePathForcibly
    )
import System.Environment (getEnv, lookupEnv, setEnv, unsetEnv)
import System.Exit (ExitCode(..))
import System.IO (hClose, openTempFile)
import System.Posix.Files
    ( ownerExecuteMode
    , ownerReadMode
    , ownerWriteMode
    , setFileMode
    , unionFileModes
    )
import System.Process
    ( CreateProcess(..)
    , proc
    , readCreateProcessWithExitCode
    )
import Test.Hspec

spec :: Spec
spec = describe "repository delivery service" do
    it "validates branch and remote names without option/ref injection" do
        validateBranchName "feature/safe-name" `shouldBe` True
        validateBranchName "-force" `shouldBe` False
        validateBranchName "bad..name" `shouldBe` False
        validateBranchName "bad name" `shouldBe` False
        validateBranchName ".hidden" `shouldBe` False
        validateBranchName "topic/.hidden" `shouldBe` False
        validateBranchName "topic.lock" `shouldBe` False
        validateRemoteName "origin" `shouldBe` True
        validateRemoteName "-origin" `shouldBe` False
        validateRemoteName "origin\n--upload-pack=evil" `shouldBe` False

    it "rejects relative paths and custom remote-helper transports" $
        withDeliveryRepository \root _ -> do
            snapshot <- expectRight =<< repositorySnapshot root
            _ <- git root ["remote", "set-url", "origin", "../redirected.git"]
            repositoryDeliveryStatus root snapshot.snapshotId
                `shouldReturnSatisfying` isInvalid
            _ <- git root
                [ "remote"
                , "set-url"
                , "origin"
                , "ext::sh -c 'touch should-not-run'"
                ]
            repositoryDeliveryStatus root snapshot.snapshotId
                `shouldReturnSatisfying` isInvalid

    it "rejects credential-bearing URLs without echoing their secret" $
        withDeliveryRepository \root _ -> do
            snapshot <- expectRight =<< repositorySnapshot root
            _ <- git root
                [ "remote"
                , "set-url"
                , "origin"
                , "ssh://git@example.test/owner/repository.git"
                ]
            _ <- expectRight
                =<< repositoryDeliveryStatus root snapshot.snapshotId
            let secret = "delivery-secret-must-not-escape"
                credentialUrl =
                    "https://user:"
                        <> secret
                        <> "@example.test/owner/repository.git"
            _ <- git root ["remote", "set-url", "origin", credentialUrl]
            result <- repositoryDeliveryStatus root snapshot.snapshotId
            result `shouldSatisfy` isInvalid
            case result of
                Left err -> do
                    deliveryErrorText err
                        `shouldNotSatisfy` Text.isInfixOf (Text.pack secret)
                    deliveryErrorText err
                        `shouldNotSatisfy`
                            Text.isInfixOf (Text.pack credentialUrl)
                Right _ -> expectationFailure "credential URL was accepted"

    it "rejects credential-bearing SCP-like URLs" $
        withDeliveryRepository \root _ -> do
            snapshot <- expectRight =<< repositorySnapshot root
            _ <- git root
                [ "remote"
                , "set-url"
                , "origin"
                , "git@owner:secret@example.test/repository.git"
                ]
            repositoryDeliveryStatus root snapshot.snapshotId
                `shouldReturnSatisfying` isInvalid

    it "previews and confirms one exact fast-forward lease push once" $
        withDeliveryRepository \root remote -> do
            appendFile (root <> "/tracked.txt") "second\n"
            _ <- git root ["add", "tracked.txt"]
            _ <- git root ["commit", "-q", "-m", "second"]

            snapshot <- expectRight =<< repositorySnapshot root
            status <- expectRight
                =<< repositoryDeliveryStatus root snapshot.snapshotId
            status.deliveryBranch `shouldBe` "main"
            status.deliveryRemote `shouldBe` "origin"
            status.deliveryAhead `shouldBe` 1
            status.deliveryBehind `shouldBe` 0

            let hookMarker = root <> "/hook-ran"
                hook = root <> "/.git/hooks/pre-push"
            writeFile hook
                ("#!/bin/sh\nprintf ran > " <> shellQuote hookMarker <> "\n")
            setFileMode hook
                (ownerReadMode
                    `unionFileModes` ownerWriteMode
                    `unionFileModes` ownerExecuteMode)
            preview <- expectRight
                =<< previewRepositoryPush root snapshot.snapshotId
            Text.length preview.pushPreviewConfirmation `shouldBe` 64
            pushed <- expectRight
                =<< confirmRepositoryPush
                    root
                    preview.pushPreviewConfirmation
            remoteHead <- Text.strip
                <$> git root
                    [ "--git-dir"
                    , remote
                    , "rev-parse"
                    , "refs/heads/main"
                    ]
            remoteHead `shouldBe` pushed.deliveryHeadOid
            doesFileExist hookMarker `shouldReturn` False

            confirmRepositoryPush root preview.pushPreviewConfirmation
                `shouldReturnSatisfying` isConfirmationRejection

    it "creates an unborn configured upstream using an exact nonexistence lease" $
        withDeliveryRepository \root remote -> do
            _ <- git root ["switch", "-q", "-c", "new-branch"]
            _ <- git root ["config", "branch.new-branch.remote", "origin"]
            _ <- git root
                [ "config"
                , "branch.new-branch.merge"
                , "refs/heads/new-branch"
                ]
            snapshot <- expectRight =<< repositorySnapshot root
            preview <- expectRight
                =<< previewRepositoryPush root snapshot.snapshotId
            pushed <- expectRight
                =<< confirmRepositoryPush
                    root preview.pushPreviewConfirmation
            remoteHead <- Text.strip
                <$> git root
                    [ "--git-dir"
                    , remote
                    , "rev-parse"
                    , "refs/heads/new-branch"
                    ]
            remoteHead `shouldBe` pushed.deliveryHeadOid

    it "consumes a preview without pushing when the local snapshot changes" $
        withDeliveryRepository \root remote -> do
            appendFile (root <> "/tracked.txt") "second\n"
            _ <- git root ["add", "tracked.txt"]
            _ <- git root ["commit", "-q", "-m", "second"]
            snapshot <- expectRight =<< repositorySnapshot root
            preview <- expectRight
                =<< previewRepositoryPush root snapshot.snapshotId
            remoteBefore <- Text.strip
                <$> git root
                    [ "--git-dir"
                    , remote
                    , "rev-parse"
                    , "refs/heads/main"
                    ]

            writeFile (root <> "/untracked.txt") "changes exact snapshot\n"
            confirmRepositoryPush root preview.pushPreviewConfirmation
                `shouldReturnSatisfying` isStale
            remoteAfter <- Text.strip
                <$> git root
                    [ "--git-dir"
                    , remote
                    , "rev-parse"
                    , "refs/heads/main"
                    ]
            remoteAfter `shouldBe` remoteBefore
            confirmRepositoryPush root preview.pushPreviewConfirmation
                `shouldReturnSatisfying` isConfirmationRejection

    it "rejects an externally changed remote after preview without pushing" $
        withDeliveryRepository \root remote -> do
            appendFile (root <> "/tracked.txt") "second\n"
            _ <- git root ["add", "tracked.txt"]
            _ <- git root ["commit", "-q", "-m", "second"]
            snapshot <- expectRight =<< repositorySnapshot root
            preview <- expectRight
                =<< previewRepositoryPush root snapshot.snapshotId
            initial <- Text.strip <$> git root ["rev-parse", "HEAD^"]
            tree <- Text.strip <$> git root ["rev-parse", "HEAD^{tree}"]
            divergent <- Text.strip
                <$> git root
                    [ "commit-tree"
                    , Text.unpack tree
                    , "-p"
                    , Text.unpack initial
                    , "-m"
                    , "remote advance"
                    ]
            _ <- git root
                [ "push"
                , "-q"
                , remote
                , Text.unpack divergent <> ":refs/heads/main"
                ]

            confirmRepositoryPush root preview.pushPreviewConfirmation
                `shouldReturnSatisfying` isStale
            remoteHead <- Text.strip
                <$> git root
                    [ "--git-dir"
                    , remote
                    , "rev-parse"
                    , "refs/heads/main"
                    ]
            remoteHead `shouldBe` divergent

    it "rejects a remote rewind observed after preview" $
        withDeliveryRepository \root remote -> do
            appendFile (root <> "/tracked.txt") "second\n"
            _ <- git root ["add", "tracked.txt"]
            _ <- git root ["commit", "-q", "-m", "second"]
            _ <- git root ["push", "-q", "origin", "main"]
            appendFile (root <> "/tracked.txt") "third\n"
            _ <- git root ["add", "tracked.txt"]
            _ <- git root ["commit", "-q", "-m", "third"]
            snapshot <- expectRight =<< repositorySnapshot root
            preview <- expectRight
                =<< previewRepositoryPush root snapshot.snapshotId
            rewound <- Text.strip <$> git root ["rev-parse", "HEAD^^"]
            _ <- git root
                [ "--git-dir"
                , remote
                , "update-ref"
                , "refs/heads/main"
                , Text.unpack rewound
                ]

            confirmRepositoryPush root preview.pushPreviewConfirmation
                `shouldReturnSatisfying` isStale
            remoteHead <- Text.strip
                <$> git root
                    [ "--git-dir"
                    , remote
                    , "rev-parse"
                    , "refs/heads/main"
                    ]
            remoteHead `shouldBe` rewound

    it "binds confirmation to the upstream push destination" $
        withDeliveryRepository \root remote -> do
            appendFile (root <> "/tracked.txt") "second\n"
            _ <- git root ["add", "tracked.txt"]
            _ <- git root ["commit", "-q", "-m", "second"]
            snapshot <- expectRight =<< repositorySnapshot root
            preview <- expectRight
                =<< previewRepositoryPush root snapshot.snapshotId
            let redirected = remote <> "-redirected"
            _ <- git root ["init", "-q", "--bare", redirected]
            _ <- git root ["remote", "set-url", "origin", redirected]

            confirmRepositoryPush root preview.pushPreviewConfirmation
                `shouldReturnSatisfying` isStale
            originalHead <- Text.strip
                <$> git root
                    [ "--git-dir"
                    , remote
                    , "rev-parse"
                    , "refs/heads/main"
                    ]
            originalHead `shouldBe` preview.pushPreviewStatus.deliveryUpstreamOid

    it "rejects an insteadOf config retarget after preview" $
        withDeliveryRepository \root remote -> do
            appendFile (root <> "/tracked.txt") "second\n"
            _ <- git root ["add", "tracked.txt"]
            _ <- git root ["commit", "-q", "-m", "second"]
            snapshot <- expectRight =<< repositorySnapshot root
            preview <- expectRight
                =<< previewRepositoryPush root snapshot.snapshotId
            let redirected = remote <> "-config-redirected"
            _ <- git root ["init", "-q", "--bare", redirected]
            _ <- git root
                [ "config"
                , "url." <> redirected <> ".insteadOf"
                , remote
                ]

            confirmRepositoryPush root preview.pushPreviewConfirmation
                `shouldReturnSatisfying` isStale
            originalHead <- Text.strip
                <$> git root
                    [ "--git-dir"
                    , remote
                    , "rev-parse"
                    , "refs/heads/main"
                    ]
            originalHead `shouldBe` preview.pushPreviewStatus.deliveryUpstreamOid

    it "rejects malformed PR inputs before invoking GitHub CLI" $
        withDeliveryRepository \root _ -> do
            snapshot <- expectRight =<< repositorySnapshot root
            previewPullRequest
                root snapshot.snapshotId "-bad" "title" "body"
                `shouldReturnSatisfying` isInvalid
            previewPullRequest
                root snapshot.snapshotId "main" "" "body"
                `shouldReturnSatisfying` isInvalid
            previewPullRequest
                root snapshot.snapshotId "main" "title"
                (Text.replicate (1024 * 1024 + 1) "x")
                `shouldReturnSatisfying` isInvalid

    it "previews and creates a PR through argv-only gh with a one-shot token" $
        withDeliveryRepository \root _ ->
            withFakeGh root \bodyCapture injectionMarker -> do
                snapshot <- expectRight =<< repositorySnapshot root
                let title =
                        "Safe title $(touch "
                            <> Text.pack injectionMarker
                            <> ")"
                    body = "Body is passed on stdin, not argv."
                preview <- expectRight
                    =<< previewPullRequest
                        root snapshot.snapshotId "main" title body
                preview.pullRequestRepository `shouldBe` "owner/repository"
                preview.pullRequestHeadRef `shouldBe` "main"
                url <- expectRight
                    =<< createPullRequest
                        root
                        preview.pullRequestConfirmation
                url `shouldBe`
                    "https://github.com/owner/repository/pull/123"
                readFile bodyCapture `shouldReturn` Text.unpack body
                doesFileExist injectionMarker `shouldReturn` False
                createPullRequest root preview.pullRequestConfirmation
                    `shouldReturnSatisfying` isConfirmationRejection

    it "ignores hostile GH_HOST and binds gh operations to github.com" $
        withDeliveryRepository \root _ ->
            withFakeGh root \_ _ -> do
                setEnv "GH_HOST" "attacker.example"
                snapshot <- expectRight =<< repositorySnapshot root
                preview <- expectRight
                    =<< previewPullRequest
                        root snapshot.snapshotId "main" "Title" "Body"
                _ <- createPullRequest root preview.pullRequestConfirmation
                    >>= expectRight
                pure ()

    it "kills a gh descendant retaining pipes after its leader exits" $
        withDeliveryRepository \root _ ->
            withFakeGh root \_ _ -> do
                let descendantPid = root <> "/gh-descendant.pid"
                setEnv "GH_RETAIN_PIPE_MARKER" descendantPid
                snapshot <- expectRight =<< repositorySnapshot root
                result <- timeout 5_000_000
                    (previewPullRequest
                        root snapshot.snapshotId "main" "Title" "Body")
                result `shouldSatisfy` \case
                    Just (Right _) -> True
                    _ -> False
                pid <- words <$> readFile descendantPid
                pid `shouldSatisfy` \case
                    [_] -> True
                    _ -> False
                processGoneWithin
                    3_000_000
                    (head pid)
                    `shouldReturn` True

    it "rejects a gh result URL for another repository" $
        withDeliveryRepository \root _ ->
            withFakeGh root \_ _ -> do
                setEnv "GH_FAKE_PR_URL"
                    "https://github.com/attacker/repository/pull/1"
                snapshot <- expectRight =<< repositorySnapshot root
                preview <- expectRight
                    =<< previewPullRequest
                        root snapshot.snapshotId "main" "Title" "Body"
                createPullRequest root preview.pullRequestConfirmation
                    `shouldReturnSatisfying` isCommandFailure

    it "rejects gh repository identity that differs from the bound remote" $
        withDeliveryRepository \root _ ->
            withFakeGh root \_ _ -> do
                setEnv "GH_FAKE_REPOSITORY" "attacker/repository"
                snapshot <- expectRight =<< repositorySnapshot root
                previewPullRequest
                    root snapshot.snapshotId "main" "Title" "Body"
                    `shouldReturnSatisfying` isUnavailable

withDeliveryRepository :: (FilePath -> FilePath -> IO value) -> IO value
withDeliveryRepository action =
    withTempDirectory "repository-delivery" \container -> do
        let root = container <> "/checkout"
            remote = container <> "/remote.git"
        createDirectory root
        _ <- git container ["init", "-q", "--bare", remote]
        _ <- git root ["init", "-q", "-b", "main"]
        _ <- git root ["config", "user.name", "Repository Delivery Test"]
        _ <- git root ["config", "user.email", "delivery@example.test"]
        writeFile (root <> "/tracked.txt") "first\n"
        _ <- git root ["add", "tracked.txt"]
        _ <- git root ["commit", "-q", "-m", "initial"]
        _ <- git root ["remote", "add", "origin", remote]
        _ <- git root ["push", "-q", "-u", "origin", "main"]
        action root remote

withTempDirectory :: String -> (FilePath -> IO value) -> IO value
withTempDirectory template action = do
    base <- getTemporaryDirectory
    bracket
        (do
            (path, handle) <- openTempFile base template
            hClose handle
            removePathForcibly path
            createDirectory path
            pure path)
        removePathForcibly
        action

withFakeGh
    :: FilePath
    -> (FilePath -> FilePath -> IO value)
    -> IO value
withFakeGh root action = do
    originalPath <- getEnv "PATH"
    originalCapture <- lookupEnv "GH_BODY_CAPTURE"
    originalFakeUrl <- lookupEnv "GH_FAKE_PR_URL"
    originalFakeRepository <- lookupEnv "GH_FAKE_REPOSITORY"
    originalRetainPipeMarker <- lookupEnv "GH_RETAIN_PIPE_MARKER"
    originalGhHost <- lookupEnv "GH_HOST"
    withTempDirectory "repository-delivery-gh" \bin -> do
        realGit <- maybe (fail "git not found") pure =<< findExecutable "git"
        localRemote <- Text.unpack . Text.strip
            <$> git root ["remote", "get-url", "origin"]
        let githubRemote = "https://github.com/owner/repository.git"
            executable = bin <> "/gh"
            gitExecutable = bin <> "/git"
            bodyCapture = bin <> "/body.txt"
            injectionMarker = root <> "/shell-injection"
        _ <- git root ["remote", "set-url", "origin", githubRemote]
        writeFile gitExecutable $ unlines
            [ "#!/bin/bash"
            , "set -eu"
            , "args=()"
            , "for arg in \"$@\"; do"
            , "  if [[ \"$arg\" == " <> shellQuote githubRemote <> " ]]; then"
            , "    args+=(" <> shellQuote localRemote <> ")"
            , "  else"
            , "    args+=(\"$arg\")"
            , "  fi"
            , "done"
            , "exec " <> shellQuote realGit <> " \"${args[@]}\""
            ]
        setFileMode gitExecutable
            (ownerReadMode
                `unionFileModes` ownerWriteMode
                `unionFileModes` ownerExecuteMode)
        writeFile executable $ unlines
            [ "#!/bin/sh"
            , "set -eu"
            , "[ \"${GH_HOST:-}\" = '' ] || exit 65"
            , "if [ \"${GH_RETAIN_PIPE_MARKER:-}\" != '' ] && [ \"$1 $2\" = 'auth status' ]; then"
            , "  sleep 30 &"
            , "  printf '%s\\n' \"$!\" > \"$GH_RETAIN_PIPE_MARKER\""
            , "  exit 0"
            , "fi"
            , "case \"$1 $2\" in"
            , "  'auth status')"
            , "    case \" $* \" in"
            , "      *' --hostname github.com '*) exit 0 ;;"
            , "      *) exit 65 ;;"
            , "    esac"
            , "    ;;"
            , "  'repo view')"
            , "    case \" $* \" in"
            , "      *' --repo github.com/owner/repository '*) ;;"
            , "      *) exit 65 ;;"
            , "    esac"
            , "    printf '{\"nameWithOwner\":\"%s\"}\\n' \"${GH_FAKE_REPOSITORY:-owner/repository}\""
            , "    ;;"
            , "  'pr list')"
            , "    case \" $* \" in"
            , "      *' --repo github.com/owner/repository '*) ;;"
            , "      *) exit 65 ;;"
            , "    esac"
            , "    printf '%s\\n' '[]'"
            , "    ;;"
            , "  'pr create')"
            , "    case \" $* \" in"
            , "      *' --repo github.com/owner/repository '*) ;;"
            , "      *) exit 65 ;;"
            , "    esac"
            , "    cat > \"$GH_BODY_CAPTURE\""
            , "    printf '%s\\n' \"${GH_FAKE_PR_URL:-https://github.com/owner/repository/pull/123}\""
            , "    ;;"
            , "  *) exit 64 ;;"
            , "esac"
            ]
        setFileMode executable
            (ownerReadMode
                `unionFileModes` ownerWriteMode
                `unionFileModes` ownerExecuteMode)
        bracket
            (do
                setEnv "PATH" (bin <> ":" <> originalPath)
                setEnv "GH_BODY_CAPTURE" bodyCapture)
            (\_ -> do
                setEnv "PATH" originalPath
                case originalCapture of
                    Nothing -> unsetEnv "GH_BODY_CAPTURE"
                    Just value -> setEnv "GH_BODY_CAPTURE" value
                case originalFakeUrl of
                    Nothing -> unsetEnv "GH_FAKE_PR_URL"
                    Just value -> setEnv "GH_FAKE_PR_URL" value
                case originalFakeRepository of
                    Nothing -> unsetEnv "GH_FAKE_REPOSITORY"
                    Just value -> setEnv "GH_FAKE_REPOSITORY" value
                case originalRetainPipeMarker of
                    Nothing -> unsetEnv "GH_RETAIN_PIPE_MARKER"
                    Just value -> setEnv "GH_RETAIN_PIPE_MARKER" value
                case originalGhHost of
                    Nothing -> unsetEnv "GH_HOST"
                    Just value -> setEnv "GH_HOST" value)
            (\_ -> action bodyCapture injectionMarker)

git :: FilePath -> [String] -> IO Text.Text
git root arguments = do
    (exitCode, output, errors) <-
        readCreateProcessWithExitCode
            (proc "git" arguments) { cwd = Just root }
            ""
    case exitCode of
        ExitSuccess -> pure (Text.pack output)
        ExitFailure code ->
            expectationFailure
                ("git exited " <> show code <> ": " <> errors)
                >> pure ""

expectRight :: (HasCallStack, Show error) => Either error value -> IO value
expectRight = \case
    Left err -> expectationFailure (show err) >> fail "unreachable"
    Right value -> pure value

isInvalid :: Either DeliveryError value -> Bool
isInvalid = \case
    Left (DeliveryInvalidRequest _) -> True
    _ -> False

isUnavailable :: Either DeliveryError value -> Bool
isUnavailable = \case
    Left (DeliveryUnavailable _) -> True
    _ -> False

isCommandFailure :: Either DeliveryError value -> Bool
isCommandFailure = \case
    Left (DeliveryCommandFailed _) -> True
    _ -> False

isStale :: Either DeliveryError value -> Bool
isStale = \case
    Left (DeliveryStale _) -> True
    _ -> False

isConfirmationRejection :: Either DeliveryError value -> Bool
isConfirmationRejection = \case
    Left (DeliveryConfirmationRejected _) -> True
    _ -> False

shouldReturnSatisfying
    :: (HasCallStack, Show value)
    => IO value
    -> (value -> Bool)
    -> Expectation
shouldReturnSatisfying action predicate =
    action >>= (`shouldSatisfy` predicate)

processGoneWithin :: Int -> String -> IO Bool
processGoneWithin remaining pid
    | remaining <= 0 = fmap not processExists
    | otherwise =
        processExists >>= \case
            False -> pure True
            True -> do
                threadDelay 100_000
                processGoneWithin (remaining - 100_000) pid
  where
    processExists = do
        (exitCode, _, _) <-
            readCreateProcessWithExitCode (proc "kill" ["-0", pid]) ""
        pure (exitCode == ExitSuccess)

shellQuote :: String -> String
shellQuote value = "'" <> concatMap escape value <> "'"
  where
    escape '\'' = "'\"'\"'"
    escape character = [character]
