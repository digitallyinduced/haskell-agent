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
    , doesDirectoryExist
    , doesFileExist
    , findExecutable
    , getTemporaryDirectory
    , listDirectory
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

    it "selects the delivery destination from local config without includes" $
        withDeliveryRepository \root _ -> do
            let includedConfig = root <> "/included-remote.config"
            writeFile includedConfig $ unlines
                [ "[remote \"origin\"]"
                , "  url = " <> deliveryRemoteUrl
                ]
            _ <- git root ["config", "--unset-all", "remote.origin.url"]
            _ <- git root ["config", "include.path", includedConfig]
            snapshot <- expectRight =<< repositorySnapshot root
            repositoryDeliveryStatus root snapshot.snapshotId
                `shouldReturnSatisfying` isInvalid

    it "rejects credential-bearing URLs without echoing their secret" $
        withDeliveryRepository \root _ -> do
            snapshot <- expectRight =<< repositorySnapshot root
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

    it "rejects percent-encoded credential-bearing URLs" $
        withDeliveryRepository \root _ -> do
            snapshot <- expectRight =<< repositorySnapshot root
            _ <- git root
                [ "remote"
                , "set-url"
                , "origin"
                , "ssh://git%3Asecret@example.test/repository.git"
                ]
            repositoryDeliveryStatus root snapshot.snapshotId
                `shouldReturnSatisfying` isInvalid
            _ <- git root
                [ "remote"
                , "set-url"
                , "origin"
                , "git%3Asecret@example.test:owner/repository.git"
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

    it "rejects a push token for PR creation and still consumes it" $
        withDeliveryRepository \root _ -> do
            appendFile (root <> "/tracked.txt") "second\n"
            _ <- git root ["add", "tracked.txt"]
            _ <- git root ["commit", "-q", "-m", "second"]
            snapshot <- expectRight =<< repositorySnapshot root
            preview <- expectRight
                =<< previewRepositoryPush root snapshot.snapshotId

            createPullRequest root preview.pushPreviewConfirmation
                `shouldReturnSatisfying` isConfirmationRejection
            confirmRepositoryPush root preview.pushPreviewConfirmation
                `shouldReturnSatisfying` isConfirmationRejection

    it "pushes SHA-256 repositories through an isolated matching object store" $
        withDeliveryRepositoryWithFormat (Just "sha256") \root remote -> do
            appendFile (root <> "/tracked.txt") "second\n"
            _ <- git root ["add", "tracked.txt"]
            _ <- git root ["commit", "-q", "-m", "second"]
            snapshot <- expectRight =<< repositorySnapshot root
            preview <- expectRight
                =<< previewRepositoryPush root snapshot.snapshotId
            pushed <- expectRight
                =<< confirmRepositoryPush
                    root preview.pushPreviewConfirmation
            Text.length pushed.deliveryHeadOid `shouldBe` 64
            remoteHead <- Text.strip
                <$> git root
                    [ "--git-dir"
                    , remote
                    , "rev-parse"
                    , "refs/heads/main"
                    ]
            remoteHead `shouldBe` pushed.deliveryHeadOid

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
            _ <- git root
                [ "push"
                , "-q"
                , remote
                , "main:main"
                ]
            _ <- git root
                [ "update-ref"
                , "refs/remotes/origin/main"
                , "HEAD"
                ]
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
            _ <- git root
                [ "remote"
                , "set-url"
                , "origin"
                , "ssh://git@redirected.example.test/owner/repository.git"
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
            originalHead
                `shouldBe` preview.pushPreviewStatus.deliveryUpstreamOid

    it "ignores repository URL rewrites and command-bearing Git config" $
        withDeliveryRepository \root remote -> do
            appendFile (root <> "/tracked.txt") "second\n"
            _ <- git root ["add", "tracked.txt"]
            _ <- git root ["commit", "-q", "-m", "second"]
            snapshot <- expectRight =<< repositorySnapshot root
            preview <- expectRight
                =<< previewRepositoryPush root snapshot.snapshotId
            let redirected = remote <> "-config-redirected"
                sshMarker = root <> "/ssh-command-ran"
                credentialMarker = root <> "/credential-helper-ran"
            _ <- git root ["clone", "-q", "--bare", remote, redirected]
            _ <- git root
                [ "config"
                , "url." <> redirected <> ".insteadOf"
                , deliveryRemoteUrl
                ]
            _ <- git root
                [ "config"
                , "core.sshCommand"
                , "sh -c 'touch " <> sshMarker <> "'"
                ]
            _ <- git root
                [ "config"
                , "credential.helper"
                , "!sh -c 'touch " <> credentialMarker <> "'"
                ]

            pushed <- expectRight
                =<< confirmRepositoryPush
                    root preview.pushPreviewConfirmation
            originalHead <- Text.strip
                <$> git root
                    [ "--git-dir"
                    , remote
                    , "rev-parse"
                    , "refs/heads/main"
                    ]
            redirectedHead <- Text.strip
                <$> git root
                    [ "--git-dir"
                    , redirected
                    , "rev-parse"
                    , "refs/heads/main"
                    ]
            originalHead `shouldBe` pushed.deliveryHeadOid
            redirectedHead
                `shouldBe` preview.pushPreviewStatus.deliveryUpstreamOid
            doesFileExist sshMarker `shouldReturn` False
            doesFileExist credentialMarker `shouldReturn` False

    it "does not execute an ssh binary supplied by the checkout PATH" $
        withDeliveryRepository \root _ -> do
            let localBin = root <> "/local-bin"
                localSsh = localBin <> "/ssh"
                marker = root <> "/checkout-ssh-ran"
            createDirectory localBin
            writeFile localSsh $ unlines
                [ "#!/bin/sh"
                , "touch " <> shellQuote marker
                , "exit 67"
                ]
            setFileMode localSsh
                (ownerReadMode
                    `unionFileModes` ownerWriteMode
                    `unionFileModes` ownerExecuteMode)
            originalPath <- getEnv "PATH"
            bracket
                (setEnv "PATH" (localBin <> ":" <> originalPath))
                (\_ -> setEnv "PATH" originalPath)
                \_ -> do
                    appendFile (root <> "/tracked.txt") "second\n"
                    _ <- git root ["add", "tracked.txt"]
                    _ <- git root ["commit", "-q", "-m", "second"]
                    snapshot <- expectRight =<< repositorySnapshot root
                    preview <- expectRight
                        =<< previewRepositoryPush root snapshot.snapshotId
                    _ <- expectRight
                        =<< confirmRepositoryPush
                            root preview.pushPreviewConfirmation
                    doesFileExist marker `shouldReturn` False

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

    it "rejects a PR token for pushing and still consumes it" $
        withDeliveryRepository \root _ ->
            withFakeGh root \_ _ -> do
                snapshot <- expectRight =<< repositorySnapshot root
                preview <- expectRight
                    =<< previewPullRequest
                        root snapshot.snapshotId "main" "Title" "Body"

                confirmRepositoryPush root preview.pullRequestConfirmation
                    `shouldReturnSatisfying` isConfirmationRejection
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

    it "keeps network Git and gh workdirs outside a checkout-controlled TMPDIR" $
        withDeliveryRepository \root _ ->
            withFakeGh root \_ _ -> do
                let hostileTemp = root <> "/hostile-tmp"
                    sshCwdCapture = root <> "/../ssh-cwd.txt"
                    ghCwdCapture = root <> "/../gh-cwd.txt"
                createDirectory hostileTemp
                originalTemp <- lookupEnv "TMPDIR"
                bracket
                    (setEnv "TMPDIR" hostileTemp)
                    (\_ -> case originalTemp of
                        Nothing -> unsetEnv "TMPDIR"
                        Just value -> setEnv "TMPDIR" value)
                    \_ -> do
                        snapshot <- expectRight =<< repositorySnapshot root
                        preview <- expectRight
                            =<< previewPullRequest
                                root snapshot.snapshotId "main" "Title" "Body"
                        _ <- expectRight
                            =<< createPullRequest
                                root preview.pullRequestConfirmation
                        pure ()
                sshCwd <- Text.strip . Text.pack <$> readFile sshCwdCapture
                ghCwd <- Text.strip . Text.pack <$> readFile ghCwdCapture
                let checkoutPrefix = Text.pack (root <> "/")
                sshCwd `shouldNotSatisfy` Text.isPrefixOf checkoutPrefix
                ghCwd `shouldNotSatisfy` Text.isPrefixOf checkoutPrefix
                listDirectory hostileTemp `shouldReturn` []
                doesDirectoryExist (hostileTemp <> "/.git")
                    `shouldReturn` False

    it "kills a gh descendant retaining pipes after its leader exits" $
        withDeliveryRepository \root _ ->
            withFakeGh root \_ _ -> do
                let descendantPid = root <> "/gh-descendant.pid"
                setEnv "GH_RETAIN_PIPE_MARKER" descendantPid
                snapshot <- expectRight =<< repositorySnapshot root
                result <- timeout 30_000_000
                    (previewPullRequest
                        root snapshot.snapshotId "main" "Title" "Body")
                result `shouldSatisfy` \case
                    Just (Right _) -> True
                    _ -> False
                pid <- words <$> readFile descendantPid
                descendant <- case pid of
                    [value] -> pure value
                    _ -> expectationFailure "expected one descendant PID"
                        >> fail "unreachable"
                processGoneWithin
                    3_000_000
                    descendant
                    `shouldReturn` True

    it "leaves indistinguishable API drafts untouched after malformed create output" $
        withDeliveryRepository \root _ ->
            withFakeGh root \_ _ -> do
                setEnv "GH_FAKE_CREATE_URL"
                    "https://github.com/attacker/repository/pull/1"
                snapshot <- expectRight =<< repositorySnapshot root
                preview <- expectRight
                    =<< previewPullRequest
                        root snapshot.snapshotId "main" "Title" "Body"
                createPullRequest root preview.pullRequestConfirmation
                    `shouldReturnSatisfying` isCommandFailure
                doesFileExist (root <> "/pull-request-closed")
                    `shouldReturn` False
                doesFileExist (root <> "/pull-request-candidates-queried")
                    `shouldReturn` False

    it "rejects gh repository identity that differs from the bound remote" $
        withDeliveryRepository \root _ ->
            withFakeGh root \_ _ -> do
                setEnv "GH_FAKE_REPOSITORY" "attacker/repository"
                snapshot <- expectRight =<< repositorySnapshot root
                previewPullRequest
                    root snapshot.snapshotId "main" "Title" "Body"
                    `shouldReturnSatisfying` isUnavailable

    it "rejects a draft PR whose created head differs from the confirmation" $
        withDeliveryRepository \root _ ->
            withFakeGh root \_ _ -> do
                setEnv "GH_FAKE_HEAD_SHA" (replicate 40 '0')
                snapshot <- expectRight =<< repositorySnapshot root
                preview <- expectRight
                    =<< previewPullRequest
                        root snapshot.snapshotId "main" "Title" "Body"
                createPullRequest root preview.pullRequestConfirmation
                    `shouldReturnSatisfying` isStale
                doesFileExist (root <> "/pull-request-closed")
                    `shouldReturn` False

    it "rejects a draft PR from an unexpected head repository" $
        withDeliveryRepository \root _ ->
            withFakeGh root \_ _ -> do
                setEnv "GH_FAKE_HEAD_REPOSITORY" "attacker/repository"
                snapshot <- expectRight =<< repositorySnapshot root
                preview <- expectRight
                    =<< previewPullRequest
                        root snapshot.snapshotId "main" "Title" "Body"
                createPullRequest root preview.pullRequestConfirmation
                    `shouldReturnSatisfying` isStale
                doesFileExist (root <> "/pull-request-closed")
                    `shouldReturn` False

    it "rejects a non-draft PR without closing it" $
        withDeliveryRepository \root _ ->
            withFakeGh root \_ _ -> do
                setEnv "GH_FAKE_DRAFT" "false"
                snapshot <- expectRight =<< repositorySnapshot root
                preview <- expectRight
                    =<< previewPullRequest
                        root snapshot.snapshotId "main" "Title" "Body"
                createPullRequest root preview.pullRequestConfirmation
                    `shouldReturnSatisfying` isStale
                doesFileExist (root <> "/pull-request-closed")
                    `shouldReturn` False

withDeliveryRepository :: (FilePath -> FilePath -> IO value) -> IO value
withDeliveryRepository action =
    withDeliveryRepositoryWithFormat Nothing action

withDeliveryRepositoryWithFormat
    :: Maybe String
    -> (FilePath -> FilePath -> IO value)
    -> IO value
withDeliveryRepositoryWithFormat objectFormat action =
    withTempDirectory "repository-delivery" \container -> do
        let root = container <> "/checkout"
            remote = container <> "/remote.git"
            bin = container <> "/transport-bin"
            sshExecutable = bin <> "/ssh"
        createDirectory root
        createDirectory bin
        originalPath <- getEnv "PATH"
        realGit <- maybe (fail "git not found") pure =<< findExecutable "git"
        let formatArguments =
                maybe [] (\format -> ["--object-format=" <> format]) objectFormat
        _ <- git container
            (["init", "-q", "--bare"] <> formatArguments <> [remote])
        _ <- git root
            (["init", "-q", "-b", "main"] <> formatArguments)
        appendFile
            (root <> "/.git/config")
            "\n[user]\n\tname = Repository Delivery Test\n\temail = delivery@example.test\n"
        writeFile (root <> "/tracked.txt") "first\n"
        _ <- git root ["add", "tracked.txt"]
        _ <- git root ["commit", "-q", "-m", "initial"]
        _ <- git root ["remote", "add", "origin", remote]
        _ <- git root ["push", "-q", "-u", "origin", "main"]
        _ <- git root ["remote", "set-url", "origin", deliveryRemoteUrl]
        writeFile sshExecutable $ unlines
            [ "#!/bin/sh"
            , "set -eu"
            , "pwd > " <> shellQuote (root <> "/../ssh-cwd.txt")
            , "case \" $* \" in"
            , "  *' git-upload-pack '*)"
            , "    exec " <> shellQuote realGit <> " upload-pack " <> shellQuote remote
            , "    ;;"
            , "  *' git-receive-pack '*)"
            , "    exec " <> shellQuote realGit <> " receive-pack " <> shellQuote remote
            , "    ;;"
            , "  *) exit 1 ;;"
            , "esac"
            ]
        setFileMode sshExecutable
            (ownerReadMode
                `unionFileModes` ownerWriteMode
                `unionFileModes` ownerExecuteMode)
        bracket
            (setEnv "PATH" (bin <> ":" <> originalPath))
            (\_ -> setEnv "PATH" originalPath)
            (\_ -> action root remote)

deliveryRemoteUrl :: String
deliveryRemoteUrl = "ssh://git@example.test/owner/repository.git"

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
    originalFakeCreateUrl <- lookupEnv "GH_FAKE_CREATE_URL"
    originalFakeApiUrl <- lookupEnv "GH_FAKE_API_URL"
    originalFakeRepository <- lookupEnv "GH_FAKE_REPOSITORY"
    originalFakeHead <- lookupEnv "GH_FAKE_HEAD_SHA"
    originalFakeHeadRepository <- lookupEnv "GH_FAKE_HEAD_REPOSITORY"
    originalFakeDraft <- lookupEnv "GH_FAKE_DRAFT"
    originalRetainPipeMarker <- lookupEnv "GH_RETAIN_PIPE_MARKER"
    originalGhHost <- lookupEnv "GH_HOST"
    withTempDirectory "repository-delivery-gh" \bin -> do
        bashExecutable <-
            maybe (fail "bash not found") pure =<< findExecutable "bash"
        realGit <- maybe (fail "git not found") pure =<< findExecutable "git"
        localRemote <- Text.unpack . Text.strip
            <$> git root ["remote", "get-url", "origin"]
        expectedHead <- Text.unpack . Text.strip
            <$> git root ["rev-parse", "HEAD"]
        let githubRemote = "https://github.com/owner/repository.git"
            executable = bin <> "/gh"
            gitExecutable = bin <> "/git"
            bodyCapture = bin <> "/body.txt"
            injectionMarker = root <> "/shell-injection"
            closeMarker = root <> "/pull-request-closed"
            candidateQueryMarker =
                root <> "/pull-request-candidates-queried"
            readyMarker = root <> "/pull-request-ready"
        _ <- git root ["remote", "set-url", "origin", githubRemote]
        writeFile gitExecutable $ unlines
            [ "#!" <> bashExecutable
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
            , "pwd > " <> shellQuote (root <> "/../gh-cwd.txt")
            , "[ \"${GH_HOST:-}\" = '' ] || exit 65"
            , "if [ \"${GH_RETAIN_PIPE_MARKER:-}\" != '' ] && [ \"$1 $2\" = 'auth status' ]; then"
            , "  sleep 30 &"
            , "  printf '%s\\n' \"$!\" > \"$GH_RETAIN_PIPE_MARKER\""
            , "  exit 0"
            , "fi"
            , "print_pr() {"
            , "  draft=${GH_FAKE_DRAFT:-true}"
            , "  [ ! -e " <> shellQuote readyMarker <> " ] || draft=false"
            , "  printf '{\"number\":123,\"html_url\":\"%s\",\"draft\":%s,\"head\":{\"sha\":\"%s\",\"ref\":\"main\",\"repo\":{\"full_name\":\"%s\"}},\"base\":{\"ref\":\"main\",\"repo\":{\"full_name\":\"%s\"}}}\\n' \\"
            , "    \"${GH_FAKE_API_URL:-https://github.com/owner/repository/pull/123}\" \"$draft\" \"${GH_FAKE_HEAD_SHA:-$GH_EXPECTED_HEAD}\" \"${GH_FAKE_HEAD_REPOSITORY:-owner/repository}\" \"${GH_FAKE_REPOSITORY:-owner/repository}\""
            , "}"
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
            , "      *' --repo github.com/owner/repository '*' --head owner:main '*) ;;"
            , "      *) exit 65 ;;"
            , "    esac"
            , "    cat > \"$GH_BODY_CAPTURE\""
            , "    printf '%s\\n' \"${GH_FAKE_CREATE_URL:-https://github.com/owner/repository/pull/123}\""
            , "    ;;"
            , "  'api repos/owner/repository/pulls/123')"
            , "    print_pr"
            , "    ;;"
            , "  'pr ready')"
            , "    : > " <> shellQuote readyMarker
            , "    exit 0"
            , "    ;;"
            , "  'api --method')"
            , "    case \"$3\" in"
            , "      GET)"
            , "        : > " <> shellQuote candidateQueryMarker
            , "        printf '['"
            , "        print_pr"
            , "        printf ']\\n'"
            , "        ;;"
            , "      PATCH)"
            , "        cat > /dev/null"
            , "        : > " <> shellQuote closeMarker
            , "        ;;"
            , "      *) exit 64 ;;"
            , "    esac"
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
                setEnv "GH_BODY_CAPTURE" bodyCapture
                setEnv "GH_EXPECTED_HEAD" expectedHead)
            (\_ -> do
                setEnv "PATH" originalPath
                case originalCapture of
                    Nothing -> unsetEnv "GH_BODY_CAPTURE"
                    Just value -> setEnv "GH_BODY_CAPTURE" value
                case originalFakeCreateUrl of
                    Nothing -> unsetEnv "GH_FAKE_CREATE_URL"
                    Just value -> setEnv "GH_FAKE_CREATE_URL" value
                case originalFakeApiUrl of
                    Nothing -> unsetEnv "GH_FAKE_API_URL"
                    Just value -> setEnv "GH_FAKE_API_URL" value
                case originalFakeRepository of
                    Nothing -> unsetEnv "GH_FAKE_REPOSITORY"
                    Just value -> setEnv "GH_FAKE_REPOSITORY" value
                case originalFakeHead of
                    Nothing -> unsetEnv "GH_FAKE_HEAD_SHA"
                    Just value -> setEnv "GH_FAKE_HEAD_SHA" value
                case originalFakeHeadRepository of
                    Nothing -> unsetEnv "GH_FAKE_HEAD_REPOSITORY"
                    Just value -> setEnv "GH_FAKE_HEAD_REPOSITORY" value
                case originalFakeDraft of
                    Nothing -> unsetEnv "GH_FAKE_DRAFT"
                    Just value -> setEnv "GH_FAKE_DRAFT" value
                unsetEnv "GH_EXPECTED_HEAD"
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
