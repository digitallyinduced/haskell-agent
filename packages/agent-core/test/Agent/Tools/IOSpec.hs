module Agent.Tools.IOSpec (spec) where

import Agent.Cancel (requestCancel)
import Agent.FileRetry
    ( appendLazyFileRetryingOpen
    , retryOnFileBusy
    , writeLazyFileAtomically
    )
import System.OsPath (equalFilePath, unsafeEncodeUtf)
import Agent.Tools.IO
    ( CommandResult(..)
    , RunningCommand(..)
    , combineCommandOutput
    , commandResultOutput
    , formatCommandResult
    , readTextFile
    , resolveUnderCwd
    , runShellCommand
    , runShellCommandStreaming
    , runningLiveOutput
    , sessionTempProcessEnv
    , startShellCommand
    , startShellCommandWithInput
    , stopShellCommand
    , writeShellCommandInput
    , writeTextFile
    )
import Agent.Tools.OutputArtifact
    ( OutputArtifact(..)
    , readOutputArtifact
    )
import Agent.Tools.Types
    ( ToolEnv(..)
    , defaultToolEnv
    , setToolRootAccessRequest
    , setToolSessionTmp
    )
import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar, threadDelay)
import Control.Concurrent.MVar (readMVar)
import Control.Exception.Safe (bracket, tryAny, tryIO)
import Control.Monad (replicateM)
import qualified Data.ByteString.Lazy.Char8 as LBS8
import Data.Either (isLeft, isRight)
import Data.IORef
import Data.List (sort)
import qualified Data.Text as Text
import System.Directory
    ( canonicalizePath
    , createDirectory
    , createDirectoryIfMissing
    , createDirectoryLink
    , getTemporaryDirectory
    , listDirectory
    , removeDirectoryRecursive
    )
import System.FilePath ((</>), takeFileName)
import System.IO (IOMode(..), hClose, openFile)
import System.IO.Error (alreadyInUseErrorType, mkIOError)
import System.Info (os)
import System.Posix.Temp (mkdtemp)
import System.Posix.Files (deviceID, fileID, getFileStatus)
import Test.Hspec

fromFilePath = unsafeEncodeUtf

spec :: Spec
spec = describe "Agent.Tools.IO" do
    describe "command result formatting" do
        it "combines stdout and stderr without redundant separators" do
            combineCommandOutput "out" "" `shouldBe` "out"
            combineCommandOutput "" "err" `shouldBe` "err"
            combineCommandOutput "out" "err" `shouldBe` "out\nerr"

        it "renders success, timeout, and cancellation consistently" do
            let result = CommandResult
                    { commandExitCode = Just 3
                    , commandStdout = "out"
                    , commandStderr = "err"
                    , commandStdoutArtifact = Nothing
                    , commandStderrArtifact = Nothing
                    , commandTimedOut = False
                    , commandCancelled = False
                    }
            commandResultOutput result `shouldBe` "out\nerr"
            formatCommandResult result `shouldBe` "exit: 3\nout\nerr"
            formatCommandResult
                result
                    { commandExitCode = Nothing
                    , commandTimedOut = True
                    }
                `shouldBe` "exit: killed (timeout)\nout\nerr"
            formatCommandResult
                result
                    { commandExitCode = Nothing
                    , commandTimedOut = True
                    , commandCancelled = True
                    }
                `shouldBe` "exit: cancelled\nout\nerr"

        it "defaults a missing ordinary exit code to one" do
            formatCommandResult CommandResult
                { commandExitCode = Nothing
                , commandStdout = ""
                , commandStderr = ""
                , commandStdoutArtifact = Nothing
                , commandStderrArtifact = Nothing
                , commandTimedOut = False
                , commandCancelled = False
                }
                `shouldBe` "exit: 1\n"

    it "retries only resource-busy IO exceptions" do
        attempts <- newIORef (0 :: Int)
        retryOnFileBusy do
            attempt <- atomicModifyIORef' attempts \current ->
                let next = current + 1
                in (next, next)
            if attempt < 3
                then ioError (mkIOError alreadyInUseErrorType "busy" Nothing Nothing)
                else pure ()
        readIORef attempts `shouldReturn` 3

    it "does not retry unrelated IO exceptions" do
        attempts <- newIORef (0 :: Int)
        result <- tryIO do
            retryOnFileBusy do
                modifyIORef' attempts (+ 1)
                ioError (userError "not retryable") :: IO ()
        result `shouldSatisfy` isLeft
        readIORef attempts `shouldReturn` 1

    it "atomically replaces a file from concurrent writers" do
        withTempDir checkConcurrentAtomicWrites

    it "appends each concurrent payload exactly once" do
        withTempDir checkConcurrentAppends

    it "retries a write after a transient GHC file lock" do
        withTempDir \dir -> do
            let path = dir </> "held.txt"
            writeFile path "old\n"
            h <- openFile path AppendMode
            _ <- forkIO do
                threadDelay 5000
                hClose h
            writeTextFile (fromFilePath path) "new\n" `shouldReturn` Right ()
            readTextFile (fromFilePath path) `shouldReturn` Right "new\n"

    it "gives up when the GHC file lock is held for the whole retry window" do
        withTempDir \dir -> do
            let path = dir </> "held.txt"
            writeFile path "old\n"
            bracket (openFile path ReadMode) hClose \_ -> do
                result <- writeTextFile (fromFilePath path) "new\n"
                result `shouldSatisfy` isLeft
                result `shouldSatisfy` either (Text.isInfixOf "resource busy") (const False)

    it "lets several threads write the same file without a lock error" do
        withTempDir \dir -> do
            let path = dir </> "race.txt"
            writeFile path "start\n"
            vars <- replicateM 8 newEmptyMVar
            mapM_
                (\(i, var) -> forkIO $
                    writeTextFile (fromFilePath path) (Text.pack (show i) <> "\n") >>= putMVar var)
                (zip [1 :: Int ..] vars)
            results <- mapM takeMVar vars
            results `shouldBe` replicate 8 (Right ())

    it "reads the same file from many threads at once" do
        withTempDir \dir -> do
            let path = dir </> "shared.txt"
                body = Text.replicate 2000 "concurrent-read\n"
            writeTextFile (fromFilePath path) body `shouldReturn` Right ()
            vars <- replicateM 32 newEmptyMVar
            mapM_ (\var -> forkIO $ readTextFile (fromFilePath path) >>= putMVar var) vars
            results <- mapM takeMVar vars
            results `shouldBe` replicate 32 (Right body)

    it "rejects missing descendants below a symlink that escapes cwd" do
        withTempDir \dir -> do
            let workspace = dir </> "workspace"
                outside = dir </> "outside"
            createDirectory workspace
            createDirectory outside
            createDirectoryLink outside (workspace </> "link")
            env <- defaultToolEnv (fromFilePath workspace)
            result <- resolveUnderCwd env
                (fromFilePath ("link" </> "missing" </> "file.txt"))
            result `shouldSatisfy` isLeft

    it "rejects missing paths whose traversal is hidden after a safe segment" do
        withTempDir \dir -> do
            let workspace = dir </> "workspace"
            createDirectory workspace
            env <- defaultToolEnv (fromFilePath workspace)
            result <- resolveUnderCwd env
                (fromFilePath ("nested" </> ".." </> ".." </> "outside.txt"))
            result `shouldSatisfy` isLeft

    it "preserves parent-segment semantics after a directory symlink" do
        withTempDir \dir -> do
            let workspace = dir </> "workspace"
                targetParent = workspace </> "a"
                target = targetParent </> "b"
            createDirectory workspace
            createDirectory targetParent
            createDirectory target
            createDirectoryLink target (workspace </> "link")
            env <- defaultToolEnv (fromFilePath workspace)
            result <- resolveUnderCwd env
                (fromFilePath ("link" </> ".." </> "file.txt"))
            canonicalParent <- canonicalizePath targetParent
            result `shouldBe` Right
                (fromFilePath (canonicalParent </> "file.txt"))

    it "allows absolute paths under an additional filesystem root" do
        withTempDir \dir -> do
            let workspace = dir </> "workspace"
                scratch = dir </> "scratch"
                scratchFile = scratch </> "artifact.txt"
            createDirectory workspace
            createDirectory scratch
            writeFile scratchFile "artifact"
            base <- defaultToolEnv (fromFilePath workspace)
            writeIORef base.toolAllowedRoots [fromFilePath scratch]
            setToolSessionTmp base (Just (fromFilePath scratch))
            let env = base
            resolveUnderCwd env (fromFilePath scratchFile)
                >>= (`shouldSatisfy` isRight)
            resolveUnderCwd env
                (fromFilePath "/haskell-agent-unrelated-root/file.txt")
                >>= (`shouldSatisfy` isLeft)

    it "maps system temp paths into the private session temp root" do
        withTempDir \dir -> do
            let workspace = dir </> "workspace"
                scratch = dir </> "scratch"
                relative = "literal-tmp" </> "artifact.txt"
            mapM_ createDirectory [workspace, scratch]
            env <- defaultToolEnv (fromFilePath workspace)
            setToolSessionTmp env (Just (fromFilePath scratch))
            requests <- newIORef []
            setToolRootAccessRequest env $ Just \root -> do
                modifyIORef' requests (<> [root])
                pure False
            canonicalScratch <- canonicalizePath scratch
            canonicalSystemTmp <- canonicalizePath "/tmp"
            let expected = Right (fromFilePath (canonicalScratch </> relative))
            resolveUnderCwd env (fromFilePath "/tmp")
                `shouldReturn` Right (fromFilePath canonicalScratch)
            resolveUnderCwd env (fromFilePath "/tmp/")
                `shouldReturn` Right (fromFilePath canonicalScratch)
            resolveUnderCwd env (fromFilePath ("/tmp" </> relative))
                `shouldReturn` expected
            resolveUnderCwd env (fromFilePath ("//tmp" </> relative))
                `shouldReturn` expected
            resolveUnderCwd env (fromFilePath ("///tmp" </> relative))
                `shouldReturn` expected
            resolveUnderCwd env (fromFilePath ("////tmp" </> relative))
                `shouldReturn` expected
            resolveUnderCwd env
                (fromFilePath ("/tmp//" <> relative))
                `shouldReturn` expected
            resolveUnderCwd env
                (fromFilePath (canonicalSystemTmp </> relative))
                `shouldReturn` expected
            resolveUnderCwd env
                (fromFilePath ("/private/tmp" </> relative))
                `shouldReturn` expected
            resolveUnderCwd env
                (fromFilePath ("/usr/../tmp" </> relative))
                `shouldReturn` expected
            readIORef requests `shouldReturn` []
            varPrivateAliases <-
                pathsReferToSameFile "/var/../private/tmp" "/private/tmp"
            varPrivate <- resolveUnderCwd env
                (fromFilePath ("/var/../private/tmp" </> relative))
            if varPrivateAliases
                then varPrivate `shouldBe` expected
                else varPrivate `shouldSatisfy` isLeft
            upperTmpAliases <- pathsReferToSameFile "/tmp" "/TMP"
            upperTmp <- resolveUnderCwd env
                (fromFilePath ("/TMP" </> relative))
            if upperTmpAliases
                then upperTmp `shouldBe` expected
                else upperTmp `shouldSatisfy` isLeft
            upperPrivateAliases <-
                pathsReferToSameFile "/private/tmp" "/PRIVATE/TMP"
            upperPrivate <- resolveUnderCwd env
                (fromFilePath ("/PRIVATE/TMP" </> relative))
            if upperPrivateAliases
                then upperPrivate `shouldBe` expected
                else upperPrivate `shouldSatisfy` isLeft

    it "preserves symlink semantics before recognizing a normalized alias" do
        withTempDir \dir -> do
            let workspace = dir </> "workspace"
                scratch = dir </> "scratch"
                requested = "/bin/../tmp/haskell-agent-alias-probe"
            mapM_ createDirectory [workspace, scratch]
            env <- defaultToolEnv (fromFilePath workspace)
            setToolSessionTmp env (Just (fromFilePath scratch))
            aliases <- pathsReferToSameFile "/bin/../tmp" "/tmp"
            result <- resolveUnderCwd env (fromFilePath requested)
            if aliases
                then do
                    canonicalScratch <- canonicalizePath scratch
                    result `shouldBe` Right
                        (fromFilePath
                            (canonicalScratch
                                </> "haskell-agent-alias-probe"))
                else result `shouldSatisfy` isLeft

    it "maps an existing host temp file unless its root was explicitly allowed" do
        withTempDir \dir ->
            withSystemTempDir \hostTemp -> do
                let workspace = dir </> "workspace"
                    scratch = dir </> "scratch"
                    hostFile = hostTemp </> "existing.txt"
                    relative = takeFileName hostTemp </> "existing.txt"
                    requested = "/tmp" </> relative
                mapM_ createDirectory [workspace, scratch]
                writeFile hostFile "host"
                canonicalScratch <- canonicalizePath scratch
                canonicalHostFile <- canonicalizePath hostFile

                mappedEnv <- defaultToolEnv (fromFilePath workspace)
                setToolSessionTmp mappedEnv (Just (fromFilePath scratch))
                resolveUnderCwd mappedEnv (fromFilePath requested)
                    `shouldReturn` Right
                        (fromFilePath (canonicalScratch </> relative))

                allowedEnv <- defaultToolEnv (fromFilePath workspace)
                setToolSessionTmp allowedEnv (Just (fromFilePath scratch))
                writeIORef allowedEnv.toolAllowedRoots [fromFilePath hostTemp]
                resolveUnderCwd allowedEnv (fromFilePath requested)
                    `shouldReturn` Right (fromFilePath canonicalHostFile)

    it "does not let a remapped temp path escape through a session symlink" do
        withTempDir \dir -> do
            let workspace = dir </> "workspace"
                scratch = dir </> "scratch"
                outside = dir </> "outside"
                alias = "literal-tmp-link"
            mapM_ createDirectory [workspace, scratch, outside]
            createDirectoryLink outside (scratch </> alias)
            env <- defaultToolEnv (fromFilePath workspace)
            setToolSessionTmp env (Just (fromFilePath scratch))
            requests <- newIORef []
            setToolRootAccessRequest env $ Just \root -> do
                modifyIORef' requests (<> [root])
                pure True
            result <- resolveUnderCwd env
                (fromFilePath ("/tmp" </> alias </> "missing.txt"))
            result `shouldSatisfy` isLeft
            result `shouldSatisfy`
                either
                    (Text.isInfixOf
                        "escapes the private session temp directory")
                    (const False)
            traversal <- resolveUnderCwd env
                (fromFilePath ("/tmp" </> ".." </> "outside.txt"))
            traversal `shouldSatisfy`
                either
                    (Text.isInfixOf
                        "escapes the private session temp directory")
                    (const False)
            doubleSlashTraversal <- resolveUnderCwd env
                (fromFilePath "//tmp/../outside.txt")
            doubleSlashTraversal `shouldSatisfy`
                either
                    (Text.isInfixOf
                        "escapes the private session temp directory")
                    (const False)
            normalizedPrefixTraversal <- resolveUnderCwd env
                (fromFilePath "/usr/../tmp/../outside.txt")
            normalizedPrefixTraversal `shouldSatisfy`
                either
                    (Text.isInfixOf
                        "escapes the private session temp directory")
                    (const False)
            readIORef requests `shouldReturn` []

    it "requests and remembers approval for an escaped filesystem root" do
        withTempDir \dir -> do
            let workspace = dir </> "workspace"
                outside = dir </> "outside"
                target = outside </> "file.txt"
            createDirectory workspace
            createDirectory outside
            writeFile target "approved"
            env <- defaultToolEnv (fromFilePath workspace)
            requests <- newIORef []
            setToolRootAccessRequest env $ Just \root -> do
                modifyIORef' requests (<> [root])
                pure True
            resolveUnderCwd env (fromFilePath target)
                `shouldReturn` Right (fromFilePath target)
            readIORef requests `shouldReturn` [fromFilePath outside]
            roots <- readIORef env.toolAllowedRoots
            roots `shouldSatisfy` any (equalFilePath (fromFilePath outside))
            resolveUnderCwd env (fromFilePath target)
                `shouldReturn` Right (fromFilePath target)
            readIORef requests `shouldReturn` [fromFilePath outside]

    it "keeps rejecting an escaped root when access is denied" do
        withTempDir \dir -> do
            let workspace = dir </> "workspace"
                outside = dir </> "outside"
                target = outside </> "file.txt"
            createDirectory workspace
            createDirectory outside
            writeFile target "denied"
            env <- defaultToolEnv (fromFilePath workspace)
            requests <- newIORef []
            setToolRootAccessRequest env $ Just \root -> do
                modifyIORef' requests (<> [root])
                pure False
            result <- resolveUnderCwd env (fromFilePath target)
            result `shouldSatisfy` isLeft
            readIORef requests `shouldReturn` [fromFilePath outside]

    it "rejects symlink escapes from an additional filesystem root" do
        withTempDir \dir -> do
            let workspace = dir </> "workspace"
                scratch = dir </> "scratch"
                outside = dir </> "outside"
            createDirectory workspace
            createDirectory scratch
            createDirectory outside
            createDirectoryLink outside (scratch </> "link")
            base <- defaultToolEnv (fromFilePath workspace)
            writeIORef base.toolAllowedRoots [fromFilePath scratch]
            let env = base
            result <- resolveUnderCwd env
                (fromFilePath (scratch </> "link" </> "missing.txt"))
            result `shouldSatisfy` isLeft

    it "sets the private temp environment for shell commands" do
        withTempDir \dir -> do
            let workspace = dir </> "workspace"
                scratch = dir </> "scratch"
            createDirectory workspace
            createDirectory scratch
            base <- defaultToolEnv (fromFilePath workspace)
            let scratchPath = fromFilePath scratch
                env = base
            setToolSessionTmp env (Just scratchPath)
            result <- runShellCommand env scratchPath
                "printf '%s\\n%s\\n%s' \"$TMPDIR\" \"$HASKELL_AGENT_TMPDIR\" \"${HASKELL_AGENT_HOST_TMPDIR-unset}\""
                5000
            result.commandStdout `shouldBe`
                Text.pack scratch <> "\n" <> Text.pack scratch <> "\nunset"

    it "isolates managed sibling scratch directories at the process boundary" do
        if os /= "darwin"
            then pendingWith "the process-level Seatbelt boundary is macOS-only"
            else withTempDir \dir -> do
                let workspace = dir </> "workspace"
                    sessions =
                        dir </> ".haskell-agent" </> "tmp" </> "sessions"
                    scratch = sessions </> "current"
                    sibling = sessions </> "other"
                    ownFile = scratch </> "own"
                    secret = sibling </> "secret"
                mapM_ (createDirectoryIfMissing True)
                    [workspace, scratch, sibling]
                writeFile ownFile "own-scratch"
                writeFile secret "sibling-secret"
                env <- defaultToolEnv (fromFilePath workspace)
                setToolSessionTmp env (Just (fromFilePath scratch))
                ownResult <- runShellCommand env (fromFilePath workspace)
                    "cat \"$TMPDIR/own\""
                    5000
                ownResult.commandExitCode `shouldBe` Just 0
                ownResult.commandStdout `shouldBe` "own-scratch"
                result <- runShellCommand env (fromFilePath workspace)
                    "cat \"$TMPDIR/../other/secret\""
                    5000
                result.commandExitCode `shouldNotBe` Just 0
                result.commandStdout `shouldNotBe` "sibling-secret"

    it "replaces inherited temp variables without exposing host temp" do
        let scratch = fromFilePath "/session/private"
        sessionTempProcessEnv scratch
            [ ("TMPDIR", "/host/tmp")
            , ("HASKELL_AGENT_TMPDIR", "/host/tmp")
            , ("HASKELL_AGENT_HOST_TMPDIR", "/tmp")
            , ("KEEP", "yes")
            ]
            `shouldBe`
                [ ("TMPDIR", "/session/private")
                , ("HASKELL_AGENT_TMPDIR", "/session/private")
                , ("KEEP", "yes")
                ]

    it "switches the effective session temp root without retaining the old one" do
        withTempDir \dir -> do
            let workspace = dir </> "workspace"
                first = dir </> "first"
                second = dir </> "second"
            mapM_ createDirectory [workspace, first, second]
            env <- defaultToolEnv (fromFilePath workspace)
            setToolSessionTmp env (Just (fromFilePath first))
            resolveUnderCwd env
                (fromFilePath (".." </> "first" </> "file.txt"))
                >>= (`shouldSatisfy` isRight)
            setToolSessionTmp env (Just (fromFilePath second))
            resolveUnderCwd env
                (fromFilePath (".." </> "first" </> "file.txt"))
                >>= (`shouldSatisfy` isLeft)
            resolveUnderCwd env (fromFilePath (second </> "file.txt"))
                >>= (`shouldSatisfy` isRight)

    it "cancels a long-running shell command via toolCancel" do
        withTempDir \dir -> do
            let osDir = fromFilePath dir
            env@ToolEnv{toolCancel} <- defaultToolEnv osDir
            done <- newEmptyMVar
            _ <- forkIO do
                result <- runShellCommand env osDir "sleep 30" 60000
                putMVar done result
            threadDelay 100000
            requestCancel toolCancel
            CommandResult{commandCancelled, commandTimedOut} <- takeMVar done
            commandCancelled `shouldBe` True
            commandTimedOut `shouldBe` False

    it "publishes accumulated foreground output before completion" do
        withTempDir \dir -> do
            let osDir = fromFilePath dir
            env <- defaultToolEnv osDir
            snapshots <- newIORef []
            done <- newEmptyMVar
            _ <- forkIO do
                result <- runShellCommandStreaming
                    env
                    osDir
                    "printf first; sleep 1; printf second"
                    5000
                    (\out err -> modifyIORef' snapshots (<> [out <> err]))
                putMVar done result
            sawFirst <- waitForSnapshot snapshots (Text.isInfixOf "first") 20
            sawFirst `shouldBe` True
            early <- readIORef snapshots
            early `shouldSatisfy`
                any (\snapshot ->
                    "first" `Text.isInfixOf` snapshot
                        && not ("second" `Text.isInfixOf` snapshot))
            result <- takeMVar done
            result.commandStdout `shouldBe` "firstsecond"
            finalSnapshots <- readIORef snapshots
            finalSnapshots `shouldSatisfy`
                any (Text.isInfixOf "firstsecond")

    it "force-kills a process group on timeout" do
        withTempDir checkTimeoutKillsProcessGroup

    it "caps foreground output" do
        withTempDir checkForegroundOutputCap

    it "preserves oversized foreground output in an artifact" do
        withTempDir checkForegroundOutputArtifact

    it "collects both output streams from a background shell command" do
        withTempDir checkBackgroundOutput

    it "keeps ordinary background-command stdin closed" do
        withTempDir \dir -> do
            let osDir = fromFilePath dir
            env <- defaultToolEnv osDir
            Right running <- startShellCommand env osDir
                "if IFS= read -r line; then printf unexpected; else printf eof; fi"
            result <- readMVar running.runningResult
            result.commandStdout `shouldBe` "eof"

    it "writes to retained background-command stdin" do
        withTempDir \dir -> do
            let osDir = fromFilePath dir
            env <- defaultToolEnv osDir
            Right running <- startShellCommandWithInput env osDir
                "IFS= read -r line; printf 'got:%s' \"$line\""
            writeShellCommandInput running "hello\n" `shouldReturn` Right ()
            result <- readMVar running.runningResult
            result.commandStdout `shouldBe` "got:hello"

    it "caps live and final background output" do
        withTempDir checkBackgroundOutputCap

    it "preserves oversized completed background output in an artifact" do
        withTempDir checkBackgroundOutputArtifact

waitForSnapshot :: IORef [Text.Text] -> (Text.Text -> Bool) -> Int -> IO Bool
waitForSnapshot _ _ 0 = pure False
waitForSnapshot ref predicate attempts = do
    snapshots <- readIORef ref
    if any predicate snapshots
        then pure True
        else do
            threadDelay 50000
            waitForSnapshot ref predicate (attempts - 1)

checkTimeoutKillsProcessGroup dir = do
    let osDir = fromFilePath dir
        heartbeatFile = dir </> "heartbeat"
        command =
            "trap '' INT TERM; "
                <> "(trap '' INT TERM; i=0; while :; do "
                <> "i=$((i + 1)); printf '%s' \"$i\" > "
                <> show heartbeatFile
                <> "; sleep 0.02; done) & wait"
    env <- defaultToolEnv osDir
    result <- runShellCommand env osDir command 200
    result.commandTimedOut `shouldBe` True
    threadDelay 200000
    before <- readFile heartbeatFile
    threadDelay 200000
    readFile heartbeatFile `shouldReturn` before

checkForegroundOutputCap dir = do
    let osDir = fromFilePath dir
    base <- defaultToolEnv osDir
    let env = base { toolStdoutCap = 64 }
    result <- runShellCommand env osDir "yes x | head -c 262144" 5000
    Text.length result.commandStdout `shouldSatisfy` (< 128)
    result.commandStdout `shouldSatisfy` Text.isInfixOf "[truncated"

checkBackgroundOutputArtifact dir = do
    let osDir = fromFilePath dir
    base <- defaultToolEnv osDir
    setToolSessionTmp base (Just osDir)
    let env = base { toolStdoutCap = 64 }
    Right running <-
        startShellCommand env osDir "yes z | head -c 131072"
    result <- readMVar running.runningResult
    case result.commandStdoutArtifact of
        Nothing -> expectationFailure "expected stdout artifact"
        Just artifact -> do
            artifact.artifactObservedBytes `shouldBe` 131072
            readOutputArtifact env artifact.artifactHandle >>= \case
                Left err -> expectationFailure (Text.unpack err)
                Right output -> Text.length output `shouldBe` 131072

checkForegroundOutputArtifact dir = do
    let osDir = fromFilePath dir
    base <- defaultToolEnv osDir
    setToolSessionTmp base (Just osDir)
    let env = base { toolStdoutCap = 64 }
    result <- runShellCommand env osDir "yes x | head -c 262144" 5000
    result.commandStdoutArtifact `shouldSatisfy` maybe False (const True)
    case result.commandStdoutArtifact of
        Nothing -> expectationFailure "expected stdout artifact"
        Just artifact -> do
            artifact.artifactObservedBytes `shouldBe` 262144
            artifact.artifactTruncated `shouldBe` False
            readOutputArtifact env artifact.artifactHandle >>= \case
                Left err -> expectationFailure (Text.unpack err)
                Right output -> Text.length output `shouldBe` 262144

checkBackgroundOutput dir = do
    let osDir = fromFilePath dir
    env <- defaultToolEnv osDir
    Right running <-
        startShellCommand env osDir "printf stdout; printf stderr >&2"
    result <- readMVar running.runningResult
    result.commandExitCode `shouldBe` Just 0
    result.commandStdout `shouldBe` "stdout"
    result.commandStderr `shouldBe` "stderr"

checkBackgroundOutputCap dir = do
    let osDir = fromFilePath dir
    base <- defaultToolEnv osDir
    let env = base { toolStdoutCap = 64 }
    Right running <-
        startShellCommand env osDir
            "yes x | head -c 262144; sleep 30"
    threadDelay 200000
    (liveOut, _) <- runningLiveOutput running
    Text.length liveOut `shouldSatisfy` (< 128)
    liveOut `shouldSatisfy` Text.isInfixOf "[truncated"
    stopShellCommand running
    result <- readMVar running.runningResult
    Text.length result.commandStdout `shouldSatisfy` (< 128)
    result.commandStdout `shouldSatisfy` Text.isInfixOf "[truncated"

checkConcurrentAtomicWrites :: FilePath -> IO ()
checkConcurrentAtomicWrites dir = do
    let path = fromFilePath (dir </> "state.json")
        payloads = map (LBS8.pack . show) [1 :: Int .. 16]
    vars <- replicateM (length payloads) newEmptyMVar
    mapM_ (startAtomicWrite path) (zip payloads vars)
    results <- mapM takeMVar vars
    results `shouldSatisfy` all isRight
    final <- LBS8.readFile (dir </> "state.json")
    final `shouldSatisfy` (`elem` payloads)
    leftovers <- filter (Text.isInfixOf ".tmp" . Text.pack)
        <$> listDirectory dir
    leftovers `shouldBe` []

startAtomicWrite path (payload, var) =
    forkIO $
        tryIO (writeLazyFileAtomically path 0o600 payload) >>= putMVar var

checkConcurrentAppends :: FilePath -> IO ()
checkConcurrentAppends dir = do
    let file = dir </> "transcript.jsonl"
        path = fromFilePath file
        payloads = map (LBS8.pack . (<> "\n") . show) [1 :: Int .. 16]
    writeFile file ""
    vars <- replicateM (length payloads) newEmptyMVar
    mapM_ (startAppend path) (zip payloads vars)
    results <- mapM takeMVar vars
    results `shouldSatisfy` all isRight
    linesWritten <- sort . LBS8.lines <$> LBS8.readFile file
    linesWritten `shouldBe` sort (map LBS8.init payloads)

startAppend path (payload, var) =
    forkIO $
        tryIO (appendLazyFileRetryingOpen path payload) >>= putMVar var

pathsReferToSameFile :: FilePath -> FilePath -> IO Bool
pathsReferToSameFile left right =
    tryAny
        ((,)
            <$> getFileStatus left
            <*> getFileStatus right) >>= \case
        Left _ -> pure False
        Right (leftStatus, rightStatus) ->
            pure $
                deviceID leftStatus == deviceID rightStatus
                    && fileID leftStatus == fileID rightStatus

withTempDir :: (FilePath -> IO a) -> IO a
withTempDir action = do
    tmp <- getTemporaryDirectory
    bracket
        (mkdtemp (tmp </> "agent-io-XXXXXX"))
        removeDirectoryRecursive
        action

withSystemTempDir :: (FilePath -> IO a) -> IO a
withSystemTempDir =
    bracket
        (mkdtemp "/tmp/agent-io-host-XXXXXX")
        removeDirectoryRecursive
