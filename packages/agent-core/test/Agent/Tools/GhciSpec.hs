module Agent.Tools.GhciSpec (spec) where

import Agent.Cancel (requestCancel, resetCancel)
import Agent.Loop (defaultLoopDispatch)
import System.OsPath (decodeUtf, unsafeEncodeUtf)
import Agent.Provider (Provider(..))
import Agent.ToolDispatch (ToolCallResult(..), dispatchToolCall, functionToolCall)
import Agent.Tools (CodingTools(..), appToolHandlers, codingToolsFor, defaultToolEnv)
import Agent.Tools.Ghci
    ( GhciClass(..)
    , GhciOutcome(..)
    , GhciResult(..)
    , GhciSession
    , classifyGhci
    , classifyGhciInput
    , closeGhciSession
    , defaultGhciExtensions
    , evalGhci
    , newGhciSession
    , typeLooksEffectful
    )
import Agent.Tools.Grok (closeGrokSession, grokTools, newGrokSession)
import Agent.Tools.PlanMode (newPlanModeEnv)
import Agent.Tools.Types (AppTool(..), ToolEnv(..))
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (cancel, poll, wait, withAsync)
import Control.Exception.Safe (SomeException, bracket, try)
import Data.Either (isRight)
import Data.IORef
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import System.Directory
    ( doesFileExist
    , getTemporaryDirectory
    , removeDirectoryRecursive
    )
import System.FilePath ((</>))
import System.Posix.Signals (nullSignal, signalProcess)
import System.Posix.Temp (mkdtemp)
import System.Posix.Types (ProcessID)
import System.Timeout (timeout)
import Test.Hspec

fromFilePath = unsafeEncodeUtf
toFilePath path = either (error . show) id (decodeUtf path)

spec :: Spec
spec = describe "Agent.Tools.Ghci" do
    describe "classifyGhciInput" do
        it "marks info commands pure and shell-outs effectful" do
            classifyGhciInput ":type id" `shouldBe` Just GhciPure
            classifyGhciInput ":kind Maybe" `shouldBe` Just GhciPure
            classifyGhciInput ":! ls" `shouldBe` Just GhciEffectful
            classifyGhciInput ":load Foo" `shouldBe` Just GhciEffectful
            classifyGhciInput ":reload" `shouldBe` Just GhciEffectful
            classifyGhciInput "let x = 1" `shouldBe` Just GhciPure
            classifyGhciInput "unsafePerformIO (pure 1)" `shouldBe` Just GhciEffectful
            classifyGhciInput "1 + 1" `shouldBe` Nothing

        it "detects IO results from :type output" do
            typeLooksEffectful "putStrLn \"hi\" :: IO ()" `shouldBe` True
            typeLooksEffectful "1 + 1 :: Num a => a" `shouldBe` False
            typeLooksEffectful "id :: a -> a" `shouldBe` False
            typeLooksEffectful "getLine :: IO String" `shouldBe` True

    describe "defaultGhciExtensions" do
        it "covers the extra extensions this repo enables on top of GHC2021" do
            defaultGhciExtensions
                `shouldBe`
                    [ "BlockArguments"
                    , "OverloadedStrings"
                    , "OverloadedRecordDot"
                    , "DuplicateRecordFields"
                    , "NoFieldSelectors"
                    , "LambdaCase"
                    , "RecordWildCards"
                    ]

    it "persists bindings across evalGhci calls" do
        withTempGhci \ghci -> do
            bind <- evalGhci ghci "let x = 41 + 1" 10000
            bind.ghciOk `shouldBe` True
            bind.ghciClass `shouldBe` GhciPure
            value <- evalGhci ghci "x" 10000
            value.ghciOk `shouldBe` True
            value.ghciOutput `shouldSatisfy` Text.isInfixOf "42"

    it "supports multiline bindings and do blocks" do
        withTempGhci \ghci -> do
            bind <- evalGhci ghci "let addOne x =\n  x + 1" 10000
            bind.ghciOk `shouldBe` True
            value <- evalGhci ghci "addOne 41" 10000
            value.ghciOk `shouldBe` True
            value.ghciOutput `shouldSatisfy` Text.isInfixOf "42"
            action <- evalGhci ghci "do\n  let x = 1\n  print (x + 1)" 10000
            action.ghciOk `shouldBe` True
            action.ghciOutput `shouldSatisfy` Text.isInfixOf "2"

    it "evaluates OverloadedStrings and LambdaCase without LANGUAGE pragmas" do
        withTempGhci \ghci -> do
            str <- evalGhci ghci "\"hello\"" 10000
            str.ghciOk `shouldBe` True
            str.ghciOutput `shouldSatisfy` Text.isInfixOf "hello"
            lam <- evalGhci ghci "(\\case 1 -> True; _ -> False) 1" 10000
            lam.ghciOk `shouldBe` True
            lam.ghciOutput `shouldSatisfy` Text.isInfixOf "True"
            shown <- evalGhci ghci ":show language" 10000
            shown.ghciOk `shouldBe` True
            mapM_
                (\ext -> shown.ghciOutput `shouldSatisfy` Text.isInfixOf (Text.pack ext))
                defaultGhciExtensions

    it "does not mistake ordinary output for diagnostics" do
        withTempGhci \ghci -> do
            result <- evalGhci ghci "\"error: Exception: <interactive>:\"" 10000
            result.ghciOk `shouldBe` True
            result.ghciStdout
                `shouldSatisfy` Text.isInfixOf "error: Exception: <interactive>:"
            result.ghciStderr `shouldBe` ""

    it "captures runtime exceptions from stderr" do
        withTempGhci \ghci -> do
            result <- evalGhci ghci "error \"boom\"" 10000
            result.ghciOk `shouldBe` False
            result.ghciStderr `shouldSatisfy` Text.isInfixOf "*** Exception: boom"

    it "classifies putStrLn as effectful and 1+1 as pure" do
        withTempGhci \ghci -> do
            classifyGhci ghci "1 + 1" >>= (`shouldBe` GhciPure)
            classifyGhci ghci "putStrLn \"hi\"" >>= (`shouldBe` GhciEffectful)
            classifyGhci ghci ":! echo hi" >>= (`shouldBe` GhciEffectful)

    it "times out a long-running IO action and recovers" do
        withTempGhci \ghci -> do
            timed <- evalGhci ghci "last [1..]" 500
            timed.ghciTimedOut `shouldBe` True
            recovered <- evalGhci ghci "2 + 2" 10000
            recovered.ghciOk `shouldBe` True
            recovered.ghciOutput `shouldSatisfy` Text.isInfixOf "4"

    it "returns promptly when timed-out output fills the event queue" do
        withTempEnv \baseEnv -> do
            let env = baseEnv { toolStdoutCap = 64 }
            bracket (newGhciSession env) closeGhciSession \ghci -> do
                warmup <- evalGhci ghci "()" 10000
                warmup.ghciOk `shouldBe` True
                completed <- timeout 5000000
                    (evalGhci ghci infiniteOutput 300)
                timed <- requireCompleted "timed-out output" completed
                timed.ghciOutcome `shouldBe` GhciTimedOut
                timed.ghciTruncated `shouldBe` True
                recovered <- evalGhci ghci "2 + 2" 10000
                recovered.ghciOk `shouldBe` True
                recovered.ghciOutput `shouldSatisfy` Text.isInfixOf "4"

    it "honors cancellation and recovers the persistent process" do
        withTempEnv \env ->
            bracket (newGhciSession env) closeGhciSession \ghci -> do
                warmup <- evalGhci ghci "()" 10000
                warmup.ghciOk `shouldBe` True
                withAsync (evalGhci ghci infiniteOutput 10000) \running -> do
                    threadDelay 100000
                    requestCancel env.toolCancel
                    completed <- timeout 5000000 (wait running)
                    cancelled <- requireCompleted "cancelled output" completed
                    cancelled.ghciOutcome `shouldBe` GhciCancelled
                resetCancel env.toolCancel
                recovered <- evalGhci ghci "2 + 3" 10000
                recovered.ghciOk `shouldBe` True
                recovered.ghciOutput `shouldSatisfy` Text.isInfixOf "5"

    it "discards a sent request after asynchronous cancellation" do
        withTempEnv \env -> do
            let pidFile = toFilePath env.toolCwd </> "cancelled.pid"
            bracket (newGhciSession env) closeGhciSession \ghci -> do
                withAsync
                    (evalGhci ghci (delayedOutputCommand pidFile) 10000)
                    \running -> do
                        waitForFile pidFile
                        childPid <- read <$> readFile pidFile
                        completed <- timeout 5000000 (cancel running)
                        _ <- requireCompleted
                            "asynchronous GHCi cancellation" completed
                        waitForProcessDeath childPid
                recovered <- evalGhci ghci "\"NEW\"" 10000
                recovered.ghciOk `shouldBe` True
                recovered.ghciOutput `shouldSatisfy` Text.isInfixOf "NEW"
                recovered.ghciOutput `shouldNotSatisfy` Text.isInfixOf "OLD"

    it "caps retained output and reports truncation" do
        withTempEnv \baseEnv -> do
            let env = baseEnv { toolStdoutCap = 32 }
            bracket (newGhciSession env) closeGhciSession \ghci -> do
                result <- evalGhci ghci "replicate 200 'x'" 10000
                result.ghciOk `shouldBe` True
                result.ghciTruncated `shouldBe` True
                result.ghciOutput `shouldSatisfy` Text.isInfixOf "[truncated"
                Text.length result.ghciStdout `shouldSatisfy` (< 100)

    it "ignores repository .ghci files" do
        withTempEnv \env -> do
            writeFile (toFilePath env.toolCwd </> ".ghci")
                "let injectedByDotGhci = (99 :: Int)\n"
            bracket (newGhciSession env) closeGhciSession \ghci -> do
                result <- evalGhci ghci "injectedByDotGhci" 10000
                result.ghciOk `shouldBe` False
                result.ghciOutput
                    `shouldSatisfy` Text.isInfixOf "Variable not in scope"

    it "keeps its framing working after NoImplicitPrelude" do
        withTempGhci \ghci -> do
            changed <- evalGhci ghci ":set -XNoImplicitPrelude" 10000
            changed.ghciOk `shouldBe` True
            result <- evalGhci ghci "1 + 1" 10000
            result.ghciOk `shouldBe` True
            result.ghciOutput `shouldSatisfy` Text.isInfixOf "2"

    it "restarts after the evaluated command exits GHCi" do
        withTempGhci \ghci -> do
            exited <- evalGhci ghci ":quit" 10000
            exited.ghciOutcome `shouldBe` GhciProcessFailed
            recovered <- evalGhci ghci "6 * 7" 10000
            recovered.ghciOk `shouldBe` True
            recovered.ghciOutput `shouldSatisfy` Text.isInfixOf "42"

    it "kills the old process group after the GHCi leader exits" do
        withTempEnv \env -> do
            let pidFile = toFilePath env.toolCwd </> "background.pid"
            bracket (newGhciSession env) closeGhciSession \ghci -> do
                spawned <- evalGhci ghci (backgroundCommand pidFile) 10000
                spawned.ghciOk `shouldBe` True
                childPid <- read <$> readFile pidFile
                processAlive childPid `shouldReturn` True
                exited <- evalGhci ghci ":quit" 10000
                exited.ghciOutcome `shouldBe` GhciProcessFailed
                waitForProcessDeath childPid

    it "is terminal and idempotent after close" do
        withTempEnv \env -> do
            ghci <- newGhciSession env
            closeGhciSession ghci
            closeGhciSession ghci
            result <- evalGhci ghci "1 + 1" 10000
            result.ghciOutcome `shouldBe` GhciProcessFailed
            result.ghciOutput `shouldSatisfy` Text.isInfixOf "closed"

    it "does not reopen after close is asynchronously interrupted" do
        withTempEnv \env -> do
            let pidFile = toFilePath env.toolCwd </> "closing-child.pid"
            bracket (newGhciSession env) closeGhciSession \ghci -> do
                spawned <- evalGhci ghci (backgroundCommand pidFile) 10000
                spawned.ghciOk `shouldBe` True
                childPid <- read <$> readFile pidFile
                withAsync (closeGhciSession ghci) \closing -> do
                    -- The child ignores INT/TERM, keeping group shutdown in
                    -- its bounded wait after the close state is published.
                    threadDelay 100000
                    poll closing >>= \case
                        Nothing -> pure ()
                        Just result ->
                            expectationFailure
                                ("close finished before cancellation: "
                                    <> show result)
                    cancel closing
                result <- evalGhci ghci "1 + 1" 10000
                result.ghciOutcome `shouldBe` GhciProcessFailed
                result.ghciOutput `shouldSatisfy` Text.isInfixOf "closed"
                closeGhciSession ghci
                waitForProcessDeath childPid

    it "exposes run_ghci through grokTools dispatch" do
        withTempTools \tools -> do
            let handlers = appToolHandlers tools
            result <- dispatchToolCall defaultLoopDispatch handlers
                (functionToolCall "c1" "run_ghci"
                    "{\"expression\":\"3 + 4\",\"timeout\":\"10000\",\"description\":\"add\"}")
            result.output `shouldSatisfy` Text.isInfixOf "class: pure"
            result.output `shouldSatisfy` Text.isInfixOf "7"
            let names = map (.appToolName) tools
            names `shouldContain` ["run_ghci"]

    it "is registered for OpenAI via codingToolsFor" do
        withTempEnv \env -> do
            coding <- codingToolsFor OpenAIProvider env Nothing Nothing
            map (.appToolName) coding.codingAppTools `shouldContain` ["run_ghci"]
            coding.codingClose

infiniteOutput :: Text.Text
infiniteOutput =
    "let loop = putStrLn (replicate 4096 'x') >> loop in loop"

backgroundCommand :: FilePath -> Text.Text
backgroundCommand pidFile =
    ":! (trap '' INT TERM; while :; do sleep 1; done) "
        <> "</dev/null >/dev/null 2>&1 & echo $! > "
        <> Text.pack (show pidFile)

delayedOutputCommand :: FilePath -> Text.Text
delayedOutputCommand pidFile =
    ":! sh -c 'echo $$ > "
        <> Text.pack pidFile
        <> "; sleep 30; printf OLD'"

requireCompleted :: String -> Maybe a -> IO a
requireCompleted label = \case
    Just value -> pure value
    Nothing -> do
        expectationFailure (label <> " did not finish promptly")
        fail label

waitForProcessDeath :: ProcessID -> IO ()
waitForProcessDeath pid = go (200 :: Int)
  where
    go remaining = do
        alive <- processAlive pid
        if not alive
            then pure ()
            else retry remaining

    retry remaining
        | remaining <= 0 =
            expectationFailure ("process remained alive: " <> show pid)
        | otherwise = do
            threadDelay 10000
            go (remaining - 1)

processAlive :: ProcessID -> IO Bool
processAlive pid =
    isRight <$> try @_ @SomeException (signalProcess nullSignal pid)

waitForFile :: FilePath -> IO ()
waitForFile path = go (500 :: Int)
  where
    go remaining = do
        exists <- doesFileExist path
        if exists
            then pure ()
            else if remaining <= 0
                then expectationFailure ("file was not created: " <> path)
                else threadDelay 10000 >> go (remaining - 1)

withTempEnv :: (ToolEnv -> IO a) -> IO a
withTempEnv action =
    bracket acquire release \dir -> defaultToolEnv (fromFilePath dir) >>= action
  where
    acquire = do
        tmp <- getTemporaryDirectory
        mkdtemp (tmp </> "agent-ghci-env-")
    release dir = removeDirectoryRecursive dir

withTempGhci :: (GhciSession -> IO a) -> IO a
withTempGhci action =
    bracket acquire release \(_, ghci) -> action ghci
  where
    acquire = do
        tmp <- getTemporaryDirectory
        dir <- mkdtemp (tmp </> "agent-ghci-")
        env <- defaultToolEnv (fromFilePath dir)
        ghci <- newGhciSession env
        pure (dir, ghci)
    release (dir, ghci) = do
        closeGhciSession ghci
        removeDirectoryRecursive dir

withTempTools :: ([AppTool] -> IO a) -> IO a
withTempTools action =
    bracket acquire release \(_, _, tools) -> action tools
  where
    acquire = do
        tmp <- getTemporaryDirectory
        dir <- mkdtemp (tmp </> "agent-ghci-tools-")
        env <- defaultToolEnv (fromFilePath dir)
        session <- newGrokSession env
        ghci <- newGhciSession env
        plan <- newPlanModeEnv env.toolCwd Nothing
        typesRef <- newIORef Map.empty
        pure (dir, (session, ghci), grokTools session ghci plan Nothing typesRef)
    release (dir, (session, ghci), _) = do
        closeGrokSession session
        closeGhciSession ghci
        removeDirectoryRecursive dir
