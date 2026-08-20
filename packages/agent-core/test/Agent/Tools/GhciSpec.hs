module Agent.Tools.GhciSpec (spec) where

import Agent.Loop (defaultLoopDispatch)
import Agent.ToolDispatch (ToolCallResult(..), dispatchToolCall, functionToolCall)
import Agent.Provider (Provider(..))
import Agent.Tools (CodingTools(..), appToolHandlers, codingToolsFor, defaultToolEnv)
import Agent.Tools.Grok (closeGrokSession, grokTools, newGrokSession)
import Agent.Tools.PlanMode (newPlanModeEnv)
import Agent.Tools.Ghci
    ( GhciClass(..)
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
import Agent.Tools.Types (AppTool(..), ToolEnv(..))
import Control.Exception.Safe (bracket)
import qualified Data.Text as Text
import System.Directory (getTemporaryDirectory, removeDirectoryRecursive)
import System.FilePath ((</>))
import System.Posix.Temp (mkdtemp)
import Data.IORef
import qualified Data.Map.Strict as Map
import Test.Hspec

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

    it "exposes run_ghci through grokTools dispatch" do
        withTempTools \tools -> do
            let handlers = appToolHandlers tools
            result <- dispatchToolCall defaultLoopDispatch handlers
                (functionToolCall "c1" "run_ghci"
                    "{\"expression\":\"3 + 4\",\"description\":\"add\"}")
            result.output `shouldSatisfy` Text.isInfixOf "class: pure"
            result.output `shouldSatisfy` Text.isInfixOf "7"
            let names = map (.appToolName) tools
            names `shouldContain` ["run_ghci"]


    it "is registered for OpenAI via codingToolsFor" do
        withTempEnv \env -> do
            coding <- codingToolsFor OpenAIProvider env Nothing Nothing
            map (.appToolName) coding.codingAppTools `shouldContain` ["run_ghci"]
            coding.codingClose

withTempEnv :: (ToolEnv -> IO a) -> IO a
withTempEnv action =
    bracket acquire release \dir -> defaultToolEnv dir >>= action
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
        env <- defaultToolEnv dir
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
        env <- defaultToolEnv dir
        session <- newGrokSession env
        ghci <- newGhciSession env
        plan <- newPlanModeEnv env.toolCwd Nothing
        typesRef <- newIORef Map.empty
        pure (dir, (session, ghci), grokTools session ghci plan Nothing typesRef)
    release (dir, (session, ghci), _) = do
        closeGrokSession session
        closeGhciSession ghci
        removeDirectoryRecursive dir
