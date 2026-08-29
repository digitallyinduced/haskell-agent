module Agent.CLI.AgentViewportRuntimeSpec (spec) where

import Agent.CLI.AgentViewport
    ( AgentEntry(..)
    , AgentTarget(..)
    , AgentViewportEnv(..)
    )
import Agent.CLI.AgentViewport.Runtime
import Agent.Loop
    ( LoopEvent(..)
    , NativeAgentStatus(..)
    )
import Agent.Subagents
    ( SubagentId(..)
    , SubagentStatus(..)
    )
import Data.IORef
    ( modifyIORef'
    , newIORef
    , readIORef
    , writeIORef
    )
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Test.Hspec

spec :: Spec
spec = do
    describe "agent viewport runtime" do
        it "owns child selection and release transitions" do
            let alpha = SubagentId "alpha"
                beta = SubagentId "beta"
                listings =
                    [ AgentChildListing "/root/alpha" alpha Running
                    , AgentChildListing
                        "/root/beta"
                        beta
                        (Completed (Just "finished"))
                    ]
                sources = Map.fromList
                    [ (alpha, AgentChildSource "model-a" (pure []))
                    , (beta, AgentChildSource "model-b" (pure []))
                    ]
            actions <- newIORef ([] :: [Text])
            runtime <- newAgentViewportRuntime AgentViewportRuntimeConfig
                { viewportConfigShowRawReasoning = False
                , viewportConfigWorkspace = "/workspace"
                , viewportConfigReadRootTranscript = pure []
                , viewportConfigListChildren = pure listings
                , viewportConfigReadChildSources = pure sources
                , viewportConfigSelectChild = \agentId ->
                    modifyIORef' actions
                        (<> ["select:" <> agentId.unSubagentId])
                , viewportConfigReleaseChild = \agentId ->
                    modifyIORef' actions
                        (<> ["release:" <> agentId.unSubagentId])
                }

            (selected, entries) <- loadAgentSnapshot runtime False
            selected `shouldBe` AgentRoot
            map (.agentTarget) entries
                `shouldBe`
                    [ AgentRoot
                    , AgentChild alpha
                    , AgentChild beta
                    ]
            map (.agentModel) entries
                `shouldBe` [Nothing, Just "model-a", Just "model-b"]
            entries !! 2
                `shouldSatisfy`
                    \entry ->
                        entry.agentTranscript == ["assistant: finished"]

            selectAgentViewport runtime (AgentChild alpha)
            selectAgentViewport runtime (AgentChild beta)
            selectAgentViewport runtime AgentRoot
            readIORef actions
                `shouldReturn`
                    [ "select:alpha"
                    , "release:alpha"
                    , "select:beta"
                    , "release:beta"
                    ]

        it "reconciles vanished selections and resets derived state" do
            let alpha = SubagentId "alpha"
            listings <- newIORef
                [AgentChildListing "/root/alpha" alpha Running]
            runtime <- newAgentViewportRuntime AgentViewportRuntimeConfig
                { viewportConfigShowRawReasoning = False
                , viewportConfigWorkspace = "/workspace"
                , viewportConfigReadRootTranscript = pure []
                , viewportConfigListChildren = readIORef listings
                , viewportConfigReadChildSources = pure Map.empty
                , viewportConfigSelectChild = const (pure ())
                , viewportConfigReleaseChild = const (pure ())
                }
            selectAgentViewport runtime (AgentChild alpha)
            writeIORef listings []

            (selected, _) <- loadAgentSnapshot runtime False
            selected `shouldBe` AgentRoot
            resetAgentViewport runtime
            readIORef
                (agentViewportEnvironment runtime).viewportSelected
                `shouldReturn` AgentRoot

        it "tracks provider-native agent events inside the runtime" do
            runtime <- newAgentViewportRuntime AgentViewportRuntimeConfig
                { viewportConfigShowRawReasoning = False
                , viewportConfigWorkspace = "/workspace"
                , viewportConfigReadRootTranscript = pure []
                , viewportConfigListChildren = pure []
                , viewportConfigReadChildSources = pure Map.empty
                , viewportConfigSelectChild = const (pure ())
                , viewportConfigReleaseChild = const (pure ())
                }
            recordAgentViewportEvent runtime
                (NativeAgentStarted
                    "native-1"
                    Nothing
                    "explore"
                    (Just "claude-sonnet"))
            recordAgentViewportEvent runtime
                (NativeAgentFinished
                    "native-1"
                    NativeAgentCompleted)

            (_, entries) <- loadAgentSnapshot runtime False
            map (.agentTarget) entries
                `shouldBe` [AgentRoot, AgentNative "native-1"]
            entries !! 1
                `shouldSatisfy`
                    \entry ->
                        entry.agentStatus == "done"
                            && entry.agentModel == Just "claude-sonnet"
