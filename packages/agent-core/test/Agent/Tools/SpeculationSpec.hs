module Agent.Tools.SpeculationSpec (spec) where

import Agent.ToolDispatch
    ( ToolArgumentInterpreter
    , ToolArgumentStreamEvent(..)
    , ToolArgumentStreamItem(..)
    , ToolArgumentUpdate(..)
    , ToolCall(..)
    , ToolCallStreamRef(..)
    , functionToolCall
    , noArgsTool
    )
import Agent.Tools.Speculation
import Agent.Tools.Types
    ( AppTool
    , ToolExecutionPolicy(..)
    , jsonTool
    , withToolArgumentInterpreter
    )
import Control.Exception.Safe (bracket, finally, throwIO)
import Control.Monad (unless)
import Data.Acquire (mkAcquire)
import Data.IORef
import Test.Hspec

data Probe = Probe
    { starts :: !(IORef [(Int, ToolCall)])
    , updates :: !(IORef [(Int, ToolArgumentUpdate)])
    , finalizations :: !(IORef [(Int, ToolCall)])
    , takes :: !(IORef [(Int, ToolCall)])
    , cancellations :: !(IORef [Int])
    , closes :: !(IORef Int)
    }

spec :: Spec
spec = describe "ToolSpeculationRuntime" do
    it "correlates item, output, and call aliases across streamed updates" do
        withProbeRuntime False \probe runtime -> do
            let itemRef = ToolCallStreamItem "item-1"
                outputRef = ToolCallStreamOutput 7
                call = functionToolCall "call-1" "probe" "{\"value\":\"done\"}"
            observeToolArgumentEvent runtime $
                ToolArgumentsStarted
                    { argumentStreamRefs = [itemRef, outputRef]
                    , argumentStreamCallId = call.callId
                    , argumentStreamName = Just call.name
                    , argumentStreamArguments = ""
                    }
            observeToolArgumentEvent runtime $
                ToolArgumentsDelta
                    { argumentStreamRefs = [outputRef]
                    , argumentStreamDelta = "{\"value\":\"par"
                    }
            observeToolArgumentEvent runtime $
                ToolArgumentsDone
                    { argumentStreamRefs = [itemRef]
                    , argumentStreamName = Nothing
                    , argumentStreamArguments = call.arguments
                    }
            retainToolSpeculation runtime [call]

            takeToolSpeculation runtime call
                `shouldReturn` Just (Right call.arguments)
            readIORef probe.starts
                `shouldReturn` [(0, functionToolCall "call-1" "probe" "")]
            readIORef probe.updates `shouldReturn`
                [ (0, ToolArgumentDeltaUpdate "{\"value\":\"par")
                , (0, ToolArgumentDoneUpdate call.arguments)
                ]
            readIORef probe.finalizations `shouldReturn` [(0, call)]
            readIORef probe.takes `shouldReturn` [(0, call)]
            readIORef probe.cancellations `shouldReturn` []

    it "can start from arguments.done and bind the final call later" do
        withProbeRuntime False \probe runtime -> do
            let outputRef = ToolCallStreamOutput 3
                call = functionToolCall "call-done" "probe" "{\"ready\":true}"
            observeToolArgumentEvent runtime $
                ToolArgumentsDone
                    { argumentStreamRefs = [outputRef]
                    , argumentStreamName = Just "probe"
                    , argumentStreamArguments = call.arguments
                    }
            observeToolArgumentEvent runtime $
                ToolCallStreamCompleted
                    { argumentStreamRefs = [outputRef]
                    , argumentStreamCall = call
                    }
            retainToolSpeculation runtime [call]

            takeToolSpeculation runtime call
                `shouldReturn` Just (Right call.arguments)
            started <- readIORef probe.starts
            length started `shouldBe` 1
            map snd started
                `shouldBe` [functionToolCall "" "probe" call.arguments]

    it "keeps concurrent calls independent" do
        withProbeRuntime False \_ runtime -> do
            let callA = functionToolCall "call-a" "probe" "a"
                callB = functionToolCall "call-b" "probe" "b"
            observeToolArgumentEvent runtime $
                startedEvent (ToolCallStreamItem "item-a") callA
            observeToolArgumentEvent runtime $
                startedEvent (ToolCallStreamItem "item-b") callB
            observeToolArgumentEvent runtime $
                ToolArgumentsDelta
                    [ToolCallStreamItem "item-a"]
                    "1"
            observeToolArgumentEvent runtime $
                ToolArgumentsDelta
                    [ToolCallStreamItem "item-b"]
                    "2"
            retainToolSpeculation runtime
                [ callA { arguments = "a1" }
                , callB { arguments = "b2" }
                ]

            takeToolSpeculation runtime (callB { arguments = "b2" })
                `shouldReturn` Just (Right "b2")
            takeToolSpeculation runtime (callA { arguments = "a1" })
                `shouldReturn` Just (Right "a1")

    it "finalizes a retained call only once" do
        withProbeRuntime False \probe runtime -> do
            let itemRef = ToolCallStreamItem "item-final"
                call = functionToolCall "call-final" "probe" "{}"
            observeToolArgumentEvent runtime $
                startedEvent itemRef call
            observeToolArgumentEvent runtime $
                ToolCallStreamCompleted
                    { argumentStreamRefs = [itemRef]
                    , argumentStreamCall = call
                    }
            retainToolSpeculation runtime [call]
            retainToolSpeculation runtime [call]
            waitForToolSpeculation runtime

            readIORef probe.finalizations `shouldReturn` [(0, call)]

    it "does not bind an aliased entry to a different tool" do
        withProbeRuntime False \probe runtime -> do
            let itemRef = ToolCallStreamItem "item-mismatch"
                call = functionToolCall "call-probe" "probe" "{}"
                mismatched =
                    functionToolCall "call-other" "other" "{}"
            observeToolArgumentEvent runtime $
                startedEvent itemRef call
            observeToolArgumentEvent runtime $
                ToolCallStreamCompleted
                    { argumentStreamRefs = [itemRef]
                    , argumentStreamCall = mismatched
                    }

            readIORef probe.finalizations `shouldReturn` []
            retainToolSpeculation runtime [call]
            waitForToolSpeculation runtime
            readIORef probe.finalizations `shouldReturn` [(0, call)]

    it "does not rebind an aliased entry to a different call id" do
        withProbeRuntime False \probe runtime -> do
            let itemRef = ToolCallStreamItem "item-call-mismatch"
                original = functionToolCall "call-original" "probe" "{}"
                conflicting = functionToolCall "call-conflicting" "probe" "{}"
            observeToolArgumentEvent runtime $
                startedEvent itemRef original
            observeToolArgumentEvent runtime $
                ToolCallStreamCompleted
                    { argumentStreamRefs = [itemRef]
                    , argumentStreamCall = conflicting
                    }

            readIORef probe.finalizations `shouldReturn` []
            retainToolSpeculation runtime [original]
            takeToolSpeculation runtime conflicting `shouldReturn` Nothing
            takeToolSpeculation runtime original
                `shouldReturn` Just (Right original.arguments)

    it "does not start an uncorrelatable arguments.done event" do
        withProbeRuntime False \probe runtime -> do
            observeToolArgumentEvent runtime $
                ToolArgumentsDone
                    { argumentStreamRefs = []
                    , argumentStreamName = Just "probe"
                    , argumentStreamArguments = "{}"
                    }

            readIORef probe.starts `shouldReturn` []

    it "cancels calls omitted from the authoritative response" do
        withProbeRuntime False \probe runtime -> do
            let callA = functionToolCall "call-a" "probe" "a"
                callB = functionToolCall "call-b" "probe" "b"
            observeToolArgumentEvent runtime $
                startedEvent (ToolCallStreamItem "item-a") callA
            observeToolArgumentEvent runtime $
                startedEvent (ToolCallStreamItem "item-b") callB
            waitForToolSpeculation runtime
            retainToolSpeculation runtime [callA]

            readIORef probe.cancellations `shouldReturn` [1]
            takeToolSpeculation runtime callB `shouldReturn` Nothing
            takeToolSpeculation runtime callA
                `shouldReturn` Just (Right "a")

    it "supports explicit discard, reset, barriers, and idempotent close" do
        (tool, probe) <- newProbeTool False
        runtime <- newToolSpeculationRuntime [tool]
        let callA = functionToolCall "call-a" "probe" "a"
            callB = functionToolCall "call-b" "probe" "b"
        observeToolArgumentEvent runtime $
            startedEvent (ToolCallStreamItem "item-a") callA
        observeToolArgumentEvent runtime $
            ToolArgumentsDelta [ToolCallStreamItem "item-a"] "1"
        waitForToolSpeculation runtime
        readIORef probe.updates `shouldReturn`
            [(0, ToolArgumentDeltaUpdate "1")]
        discardToolSpeculation runtime callA
        observeToolArgumentEvent runtime $
            startedEvent (ToolCallStreamItem "item-b") callB
        waitForToolSpeculation runtime
        resetToolSpeculationRuntime runtime
        closeToolSpeculationRuntime runtime
        closeToolSpeculationRuntime runtime

        readIORef probe.cancellations `shouldReturn` [0, 1]
        readIORef probe.closes `shouldReturn` 1

    it "turns prepared-result failures into ordinary misses" do
        withProbeRuntime True \probe runtime -> do
            let call = functionToolCall "call-fail" "probe" "{}"
            observeToolArgumentEvent runtime $
                startedEvent (ToolCallStreamItem "item-fail") call
            retainToolSpeculation runtime [call]

            takeToolSpeculation runtime call `shouldReturn` Nothing
            readIORef probe.cancellations `shouldReturn` []

withProbeRuntime
    :: Bool
    -> (Probe -> ToolSpeculationRuntime -> IO a)
    -> IO a
withProbeRuntime failTake action = do
    (tool, probe) <- newProbeTool failTake
    bracket
        (newToolSpeculationRuntime [tool])
        closeToolSpeculationRuntime
        (action probe)

newProbeTool :: Bool -> IO (AppTool, Probe)
newProbeTool failTake = do
    nextId <- newIORef 0
    starts <- newIORef []
    updates <- newIORef []
    finalizations <- newIORef []
    takes <- newIORef []
    cancellations <- newIORef []
    closes <- newIORef 0
    let probe = Probe
            { starts
            , updates
            , finalizations
            , takes
            , cancellations
            , closes
            }
        interpreter = probeInterpreter nextId probe failTake
        factory =
            mkAcquire
                (pure interpreter)
                (\_ -> modifyIORef' closes (+ 1))
        tool =
            withToolArgumentInterpreter factory $
                jsonTool
                    "probe"
                    "probe"
                    []
                    True
                    ParallelSafe
                    (noArgsTool "probe" (pure (Right "ordinary")))
    pure (tool, probe)

probeInterpreter
    :: IORef Int
    -> Probe
    -> Bool
    -> ToolArgumentInterpreter
probeInterpreter nextId probe failTake call source = do
    entryId <- atomicModifyIORef' nextId \current ->
        (current + 1, current)
    modifyIORef' probe.starts (<> [(entryId, call)])
    argumentsRef <- newIORef call.arguments
    completed <- newIORef False
    let consume =
            source >>= \case
                ToolArgumentStreamUpdate update -> do
                    modifyIORef' probe.updates (<> [(entryId, update)])
                    case update of
                        ToolArgumentDeltaUpdate delta ->
                            modifyIORef' argumentsRef (<> delta)
                        ToolArgumentDoneUpdate arguments ->
                            writeIORef argumentsRef arguments
                    consume
                ToolArgumentStreamFinal finalCall -> do
                    modifyIORef'
                        probe.finalizations
                        (<> [(entryId, finalCall)])
                    writeIORef argumentsRef finalCall.arguments
                    writeIORef completed True
    consume `finally` do
        finished <- readIORef completed
        unless finished $
            modifyIORef' probe.cancellations (<> [entryId])
    pure \finalCall -> do
        modifyIORef' probe.takes (<> [(entryId, finalCall)])
        if failTake
            then throwIO (userError "probe take failed")
            else Just . Right <$> readIORef argumentsRef

startedEvent :: ToolCallStreamRef -> ToolCall -> ToolArgumentStreamEvent
startedEvent ref call =
    ToolArgumentsStarted
        { argumentStreamRefs = [ref]
        , argumentStreamCallId = call.callId
        , argumentStreamName = Just call.name
        , argumentStreamArguments = call.arguments
        }
