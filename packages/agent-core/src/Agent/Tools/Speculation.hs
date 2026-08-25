module Agent.Tools.Speculation
    ( ToolSpeculationRuntime
    , newToolSpeculationRuntime
    , closeToolSpeculationRuntime
    , resetToolSpeculationRuntime
    , observeToolArgumentEvent
    , retainToolSpeculation
    , takeToolSpeculation
    , discardToolSpeculation
    , waitForToolSpeculation
    ) where

import Agent.ResourceScope
    ( ResourceKey
    , ResourceScope
    , allocateAcquireResource
    , allocateResource
    , closeResourceScope
    , newResourceScope
    , releaseResource
    )
import Agent.ToolDispatch
    ( StreamedTool(StreamedTool)
    , ToolArgumentStreamEvent(..)
    , ToolCall(..)
    , ToolCallStreamRef(..)
    , ToolInput(..)
    , ToolResult
    , canonicalToolName
    , functionToolCall
    )
import Agent.Tools.Types (AppTool(..))
import Control.Applicative ((<|>))
import Control.Concurrent.Async
    ( Async
    , asyncWithUnmask
    , cancel
    , poll
    , race
    , waitCatch
    )
import Control.Concurrent.MVar
    ( MVar
    , modifyMVar
    , newMVar
    , readMVar
    )
import Data.IORef
    ( atomicModifyIORef
    , newIORef
    , writeIORef
    )
import Control.Concurrent.STM
    ( TMVar
    , TQueue
    , atomically
    , newEmptyTMVarIO
    , newTQueueIO
    , putTMVar
    , readTQueue
    , takeTMVar
    , writeTQueue
    )
import Control.Exception.Safe
    ( SomeException
    , finally
    , mask
    , onException
    , tryAny
    )
import Control.Monad (foldM, forM_, guard, join, void)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text

-- | Session-scoped router for streamed tool interpreters.
--
-- Provider adapters feed a small provider-neutral event stream. This runtime
-- correlates those events onto one interpreter state per call and feeds that
-- state accumulated argument text. Speculation is not part of this API: a tool
-- may prefetch inside its 'streamedInterpret' implementation.
data ToolSpeculationRuntime = ToolSpeculationRuntime
    { interpreters :: !(Map.Map Text StreamedTool)
    , resources :: !ResourceScope
    , state :: !(MVar RuntimeState)
    }

data RuntimeState = RuntimeState
    { closed :: !Bool
    , nextEntryId :: !Int
    , aliases :: !(Map.Map ToolCallStreamRef Int)
    , activeEntries :: !(Map.Map Int ActiveEntry)
    }

data ActiveEntry = ActiveEntry
    { toolName :: !Text
    , entryCallId :: !(Maybe Text)
    , finalized :: !Bool
    , accumulated :: !Text
    , argumentQueue :: !(TQueue ArgumentStreamCommand)
    , interpreterWorker :: !(Async (Maybe ToolResult))
    , resourceKey :: !ResourceKey
    }

data ArgumentStreamCommand
    = DeliverPrefix !Text
    | DeliverDone !Text
    | ReachArgumentBarrier !(TMVar ())

newToolSpeculationRuntime
    :: [AppTool]
    -> IO ToolSpeculationRuntime
newToolSpeculationRuntime tools = mask \restore -> do
    resources <- newResourceScope
    interpreters <-
        restore (foldM (openOne resources) Map.empty tools)
            `onException` closeResourceScope resources
    state <- newMVar RuntimeState
        { closed = False
        , nextEntryId = 0
        , aliases = Map.empty
        , activeEntries = Map.empty
        }
    pure ToolSpeculationRuntime { interpreters, resources, state }
  where
    openOne
        :: ResourceScope
        -> Map.Map Text StreamedTool
        -> AppTool
        -> IO (Map.Map Text StreamedTool)
    openOne resources current tool =
        case tool.appToolArgumentInterpreter of
            Nothing -> pure current
            Just acquireTool
                | Map.member name current -> pure current
                | otherwise ->
                    tryAny
                        (allocateAcquireResource
                            resources
                            acquireTool) >>= \case
                        Left (_ :: SomeException) -> pure current
                        Right (_, streamed) ->
                            pure (Map.insert name streamed current)
              where
                name = canonicalToolName tool.appToolName

closeToolSpeculationRuntime :: ToolSpeculationRuntime -> IO ()
closeToolSpeculationRuntime runtime = mask \restore -> do
    wasOpen <-
        modifyMVar runtime.state \current ->
            if current.closed
                then pure (current, False)
                else pure
                    ( current
                        { closed = True
                        , aliases = Map.empty
                        , activeEntries = Map.empty
                        }
                    , True
                    )
    if wasOpen
        then restore (closeResourceScope runtime.resources)
        else pure ()

resetToolSpeculationRuntime :: ToolSpeculationRuntime -> IO ()
resetToolSpeculationRuntime runtime = do
    entries <- modifyMVar runtime.state \current ->
        if current.closed
            then pure (current, [])
            else pure
                ( current
                    { aliases = Map.empty
                    , activeEntries = Map.empty
                    }
                , Map.elems current.activeEntries
                )
    releaseEntries entries

-- | Best-effort provider-neutral stream observation. Synchronous failures in
-- interpreter plumbing are contained and leave ordinary tool execution intact.
observeToolArgumentEvent
    :: ToolSpeculationRuntime
    -> ToolArgumentStreamEvent
    -> IO ()
observeToolArgumentEvent runtime event = do
    _ <- tryAny (observe event) :: IO (Either SomeException ())
    pure ()
  where
    observe = \case
        ToolArgumentsStarted
            { argumentStreamRefs
            , argumentStreamCallId
            , argumentStreamName
            , argumentStreamArguments
            } ->
                forM_ argumentStreamName \name -> do
                    let call =
                            functionToolCall
                                argumentStreamCallId
                                name
                                argumentStreamArguments
                    active <-
                        ensureActive
                            runtime
                            call
                            (withCallRef
                                argumentStreamCallId
                                argumentStreamRefs)
                    forM_ active \(entryId, _) ->
                        feedPrefix
                            runtime
                            entryId
                            (const argumentStreamArguments)
        ToolArgumentsDelta
            { argumentStreamRefs
            , argumentStreamDelta
            } ->
                lookupActive runtime argumentStreamRefs Nothing >>= mapM_
                    (\(entryId, _) ->
                        feedPrefix
                            runtime
                            entryId
                            (<> argumentStreamDelta))
        ToolArgumentsDone
            { argumentStreamRefs
            , argumentStreamName
            , argumentStreamArguments
            } -> do
                existing <-
                    lookupActive
                        runtime
                        argumentStreamRefs
                        argumentStreamName
                active <- case existing of
                    Just entry -> pure (Just entry)
                    Nothing -> case argumentStreamName of
                        Nothing -> pure Nothing
                        Just name
                            | null (normalizeRefs argumentStreamRefs) ->
                                pure Nothing
                            | otherwise ->
                                ensureActive
                                    runtime
                                    (functionToolCall
                                        ""
                                        name
                                        argumentStreamArguments)
                                    argumentStreamRefs
                forM_ active \(entryId, _) ->
                    feedPrefix
                        runtime
                        entryId
                        (const argumentStreamArguments)
        ToolCallStreamCompleted
            { argumentStreamRefs
            , argumentStreamCall
            } -> do
                active <-
                    ensureActive
                        runtime
                        argumentStreamCall
                        (withCallRef
                            argumentStreamCall.callId
                            argumentStreamRefs)
                -- Output-item completion is useful streamed information, but
                -- the completed response retained below remains authoritative.
                forM_ active \(entryId, _) ->
                    feedPrefix
                        runtime
                        entryId
                        (const argumentStreamCall.arguments)

-- | Keep only calls present in the authoritative completed response and send
-- each retained interpreter its terminal call. Calls omitted from the response
-- are released immediately.
retainToolSpeculation
    :: ToolSpeculationRuntime
    -> [ToolCall]
    -> IO ()
retainToolSpeculation runtime calls = do
    let finalCalls =
            Map.fromList
                [ (call.callId, call)
                | call <- calls
                ]
    (retained, abandoned) <-
        modifyMVar runtime.state \current ->
            if current.closed
                then pure (current, ([], []))
                else do
                    let retainedWithCalls =
                            mapMaybe
                                (\(entryId, entry) -> do
                                    finalCallId <- entry.entryCallId
                                    finalCall <- Map.lookup finalCallId finalCalls
                                    guard $
                                        entry.toolName
                                            == canonicalToolName finalCall.name
                                    pure (entryId, entry, finalCall))
                                (Map.toList current.activeEntries)
                        retainedIds =
                            Set.fromList
                                [ entryId
                                | (entryId, _, _) <- retainedWithCalls
                                ]
                        abandonedEntries =
                            [ entry
                            | (entryId, entry) <-
                                Map.toList current.activeEntries
                            , Set.notMember entryId retainedIds
                            ]
                        retainedEntries =
                            [ ( entryId
                              , if entry.finalized
                                    then entry
                                    else entry { finalized = True }
                              , finalCall
                              , not entry.finalized
                              )
                            | (entryId, entry, finalCall) <-
                                retainedWithCalls
                            ]
                        keptEntries = Map.fromList
                            [ (entryId, entry)
                            | (entryId, entry, _, _) <- retainedEntries
                            ]
                        keptAliases =
                            Map.filter (`Set.member` retainedIds)
                                current.aliases
                    pure
                        ( current
                            { aliases = keptAliases
                            , activeEntries = keptEntries
                            }
                        , ( [ (entryId, entry, finalCall)
                            | (entryId, entry, finalCall, shouldFinalize) <-
                                retainedEntries
                            , shouldFinalize
                            ]
                          , abandonedEntries
                          )
                        )
    releaseEntries abandoned
    forM_ retained \(entryId, _entry, call) ->
        feedPrefix runtime entryId (const call.arguments)

-- | Finish one interpreter after normal approval and scheduling. Any
-- synchronous interpreter failure becomes an ordinary miss, which falls
-- back to the ordinary tool handler.
takeToolSpeculation
    :: ToolSpeculationRuntime
    -> ToolCall
    -> IO (Maybe ToolResult)
takeToolSpeculation runtime call = do
    selected <- removeByCallId runtime call.callId
    case selected of
        Nothing -> pure Nothing
        Just entry ->
            consume entry `finally` safeReleaseEntry entry
  where
    consume entry
        | not (entryMatchesCall entry call) = pure Nothing
        | otherwise = do
            enqueueCommand entry (DeliverDone call.arguments)
            waitCatch entry.interpreterWorker >>= \case
                Left _ -> pure Nothing
                Right result -> pure result

discardToolSpeculation
    :: ToolSpeculationRuntime
    -> ToolCall
    -> IO ()
discardToolSpeculation runtime call =
    removeByCallId runtime call.callId
        >>= mapM_ safeReleaseEntry

-- | Wait until each active interpreter has processed all argument items
-- currently queued for it, or until the interpreter itself has completed.
waitForToolSpeculation :: ToolSpeculationRuntime -> IO ()
waitForToolSpeculation runtime = do
    entries <-
        Map.elems . (.activeEntries)
            <$> readMVar runtime.state
    forM_ entries waitForEntry

waitForEntry :: ActiveEntry -> IO ()
waitForEntry entry =
    poll entry.interpreterWorker >>= \case
        Just _ -> pure ()
        Nothing -> do
            reached <- newEmptyTMVarIO
            atomically $
                writeTQueue
                    entry.argumentQueue
                    (ReachArgumentBarrier reached)
            void $
                race
                    (atomically (takeTMVar reached))
                    (void (waitCatch entry.interpreterWorker))

ensureActive
    :: ToolSpeculationRuntime
    -> ToolCall
    -> [ToolCallStreamRef]
    -> IO (Maybe (Int, ActiveEntry))
ensureActive runtime call rawRefs =
    modifyMVar runtime.state \current ->
        if current.closed
            then pure (current, Nothing)
            else do
                let refs = normalizeRefs rawRefs
                    referenced = referencedEntryIds refs current
                case Set.toList referenced of
                    [entryId] ->
                        case Map.lookup entryId current.activeEntries of
                            Nothing -> pure (current, Nothing)
                            Just entry
                                | not (entryMatchesCall entry call) ->
                                    pure (current, Nothing)
                                | otherwise -> do
                                    let callId =
                                            nonEmpty call.callId
                                                <|> entry.entryCallId
                                        updatedEntry =
                                            entry { entryCallId = callId }
                                        updated =
                                            attachAliases
                                                entryId
                                                refs
                                                current
                                                    { activeEntries =
                                                        Map.insert
                                                            entryId
                                                            updatedEntry
                                                            current.activeEntries
                                                    }
                                    pure
                                        ( updated
                                        , Just (entryId, updatedEntry)
                                        )
                    []
                        | Text.null (Text.strip call.name) ->
                            pure (current, Nothing)
                        | otherwise ->
                            case
                                Map.lookup
                                    (canonicalToolName call.name)
                                    runtime.interpreters
                            of
                                Nothing -> pure (current, Nothing)
                                Just interpreter -> do
                                    started <-
                                        tryAny
                                            (startInterpreter
                                                runtime
                                                interpreter
                                                call)
                                    case started of
                                        Left (_ :: SomeException) ->
                                            pure (current, Nothing)
                                        Right active -> do
                                            let entryId = current.nextEntryId
                                                entry =
                                                    active
                                                        { toolName =
                                                            canonicalToolName
                                                                call.name
                                                        , entryCallId =
                                                            nonEmpty call.callId
                                                        }
                                                inserted =
                                                    current
                                                        { nextEntryId =
                                                            entryId + 1
                                                        , activeEntries =
                                                            Map.insert
                                                                entryId
                                                                entry
                                                                current.activeEntries
                                                        }
                                                updated =
                                                    attachAliases
                                                        entryId
                                                        refs
                                                        inserted
                                            pure
                                                ( updated
                                                , Just (entryId, entry)
                                                )
                    _ -> pure (current, Nothing)

startInterpreter
    :: ToolSpeculationRuntime
    -> StreamedTool
    -> ToolCall
    -> IO ActiveEntry
startInterpreter runtime streamed call = mask \_ -> do
    argumentQueue <- newTQueueIO
    (resourceKey, interpreterWorker) <-
        allocateResource
            runtime.resources
            (asyncWithUnmask \unmask ->
                unmask (runStreamed streamed argumentQueue))
            stopInterpreter
    pure ActiveEntry
        { toolName = ""
        , entryCallId = Nothing
        , finalized = False
        , accumulated = call.arguments
        , argumentQueue
        , interpreterWorker
        , resourceKey
        }

runStreamed
    :: StreamedTool
    -> TQueue ArgumentStreamCommand
    -> IO (Maybe ToolResult)
runStreamed (StreamedTool start interpret close) queue =
    mask \restore -> do
        initial <- start
        live <- newIORef (Just initial)
        let
            commit state = writeIORef live (Just state)
            finished = writeIORef live Nothing
            closeLive = do
                pending <- atomicModifyIORef live \s -> (Nothing, s)
                mapM_ close pending
            go state = restore (step state) `onException` closeLive
            step state =
                atomically (readTQueue queue) >>= \case
                    ReachArgumentBarrier reached -> do
                        atomically (putTMVar reached ())
                        go state
                    DeliverPrefix text ->
                        interpret state (ToolPrefix text) >>= \case
                            Right next -> do
                                commit next
                                go next
                            Left result -> do
                                finished
                                pure (Just result)
                    DeliverDone text ->
                        interpret state (ToolDone text) >>= \case
                            Left result -> do
                                finished
                                pure (Just result)
                            Right leftover -> do
                                close leftover
                                finished
                                pure Nothing
        go initial

feedPrefix
    :: ToolSpeculationRuntime
    -> Int
    -> (Text -> Text)
    -> IO ()
feedPrefix runtime entryId update =
    join $
        modifyMVar runtime.state \current ->
            if current.closed
                then pure (current, pure ())
                else case Map.lookup entryId current.activeEntries of
                    Nothing -> pure (current, pure ())
                    Just entry -> do
                        let accumulated = update entry.accumulated
                            updated = entry { accumulated }
                        pure
                            ( current
                                { activeEntries =
                                    Map.insert
                                        entryId
                                        updated
                                        current.activeEntries
                                }
                            , if Text.null accumulated
                                then pure ()
                                else
                                    runEntryAction runtime entryId $
                                        enqueueCommand
                                            updated
                                            (DeliverPrefix accumulated)
                            )

enqueueCommand :: ActiveEntry -> ArgumentStreamCommand -> IO ()
enqueueCommand entry command =
    atomically $
        writeTQueue entry.argumentQueue command

lookupActive
    :: ToolSpeculationRuntime
    -> [ToolCallStreamRef]
    -> Maybe Text
    -> IO (Maybe (Int, ActiveEntry))
lookupActive runtime rawRefs maybeName =
    modifyMVar runtime.state \current ->
        if current.closed
            then pure (current, Nothing)
            else do
                let refs = normalizeRefs rawRefs
                case Set.toList (referencedEntryIds refs current) of
                    [entryId] ->
                        case Map.lookup entryId current.activeEntries of
                            Just entry
                                | entryMatchesName entry maybeName ->
                                    pure
                                        ( attachAliases entryId refs current
                                        , Just (entryId, entry)
                                        )
                            _ -> pure (current, Nothing)
                    _ -> pure (current, Nothing)

referencedEntryIds
    :: [ToolCallStreamRef]
    -> RuntimeState
    -> Set.Set Int
referencedEntryIds refs current =
    Set.fromList
        [ entryId
        | ref <- refs
        , entryId <- maybe [] pure (Map.lookup ref current.aliases)
        ]

attachAliases
    :: Int
    -> [ToolCallStreamRef]
    -> RuntimeState
    -> RuntimeState
attachAliases entryId refs current =
    current
        { aliases =
            foldr
                (\ref -> Map.insert ref entryId)
                current.aliases
                refs
        }

removeByCallId
    :: ToolSpeculationRuntime
    -> Text
    -> IO (Maybe ActiveEntry)
removeByCallId runtime callId =
    modifyMVar runtime.state \current ->
        case Map.lookup (ToolCallStreamCall callId) current.aliases of
            Nothing -> pure (current, Nothing)
            Just entryId ->
                let (updated, entry) = removeEntry entryId current
                in pure (updated, entry)

discardEntry :: ToolSpeculationRuntime -> Int -> IO ()
discardEntry runtime entryId = do
    entry <- modifyMVar runtime.state \current ->
        let (updated, removed) = removeEntry entryId current
        in pure (updated, removed)
    mapM_ safeReleaseEntry entry

removeEntry
    :: Int
    -> RuntimeState
    -> (RuntimeState, Maybe ActiveEntry)
removeEntry entryId current =
    ( current
        { aliases = Map.filter (/= entryId) current.aliases
        , activeEntries = Map.delete entryId current.activeEntries
        }
    , Map.lookup entryId current.activeEntries
    )

releaseEntries :: [ActiveEntry] -> IO ()
releaseEntries = mapM_ safeReleaseEntry

safeReleaseEntry :: ActiveEntry -> IO ()
safeReleaseEntry = safeIO . releaseResource . (.resourceKey)

stopInterpreter :: Async a -> IO ()
stopInterpreter worker = do
    cancel worker
    void (waitCatch worker)

runEntryAction
    :: ToolSpeculationRuntime
    -> Int
    -> IO ()
    -> IO ()
runEntryAction runtime entryId action =
    tryAny action >>= \case
        Right () -> pure ()
        Left (_ :: SomeException) ->
            discardEntry runtime entryId

entryMatchesCall :: ActiveEntry -> ToolCall -> Bool
entryMatchesCall entry call =
    entry.toolName == canonicalToolName call.name
        && callIdsCompatible entry.entryCallId (nonEmpty call.callId)

callIdsCompatible :: Maybe Text -> Maybe Text -> Bool
callIdsCompatible (Just existing) (Just incoming) = existing == incoming
callIdsCompatible _ _ = True

entryMatchesName :: ActiveEntry -> Maybe Text -> Bool
entryMatchesName entry =
    maybe True
        (\name ->
            Text.null (Text.strip name)
                || entry.toolName == canonicalToolName name)

withCallRef
    :: Text
    -> [ToolCallStreamRef]
    -> [ToolCallStreamRef]
withCallRef callId refs =
    maybe refs (: refs) (ToolCallStreamCall <$> nonEmpty callId)

normalizeRefs :: [ToolCallStreamRef] -> [ToolCallStreamRef]
normalizeRefs = Set.toList . Set.fromList

nonEmpty :: Text -> Maybe Text
nonEmpty value
    | Text.null value = Nothing
    | otherwise = Just value

safeIO :: IO () -> IO ()
safeIO action = do
    _ <- tryAny action :: IO (Either SomeException ())
    pure ()
