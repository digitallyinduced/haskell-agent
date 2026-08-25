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

import Agent.ToolDispatch
    ( ActiveToolSpeculation(..)
    , ToolArgumentStreamEvent(..)
    , ToolArgumentUpdate(..)
    , ToolCall(..)
    , ToolCallStreamRef(..)
    , ToolSpeculator(..)
    , ToolSpeculatorSession(..)
    , canonicalToolName
    , functionToolCall
    )
import Agent.Tools.Types (AppTool(..))
import Control.Applicative ((<|>))
import Control.Concurrent.MVar
    ( MVar
    , modifyMVar
    , newMVar
    , readMVar
    )
import Control.Exception.Safe
    ( SomeException
    , finally
    , mask
    , onException
    , tryAny
    )
import Control.Monad (foldM, forM_, guard, void)
import Data.IORef
    ( atomicModifyIORef'
    , newIORef
    , readIORef
    )
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text

-- | Session-scoped router for tool-owned speculative argument parsers.
--
-- Provider adapters feed this runtime a small provider-neutral event stream.
-- It owns transport-call correlation and lifecycle cleanup; each registered
-- tool owns only the parser and speculative work for one call.
data ToolSpeculationRuntime = ToolSpeculationRuntime
    { sessions :: !(Map.Map Text ToolSpeculatorSession)
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
    , callId :: !(Maybe Text)
    , finalized :: !Bool
    , speculation :: !ActiveToolSpeculation
    }

newToolSpeculationRuntime
    :: [AppTool]
    -> IO ToolSpeculationRuntime
newToolSpeculationRuntime tools = mask \restore -> do
    openedRef <- newIORef ([] :: [ToolSpeculatorSession])
    let cleanup =
            readIORef openedRef
                >>= mapM_
                    (\session ->
                        safeIO session.closeToolSpeculatorSession)
        openOne
            :: Map.Map Text ToolSpeculatorSession
            -> AppTool
            -> IO (Map.Map Text ToolSpeculatorSession)
        openOne current tool =
            case tool.appToolSpeculator of
                Nothing -> pure current
                Just factory
                    | Map.member name current -> pure current
                    | otherwise ->
                    tryAny (restore factory.openToolSpeculator) >>= \case
                        Left (_ :: SomeException) -> pure current
                        Right session -> do
                            atomicModifyIORef' openedRef \opened ->
                                (session : opened, ())
                            pure (Map.insert name session current)
                  where
                    name = canonicalToolName tool.appToolName
    sessions <-
        restore (foldM openOne Map.empty tools)
            `onException` cleanup
    state <- newMVar RuntimeState
        { closed = False
        , nextEntryId = 0
        , aliases = Map.empty
        , activeEntries = Map.empty
        }
    pure ToolSpeculationRuntime { sessions, state }

closeToolSpeculationRuntime :: ToolSpeculationRuntime -> IO ()
closeToolSpeculationRuntime runtime = mask \restore -> do
    (wasOpen, entries) <-
        modifyMVar runtime.state \current ->
            if current.closed
                then pure (current, (False, []))
                else pure
                    ( current
                        { closed = True
                        , aliases = Map.empty
                        , activeEntries = Map.empty
                        }
                    , (True, Map.elems current.activeEntries)
                    )
    if wasOpen
        then
            restore (cancelEntries entries)
                `finally`
                    forM_ (Map.elems runtime.sessions) \session ->
                        restore (safeIO session.closeToolSpeculatorSession)
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
    cancelEntries entries

-- | Best-effort provider-neutral stream observation. Synchronous failures in
-- speculative code are contained and leave ordinary tool execution intact.
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
                    void $
                        ensureActive
                            runtime
                            call
                            (withCallRef
                                argumentStreamCallId
                                argumentStreamRefs)
        ToolArgumentsDelta
            { argumentStreamRefs
            , argumentStreamDelta
            } ->
                lookupActive runtime argumentStreamRefs Nothing >>= mapM_
                    (\(entryId, entry) ->
                        runEntryAction runtime entryId $
                            entry.speculation.updateToolArguments
                                (ToolArgumentDeltaUpdate argumentStreamDelta))
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
                forM_ active \(entryId, entry) ->
                    runEntryAction runtime entryId $
                        entry.speculation.updateToolArguments
                            (ToolArgumentDoneUpdate argumentStreamArguments)
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
                forM_ active \(entryId, _) ->
                    finalizeEntry runtime entryId argumentStreamCall

-- | Keep only calls present in the authoritative completed response and
-- finalize each retained parser with the complete arguments.
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
                                    finalCallId <- entry.callId
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
    cancelEntries abandoned
    forM_ retained \(entryId, entry, call) ->
        runEntryAction runtime entryId $
            entry.speculation.finalizeToolSpeculation call

-- | Consume one finalized speculative result after normal approval and
-- scheduling. Any failure becomes a miss so the ordinary handler can run.
takeToolSpeculation
    :: ToolSpeculationRuntime
    -> ToolCall
    -> IO (Maybe (Either Text Text))
takeToolSpeculation runtime call = do
    selected <- removeByCallId runtime call.callId
    case selected of
        Nothing -> pure Nothing
        Just entry -> do
            result <-
                tryAny
                    (entry.speculation.takeToolSpeculatedResult call)
                    :: IO
                        (Either
                            SomeException
                            (Maybe (Either Text Text)))
            closeEntry entry
            pure (either (const Nothing) id result)

discardToolSpeculation
    :: ToolSpeculationRuntime
    -> ToolCall
    -> IO ()
discardToolSpeculation runtime call =
    removeByCallId runtime call.callId
        >>= mapM_ closeEntry

waitForToolSpeculation :: ToolSpeculationRuntime -> IO ()
waitForToolSpeculation runtime = do
    entries <-
        Map.elems . (.activeEntries)
            <$> readMVar runtime.state
    forM_ entries \entry ->
        safeIO entry.speculation.waitActiveToolSpeculation

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
                                            nonEmpty call.callId <|> entry.callId
                                        updatedEntry =
                                            ActiveEntry
                                                { toolName = entry.toolName
                                                , callId
                                                , finalized = entry.finalized
                                                , speculation =
                                                    entry.speculation
                                                }
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
                                    runtime.sessions
                            of
                                Nothing -> pure (current, Nothing)
                                Just session -> do
                                    started <-
                                        tryAny
                                            (session.startToolSpeculation call)
                                    case started of
                                        Left (_ :: SomeException) ->
                                            pure (current, Nothing)
                                        Right active -> do
                                            let entryId = current.nextEntryId
                                                entry = ActiveEntry
                                                    { toolName =
                                                        canonicalToolName
                                                            call.name
                                                    , callId =
                                                        nonEmpty call.callId
                                                    , finalized = False
                                                    , speculation = active
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
    mapM_ closeEntry entry

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

cancelEntries :: [ActiveEntry] -> IO ()
cancelEntries = mapM_ closeEntry

closeEntry :: ActiveEntry -> IO ()
closeEntry entry = do
    safeIO entry.speculation.cancelActiveToolSpeculation
    safeIO entry.speculation.waitActiveToolSpeculation

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

finalizeEntry
    :: ToolSpeculationRuntime
    -> Int
    -> ToolCall
    -> IO ()
finalizeEntry runtime entryId call = do
    selected <-
        modifyMVar runtime.state \current ->
            case Map.lookup entryId current.activeEntries of
                Just entry
                    | entryMatchesCall entry call
                    , not entry.finalized ->
                        let updatedEntry =
                                entry
                                    { callId =
                                        nonEmpty call.callId <|> entry.callId
                                    , finalized = True
                                    }
                        in pure
                            ( current
                                { activeEntries =
                                    Map.insert
                                        entryId
                                        updatedEntry
                                        current.activeEntries
                                }
                            , Just updatedEntry
                            )
                _ -> pure (current, Nothing)
    forM_ selected \entry ->
        runEntryAction runtime entryId $
            entry.speculation.finalizeToolSpeculation call

entryMatchesCall :: ActiveEntry -> ToolCall -> Bool
entryMatchesCall entry call =
    entry.toolName == canonicalToolName call.name
        && callIdsCompatible entry.callId (nonEmpty call.callId)

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
