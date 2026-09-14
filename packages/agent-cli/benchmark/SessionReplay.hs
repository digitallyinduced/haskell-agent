{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

-- Offline session replay: real conversation ownership, bounded root history,
-- complete drawApp, and retained Brick caches. No provider/network/MCP/DB
-- services are started. This is not total memory of a connected CLI process.
module Main (main) where

import Agent.CLI.AgentViewport (AgentEntry(..), AgentTarget(..))
import Agent.CLI.Interrupt (CtrlCDecision(..))
import Agent.CLI.Session.Types (SessionTurn(..), SessionTurnPage(..), TranscriptEffect(..), sessionTurnDecoder)
import Agent.CLI.Session.History (foldSessionItems)
import Agent.CLI.TUI.App
    ( drawApp, initialFullscreenAppState, newFullscreenInputBuffer
    , newFullscreenRuntimeWithSyntaxLoader, resetHistoryPage
    )
import Agent.CLI.TUI.History (HistoryDirection(..), HistoryGeneration(..), HistoryWindow(..))
import Agent.CLI.TUI.SessionHistory (sessionHistoryPage)
import Agent.CLI.TUI.Types (AppState(..), Name(..))
import Agent.CLI.TUI.Composer (handleComposerKey, applyComposerUiEvent)
import Agent.TUI.Model (reduceUi)
import Agent.Json (rawJsonFromEncoding)
import qualified Agent.Json.Decode as Decode
import Agent.Responses.Types
import Agent.Runtime.ConversationStore
    ( ConversationStore, TranscriptCheckpoint(..), newColdConversationStore
    , newConversationStore
    )
import Agent.TUI.Model (BlockKind(..), UiBlock(..), UiState(..), UiEvent(..), Focus(..), initialUiState)
import Agent.TUI.Motion (MotionMode(..))
import qualified Agent.TUI.Theme as Theme
import Brick (renderFinal)
import Brick.Types (RenderState)
import qualified Brick.Main as Brick
import Brick.Types (get)
import Control.Concurrent (newEmptyMVar, putMVar, takeMVar)
import qualified Control.Exception as E
import Control.Monad.IO.Class (liftIO)
import Control.DeepSeq (force)
-- safe-exceptions does not export the forcing primitive.
import Control.Exception (evaluate)
import Control.Exception.Safe (bracket)
import Control.Monad (forM, unless)
import Control.Monad.State.Strict (modify')
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as LBS
import Data.Foldable (toList)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List (foldl', sort, stripPrefix)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LT
import Data.Time (UTCTime(..), fromGregorian)
import Foreign.StablePtr (newStablePtr, freeStablePtr)
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Stats
import qualified Graphics.Vty as V
import Graphics.Vty.PictureToSpans (displayOpsForPic)
import Graphics.Vty.Span (SpanOp(..))
import System.CPUTime (getCPUTime)
import System.Environment (getArgs)
import System.Exit (die)
import System.Mem (performGC)

data Source = Fixture !Int !Int | Saved !FilePath
data Residency = Resident | Idle deriving (Eq, Show)
data Trace = Static | Resize | Interaction | Paste !Int | PasteMiddle !Int deriving (Eq, Show)
data Retained = Retained !AppState !(RenderState Name) !ConversationStore
data Sample = Sample !Double !Double !Integer !Integer !Integer

main :: IO ()
main = do
    enabled <- getRTSStatsEnabled
    unless enabled (die "enable RTS statistics with +RTS -N4 -T")
    rawArgs <- getArgs
    let (verify, args) = case rawArgs of
            "verify" : rest -> (True, rest)
            _ -> (False, rawArgs)
    (source, trace, residency, count) <- case args of
        ["fixture", trace, residency, turns, rows, samples] ->
            (,,,) <$> (Fixture <$> positive turns <*> positive rows)
                <*> parseTrace trace <*> parseResidency residency <*> positive samples
        ["file", trace, residency, path, samples] ->
            (,,,) (Saved path) <$> parseTrace trace <*> parseResidency residency <*> positive samples
        _ -> die "usage: session-replay-bench (fixture TRACE RESIDENCY TURNS BODY_LINES SAMPLES | file TRACE RESIDENCY JSONL SAMPLES); TRACE=static|resize|interaction|paste-BYTES|paste-middle-BYTES; RESIDENCY=resident|idle"
    samples <- forM [1 .. count] \_ -> do
        stateRef <- newIORef Nothing
        -- The IORef owns exactly the current complete state, not a list of
        -- historical frames. StablePtr keeps all ownership roots alive.
        bracket (newStablePtr stateRef) freeStablePtr \_ -> do
            setup <- measure do
                owned <- prepare source residency
                writeIORef stateRef (Just owned)
                renderFrame verify Static stateRef 0
            editing <- measure (editTrace verify trace stateRef)
            redraw <- measure $
                mapM_ (renderFrame verify trace stateRef) [0 .. 24]
            if verify then checkEdits verify trace stateRef else pure ()
            pure (setup, editing, redraw)
    putStrLn ("source=" <> sourceLabel source <> " trace=" <> show trace
        <> " residency=" <> show residency <> " samples=" <> show count)
    printSamples "setup_decode_project_first_render" [a | (a,_,_) <- samples]
    case trace of
        Paste _ -> printSamples "paste_200_edits_20_redraws" [b | (_,b,_) <- samples]
        PasteMiddle _ -> printSamples "paste_middle_200_edits_20_redraws" [b | (_,b,_) <- samples]
        _ -> pure ()
    printSamples "redraw_25_frames" [c | (_,_,c) <- samples]
  where
    positive raw = case reads raw of
        [(n, "")] | n > 0 -> pure n
        _ -> die "expected positive integer"
    parseTrace = \case
        "static" -> pure Static
        "resize" -> pure Resize
        "interaction" -> pure Interaction
        raw -> case stripPrefix "paste-middle-" raw of
            Just size -> PasteMiddle <$> positive size
            Nothing -> case stripPrefix "paste-" raw of
                Just size -> Paste <$> positive size
                Nothing -> die "unknown trace (expected static|resize|interaction|paste-BYTES|paste-middle-BYTES)"
    parseResidency = \case
        "resident" -> pure Resident
        "idle" -> pure Idle
        _ -> die "unknown residency"

sourceLabel :: Source -> String
sourceLabel (Fixture turns rows) = "synthetic:" <> show turns <> "x" <> show rows
sourceLabel (Saved _) = "saved-jsonl"

loadSource :: Source -> IO [SessionTurn]
loadSource (Fixture turns rows) = pure [fixtureTurn index rows | index <- [1 .. turns]]
loadSource (Saved path) = do
    bytes <- BS.readFile path
    let rows = filter (not . BSC.all (`elem` [' ', '\t', '\r'])) (BSC.lines bytes)
    unless (not (null rows)) (die "empty saved transcript")
    forM rows \row -> case Decode.decodeEither sessionTurnDecoder row of
        Left _ -> die "invalid saved transcript JSONL (content omitted)"
        Right turn -> pure turn

{-# NOINLINE prepare #-}
prepare :: Source -> Residency -> IO Retained
prepare source residency = do
    turns <- loadSource source
    -- Force the canonical transcript before timing rendering or allowing
    -- idle ownership to release it. The encoded bytes are not retained.
    let items = foldSessionItems turns
    _ <- evaluate (LBS.length (Aeson.encode items))
    conversation <- case residency of
        Resident -> newConversationStore Nothing items []
        Idle -> newColdConversationStore Nothing
            (TranscriptCheckpoint "offline replay"
                (foldSessionItems <$> loadSource source)) []
    input <- newFullscreenInputBuffer
    runtime <- newFullscreenRuntimeWithSyntaxLoader
        (pure (Left "disabled in offline replay")) input
        (pure ()) (const (pure ())) (pure WarnExit) (const (pure True))
        (const (pure ())) (const (pure ())) (pure (AgentRoot, [rootEntry]))
        (const (pure ())) (pure ()) (const (pure ())) MotionOff False initialUiState
    let base = initialFullscreenAppState runtime (map (.turnUserText) turns)
            AgentRoot [rootEntry] 0
        total = length turns
        generationStart = foldl' (\start (cursor, turn) ->
            if turn.turnEffect == TranscriptReset then cursor else start)
            0 (zip [0 ..] turns)
        pageStart = max generationStart (max 0 (total - 80))
        -- Same initial page size as Runtime.HistorySource. resetHistoryPage
        -- performs production block-ID remapping and bounded-window trimming.
        page = SessionTurnPage
            { pageTurns = zip [fromIntegral pageStart ..] (drop pageStart turns)
            , pageGenerationStart = fromIntegral generationStart
            , pageTotalTurns = fromIntegral total
            , pageHasOlder = pageStart > generationStart
            , pageHasNewer = False
            }
        app = resetHistoryPage
            (sessionHistoryPage (HistoryGeneration 0) HistoryNewer page) base
    -- Eagerly consume the projected history without retaining the temporary
    -- Show result or original SessionTurn list as an extra ownership root.
    _ <- evaluate (length (show app.appHistoryWindow.historyWindowTurns))
    _ <- evaluate (sum (map Text.length app.appHistory))
    pure (Retained app emptyRenderState conversation)

rootEntry :: AgentEntry
rootEntry = AgentEntry
    { agentTarget = AgentRoot, agentPath = "/root", agentStatus = "idle"
    , agentModel = Nothing, agentSteps = [], agentTranscript = []
    , agentConversation = initialUiState
    }

-- Verification prints exact attributed characters only when explicitly
-- requested. Do not use verify on private transcripts in public logs.
renderFrame :: Bool -> Trace -> IORef (Maybe Retained) -> Int -> IO ()
renderFrame verify trace ref frame = do
    Just (Retained previousApp previousRender conversation) <- readIORef ref
    let app = frameApp trace frame previousApp
        region = case trace of
            Resize -> [(100,32),(76,24),(120,40),(92,28)] !! (frame `mod` 4)
            _ -> (100,32)
        (next, picture, _, extents) = renderFinal Theme.terminalDefault
            (drawApp app) region (const Nothing) previousRender
        !score = force (pictureScore region picture + length extents)
    if verify
        then print (frame, region,
            [ concatMap spanCharacters (toList row)
            | row <- toList (displayOpsForPic picture region)
            ], show extents)
        else pure ()
    score `seq` writeIORef ref (Just (Retained app next conversation))
  where
    spanCharacters (TextSpan attr _ _ text) =
        [(attr, char) | char <- LT.unpack text]
    spanCharacters (Skip width) = replicate width (V.defAttr, ' ')
    spanCharacters (RowEnd width) = replicate width (V.defAttr, ' ')

frameApp :: Trace -> Int -> AppState -> AppState
frameApp trace frame state = state
    { appUi = state.appUi
        { uiElapsedMillis = frame
        , uiFollow = composerFocus
        , uiFocus = if composerFocus then FocusComposer else FocusScrollback
        }
    , appHistorySelectedBlock = if phase == 1 then chosen else Nothing
    , appHoveredControl = if phase == 3 || phase == 4 then copyName else Nothing
    , appPressedControl = if phase == 4 then copyName else Nothing
    }
  where
    composerFocus = case trace of
        Static -> True
        Paste _ -> True
        PasteMiddle _ -> True
        _ -> False
    phase = if trace == Interaction then frame `mod` 6 else 0
    candidates =
        [ block.blockId
        | block <- Map.elems state.appHistoryWindow.historyWindowBlocksById
        , block.blockKind == BlockAssistant
        ]
    chosen = case candidates of
        [] -> Nothing
        _ -> Just (candidates !! ((frame `div` 6) `mod` length candidates))
    copyName = (\ident -> CodeCopy AgentRoot ident 1) <$> chosen

-- Seed the post-paste draft without clipboard/path/image loaders, then call
-- the existing public composer key handler for every typing edit. No undo
-- representation appears here: identical harness source works before/after
-- the undo implementation change. The skipped empty pre-paste undo entry
-- would be evicted by these 200 subsequent edits in either implementation.
editTrace :: Bool -> Trace -> IORef (Maybe Retained) -> IO ()
editTrace verify trace ref = case trace of
    Paste bytes -> edit bytes bytes
    PasteMiddle bytes -> edit bytes (bytes `div` 2)
    _ -> pure ()
  where
    edit bytes cursor = do
        Just (Retained app rendered conversation) <- readIORef ref
        let text = pasteText bytes
            seeded = applyUiEvent (UiSetDraft text cursor) app
        writeIORef ref (Just (Retained seeded rendered conversation))
        mapM_ (\index -> do
            composerKey ref (V.EvKey (V.KChar (typedChar index)) [])
            if index `mod` 10 == 0 then renderFrame verify trace ref index else pure ())
            [1 .. 200 :: Int]

pasteText :: Int -> Text.Text
pasteText bytes = Text.take bytes (Text.replicate (bytes `div` 80 + 1)
    "A pasted diagnostic line with enough detail to review before sending to the model.\n")

typedChar :: Int -> Char
typedChar index = toEnum (97 + index `mod` 26)

-- Local composer events update the shared UI reducer before composer
-- metadata, matching the production app reducer for these draft events.
applyUiEvent :: UiEvent -> AppState -> AppState
applyUiEvent event state = applyComposerUiEvent event
    (state { appUi = reduceUi event state.appUi })

composerKey :: IORef (Maybe Retained) -> V.Event -> IO ()
composerKey ref event = do
        Just (Retained app rendered conversation) <- readIORef ref
        stopped <- newEmptyMVar
        let apply event' adjust = modify' (adjust . applyUiEvent event')
            action = handleComposerKey apply (pure WarnExit) (const (pure ())) event
            inert = V.Vty
                { V.update = const (die "unexpected terminal update")
                , V.nextEvent = E.throwIO E.ThreadKilled `E.finally` putMVar stopped ()
                , V.nextEventNonblocking = pure Nothing
                , V.inputIface = error "unexpected terminal input access"
                , V.outputIface = error "unexpected terminal output access"
                , V.refresh = pure ()
                , V.shutdown = pure ()
                , V.isShutdown = pure False
                }
            runner = Brick.App
                { Brick.appDraw = const []
                , Brick.appChooseCursor = const (const Nothing)
                , Brick.appHandleEvent = const Brick.halt
                , Brick.appStartEvent = do
                    action
                    next <- get
                    liftIO $ writeIORef ref (Just (Retained next rendered conversation))
                    liftIO $ E.throwIO ReplayFinished
                , Brick.appAttrMap = const Theme.terminalDefault
                }
        -- Brick hides its EventM runner. Its public startup hook provides an
        -- environment without a terminal. Stop before the rendering loop and
        -- wait for the inert input thread to terminate. This adapter overhead
        -- is identical in baseline and candidate, but is not user input IO.
        E.catch
            (Brick.customMainWithVty inert (pure inert) Nothing runner app >> pure ())
            (\ReplayFinished -> pure ())
        takeMVar stopped

data ReplayFinished = ReplayFinished deriving Show
instance E.Exception ReplayFinished

-- Outside all measured phases: validate actual editing and the retained
-- history without depending on its representation or printing draft data.
checkEdits :: Bool -> Trace -> IORef (Maybe Retained) -> IO ()
checkEdits verify trace ref = case trace of
    Paste bytes -> check bytes bytes
    PasteMiddle bytes -> check bytes (bytes `div` 2)
    _ -> pure ()
  where
    check bytes cursor = do
        Just (Retained app _ _) <- readIORef ref
        let original = pasteText bytes
            expected = Text.take cursor original
                <> Text.pack (map typedChar [1 .. 200])
                <> Text.drop cursor original
        unless (app.appUi.uiDraft == expected && length app.appUndo == 200)
            (die "composer replay edit/undo count mismatch (content omitted)")
        if not verify then pure () else do
            mapM_ (\_ -> composerKey ref (V.EvKey (V.KChar '_') [V.MCtrl]))
                [1 .. 200 :: Int]
            Just (Retained restored _ _) <- readIORef ref
            unless (restored.appUi.uiDraft == original
                    && restored.appUi.uiCursor == cursor
                    && null restored.appUndo)
                (die "composer replay undo restoration mismatch (content omitted)")

pictureScore :: V.DisplayRegion -> V.Picture -> Int
pictureScore region picture = sum
    [ case span of
        TextSpan attr width chars text ->
            length (show attr) + width + chars + fromIntegral (LT.length text)
        Skip width -> width
        RowEnd width -> width
    | row <- toList (displayOpsForPic picture region), span <- toList row
    ]

measure :: IO () -> IO Sample
measure action = do
    performGC
    before <- getRTSStats
    cpu <- getCPUTime
    wall <- getMonotonicTimeNSec
    action
    wallEnd <- getMonotonicTimeNSec
    cpuEnd <- getCPUTime
    performGC
    performGC
    after <- getRTSStats
    pure (Sample (fromIntegral (wallEnd-wall) / 1e6)
        (fromIntegral (cpuEnd-cpu) / 1e9)
        (fromIntegral (after.allocated_bytes-before.allocated_bytes))
        (fromIntegral after.gc.gcdetails_live_bytes)
        (fromIntegral after.gc.gcdetails_mem_in_use_bytes))

printSamples :: String -> [Sample] -> IO ()
printSamples label samples = putStrLn $ unwords
    [ label, "elapsed_ms=" <> show (middle [x | Sample x _ _ _ _ <- samples])
    , "cpu_ms=" <> show (middle [x | Sample _ x _ _ _ <- samples])
    , "allocated_bytes=" <> show (middle [x | Sample _ _ x _ _ <- samples])
    , "live_bytes=" <> show (middle [x | Sample _ _ _ x _ <- samples])
    , "rts_memory_in_use_bytes=" <> show (middle [x | Sample _ _ _ _ x <- samples])
    ]
  where middle values = sort values !! (length values `div` 2)

fixtureTurn :: Int -> Int -> SessionTurn
fixtureTurn index rows = SessionTurn
    { turnAt = UTCTime (fromGregorian 2026 9 12) 0
    , turnUserText = "Inspect the implementation and add regression coverage: " <> suffix
    , turnAssistantText = Just answer
    , turnError = Nothing, turnResponseId = Nothing
    , turnEffect = TranscriptAppend
    , turnItems =
        [ MessageItem ResponseMessage
            { messageId = Nothing, role = RoleUser
            , content = MessageContentText ("Review module " <> suffix)
            , status = Nothing, phase = Nothing, passthrough = Nothing
            }
        , FunctionCallItem FunctionCall
            { itemId = Nothing, callId = call, name = "read_file"
            , namespace = Nothing, provider = Nothing
            , arguments = "{\"target_file\":\"src/Main.hs\"}"
            , encryptedFunctionArgs = Nothing, status = Nothing, async = Nothing
            }
        , FunctionCallOutputItem FunctionCallOutput
            { localOutcome = Nothing, itemId = Nothing, callId = call
            , name = Just "read_file", namespace = Nothing, provider = Nothing
            , output = rawJsonFromEncoding (Aeson.toEncoding (Aeson.String toolBody))
            , status = Nothing, async = Nothing
            }
        , MessageItem ResponseMessage
            { messageId = Nothing, role = RoleAssistant
            , content = MessageContentText answer, status = Nothing
            , phase = Just "final_answer", passthrough = Nothing
            }
        ]
    , turnDisplayItems = [], turnUsage = Nothing, turnProviderTelemetry = []
    }
  where
    suffix = Text.pack (show index)
    call = "read-" <> suffix
    toolBody = Text.unlines ["line " <> Text.pack (show n) <> ": value_" <> suffix
        <> " = traverse validate inputs" | n <- [1 .. rows * 5]]
    answer = Text.unlines $
        ["## Review " <> suffix, "Unicode: Café 完了🙂. A **retained** coding response."]
        <> concat
            [ ["- Change " <> Text.pack (show n) <> " preserves behavior."
              , "```haskell", "result_" <> suffix <> " = traverse validate inputs", "```"]
            | n <- [1 .. rows]
            ]

emptyRenderState :: RenderState Name
emptyRenderState = read
    "RS {viewportMap = fromList [], rsScrollRequests = [], \
    \observedNames = fromList [], renderCache = fromList [], \
    \clickableNames = [], requestedVisibleNames_ = fromList [], \
    \reportedExtents = fromList []}"

instance Read Name where
    readsPrec _ _ = []
