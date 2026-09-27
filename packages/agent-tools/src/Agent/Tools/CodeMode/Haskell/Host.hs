{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
-- | Experimental GHCi code-mode process host. This is not a sandbox.
module Agent.Tools.CodeMode.Haskell.Host
    ( HaskellHost
    , newHaskellHost
    , prepareHaskellBindings
    , execHaskellCell
    , execHaskellCellWithRepair
    , execHaskellCellWithFilesAndRepair
    , HaskellRepairRequest(..)
    , HaskellRepairHandler
    , waitHaskellCell
    , terminateHaskellCell
    , readRunningHaskellCells
    , closeHaskellHost
    , haskellCellEnvironment
    , haskellCellGuidance
    ) where

import Agent.Process (terminateProcessGroup)
import Agent.ToolDispatch (ToolResultFile(..))
import Agent.Tools.CodeMode.Host.Types
    ( CodeModeError(..), CodeModeResult(..), CodeModeToolHandler, CodeModeFileToolHandler )
import Control.Concurrent.Async
    ( Async, asyncWithUnmask, cancel, concurrently_, link, poll, wait, waitCatch, withAsync )
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Exception.Safe
    ( bracket, bracketOnError, displayException, finally, mask_, onException, tryAny )
import Control.Monad (unless, void, when)
import Data.Aeson (Value(..), encode, eitherDecodeStrict', object, (.=))
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Lazy as LBS
import Data.IORef
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Text.Encoding.Error (lenientDecode)
import qualified Data.Text.IO as TextIO
import Data.Time.Clock (UTCTime, getCurrentTime)
import System.Directory (findExecutable, getTemporaryDirectory, removeFile, removePathForcibly)
import System.Environment (getEnvironment, lookupEnv)
import System.FilePath ((</>), takeFileName)
import System.IO
import qualified System.Posix.IO as Posix
import System.Posix.Temp (mkdtemp)
import System.Posix.Types (ProcessGroupID)
import System.Process

data HaskellHost = HaskellHost
    { hostDirectory :: !FilePath
    , hostExecutable :: !FilePath
    , hostSupportSource :: !Text
    , hostState :: !(MVar HostState)
    , hostStoredValues :: !(IORef (Map.Map Text Value))
    }

-- | A repair conversation receives compilation context, never session history
-- or tool execution authority. Attempts are numbered starting at one.
data HaskellRepairRequest = HaskellRepairRequest
    { repairOriginalSource :: !Text
    , repairCurrentSource :: !Text
    , repairDiagnostics :: !Text
    , repairBindings :: !Text
    , repairEnvironment :: !Text
    , repairAttempt :: !Int
    }
    deriving (Eq, Show)

type HaskellRepairHandler = HaskellRepairRequest -> IO (Maybe Text)

data HostState = HostState
    { stateClosed :: !Bool
    , stateNextIdentifier :: !Int
    , stateWorker :: !(Maybe Worker)
    , stateCells :: !(Map.Map Text HaskellCell)
    }

data Worker = Worker
    { workerInput :: !Handle
    , workerOutput :: !Handle
    , workerError :: !Handle
    , workerRequests :: !Handle
    , workerReplies :: !Handle
    , workerProcess :: !ProcessHandle
    , workerGroup :: !(Maybe ProcessGroupID)
    }

data HaskellCell = HaskellCell
    { haskellIdentifier :: !Text
    , haskellStarted :: !UTCTime
    , haskellTask :: !(Async ())
    , haskellOutcome :: !(TMVar (Either Text ()))
    , haskellOutput :: !(TVar [Value])
    , haskellObserver :: !(MVar ())
    }

newHaskellHost :: FilePath -> IO (Either Text HaskellHost)
newHaskellHost supportPath = do
    executable <- fromMaybe "ghci" <$> lookupEnv "HASKELL_AGENT_GHCI"
    findExecutable executable >>= \case
        Nothing -> pure $ Left
            "Haskell code mode requires GHCi with aeson, async and safe-exceptions. Run nix build .#code-mode-ghci and set HASKELL_AGENT_GHCI=result/bin/ghci."
        Just resolved -> do
            result <- tryAny $ do
                directory <- getTemporaryDirectory >>= \root ->
                    mkdtemp (root </> "haskell-code-mode-")
                flip onException (removePathForcibly directory) $ do
                    supportSource <- TextIO.readFile supportPath
                    state <- newMVar (HostState False 0 Nothing Map.empty)
                    stored <- newIORef Map.empty
                    let host = HaskellHost directory resolved supportSource state stored
                    probe <- execHaskellCell host "pure ()" "module Tools where\n"
                        (\_ _ -> pure (Left "no tools during startup")) 30000
                        `onException` closeHaskellHost host
                    case probe of
                        Right CodeModeFinished{} -> pure host
                        _ -> do
                            closeHaskellHost host
                            fail ("GHCi runtime readiness probe failed: " <> show probe)
            pure $ either (Left . Text.pack . displayException) Right result

-- | Validate a generated tool module before publishing its declarations.
-- Refresh while a cell is running fails without changing that cell's snapshot.
-- Each subsequent exec supplies its captured source again, so an unsuccessful
-- preparation cannot invalidate a previously published tool surface.
prepareHaskellBindings :: HaskellHost -> Text -> IO (Either Text ())
prepareHaskellBindings host bindings = do
    result <- execHaskellCell host "pure ()" bindings
        (\_ _ -> pure (Left "Tool calls are unavailable while preparing bindings.")) 30000
    case result of
        Right CodeModeFinished{} -> pure (Right ())
        Right (CodeModeRunning identifier _) -> do
            void (terminateHaskellCell host identifier)
            pure (Left "Preparing Haskell tool bindings exceeded the 30-second startup deadline.")
        _ -> pure (Left ("Unable to prepare Haskell tool bindings: " <> Text.pack (show result)))

execHaskellCell
    :: HaskellHost -> Text -> Text -> CodeModeToolHandler -> Int
    -> IO (Either CodeModeError CodeModeResult)
execHaskellCell = execHaskellCellWithRepair Nothing

execHaskellCellWithRepair
    :: Maybe HaskellRepairHandler
    -> HaskellHost -> Text -> Text -> CodeModeToolHandler -> Int
    -> IO (Either CodeModeError CodeModeResult)
execHaskellCellWithRepair repair host source bindings handler yieldMilliseconds
    = execHaskellCellWithFilesAndRepair repair host source bindings
        (\name arguments -> fmap (\value -> (value, [])) <$> handler name arguments)
        yieldMilliseconds

execHaskellCellWithFilesAndRepair
    :: Maybe HaskellRepairHandler
    -> HaskellHost -> Text -> Text -> CodeModeFileToolHandler -> Int
    -> IO (Either CodeModeError CodeModeResult)
execHaskellCellWithFilesAndRepair repair host source bindings handler yieldMilliseconds
    | BS.length (Text.encodeUtf8 source) > 1024 * 1024 =
        pure (Left (CodeModeResourceError "Haskell cell exceeds the 1 MiB source limit"))
    | otherwise = do
        -- Keep process/task ownership masked until both handles are recorded.
        allocation <- modifyMVarMasked host.hostState $ \state -> do
            active <- filterMCells (fmap isNothing . poll . (.haskellTask))
                (Map.elems state.stateCells)
            if state.stateClosed then
                pure (state, Left (CodeModeResourceError "Haskell code-mode host is closed"))
            else if not (null active) then
                pure (state, Left (CodeModeResourceError
                    "A Haskell cell is already running; use wait or terminate it before exec."))
            else if Map.size state.stateCells >= 64 then
                pure (state, Left (CodeModeResourceError
                    "Too many unobserved Haskell cells; use wait to consume completed results."))
            else do
                result <- tryAny $
                    bracketOnError (acquireWorker state) (stopWorker . fst) $ \(worker, restarted) -> do
                    let identifier = Text.pack (show state.stateNextIdentifier)
                    outcome <- newEmptyTMVarIO
                    output <- newTVarIO []
                    outputBytes <- newTVarIO 0
                    when restarted $ appendOutput output outputBytes $ textContent
                        "GHCi worker restarted. Explicit stored values were retained; previous effects were not replayed."
                    observer <- newMVar ()
                    started <- getCurrentTime
                    -- The session owns this asynchronous evaluator. Its handle is
                    -- retained below and cancelled/joined by terminate and close.
                    task <- asyncWithUnmask $ \unmask -> do
                        execution <- tryAny $ unmask $
                            evaluateCell repair host worker identifier source bindings handler
                                output outputBytes
                        case execution of
                            Left exception -> do
                                stopWorker worker
                                atomically $ void $ tryPutTMVar outcome
                                    (Left (Text.pack (displayException exception)))
                            Right value -> atomically $ void $ tryPutTMVar outcome value
                    let cell = HaskellCell identifier started task outcome output observer
                    pure (worker, cell)
                case result of
                    Left exception -> pure (state, Left (CodeModeStartupError
                        (Text.pack (displayException exception))))
                    Right (worker, cell) ->
                        pure (state
                            { stateWorker = Just worker
                            , stateNextIdentifier = state.stateNextIdentifier + 1
                            , stateCells = Map.insert cell.haskellIdentifier cell state.stateCells
                            }, Right cell.haskellIdentifier)
        case allocation of
            Left err -> pure (Left err)
            Right identifier -> waitHaskellCell host identifier yieldMilliseconds
  where
    isNothing Nothing = True
    isNothing _ = False
    acquireWorker :: HostState -> IO (Worker, Bool)
    acquireWorker state = case state.stateWorker of
        Nothing -> (, state.stateNextIdentifier > 0) <$> startWorker host
        Just previous -> getProcessExitCode previous.workerProcess >>= \case
            Nothing -> pure (previous, False)
            Just _ -> stopWorker previous >> ((, True) <$> startWorker host)

filterMCells :: (a -> IO Bool) -> [a] -> IO [a]
filterMCells predicate = foldr (\value rest -> do
    include <- predicate value
    remaining <- rest
    pure (if include then value : remaining else remaining)) (pure [])

waitHaskellCell :: HaskellHost -> Text -> Int -> IO (Either CodeModeError CodeModeResult)
waitHaskellCell host identifier milliseconds = do
    found <- withMVar host.hostState (pure . Map.lookup identifier . (.stateCells))
    case found of
        Nothing -> pure (Left (CodeModeUnknownCell identifier))
        Just cell -> tryTakeMVar cell.haskellObserver >>= \case
            Nothing -> pure (Left (CodeModeBusyObserver identifier))
            Just () -> flip finally (putMVar cell.haskellObserver ()) $ do
                timer <- registerDelay (max 0 (min 3600000 milliseconds) * 1000)
                result <- atomically $
                    (Just <$> readTMVar cell.haskellOutcome)
                    `orElse` (readTVar timer >>= check >> pure Nothing)
                contents <- drainOutput cell
                case result of
                    Nothing -> pure ()
                    Just _ -> do
                        void (waitCatch cell.haskellTask)
                        modifyMVar_ host.hostState $ \state ->
                            pure state { stateCells = Map.delete identifier state.stateCells }
                pure $ Right $ case result of
                    Nothing -> CodeModeRunning identifier contents
                    Just (Right ()) -> CodeModeFinished identifier contents
                    Just (Left message) -> CodeModeFailed identifier contents message

terminateHaskellCell :: HaskellHost -> Text -> IO (Either CodeModeError CodeModeResult)
terminateHaskellCell host identifier = modifyMVarMasked host.hostState $ \state ->
    case Map.lookup identifier state.stateCells of
        Nothing -> pure (state, Left (CodeModeUnknownCell identifier))
        Just cell -> do
            complete <- atomically (tryReadTMVar cell.haskellOutcome)
            case complete of
                Nothing -> do
                    -- Publish before interruptible teardown so a cancelled
                    -- terminator cannot leave a dead task with no outcome.
                    atomically $ void $ tryPutTMVar cell.haskellOutcome (Left "Haskell cell terminated")
                    mapM_ killWorker state.stateWorker
                    cancel cell.haskellTask
                    mapM_ stopWorker state.stateWorker
                    contents <- drainOutput cell
                    pure (state { stateWorker = Nothing
                        , stateCells = Map.delete identifier state.stateCells
                        }, Right (CodeModeTerminated identifier contents))
                Just result -> do
                    contents <- drainOutput cell
                    pure (state { stateCells = Map.delete identifier state.stateCells }
                        , Right $ either (CodeModeFailed identifier contents)
                        (const (CodeModeFinished identifier contents)) result)

readRunningHaskellCells :: HaskellHost -> IO [(Text, UTCTime)]
readRunningHaskellCells host = withMVar host.hostState $ \state -> do
    active <- filterMCells (atomically . isEmptyTMVar . (.haskellOutcome))
        (Map.elems state.stateCells)
    pure [(cell.haskellIdentifier, cell.haskellStarted) | cell <- active]

closeHaskellHost :: HaskellHost -> IO ()
closeHaskellHost host = modifyMVarMasked_ host.hostState $ \state -> do
    unless state.stateClosed $ do
        mapM_ (\cell -> atomically $ void $ tryPutTMVar cell.haskellOutcome
            (Left "Haskell code-mode host closed")) (Map.elems state.stateCells)
        mapM_ killWorker state.stateWorker
        mapM_ (cancel . (.haskellTask)) (Map.elems state.stateCells)
        mapM_ stopWorker state.stateWorker
        void $ tryAny $ removePathForcibly host.hostDirectory
    pure state { stateClosed = True, stateWorker = Nothing, stateCells = Map.empty }

drainOutput :: HaskellCell -> IO Value
drainOutput cell = atomically $ do
    values <- readTVar cell.haskellOutput
    writeTVar cell.haskellOutput []
    pure (object ["content" .= reverse values])

appendOutput :: TVar [Value] -> TVar Int -> Value -> IO ()
appendOutput output bytes value = atomically $ do
    used <- readTVar bytes
    let size = fromIntegral (LBS.length (encode value))
    when (used < 1024 * 1024) $ do
        writeTVar bytes (used + size)
        if used + size >= 1024 * 1024 then
            modifyTVar' output (textContent "[Haskell cell output truncated at 1 MiB]" :)
        else modifyTVar' output (value :)

textContent :: Text -> Value
textContent value = object ["type" .= ("text" :: Text), "text" .= value]

startWorker :: HaskellHost -> IO Worker
startWorker host = mask_ $
    bracket Posix.createPipe closeDescriptors $ \(requestRead, requestWrite) ->
    bracket Posix.createPipe closeDescriptors $ \(replyRead, replyWrite) ->
    bracketOnError (duplicateHandle requestRead) hClose $ \requestHandle ->
    bracketOnError (duplicateHandle replyWrite) hClose $ \replyHandle -> do
        Posix.setFdOption requestWrite Posix.CloseOnExec False
        Posix.setFdOption replyRead Posix.CloseOnExec False
        Posix.setFdOption requestRead Posix.CloseOnExec True
        Posix.setFdOption replyWrite Posix.CloseOnExec True
        environment <- getEnvironment
        runtimeOptions <- fromMaybe "-M1G" <$> lookupEnv "HASKELL_AGENT_GHCI_RTS"
        let overrides =
                [ ("HASKELL_AGENT_CODE_INPUT", show (fromIntegral replyRead :: Int))
                , ("HASKELL_AGENT_CODE_OUTPUT", show (fromIntegral requestWrite :: Int))
                , ("GHCRTS", runtimeOptions)
                ]
        (input, output, errors, process) <- createProcess
            (proc host.hostExecutable
                [ "-ignore-dot-ghci", "-v0", "-fno-defer-type-errors"
                , "-fno-defer-typed-holes", "-fno-defer-out-of-scope-variables"
                , "-i" <> host.hostDirectory
                ])
                { std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe
                , create_group = True, close_fds = False
                , env = Just (overrides <> filter (\(name, _) -> name `notElem` map fst overrides) environment)
                }
        group <- getPid process
        flip onException (do
            terminateProcessGroup group process
            mapM_ (mapM_ (void . tryAny . hClose)) [input, output, errors]) $ do
            case (input, output, errors) of
                (Just inputHandle, Just outputHandle, Just errorHandle) -> do
                    mapM_ (\handle -> hSetBinaryMode handle True >> hSetBuffering handle NoBuffering)
                        [inputHandle, outputHandle, errorHandle, requestHandle, replyHandle]
                    BS.hPut inputHandle ":set prompt \"\"\n:set prompt-cont \"\"\n:set -XOverloadedStrings\n"
                    pure (Worker inputHandle outputHandle errorHandle requestHandle replyHandle process group)
                _ -> fail "GHCi did not provide its standard stream pipes"
  where
    closeDescriptors (left, right) = mapM_ (void . tryAny . Posix.closeFd) [left, right]
    duplicateHandle descriptor =
        bracketOnError (Posix.dup descriptor) Posix.closeFd $ \copy -> do
            Posix.setFdOption copy Posix.CloseOnExec True
            Posix.fdToHandle copy

stopWorker :: Worker -> IO ()
stopWorker worker = do
    killWorker worker
    mapM_ (void . tryAny . hClose)
        [ worker.workerInput, worker.workerOutput, worker.workerError
        , worker.workerRequests, worker.workerReplies ]

killWorker :: Worker -> IO ()
killWorker worker =
    void $ tryAny $ terminateProcessGroup worker.workerGroup worker.workerProcess

evaluateCell
    :: Maybe HaskellRepairHandler
    -> HaskellHost -> Worker -> Text -> Text -> Text -> CodeModeFileToolHandler
    -> TVar [Value] -> TVar Int -> IO (Either Text ())
evaluateCell repair host worker identifier source bindings handler output bytes = do
    TextIO.writeFile (host.hostDirectory </> "Tools.hs") bindings
    TextIO.writeFile supportPath
        (Text.replace "__HASKELL_CODE_CELL__" identifier host.hostSupportSource)
    compileAttempt 0 source
  where
    supportPath = host.hostDirectory </> "CodeModeSupport.hs"
    marker = "__HASKELL_CODE_MODE_COMPLETE_"
        <> Text.pack (takeFileName host.hostDirectory) <> "_" <> identifier <> "__"
    send commands = do
        BS.hPut worker.workerInput (Text.encodeUtf8 (Text.unlines commands))
        hFlush worker.workerInput
    compileAttempt attempt currentSource = do
        let moduleName = "CodeModeCell" <> identifier <> "Attempt" <> Text.pack (show attempt)
            modulePath = host.hostDirectory </> Text.unpack moduleName <> ".hs"
            moduleSource = Text.unlines
                [ "{-# LANGUAGE OverloadedStrings, DuplicateRecordFields, OverloadedRecordDot #-}"
                , "{-# OPTIONS_GHC -Werror=missing-fields -fno-defer-type-errors -fno-defer-typed-holes -fno-defer-out-of-scope-variables -XNoTemplateHaskell -XNoQuasiQuotes #-}"
                , "module " <> moduleName <> " where"
                , haskellCellEnvironment
                , "cell ="
                , Text.unlines (map ("    " <>) (Text.lines currentSource))
                ]
        TextIO.writeFile modulePath moduleSource
        flip finally (void $ tryAny $ removeFile modulePath) $ do
            (compiled, messages) <- checkCompilation moduleName modulePath
            case compiled of
                True -> do
                    unless (Text.null messages) (appendOutput output bytes (textContent messages))
                    when (attempt > 0) $ appendOutput output bytes $ textContent
                        ("Haskell cell repaired before execution after "
                            <> Text.pack (show attempt) <> " attempt(s).")
                    executeCompiled moduleName
                False -> do
                    let failedCompilation = do
                            appendOutput output bytes (textContent messages)
                            pure (Left "Haskell cell did not execute; see compiler diagnostics above.")
                    case repair of
                        Just repairSource | attempt < 2 -> do
                            -- A broken generated module or runtime installation
                            -- is not an error in model-authored code. Verify the
                            -- same dependencies with a trusted inert declaration
                            -- before starting a repair conversation.
                            dependenciesValid <- validateDependencies
                            if not dependenciesValid then failedCompilation else do
                                repaired <- tryAny $ repairSource HaskellRepairRequest
                                    { repairOriginalSource = source
                                    , repairCurrentSource = currentSource
                                    , repairDiagnostics = messages
                                    , repairBindings = bindings
                                    , repairEnvironment = haskellCellEnvironment
                                        <> "\n" <> haskellCellGuidance
                                        <> "\nCodeModeSupport helper signatures:\n"
                                        <> Text.unlines (filter isPublicHelperSignature
                                            (Text.lines host.hostSupportSource))
                                    , repairAttempt = attempt + 1
                                    }
                                case repaired of
                                    Right (Just revised)
                                        | not (Text.null (Text.strip revised))
                                        , revised /= currentSource
                                        , BS.length (Text.encodeUtf8 revised) <= 1024 * 1024 ->
                                            compileAttempt (attempt + 1) revised
                                    _ -> failedCompilation
                        _ -> failedCompilation
    validateDependencies = do
        let moduleName = "CodeModeValidation" <> identifier
            modulePath = host.hostDirectory </> Text.unpack moduleName <> ".hs"
        TextIO.writeFile modulePath $ Text.unlines
            [ "module " <> moduleName <> " where"
            , haskellCellEnvironment
            , "cell = pure ()"
            ]
        flip finally (void $ tryAny $ removeFile modulePath) $
            fst <$> checkCompilation moduleName modulePath
    checkCompilation moduleName modulePath = do
        let compiledMarker = marker <> "_" <> moduleName <> "_COMPILED"
            boundary = marker <> "_" <> moduleName <> "_COMPILE_BOUNDARY"
        compiled <- newIORef False
        diagnostics <- newTVarIO ""
        let collectDiagnostic message
                | Text.stripEnd message == compiledMarker = writeIORef compiled True
                | otherwise = atomically $ modifyTVar' diagnostics
                    (\previous -> Text.take 65536 (previous <> message))
        -- This expression mentions the cell at its declared type without
        -- evaluating it. Compilation acknowledgement and execution are
        -- separate commands: a runtime failure can never enter repair.
        send
            [ ":load " <> Text.pack (show modulePath)
            , "import qualified Prelude as CodeModePrelude"
            , "import qualified System.IO as CodeModeIO"
            , "CodeModePrelude.const (CodeModeIO.hPutStrLn CodeModeIO.stdout "
                <> Text.pack (show compiledMarker) <> ") ("
                <> moduleName <> ".cell :: CodeModePrelude.IO ())"
            , "CodeModeIO.hPutStrLn CodeModeIO.stdout " <> Text.pack (show boundary)
                <> " CodeModePrelude.>> CodeModeIO.hFlush CodeModeIO.stdout"
            , "CodeModeIO.hPutStrLn CodeModeIO.stderr " <> Text.pack (show boundary)
                <> " CodeModePrelude.>> CodeModeIO.hFlush CodeModeIO.stderr"
            ]
        concurrently_
            (collectStream worker.workerOutput boundary collectDiagnostic)
            (collectStream worker.workerError boundary collectDiagnostic)
        (,) <$> readIORef compiled <*> readTVarIO diagnostics
    executeCompiled moduleName = do
        completion <- newIORef Nothing
        let collect handle = collectStream handle marker (appendOutput output bytes . textContent)
        -- Readers are scoped to a cell. A worker failure cancels every sibling.
        withAsync (serveRequests host worker identifier handler output bytes completion) $ \requests -> do
            send
                [ "CodeModeSupport.runCell " <> Text.pack (show identifier) <> " " <> moduleName <> ".cell"
                , ":load " <> Text.pack (show supportPath)
                , "CodeModeSupport.barrier " <> Text.pack (show identifier) <> " " <> Text.pack (show marker)
                ]
            concurrently_ (wait requests) $
                concurrently_ (collect worker.workerOutput) (collect worker.workerError)
        result <- readIORef completion
        pure $ fromMaybe (Left "Haskell cell did not execute; see compiler diagnostics above.") result

isPublicHelperSignature :: Text -> Bool
isPublicHelperSignature line = any (\name -> (name <> " ::") `Text.isPrefixOf` line)
    [ "callTool", "text", "json", "image", "generatedImage", "audio"
    , "store", "load", "decodeJson"
    ]

haskellCellEnvironment :: Text
haskellCellEnvironment = Text.unlines
    [ "import qualified Tools"
    , "import CodeModeSupport"
    , "import Control.Monad"
    , "import Control.Concurrent.Async (mapConcurrently, mapConcurrently_, forConcurrently, forConcurrently_, concurrently, concurrently_)"
    , "import Data.Aeson"
    , "import qualified Data.Aeson.KeyMap as KeyMap"
    , "import qualified Data.Aeson.Key as Key"
    , "import qualified Data.Aeson.Types as AesonTypes"
    , "import qualified Data.Vector as Vector"
    , "import qualified Data.Map.Strict as Map"
    , "import qualified Data.Scientific as Scientific"
    , "import qualified Data.List as List"
    , "import Data.Text (Text)"
    , "import qualified Data.Text as Text"
    , "cell :: IO ()"
    ]

-- Shared by the main tool description and compiler-repair context.
haskellCellGuidance :: Text
haskellCellGuidance = Text.unlines
    [ "Generated records use NoFieldSelectors: read fields with record.field (OverloadedRecordDot), not Tools.field record. Construct records with Tools.Constructor { Tools.field = value }."
    , "For JSON, use decodeJson with an explicit result type, then traverse Value directly. Never parse show Value: it is Haskell debugging output, not JSON."
    , "Object contains a KeyMap Value; use KeyMap.lookup, with Key.fromText for dynamic Text keys. Array contains a Vector Value; use Vector.toList. Number contains Scientific; use Scientific.toBoundedInteger with an explicit integral type when needed."
    , "For typed parsing use AesonTypes.parseEither with withObject and (.:); Map.fromList and List.sortOn are available for indexing and sorting."
    , "Small JSON traversal example (selects the string names from an items array):"
    , "```haskell"
    , "do"
    , "  let raw = String \"{\\\"items\\\":[{\\\"name\\\":\\\"tea\\\"},{\\\"name\\\":\\\"rice\\\"}]}\""
    , "  value <- either (fail . Text.unpack) pure (decodeJson raw :: Either Text Value)"
    , "  case value of"
    , "    Object root | Just (Array items) <- KeyMap.lookup \"items\" root ->"
    , "      json (toJSON [name | Object item <- Vector.toList items, Just (String name) <- [KeyMap.lookup \"name\" item]])"
    , "    _ -> fail \"expected an object with an items array\""
    , "```"
    ]

collectStream :: Handle -> Text -> (Text -> IO ()) -> IO ()
collectStream handle marker emit = loop BS.empty
  where
    encodedMarker = Text.encodeUtf8 marker
    loop retained = do
        chunk <- BS.hGetSome handle 4096
        when (BS.null chunk) (fail "GHCi process exited before the cell completed")
        consume (retained <> chunk)
    consume bytes = case BS.elemIndex 10 bytes of
        Just position -> do
            let (line, suffix) = BS.splitAt position bytes
                rest = BS.drop 1 suffix
            if line == encodedMarker then pure ()
            else do
                unless (BS.null line) (emit (Text.decodeUtf8With lenientDecode line <> "\n"))
                consume rest
        Nothing
            | BS.length bytes > 8192 -> do
                emit (Text.decodeUtf8With lenientDecode (BS.take 4096 bytes))
                loop (BS.drop 4096 bytes)
            | otherwise -> loop bytes

serveRequests
    :: HaskellHost -> Worker -> Text -> CodeModeFileToolHandler
    -> TVar [Value] -> TVar Int -> IORef (Maybe (Either Text ())) -> IO ()
serveRequests host worker identifier handler output bytes completion = do
    retained <- newIORef BS.empty
    writer <- newMVar True
    fileBudget <- newIORef (0 :: Int, 0 :: Int)
    loop fileBudget retained writer
  where
    loop fileBudget retained writer = do
        line <- readControlLine worker.workerRequests retained
        value <- either fail pure (eitherDecodeStrict' line)
        (scope, requestId, method, arguments) <- case value of
            Object fields
                | Just (String cell) <- KeyMap.lookup "cell" fields, cell == identifier
                , Just scope@(String _) <- KeyMap.lookup "scope" fields
                , Just requestId@(Number _) <- KeyMap.lookup "id" fields
                , Just (String method) <- KeyMap.lookup "method" fields
                , Just arguments <- KeyMap.lookup "arguments" fields -> pure (scope, requestId, method, arguments)
            _ -> fail "invalid GHCi control message"
        case (method, arguments) of
            ("tool", Object fields)
                | Just (String name) <- KeyMap.lookup "name" fields
                , Just arguments' <- KeyMap.lookup "arguments" fields ->
                    -- Every callback is scoped to this cell's request loop.
                    withAsync (invoke fileBudget name arguments' >>= respond writer scope requestId method) $ \task ->
                        link task >> loop fileBudget retained writer
            _ -> do
                (continue, result) <- dispatch method arguments
                respond writer scope requestId method result
                when continue (loop fileBudget retained writer)
    invoke fileBudget name arguments = do
        result <- handler name arguments
        case result of
            Left message -> pure (Left message)
            Right (value, files) -> do
                accepted <- atomicModifyIORef' fileBudget \(count, size) ->
                    let nextCount = count + length files
                        nextSize = size + sum (map (BS.length . (.fileData)) files)
                    in if nextCount <= 8 && nextSize <= 20 * 1024 * 1024
                        then ((nextCount, nextSize), True)
                        else ((count, size), False)
                if accepted
                    then do
                        -- Attachments stay host-side and have their own byte
                        -- budget, independent of clipped textual cell output.
                        atomically $ modifyTVar' output
                            (\values -> reverse (map nativeFilePart files) <> values)
                        pure (Right value)
                    else pure (Left "native file output exceeds the cell limit (8 files, 20 MiB)")
    dispatch method arguments = case (method, arguments) of
            ("content", content) ->
                appendOutput output bytes (sanitizeWorkerContent content) >> pure (True, Right Null)
            ("store", Object fields)
                | Just (String key) <- KeyMap.lookup "key" fields
                , Just stored <- KeyMap.lookup "value" fields -> do
                    atomicModifyIORef' host.hostStoredValues (\values -> (Map.insert key stored values, ()))
                    pure (True, Right Null)
            ("load", String key) -> do
                values <- readIORef host.hostStoredValues
                pure (True, maybe (Left ("No stored value named " <> key)) Right (Map.lookup key values))
            ("completed", Object fields)
                | Just (String cell) <- KeyMap.lookup "cell" fields, cell == identifier
                , Just errorValue <- KeyMap.lookup "error" fields -> do
                    result <- case errorValue of
                        Null -> pure (Right ())
                        String message -> pure (Left message)
                        _ -> fail "invalid GHCi completion"
                    writeIORef completion (Just result)
                    pure (True, Right Null)
            ("barrier", String cell) | cell == identifier ->
                pure (False, Right Null)
            _ -> fail "unexpected or stale GHCi control message"
    respond writer scope requestId method result = modifyMVar_ writer $ \callbacksOpen ->
        flip onException (killWorker worker) $ do
            -- Completion closes callback replies under the write lock. Thus the
            -- run reader cannot consume half a late reply while being cancelled
            -- before the separate barrier reader starts.
            when (callbacksOpen || method == "barrier") $ do
                let response = either (\message -> object ["scope" .= scope, "id" .= requestId, "error" .= message])
                        (\result' -> object ["scope" .= scope, "id" .= requestId, "result" .= result']) result
                LBS.hPutStr worker.workerReplies (encode response)
                BS.hPut worker.workerReplies "\n"
                hFlush worker.workerReplies
            pure (callbacksOpen && method /= "completed")

nativeFilePart :: ToolResultFile -> Value
nativeFilePart file = object
    [ "type" .= ("native_file" :: Text)
    , "filename" .= file.fileName
    , "mime_type" .= file.fileMimeType
    , "data" .= Text.decodeUtf8 (Base64.encode file.fileData)
    ]

sanitizeWorkerContent :: Value -> Value
sanitizeWorkerContent (Object fields)
    | KeyMap.lookup "type" fields == Just (String "native_file") =
        object ["type" .= ("text" :: Text), "text" .= ("[worker-supplied native file rejected]" :: Text)]
    | otherwise = Object (fmap sanitizeWorkerContent fields)
sanitizeWorkerContent (Array values) = Array (fmap sanitizeWorkerContent values)
sanitizeWorkerContent value = value

-- Preserve coalesced frames while bounding each message before decoding.
readControlLine :: Handle -> IORef BS.ByteString -> IO BS.ByteString
readControlLine handle retained = readIORef retained >>= loop [] 0
  where
    loop chunks size chunk =
        case BS.elemIndex 10 chunk of
            Nothing -> do
                let total = size + BS.length chunk
                when (total > 8 * 1024 * 1024) (fail "GHCi control message exceeds 8 MiB")
                next <- BS.hGetSome handle 4096
                when (BS.null next) (fail "GHCi control pipe closed")
                loop (chunk : chunks) total next
            Just position -> do
                when (size + position > 8 * 1024 * 1024) (fail "GHCi control message exceeds 8 MiB")
                writeIORef retained (BS.drop (position + 1) chunk)
                pure $ BS.concat (reverse (BS.take position chunk : chunks))
