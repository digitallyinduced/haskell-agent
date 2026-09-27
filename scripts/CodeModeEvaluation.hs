{-# LANGUAGE OverloadedStrings, OverloadedRecordDot, NoFieldSelectors, DuplicateRecordFields, BlockArguments, LambdaCase #-}
module CodeModeEvaluation
    (runEvaluation, runPreflight, runFulfillmentEvaluation, runFulfillmentPreflight) where

import qualified CodeModeFulfillment as Fulfillment
import Agent.Accounts.Auth (loadAuth, LoadedAuth(..))
import Agent.CLI.Tools (schemasFromAppTools)
import Agent.CLI.CodeModeRuntime (codeModeBackendInstructions, codeModeRepairHandlerWithUsage)
import Agent.Dialect (codexDialect)
import Agent.Loop
import Agent.OpenAI.LoopBackend (openAiBackendReconnecting)
import Agent.OpenAI.WebSocketClient (withCodexWsRetrying)
import Agent.Provider (Provider(..), TokenProvider)
import Agent.Runtime.ProviderRequest (requestParams)
import Agent.ToolArgs (objectArgsExact, reqText)
import Agent.ToolDSL (PropertySchema(..), PropertyType(..))
import Agent.ToolDispatch
import Agent.Tools.CodeMode.Backend
import Agent.Tools.CodeMode.Host (codeModeWorkerPath, ImageDetailVisibility(..))
import Agent.Tools.CodeMode.Tool
import Agent.Tools.Types
import Control.Exception.Safe (bracket, tryAny, displayException, finally)
import Control.Concurrent.Async (race)
import Control.Concurrent.MVar
import Control.Monad (forM, forM_, unless)
import Data.Aeson (Value(..), encode, object, (.=), toJSON, eitherDecode)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as BS
import Data.IORef
import Data.List (sort)
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as TextIO
import GHC.Clock (getMonotonicTimeNSec)
import System.Directory
import System.FilePath
import System.Timeout (timeout)
import Text.Printf (printf)
import Text.Read (readMaybe)

-- Behavioral evaluation, not an optimized runtime microbenchmark. Fixtures
-- reproduce the read-only portions of eval/GhciVsBash.hs.
runEvaluation :: FilePath -> Int -> IO ()
runEvaluation = runEvaluationTasks [0..3]

runFulfillmentEvaluation :: FilePath -> Int -> IO ()
runFulfillmentEvaluation = runEvaluationTasks [0..4]

runEvaluationTasks :: [Int] -> FilePath -> Int -> IO ()
runEvaluationTasks tasks destination trials = do
    createDirectoryIfMissing True destination
    existing <- listDirectory destination
    unless (null existing) (fail "evaluation destination must be empty")
    auth <- loadAuth (Just OpenAIProvider) >>= either (fail . Text.unpack) pure
    let schedule = [(trial, task, language) | trial <- [1..trials], task <- tasks,
            language <- if even (trial + task)
                then [HaskellBackend, JavaScriptBackend]
                else [JavaScriptBackend, HaskellBackend]]
    rows <- forM schedule \(trial, task, language) ->
        runSample destination auth.loadedTokenProvider trial task language
    LBS.writeFile (destination </> "results.json") (encode rows)

runSample :: FilePath -> TokenProvider -> Int -> Int -> CodeModeBackend -> IO Value
runSample destination provider trial task language = do
    let label = show trial <> "-" <> show task <> "-" <> show language
        directory = destination </> label
        fixture = directory </> "fixture"
    createDirectoryIfMissing True fixture
    prepareFixture fixture trial
    fulfillment <- if task == 4 then Just <$> Fulfillment.newFulfillmentFixture trial else pure Nothing
    let nestedTools = maybe (fixtureTools fixture trial) Fulfillment.fulfillmentTools fulfillment
        prompt = if task == 4 then Fulfillment.fulfillmentPrompt else taskPrompt task
        expected = if task == 4 then Fulfillment.fulfillmentExpected trial else taskExpected task trial
        name = if task == 4 then "order-fulfillment" else taskName task
    beforeFixture <- fixtureSnapshot fixture
    LBS.writeFile (directory </> "fixture-before.json") (encode beforeFixture)
    trace <- newIORef ([] :: [Value])
    usage <- newIORef emptyTokenUsage
    steps <- newIORef (0 :: Int)
    calls <- newIORef (0 :: Int)
    nestedCalls <- newIORef (0 :: Int)
    repairUsage <- newIORef emptyTokenUsage
    repairAttempts <- newIORef (0 :: Int)
    repairResponses <- newIORef (0 :: Int)
    repairSeconds <- newIORef (0 :: Double)
    limitReached <- newEmptyMVar
    let record value = atomicModifyIORef' trace (\events -> (value : events, ()))
        invoke tool call = do
            result <- budgetedInvoke 128 nestedCalls limitReached tool call
            record (object ["kind" .= ("nested" :: Text), "name" .= call.name
                , "arguments" .= call.arguments
                , "structured" .= either (const False) (isJust . toolCallResultStructured) result])
            pure result
    worker <- codeModeWorkerPath
    started <- getMonotonicTimeNSec
    outcome <- tryAny $ timeout 180000000 $ withCodexWsRetrying provider \connection credential -> do
      healthy <- newIORef True
      let factory params = openAiBackendReconnecting provider credential healthy connection (pure params)
          observeRepair tokens = do
              modifyIORef' repairUsage (`addTokenUsage` tokens)
              modifyIORef' repairResponses (+1)
          repair request = do
              modifyIORef' repairAttempts (+1)
              before <- getMonotonicTimeNSec
              revised <- (do
                  result <- withCodexWsRetrying provider \repairConnection repairCredential -> do
                      repairHealthy <- newIORef True
                      let repairFactory params = openAiBackendReconnecting provider repairCredential
                              repairHealthy repairConnection (pure params)
                      Right <$> codeModeRepairHandlerWithUsage observeRepair repairFactory request
                  either (fail . show) pure result) `finally` do
                  after <- getMonotonicTimeNSec
                  modifyIORef' repairSeconds (+ (fromIntegral (after-before) / 1e9))
              record (object ["kind" .= ("repair" :: Text), "revised" .= revised])
              pure revised
      bracket
        (newCodeModeToolSetWithRepair (Just repair) language CodeOnlyToolMode ImageDetailVisible
            worker invoke [CodeModeNestedSpec tool Nothing | tool <- nestedTools]
            >>= either (fail . Text.unpack) pure)
        (.closeCodeModeToolSet) \toolSet -> do
            let instructions = (if task == 4
                    then "Complete the simulated order fulfillment task using exec and its nested tools. "
                    else "Complete read-only data analysis tasks using exec and its nested tools. ")
                    <> "Use the provided nested tools for all filesystem access; do not access files, processes, or networks directly. "
                    <> "Perform calculations in exec, not mentally. Return only the exact requested plain text, no Markdown. "
                    <> codeModeBackendInstructions language
                refresh = do
                    tools <- toolSet.codeModeRefreshToolSet
                        [CodeModeNestedSpec tool Nothing | tool <- nestedTools]
                        >>= either (fail . Text.unpack) pure
                    let params = requestParams OpenAIProvider "gpt-6-sol" instructions
                            (schemasFromAppTools codexDialect tools) "low"
                    pure (factory params, tools)
            result <- race (readMVar limitReached) $
                evaluateTurns refresh record usage steps calls
                    emptyBackendSnapshot Nothing [UserMessage prompt] 0
            pure (Right (either (const (Left "nested callback budget exhausted")) id result))
    ended <- getMonotonicTimeNSec
    afterFixture <- tryAny (fixtureSnapshot fixture)
    let fixtureUnchanged = either (const False) (== beforeFixture) afterFixture
    LBS.writeFile (directory </> "fixture-after.json")
        (encode (either (String . Text.pack . displayException) id afterFixture))
    observedUsage <- readIORef usage
    observedSteps <- readIORef steps
    observedCalls <- readIORef calls
    observedNested <- readIORef nestedCalls
    observedRepair <- readIORef repairUsage
    observedAttempts <- readIORef repairAttempts
    observedResponses <- readIORef repairResponses
    observedRepairSeconds <- readIORef repairSeconds
    observedTrace <- reverse <$> readIORef trace
    audit <- traverse Fulfillment.fulfillmentSnapshot fulfillment
    auditPassed <- maybe (pure True) Fulfillment.fulfillmentPassed fulfillment
    let (status, answer) = case outcome of
            Left err -> ("exception", Text.pack (displayException err))
            Right Nothing -> ("timeout", "")
            Right (Just (Left err)) -> ("transport-error", Text.pack (show err))
            Right (Just (Right (Left err))) -> ("incomplete", err)
            Right (Just (Right (Right value))) -> ("completed", value)
        passed = status == "completed" && Text.strip answer == expected
            && observedCalls > 0 && observedNested > 0 && fixtureUnchanged && auditPassed
        row = object
            [ "trial" .= trial, "task" .= name, "backend" .= show language
            , "model" .= ("gpt-6-sol" :: Text), "effort" .= ("low" :: Text)
            , "seconds" .= (fromIntegral (ended-started) / 1e9 :: Double)
            , "status" .= (status :: Text), "passed" .= passed, "answer" .= answer
            , "inputTokens" .= observedUsage.inputTokens
            , "outputTokens" .= observedUsage.outputTokens
            , "cachedTokens" .= observedUsage.cachedTokens
            , "modelTurns" .= observedSteps, "execCalls" .= observedCalls
            , "nestedCalls" .= observedNested
            , "repairAttempts" .= observedAttempts
            , "repairResponses" .= observedResponses
            , "repairSeconds" .= observedRepairSeconds
            , "repairInputTokens" .= (if observedAttempts == observedResponses then Just observedRepair.inputTokens else Nothing)
            , "repairOutputTokens" .= (if observedAttempts == observedResponses then Just observedRepair.outputTokens else Nothing)
            , "repairCachedTokens" .= observedRepair.cachedTokens
            , "incompleteUsage" .= (observedAttempts /= observedResponses ||
                case outcome of Right (Just (Right (Right _))) -> False; _ -> True)
            , "expected" .= expected
            , "fixtureUnchanged" .= fixtureUnchanged
            , "fulfillmentAudit" .= audit
            ]
    LBS.writeFile (directory </> "trace.json") (encode observedTrace)
    LBS.writeFile (directory </> "result.json") (encode row)
    LBS.putStr (encode row <> "\n")
    pure row

-- Exact byte snapshots are small for these fixtures. Reject links instead of
-- following them; this detects final-state mutations, not transient writes.
fixtureSnapshot :: FilePath -> IO Value
fixtureSnapshot root = toJSON <$> walk ""
  where
    walk relative = do
        names <- sort <$> listDirectory (root </> relative)
        fmap concat $ forM names \name -> do
            let path = relative </> name
                absolute = root </> path
            link <- pathIsSymbolicLink absolute
            whenLink link
            directory <- doesDirectoryExist absolute
            if directory
                then (object ["path" .= path, "directory" .= True] :) <$> walk path
                else do
                    bytes <- BS.readFile absolute
                    pure [object ["path" .= path, "bytes" .= BS.unpack bytes]]
    whenLink True = fail "fixture contains symbolic link"
    whenLink False = pure ()

budgetedInvoke :: Int -> IORef Int -> MVar () -> CodeModeNestedInvoke
budgetedInvoke limit calls reached tool call = do
    allowed <- atomicModifyIORef' calls (\n -> if n < limit then (n+1, True) else (n, False))
    if allowed
        then Right <$> dispatchToolCall defaultLoopDispatch [tool.appToolHandler] call
        else do
            _ <- tryPutMVar reached ()
            -- The owner observes the signal and closes the worker. Do not send
            -- repeated error replies that permit a runaway callback loop.
            blocked <- newEmptyMVar
            takeMVar blocked

runPreflight :: FilePath -> IO ()
runPreflight destination = do
    createDirectoryIfMissing True destination
    worker <- codeModeWorkerPath
    rows <- forM [(seed, backend) | seed <- [1..10], backend <- [JavaScriptBackend, HaskellBackend]] \(seed, backend) -> do
        let fixture = destination </> show seed <> "-" <> show backend
        createDirectoryIfMissing True fixture
        prepareFixture fixture seed
        calls <- newIORef 0
        reached <- newEmptyMVar
        let specs = [CodeModeNestedSpec tool Nothing | tool <- fixtureTools fixture seed]
        bracket
            (newCodeModeToolSetWithBackend backend CodeOnlyToolMode ImageDetailVisible worker
                (budgetedInvoke 128 calls reached) specs >>= either (fail . Text.unpack) pure)
            (.closeCodeModeToolSet) \tools -> do
                forM_ [0..3] \task -> do
                    result <- timeout 15000000 $ dispatchToolCall defaultLoopDispatch
                        (map (.appToolHandler) tools.codeModeTools)
                        (customToolCall "preflight" "exec" (referenceCell backend task))
                    case result of
                        Just value | value.toolResultOutcome /= Just ToolFailed
                            , Text.strip (taskExpected task seed) ==
                                Text.strip (Text.drop (Text.length "\nOutput:\n") (snd (Text.breakOn "\nOutput:\n" value.output))) -> pure ()
                        _ -> fail ("preflight failed " <> show (seed, backend, task) <> ": " <> maybe "timeout" (Text.unpack . (.output)) result)
                pure (object ["seed" .= seed, "backend" .= show backend, "tasksPassed" .= (4 :: Int)])
    forM_ [JavaScriptBackend, HaskellBackend] \backend -> do
        calls <- newIORef 0
        reached <- newEmptyMVar
        let fixture = destination </> "1-JavaScriptBackend"
        result <- timeout 15000000 $ bracket
            (newCodeModeToolSetWithBackend backend CodeOnlyToolMode ImageDetailVisible worker
                (budgetedInvoke 4 calls reached)
                [CodeModeNestedSpec tool Nothing | tool <- fixtureTools fixture 1] >>= either (fail . Text.unpack) pure)
            (.closeCodeModeToolSet) \tools ->
                race (readMVar reached) $ dispatchToolCall defaultLoopDispatch
                    (map (.appToolHandler) tools.codeModeTools)
                    (customToolCall "budget-check" "exec" (case backend of
                        JavaScriptBackend -> "while(true) await tools.list_directory({path:'.'});"
                        HaskellBackend -> "forever (callTool \"list_directory\" (object [\"path\" .= (\".\" :: Text)]) >> pure ())"))
        count <- readIORef calls
        unless (case result of Just (Left ()) -> count == 4; _ -> False)
            (fail ("budget preflight failed " <> show backend))
    LBS.writeFile (destination </> "preflight.json") (encode rows)
    putStrLn "Preflight passed: 80 task solutions and both callback-limit termination checks."

runFulfillmentPreflight :: FilePath -> IO ()
runFulfillmentPreflight destination = do
    Fulfillment.validateFulfillmentAudit
    runPreflight destination
    worker <- codeModeWorkerPath
    rows <- fmap concat $ forM [(seed, backend) | seed <- [1..10], backend <- [JavaScriptBackend, HaskellBackend]] \(seed, backend) ->
        forM (("reference", Fulfillment.fulfillmentReference seed backend) : Fulfillment.fulfillmentFaults seed backend) \(caseName, source) -> do
            fixture <- Fulfillment.newFulfillmentFixture seed
            calls <- newIORef 0
            reached <- newEmptyMVar
            let specs = [CodeModeNestedSpec tool Nothing | tool <- Fulfillment.fulfillmentTools fixture]
            started <- getMonotonicTimeNSec
            result <- timeout 15000000 $ bracket
                (newCodeModeToolSetWithBackend backend CodeOnlyToolMode ImageDetailVisible worker
                    (budgetedInvoke 128 calls reached) specs >>= either (fail . Text.unpack) pure)
                (.closeCodeModeToolSet) \tools ->
                    race (readMVar reached) $ dispatchToolCall defaultLoopDispatch
                        (map (.appToolHandler) tools.codeModeTools)
                        (customToolCall "fulfillment-preflight" "exec" source)
            ended <- getMonotonicTimeNSec
            audit <- Fulfillment.fulfillmentSnapshot fixture
            correctState <- Fulfillment.fulfillmentPassed fixture
            observedCalls <- readIORef calls
            let output = case result of Just (Right value) -> value.output; _ -> ""
                completed = case result of Just (Right _) -> True; _ -> False
                failed = "Script failed" `Text.isInfixOf` output ||
                    case result of Just (Right value) -> value.toolResultOutcome == Just ToolFailed; _ -> True
                passed = if caseName == "reference"
                    then completed && correctState && not failed && Fulfillment.fulfillmentExpected seed `Text.isInfixOf` output
                    else True
                row = object ["trial" .= seed, "backend" .= show backend, "case" .= caseName
                    , "source" .= source, "output" .= output, "audit" .= audit
                    , "passed" .= passed, "cellFailed" .= failed, "completed" .= completed
                    , "nestedCalls" .= observedCalls
                    , "seconds" .= (fromIntegral (ended-started) / 1e9 :: Double)]
            let caseDirectory = destination </> "fulfillment" </> show seed <> "-" <> show backend
            createDirectoryIfMissing True caseDirectory
            LBS.writeFile (caseDirectory </> Text.unpack caseName <> ".json") (encode row)
            unless passed (fail ("fulfillment preflight failed: " <> show row))
            pure row
    LBS.writeFile (destination </> "fulfillment-preflight.json") (encode rows)
    putStrLn ("Fulfillment preflight passed: 20 reference workflows and "
        <> show (length rows - 20) <> " controlled fault executions.")

referenceCell :: CodeModeBackend -> Int -> Text
referenceCell JavaScriptBackend 0 = "const a=(await tools.read_file({path:'numbers.csv'})).trim().split(',').map(Number);const s=a.reduce((x,y)=>x+y,0);text(`count=${a.length}\\nsum=${s}\\nminimum=${Math.min(...a)}\\nmaximum=${Math.max(...a)}\\nmean=${(s/a.length).toFixed(2)}`);"
referenceCell JavaScriptBackend 1 = "async function walk(p){let all=[];for(const n of await tools.list_directory({path:p})){if(n.endsWith('/'))all.push(...await walk(p+n));else if(n.endsWith('.log'))all.push(...(await tools.read_file({path:p+n})).trim().split('\\n'));}return all;}const out=[];for(const s of await tools.list_directory({path:'logs'})){const a=await walk('logs/'+s);out.push(`${s.slice(0,-1)} errors=${a.filter(x=>x.startsWith('ERROR ')).length} warnings=${a.filter(x=>x.startsWith('WARN ')).length}`);}text(out.join('\\n'));"
referenceCell JavaScriptBackend 2 = "let c='start',a=[];while(c!==null){const p=await tools.fetch_page({cursor:c});a.push(...p.values);c=p.nextCursor;}a=a.filter(x=>x%3===0);text(`count=${a.length}\\nsum=${a.reduce((x,y)=>x+y,0)}`);"
referenceCell JavaScriptBackend _ = "const [cs,os]=await Promise.all([tools.read_data({path:'customers.json'}),tools.read_data({path:'orders.json'})]);const totals={};for(const o of os){const c=cs.find(c=>c.id===o.customerId);if(o.status==='paid'&&c?.active)totals[c.region]=(totals[c.region]||0)+o.amountCents;}text(Object.keys(totals).sort().map(r=>`${r} amountCents=${totals[r]}`).join('\\n'));"
referenceCell HaskellBackend 0 = Text.unlines
    ["do", "  String raw <- callTool \"read_file\" (object [\"path\" .= (\"numbers.csv\" :: Text)])"
    , "  let a = map (read . Text.unpack) (Text.splitOn \",\" (Text.strip raw)) :: [Int]"
    , "      s = sum a"
    , "      cents = round (fromIntegral s * 100 / fromIntegral (length a) :: Double) :: Int"
    , "      dec = show (abs cents `mod` 100)"
    , "      meanText = show (cents `div` 100) <> \".\" <> (if length dec == 1 then \"0\" else \"\") <> dec"
    , "  text (Text.pack (\"count=\" <> show (length a) <> \"\\nsum=\" <> show s <> \"\\nminimum=\" <> show (minimum a) <> \"\\nmaximum=\" <> show (maximum a) <> \"\\nmean=\" <> meanText))"]
referenceCell HaskellBackend 1 = Text.unlines
    ["do"
    , "  let listing p = do {v <- callTool \"list_directory\" (object [\"path\" .= (p :: Text)]); case fromJSON v of {Success a -> pure (a :: [Text]); Error e -> fail e}}"
    , "      walk p = do"
    , "        names <- listing p"
    , "        fmap concat $ forM names $ \\n -> if Text.isSuffixOf \"/\" n then walk (p <> n) else if Text.isSuffixOf \".log\" n then do {String t <- callTool \"read_file\" (object [\"path\" .= (p <> n)]); pure (Text.lines t)} else pure []"
    , "  services <- listing \"logs\""
    , "  linesOut <- forM services $ \\s -> do"
    , "    rows <- walk (\"logs/\" <> s)"
    , "    pure (Text.dropEnd 1 s <> \" errors=\" <> Text.pack (show (length (filter (Text.isPrefixOf \"ERROR \") rows))) <> \" warnings=\" <> Text.pack (show (length (filter (Text.isPrefixOf \"WARN \") rows))))"
    , "  text (Text.intercalate \"\\n\" linesOut)"]
referenceCell HaskellBackend 2 = Text.unlines
    ["do"
    , "  let pages cursor = do"
    , "        result <- Tools.fetch_page Tools.ToolArguments_fetch_page {Tools.cursor = cursor}"
    , "        page <- either (fail . Text.unpack) pure result.decodedResult"
    , "        rest <- maybe (pure []) pages page.nextCursor"
    , "        pure (page.values <> rest)"
    , "  allValues <- pages \"start\""
    , "  let selected = filter (\\n -> n `mod` 3 == 0) allValues"
    , "  text (\"count=\" <> Text.pack (show (length selected)) <> \"\\nsum=\" <> Text.pack (show (sum selected)))"]
referenceCell HaskellBackend _ = Text.unlines
    ["do"
    , "  customersValue <- callTool \"read_data\" (object [\"path\" .= (\"customers.json\" :: Text)])"
    , "  ordersValue <- callTool \"read_data\" (object [\"path\" .= (\"orders.json\" :: Text)])"
    , "  let Success customers = fromJSON customersValue :: Result [Object]"
    , "      Success orders = fromJSON ordersValue :: Result [Object]"
    , "      one [x] = x; one _ = error \"invalid fixture field\""
    , "      customer o = let fields = foldr (:) [] o in (one [n | Number n <- fields], one [r | String r <- fields], one [b | Bool b <- fields])"
    , "      order o = let fields = foldr (:) [] o in (one [n | Number n <- fields, n < 100], one [s | String s <- fields], one [n | Number n <- fields, n >= 100])"
    , "      totals region = sum [amount | o <- orders, let (identifier,status,amount) = order o, status == \"paid\", c <- customers, let (customerId,r,active) = customer c, identifier == customerId, active, r == region]"
    , "  text (Text.intercalate \"\\n\" [region <> \" amountCents=\" <> Text.pack (show (round (totals region) :: Integer)) | region <- [\"east\",\"north\",\"west\"]])"]

evaluateTurns :: IO (Backend, [AppTool]) -> (Value -> IO ()) -> IORef TokenUsage
    -> IORef Int -> IORef Int -> BackendSnapshot -> Maybe Text -> [TurnInput]
    -> Int -> IO (Either Text Text)
evaluateTurns refresh record usage steps calls snapshot previous inputs turn
    | turn >= 8 = pure (Left "model turn limit")
    | otherwise = do
        (backend, tools) <- refresh
        modifyIORef' steps (+1)
        backend.submitTurn snapshot previous inputs (\_ -> pure ()) >>= \case
            Left err -> pure (Left (Text.pack (show err)))
            Right result -> do
                let output = result.backendOutput
                modifyIORef' usage (`addTokenUsage` output.tokenUsage)
                record (object ["kind" .= ("model" :: Text), "turn" .= turn
                    , "completion" .= show output.completion
                    , "assistant" .= output.assistantText, "usage" .= output.tokenUsage])
                if output.completion /= TurnCompleted
                    then pure (Left ("incomplete: " <> Text.pack (show output.completion)))
                    else if null output.toolCalls
                        then pure (maybe (Left "missing final answer") Right output.assistantText)
                        else do
                            replies <- forM output.toolCalls \call -> do
                                if call.name == "exec" then modifyIORef' calls (+1) else pure ()
                                resultCall <- dispatchToolCall defaultLoopDispatch
                                    (map (.appToolHandler) tools) call
                                record (object ["kind" .= ("tool" :: Text)
                                    , "name" .= call.name, "arguments" .= call.arguments
                                    , "output" .= resultCall.output
                                    , "outcome" .= show resultCall.toolResultOutcome])
                                pure (CompletedTool resultCall)
                            evaluateTurns refresh record usage steps calls
                                result.backendState (Just output.responseId) replies (turn+1)

fixtureTools :: FilePath -> Int -> [AppTool]
fixtureTools directory seed =
    [ makeTool "read_file" "Read a UTF-8 fixture file. Returns its entire contents as text."
        (Just (object ["type" .= ("string" :: Text)]))
        \path -> String <$> TextIO.readFile path
    , makeTool "list_directory" "List immediate directory entries, sorted. Returns a structured array of names, not JSON text; directories end in slash."
        (Just (object ["type" .= ("array" :: Text), "items" .= object ["type" .= ("string" :: Text)]]))
        \path -> do
            names <- sort <$> listDirectory path
            entries <- forM names \name -> do
                directoryEntry <- doesDirectoryExist (path </> name)
                pure (name <> if directoryEntry then "/" else "")
            pure (toJSON entries)
    , makeTool "read_data" "Read a JSON dataset file as structured JSON, not text. Available files: customers.json and orders.json."
        Nothing \path -> LBS.readFile path >>= either fail pure . eitherDecode
    , pageTool seed
    ]
  where
    makeTool name description schema action =
      let tool = jsonAppTool name description [PropertySchema "path" PropertyString True (Just "Relative fixture path; use . for root.")]
            AlwaysReadOnly $ typedRichToolWithCall name (objectArgsExact ["path"] (\obj -> reqText obj "path")) \_ relative -> do
                canonical <- canonicalizePath (directory </> Text.unpack relative)
                root <- canonicalizePath directory
                if canonical /= root && not (addTrailingPathSeparator root `isPrefixOfPath` canonical)
                    then pure (Left "path outside fixture")
                    else Right . structuredResult <$> action canonical
      in tool { appToolOutputMetadata = Just (ToolOutputMetadata schema JsonToolOutput) }
    isPrefixOfPath prefix path = Text.pack prefix `Text.isPrefixOf` Text.pack path

structuredResult :: Value -> ToolHandlerResult
structuredResult value = withToolHandlerStructuredResult value $
    ToolHandlerResult (Text.decodeUtf8 (LBS.toStrict (encode value))) []

pageTool :: Int -> AppTool
pageTool seed =
    let tool = jsonAppTool "fetch_page"
            "Retrieve a page of analytics integers. Start cursor=start; follow nextCursor until null. Returns structured JSON {values: integer[], nextCursor: string|null}."
            [PropertySchema "cursor" PropertyString True (Just "start or the previous page's nextCursor")]
            AlwaysReadOnly $ typedRichToolWithCall "fetch_page"
                (objectArgsExact ["cursor"] (\obj -> reqText obj "cursor")) \_ cursor ->
                    case if cursor == "start" then Just 0 else readMaybe (Text.unpack cursor) of
                        Just page | page >= 0 && page < 4 -> pure (Right (structuredResult (object
                            ["values" .= take 25 (drop (page*25) (analyticsValues seed))
                            , "nextCursor" .= (if page < 3 then Just (show (page+1)) else Nothing)])))
                        _ -> pure (Left "invalid cursor")
    in tool { appToolOutputMetadata = Just (ToolOutputMetadata
        (Just (object ["type" .= ("object" :: Text), "additionalProperties" .= False
            , "required" .= (["values", "nextCursor"] :: [Text])
            , "properties" .= object
                ["values" .= object ["type" .= ("array" :: Text), "items" .= object ["type" .= ("integer" :: Text)]]
                , "nextCursor" .= object ["type" .= (["string", "null"] :: [Text])]]])) JsonToolOutput) }

taskName :: Int -> Text
taskName 0 = "data-summary"
taskName 1 = "tree-audit"
taskName 2 = "paginated-analytics"
taskName _ = "customer-order-join"

taskPrompt :: Int -> Text
taskPrompt 0 = "Read numbers.csv using the nested read_file tool and compute a summary in exec. Return exactly five lines: count=<integer>, sum=<integer>, minimum=<integer>, maximum=<integer>, mean=<decimal with two digits>. Put each field on its own line; no commas or explanation."
taskPrompt 1 = "Recursively inspect logs/ using nested list_directory and read_file tools. Count ERROR and WARN lines per service (first directory under logs). Only .log files count; ignore other files. Return one line per service, sorted by service: <service> errors=<count> warnings=<count>. No explanation."
taskPrompt 2 = "Use fetch_page to retrieve every analytics page, starting cursor=start and following nextCursor until null. Among all values retain only integers divisible by 3. Return exactly two lines: count=<retained count> and sum=<retained sum>."
taskPrompt _ = "Use read_data to retrieve customers.json and orders.json. Join orders.customerId to customers.id. Include only orders with status=paid and customers with active=true. Sum amountCents per customer region. Return one line per region sorted alphabetically: <region> amountCents=<integer>. Ignore unmatched orders. No explanation."

taskExpected :: Int -> Int -> Text
taskExpected 0 seed =
    let values = csvValues seed
    in Text.intercalate "\n"
        ["count=" <> shown (length values), "sum=" <> shown (sum values)
        , "minimum=" <> shown (minimum values), "maximum=" <> shown (maximum values)
        , "mean=" <> Text.pack (printf "%.2f" (fromIntegral (sum values) / fromIntegral (length values) :: Double))]
taskExpected 1 seed = "api errors=" <> shown (3+seed) <> " warnings=2\nbilling errors=1 warnings=3\nworker errors=2 warnings=1"
taskExpected 2 seed =
    let values = filter (\n -> n `mod` 3 == 0) (analyticsValues seed)
    in "count=" <> shown (length values) <> "\nsum=" <> shown (sum values)
taskExpected _ seed = Text.intercalate "\n"
    [region <> " amountCents=" <> shown (sum
        [amount | (customerId, status, amount) <- orderRows seed, status == "paid"
                , (identifier, customerRegion, active) <- customerRows
                , identifier == customerId, active, customerRegion == region])
    | region <- ["east", "north", "west"]]

shown :: Show a => a -> Text
shown = Text.pack . show

csvValues :: Int -> [Int]
csvValues seed = [((i*37 + seed*13) `mod` 101)-40 | i <- [1..121]]

analyticsValues :: Int -> [Int]
analyticsValues seed = [((i*19 + seed*7) `mod` 211)-90 | i <- [1..100]]

customerRows :: [(Int, Text, Bool)]
customerRows = [(i, ["east", "north", "west"] !! (i `mod` 3), i `mod` 4 /= 0) | i <- [1..18]]

orderRows :: Int -> [(Int, Text, Int)]
orderRows seed = [(1 + i `mod` 20, if i `mod` 5 == 0 then "pending" else "paid", 100 + ((i*31+seed*17) `mod` 1000)) | i <- [1..90]]

prepareFixture :: FilePath -> Int -> IO ()
prepareFixture root seed = do
    TextIO.writeFile (root </> "numbers.csv") (Text.intercalate "," (map shown (csvValues seed)) <> "\n")
    LBS.writeFile (root </> "customers.json") (encode
        [object ["id" .= i, "region" .= region, "active" .= active] | (i,region,active) <- customerRows])
    LBS.writeFile (root </> "orders.json") (encode
        [object ["customerId" .= i, "status" .= status, "amountCents" .= amount] | (i,status,amount) <- orderRows seed])
    forM_
        [ ("logs/api/2026-08-21.log", ["INFO start", "WARN slow", "ERROR timeout"] <> replicate seed "ERROR additional")
        , ("logs/api/archive/2026-08-20.log", ["ERROR reset", "WARN retry", "ERROR failed"])
        , ("logs/api/notes.txt", ["ERROR ignored"])
        , ("logs/billing/current.log", ["WARN late", "WARN retry", "ERROR declined", "WARN queued"])
        , ("logs/worker/a.log", ["INFO ready", "ERROR crash"])
        , ("logs/worker/nested/b.log", ["WARN busy", "ERROR lost"])
        ] \(relative, rows) -> do
            createDirectoryIfMissing True (takeDirectory (root </> relative))
            TextIO.writeFile (root </> relative) (Text.unlines rows)
