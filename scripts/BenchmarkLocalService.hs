{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
-- Build with ghc -O2 -threaded -rtsopts -XImportQualifiedPost
-- -package aeson -package async -package safe-exceptions
-- -package wai -package warp -package http-types.
-- The baseline needs only the documented private-home/state-directory shims.
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (concurrently)
import Control.Exception.Safe (finally)
import Control.Monad (forM, forM_, unless)
import Data.Aeson
import Data.Aeson.KeyMap qualified as Map
import Data.ByteString.Char8 qualified as Bytes
import Data.ByteString.Lazy.Char8 qualified as Lazy
import Data.Char (isDigit)
import Data.List (sort, find)
import Data.IORef
import Data.Maybe (fromMaybe)
import Data.String (fromString)
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Clock (getMonotonicTimeNSec)
import Data.ByteString.Builder (lazyByteString)
import Network.HTTP.Types (status200, hContentType)
import Network.Wai (strictRequestBody, responseStream)
import Network.Wai.Handler.Warp qualified as Warp
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.IO
import System.IO.Error (isEOFError, tryIOError)
import System.Process
import Text.Read (readMaybe)

main :: IO ()
main = getArgs >>= \case
    ["provider"] -> provider
    ["mcp", logPath, delay] -> mcp logPath (read delay)
    ["--local"] -> do
        server <- getEnv "BENCH_SERVER_BIN"
        rawSystem server ["--local", "+RTS", "-s", "-RTS"] >>= exitWith
    ["bench", baseline, client, server, root] -> benchmark baseline client server root
    _ -> die "Usage: BenchmarkLocalService provider | bench BASELINE CLI SERVER SCRATCH_ROOT | mcp LOG DELAY_MS"

provider :: IO ()
provider = do
    counter <- newIORef (0 :: Int)
    Warp.runSettings (Warp.setHost "127.0.0.1" (Warp.setPort 18886 Warp.defaultSettings)) \request respond -> do
        _ <- strictRequestBody request
        count <- atomicModifyIORef' counter (\value -> (value + 1, value + 1))
        let completed = object ["type" .= ("response.completed" :: Text), "response" .= object
                [ "id" .= ("fixture-" <> Text.pack (show count)), "created_at" .= (0 :: Int)
                , "model" .= ("fixture" :: Text), "status" .= ("completed" :: Text)
                , "output" .= [object ["id" .= ("message-fixture" :: Text), "type" .= ("message" :: Text)
                    , "role" .= ("assistant" :: Text), "status" .= ("completed" :: Text)
                    , "content" .= [object ["type" .= ("output_text" :: Text)
                        , "text" .= ("Local fixture response " <> Text.pack (show count)), "annotations" .= ([] :: [Value])]]]]
                , "usage" .= object ["input_tokens" .= (100 :: Int), "output_tokens" .= (8 :: Int)
                    , "total_tokens" .= (108 :: Int)] ]]
        respond $ responseStream status200 [(hContentType, "text/event-stream")] \write flush -> do
            write "event: response.created\ndata: {\"type\":\"response.created\",\"response\":{\"id\":\"fixture-start\"}}\n\n"
            flush
            write (lazyByteString ("event: response.completed\ndata: " <> encode completed <> "\n\n"))
            flush

mcp :: FilePath -> Int -> IO ()
mcp logPath delay = do
    hSetBuffering stdout LineBuffering
    stamp logPath "start"
    threadDelay (delay * 1000)
    loop `finally` (threadDelay (delay * 1000) >> stamp logPath "close")
  where
    loop = tryIOError (Bytes.hGetLine stdin) >>= \case
        Left err | isEOFError err -> pure ()
        Left err -> ioError err
        Right line -> do
            case eitherDecodeStrict' line of
                Right (Object fields) | Just identifier <- Map.lookup "id" fields -> do
                    let method = Map.lookup "method" fields
                        result = case method of
                            Just (String "initialize") -> object
                                [ "protocolVersion" .= ("2025-06-18" :: Text)
                                , "serverInfo" .= object ["name" .= ("fixture" :: Text), "version" .= ("1" :: Text)]
                                , "capabilities" .= object ["tools" .= object []] ]
                            Just (String "tools/list") -> object ["tools" .= ([] :: [Value])]
                            _ -> object []
                    Lazy.putStrLn (encode (object ["jsonrpc" .= ("2.0" :: Text), "id" .= identifier, "result" .= result]))
                _ -> pure ()
            loop

stamp :: FilePath -> Text -> IO ()
stamp path event = do
    now <- getMonotonicTimeNSec
    Lazy.appendFile path (encode (object ["event" .= event, "timeNs" .= now]) <> "\n")

benchmark :: FilePath -> FilePath -> FilePath -> FilePath -> IO ()
benchmark baseline client server root = do
    self <- getExecutablePath
    baseEnvironment <- filter (\(key, _) -> key `notElem`
        ["HASKELL_AGENT_HOME", "HASKELL_AGENT_RUNTIME_DIR", "HASKELL_AGENT_SERVER",
         "AGENT_SERVER_STATE_DIRECTORY", "BENCH_SERVER_BIN", "GHCRTS"]) <$> getEnvironment
    let directory = root </> "local-service-benchmark"
        runtime = root </> "rb"
        oldHome = root </> "bo"
        newHome = root </> "bn"
        environment home state =
            [ ("HASKELL_AGENT_HOME", home), ("HASKELL_AGENT_RUNTIME_DIR", runtime)
            , ("HASKELL_AGENT_SERVER", self), ("BENCH_SERVER_BIN", server)
            , ("AGENT_SERVER_STATE_DIRECTORY", root </> state) ] <> baseEnvironment
        oldEnv = environment oldHome "xo"
        newEnv = environment newHome "xn"
        stop = do
            (code, _, err) <- readCreateProcessWithExitCode
                (proc client ["server", "stop", "--force"]){env = Just newEnv} ""
            unless (code == ExitSuccess) (die ("benchmark service stop failed: " <> err))
    createDirectoryIfMissing True directory
    forM_ [oldHome, newHome] \home -> do
        createDirectoryIfMissing True (home </> ".haskell-agent")
        Lazy.writeFile (home </> ".haskell-agent/models.json") (encode (object
            ["version" .= (1 :: Int), "connections" .= object
                ["fixture" .= object ["api" .= ("responses" :: Text),
                 "base_url" .= ("http://127.0.0.1:18886/v1" :: Text), "api_key_optional" .= True]]
            , "models" .= [object ["id" .= ("fixture" :: Text), "connection" .= ("fixture" :: Text)
                , "model" .= ("fixture" :: Text), "dialect" .= ("generic-responses" :: Text)
                , "context_window" .= (32768 :: Int)]]]))
    rows <- forM [1, 4, 1 :: Int] \count -> do
        let oldLog = directory </> ("baseline-mcp-" <> show count <> ".jsonl")
            newLog = directory </> ("service-mcp-" <> show count <> ".jsonl")
            configure home path = Lazy.writeFile (home </> ".haskell-agent/config.json") $
                encode (object ["version" .= (1 :: Int), "mcpInitStrategy" .= ("blocking" :: Text),
                    "mcpServers" .= Map.fromList
                        [(fromStringKey ("fixture" <> show index), object
                            ["enabled" .= True, "command" .= self, "args" .= ["mcp", path, "120"]])
                        | index <- [1 .. count]]])
        stop
        logExists <- doesFileExist (runtime </> "local-server.log")
        if logExists then writeFile (runtime </> "local-server.log") "" else pure ()
        configure oldHome oldLog
        configure newHome newLog
        Lazy.writeFile oldLog ""
        Lazy.writeFile newLog ""
        coldOld <- sample baseline oldEnv
        coldNew <- sample client newEnv
        before <- sequence (replicate 7 (sample baseline oldEnv))
        after <- sequence (replicate 7 (sample client newEnv))
        descriptor <- eitherDecode <$> Lazy.readFile (runtime </> "local-server.json")
        resident <- case descriptor of
            Right (Object fields) | Just (Number pid) <- Map.lookup "processId" fields -> do
                (_, output, _) <- readProcessWithExitCode "ps" ["-o", "rss=", "-p", show (round pid :: Integer)] ""
                pure (readMaybe output :: Maybe Integer)
            _ -> pure Nothing
        stopping <- getMonotonicTimeNSec
        stop
        serverLog <- readServerStats (runtime </> "local-server.log") (100 :: Int)
        stopped <- getMonotonicTimeNSec
        oldStarts <- countStarts oldLog
        newStarts <- countStarts newLog
        pure $ object
            [ "mcpServers" .= count, "samples" .= (7 :: Int)
            , "baselineCold" .= sampleValue coldOld, "serviceCold" .= sampleValue coldNew
            , "baseline" .= aggregate before, "service" .= aggregate after
            , "baselineMcpStarts" .= oldStarts, "serviceMcpStarts" .= newStarts
            , "serverResidentKiB" .= resident
            , "serverAllocatedBytesIncludingStartupEightTurns" .= allocated serverLog
            , "serverCpuSecondsIncludingStartupEightTurns" .= serverCpuTime serverLog
            , "serverStopAndCleanupMs" .= (fromIntegral (stopped - stopping) / 1e6 :: Double) ]
    Lazy.writeFile (directory </> "results.json") (encode rows)
    Lazy.putStrLn (encode rows)

fromStringKey :: String -> Key
fromStringKey = fromString

sample executable environment = do
    started <- getMonotonicTimeNSec
    ((code, ended), ((output, firstOutput), diagnostic)) <- withCreateProcess
        (proc "/usr/bin/time" ["-l", executable, "--model", "fixture", "--minimal",
            "--no-code-mode", "-p", "fixture benchmark", "+RTS", "-s", "-RTS"])
            {env = Just environment, std_in = NoStream, std_out = CreatePipe, std_err = CreatePipe}
        \_ output errors process -> case (output, errors) of
            (Just outputHandle, Just errorHandle) -> concurrently
                (do code <- waitForProcess process; ended <- getMonotonicTimeNSec; pure (code, ended))
                (concurrently (readOutput outputHandle) (Bytes.unpack <$> Bytes.hGetContents errorHandle))
            _ -> fail "benchmark process pipes are unavailable"
    unless (code == ExitSuccess && "Local fixture response" `Bytes.isInfixOf` output)
        (die ("benchmark turn failed: " <> take 2000 diagnostic))
    pure (fromIntegral (ended - started) / 1e6 :: Double, allocated diagnostic, cpuTime diagnostic,
        fmap (\first -> fromIntegral (ended - first) / 1e6 :: Double) firstOutput)

readOutput handle = go [] Nothing
  where
    go chunks first = do
        chunk <- Bytes.hGetSome handle 4096
        if Bytes.null chunk then pure (Bytes.concat (reverse chunks), first) else do
            let updated = chunk : chunks
            observed <- if first == Nothing && "Local fixture response" `Bytes.isInfixOf` Bytes.concat (reverse updated)
                then Just <$> getMonotonicTimeNSec else pure first
            go updated observed

allocated text = do
    line <- find (Text.isInfixOf "bytes allocated in the heap" . Text.pack) (reverse (lines text))
    case reverse (takeWhile (/= "bytes") (words line)) of
        amount : _ -> readMaybe (filter isDigit amount) :: Maybe Integer
        _ -> Nothing

serverCpuTime text = do
    line <- find (\value -> case words value of "Total" : "time" : _ -> True; _ -> False)
        (reverse (lines text))
    case words line of
        _ : _ : seconds : _ -> readMaybe (filter (/= 's') seconds) :: Maybe Double
        _ -> Nothing

cpuTime text = do
    line <- find (\value -> " real " `Text.isInfixOf` Text.pack value && " user " `Text.isInfixOf` Text.pack value) (lines text)
    case words line of
        _ : "real" : user : "user" : system : "sys" : _ ->
            (+) <$> (readMaybe user :: Maybe Double) <*> readMaybe system
        _ -> Nothing

sampleValue (elapsed, allocation, cpu, exitTime) = object
    ["elapsedMs" .= elapsed, "allocatedBytes" .= allocation, "cpuSeconds" .= cpu, "afterAnswerExitMs" .= exitTime]
aggregate values = object
    [ "medianElapsedMs" .= median [elapsed | (elapsed, _, _, _) <- values]
    , "medianAllocatedBytes" .= medianMaybe [fromIntegral value :: Double | (_, Just value, _, _) <- values]
    , "medianCpuSeconds" .= medianMaybe [value | (_, _, Just value, _) <- values]
    , "medianAfterAnswerExitMs" .= medianMaybe [value | (_, _, _, Just value) <- values]
    , "runs" .= map sampleValue values ]
median [] = 0 :: Double
median values = sort values !! (length values `div` 2)
medianMaybe [] = Nothing
medianMaybe values = Just (median values)
readServerStats path remaining = do
    text <- Bytes.unpack <$> Bytes.readFile path
    case allocated text of
        Nothing | remaining > 0 -> threadDelay 10000 >> readServerStats path (remaining - 1)
        _ -> pure text
countStarts path = length . filter (Text.isInfixOf "\"start\"" . Text.pack) . lines . Bytes.unpack <$> Bytes.readFile path
