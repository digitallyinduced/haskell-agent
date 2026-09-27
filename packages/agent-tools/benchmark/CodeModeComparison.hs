-- | Compare real code-mode hosts, not language arithmetic microbenchmarks.
-- Model generation, approvals, MCP transports and repair are deliberately
-- outside this benchmark. Fixture callbacks are shared by both backends.
module Main (main) where

import qualified Agent.Tools.CodeMode.Host as JS
import qualified Agent.Tools.CodeMode.Haskell.Host as HS
import Agent.Tools.CodeMode.Haskell.Bindings
import Control.Concurrent (threadDelay)
import Control.Exception (evaluate)
import Control.Exception.Safe (bracket)
import Control.Monad (forM, forM_, unless)
import Data.Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import Data.IORef
import Data.List (sort)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.Vector as Vector
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Stats
import System.CPUTime (getCPUTime)
import System.Directory (listDirectory, doesDirectoryExist)
import System.Environment (getArgs)
import System.FilePath ((</>), takeExtension)
import System.IO (BufferMode (LineBuffering), hSetBuffering, stdout)
import System.Mem (performGC)
import Text.Printf (printf)
import Text.Read (readMaybe)

data Backend = JavaScript | Haskell deriving (Eq, Show)
data Runtime = JavaScriptRuntime JS.CodeModeHost | HaskellRuntime HS.HaskellHost
data Workload = Workload
    { name :: !String
    , size :: !Int
    , javascript :: !Text
    , haskell :: !Text
    , handler :: !JS.CodeModeToolHandler
    , expected :: !Text
    }
data Measurement = Measurement !Double !Double !Integer

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    args <- getArgs
    (samples, catalogSize) <- case args of
        [value] | Just n <- readMaybe value, n >= 3, odd n -> pure (n, 1)
        [value, count] | Just n <- readMaybe value, n >= 3, odd n
            , Just catalogSize <- readMaybe count, catalogSize > 0 ->
                pure (n, catalogSize)
        _ -> fail "usage: code-mode-comparison-bench SAMPLES [CATALOG_SIZE] +RTS -T -N2 (SAMPLES must be odd and >= 3)"
    let names = "query" : ["catalog_query_" <> shown i | i <- [1..catalogSize-1 :: Int]]
        generatedBindings = bindings names
    enabled <- getRTSStatsEnabled
    unless enabled (fail "RTS statistics required: +RTS -T")
    files <- take 32 . sort <$> sourceFiles "packages/agent-tools/src"
    unless (length files == 32) (fail "run from the repository root")
    workloads <- sequence
        [ repositoryWorkload (take n files) | n <- [8,32] ]
    let allWorkloads = workloads <> [analyticsWorkload n | n <- [8,32]]
            <> [paginationWorkload n | n <- [4,16]]
    -- Force sources and expectations before measuring.
    forM_ allWorkloads \w -> evaluate (Text.length w.javascript + Text.length w.haskell + Text.length w.expected)
    _ <- evaluate (Text.length generatedBindings)
    putStrLn "kind,task,size,phase,backend,sample,elapsed_ms,parent_cpu_ms,parent_allocated_bytes,catalog_size"
    forM_ allWorkloads \workload -> forM_ ["cold-first-result","cold-lifecycle","warm"] \phase -> do
        count <- newIORef (0 :: Int)
        let callback name value = atomicModifyIORef' count (\n -> (n+1, ())) >> workload.handler name value
            run runtime = do
                writeIORef count 0
                output <- execute names generatedBindings runtime workload callback
                actualCount <- readIORef count
                unless (output == workload.expected && actualCount == workload.size) $
                    fail ("validation failed: " <> show (workload.name, output, workload.expected, actualCount))
                evaluate (Text.length output) >> pure ()
            roundMeasurements runtimes sampleIndex =
                forM (if odd sampleIndex then [JavaScript,Haskell] else [Haskell,JavaScript]) \backend -> do
                    measured <- case lookup backend runtimes of
                        Just runtime -> measure (run runtime)
                        Nothing | phase == "cold-lifecycle" ->
                            measure (bracket (acquire backend callback) close run)
                        Nothing -> measureResource (acquire backend callback) close run
                    printMeasurement catalogSize "sample" workload phase backend sampleIndex measured
                    pure (backend, measured)
            collect runtimes = concat <$> mapM (roundMeasurements runtimes) [1..samples]
        measurements <- if phase /= "warm"
            then collect []
            else bracket (acquire JavaScript callback) close \js ->
                bracket (acquire Haskell callback) close \hs -> do
                    run js
                    run hs
                    collect [(JavaScript,js),(Haskell,hs)]
        forM_ [JavaScript,Haskell] \backend ->
            printMeasurement catalogSize "median" workload phase backend samples
                (medians [m | (b,m) <- measurements, b == backend])

sourceFiles :: FilePath -> IO [FilePath]
sourceFiles root = do
    children <- sort <$> listDirectory root
    concat <$> forM children \child -> do
        let path = root </> child
        directory <- doesDirectoryExist path
        if directory then sourceFiles path
            else pure [path | takeExtension path == ".hs"]

bindings :: [Text] -> Text
bindings names = case generateHaskellBindings [(name, Just (object
    [ "type" .= ("object" :: Text)
    , "properties" .= object ["index" .= object ["type" .= ("integer" :: Text)]]
    , "required" .= (["index"] :: [Text])
    , "additionalProperties" .= False
    ])) | name <- names] of
    Right generated -> generated.bindingsModuleSource
    Left problem -> error (Text.unpack problem)

acquire :: Backend -> JS.CodeModeToolHandler -> IO Runtime
acquire JavaScript callback = JavaScriptRuntime <$> JS.newCodeModeHost
    (JS.defaultCodeModeConfig "packages/agent-tools/data/code-mode/worker.mjs" callback)
acquire Haskell _ = HaskellRuntime <$> (HS.newHaskellHost
    "packages/agent-tools/data/code-mode/CodeModeSupport.hs" >>= either (fail . Text.unpack) pure)

close :: Runtime -> IO ()
close (JavaScriptRuntime host) = JS.closeCodeModeHost host
close (HaskellRuntime host) = HS.closeHaskellHost host

execute :: [Text] -> Text -> Runtime -> Workload -> JS.CodeModeToolHandler -> IO Text
execute names generatedBindings runtime workload callback = do
    result <- case runtime of
        JavaScriptRuntime host -> JS.execCodeCell host workload.javascript names 60000
        HaskellRuntime host -> HS.execHaskellCell host workload.haskell generatedBindings callback 60000
    value <- finish result
    case value of
        Object fields | Just (Array contents) <- KeyMap.lookup "content" fields ->
            pure $ Text.concat [t | Object item <- Vector.toList contents, Just (String t) <- [KeyMap.lookup "text" item]]
        _ -> fail ("unexpected output " <> show value)
  where
    finish (Right (JS.CodeModeFinished _ value)) = pure value
    finish (Right (JS.CodeModeRunning identifier _)) = case runtime of
        JavaScriptRuntime host -> JS.waitCodeCell host identifier 60000 >>= finish
        HaskellRuntime host -> HS.waitHaskellCell host identifier 60000 >>= finish
    finish result = fail ("execution failed: " <> show result)

repositoryWorkload :: [FilePath] -> IO Workload
repositoryWorkload paths = do
    contents <- mapM Text.readFile paths
    let n = length paths
        total = sum [length (filter ("import " `Text.isPrefixOf`) (Text.lines body)) | body <- contents]
    pure Workload
        { name = "repository-import-audit", size = n
        , javascript = "const files = await Promise.all(Array.from({length:" <> shown n <> "},(_,i)=>tools.query({index:i}))); text(String(files.reduce((n,s)=>n+s.split('\\n').filter(l=>l.startsWith('import ')).length,0)));"
        , haskell = "do\n  files <- mapConcurrently (\\i -> Tools.query (Tools.ToolArguments_query { Tools.index = i }) >>= \\v -> case fromJSON v of { Success s -> pure (s :: Text); Error e -> fail e }) [0.." <> shown (n-1) <> "]\n  text (Text.pack (show (sum [length (filter (Text.isPrefixOf \"import \") (Text.lines s)) | s <- files])))"
        , handler = \_ value -> case argumentIndex value of
            Success index | index >= 0 && index < n -> Right . String <$> Text.readFile (paths !! index)
            _ -> pure (Left "invalid file index")
        , expected = shown total
        }

analyticsWorkload :: Int -> Workload
analyticsWorkload n = Workload
    { name = "analytics-parallel-fixture", size = n
    , javascript = "const rows=await Promise.all(Array.from({length:" <> shown n <> "},(_,i)=>tools.query({index:i})));text(String(rows.flat().filter(x=>x%3===0).reduce((a,b)=>a+b,0)));"
    , haskell = "do\n  rows <- mapConcurrently (\\i -> Tools.query (Tools.ToolArguments_query { Tools.index = i }) >>= \\v -> case fromJSON v of { Success xs -> pure (xs :: [Int]); Error e -> fail e }) [0.." <> shown (n-1) <> "]\n  text (Text.pack (show (sum (filter (\\x -> x `mod` 3 == 0) (concat rows)))))"
    , handler = \_ value -> case argumentIndex value of
        Success index | index >= 0 && index < n -> threadDelay 25000 >> pure (Right (toJSON (rows index)))
        _ -> pure (Left "invalid analytics index")
    , expected = shown (sum (filter (\x -> x `mod` 3 == 0) (concatMap rows [0..n-1])))
    }
  where rows index = [index * 1000 + offset | offset <- [1..1000 :: Int]]

paginationWorkload :: Int -> Workload
paginationWorkload n = Workload
    { name = "pagination-dependent-fixture", size = n
    , javascript = "let cursor=0,total=0;do{const [next,rows]=await tools.query({index:cursor});total+=rows.filter(x=>x%3===0).reduce((a,b)=>a+b,0);cursor=next;}while(cursor>=0);text(String(total));"
    , haskell = Text.unlines
        [ "let loop cursor total = do"
        , "      value <- Tools.query (Tools.ToolArguments_query { Tools.index = cursor })"
        , "      (next, rows) <- case fromJSON value of { Success result -> pure (result :: (Integer, [Int])); Error e -> fail e }"
        , "      let accumulated = total + sum (filter (\\x -> x `mod` 3 == 0) rows)"
        , "      if next < 0 then text (Text.pack (show accumulated)) else loop next accumulated"
        , "in loop 0 0"
        ]
    , handler = \_ value -> case argumentIndex value of
        Success index | index >= 0 && index < n -> threadDelay 10000 >>
            pure (Right (toJSON (if index+1 == n then -1 else index+1, rows index)))
        _ -> pure (Left "invalid page cursor")
    , expected = shown (sum (filter (\x -> x `mod` 3 == 0) (concatMap rows [0..n-1])))
    }
  where rows index = [index * 500 + offset | offset <- [1..500 :: Int]]

shown :: Show a => a -> Text
shown = Text.pack . show

argumentIndex :: Value -> Result Int
argumentIndex (Object fields) = maybe (Error "missing index") fromJSON (KeyMap.lookup "index" fields)
argumentIndex _ = Error "expected object"

-- Worker process CPU and allocation are NOT included in these parent RTS
-- counters. Wall time includes the workers. Collection before/after a sample
-- makes allocated_bytes account for allocations below the nursery threshold.
measure :: IO () -> IO Measurement
measure action = measureResource (pure ()) (const (pure ())) (const action)

-- Start before acquisition, finish before release: cold means host-start to
-- first validated result, not lifecycle teardown or catalog publication.
measureResource :: IO a -> (a -> IO ()) -> (a -> IO ()) -> IO Measurement
measureResource acquireResource releaseResource action = do
    performGC
    before <- getRTSStats
    cpu <- getCPUTime
    start <- getMonotonicTimeNSec
    bracket acquireResource releaseResource \resource -> do
        action resource
        end <- getMonotonicTimeNSec
        cpuEnd <- getCPUTime
        performGC
        after <- getRTSStats
        pure (Measurement (fromIntegral (end-start) / 1e6)
            (fromIntegral (cpuEnd-cpu) / 1e9)
            (fromIntegral (allocated_bytes after - allocated_bytes before)))

medians :: [Measurement] -> Measurement
medians values = Measurement
    (median [a | Measurement a _ _ <- values])
    (median [b | Measurement _ b _ <- values])
    (median [c | Measurement _ _ c <- values])
  where median xs = sort xs !! (length xs `div` 2)

printMeasurement :: Int -> String -> Workload -> String -> Backend -> Int -> Measurement -> IO ()
printMeasurement catalogSize kind workload phase backend sample (Measurement elapsed cpu allocated) =
    printf "%s,%s,%d,%s,%s,%d,%.3f,%.3f,%d,%d\n"
        kind workload.name workload.size phase (show backend) sample elapsed cpu allocated catalogSize
