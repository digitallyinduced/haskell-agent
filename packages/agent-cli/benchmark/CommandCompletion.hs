{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}

-- The original name-completion algorithm remains here as the baseline.
-- Compile this component and its home modules with -O2; see CommandCompletion.md.
module Main (main) where

import Agent.CLI.Command.Catalog (slashCommands)
import Agent.CLI.Command.Types
import qualified Agent.CLI.Command.Menu as Menu
import Agent.CLI.Command.CompletionIndex
import Agent.Dialect (DialectId(..))
import Control.DeepSeq (NFData, force)
import Control.Exception (evaluate)
import Control.Exception.Safe (bracket)
import Control.Monad (forM, forM_, unless, foldM)
import Data.IORef (IORef, newIORef, readIORef)
import Data.List (sort, sortOn)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Maybe (mapMaybe)
import Data.Ord (Down(..))
import Data.Text (Text)
import qualified Data.Text as Text
import Foreign.StablePtr (newStablePtr, freeStablePtr)
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Stats
import System.CPUTime (getCPUTime)
import System.Environment (getArgs)
import System.Exit (die)
import System.IO (hSetBuffering, BufferMode(..), stdout)
import System.Mem (performGC)
import Text.Printf (printf)
import Text.Read (readMaybe)

type Fixture = ([(Text, [Text])], [Text])
type NormalizedNames = [(Text, Text)]

data Measurement = Measurement !Double !Double !Integer !Int

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    unlessM getRTSStatsEnabled (die "RTS statistics required: +RTS -T")
    arguments <- getArgs
    (mode, skillCount, iterations, samples) <- case arguments of
        [selection, skills, queries, repetitions]
            | selection `elem` ["prefix", "menu", "menu-distributed"]
            , Just count <- readMaybe skills, count >= 0
            , Just queryCount <- readMaybe queries, queryCount > 0
            , Just sampleCount <- readMaybe repetitions, sampleCount > 0 ->
                pure (selection, count, queryCount, sampleCount)
        _ -> die "usage: command-completion-bench (prefix|menu|menu-distributed) SKILLS QUERIES SAMPLES +RTS -T"
    if mode /= "prefix"
        then runMenuBenchmark (mode == "menu-distributed") skillCount iterations samples
        else runPrefixBenchmark skillCount iterations samples

runPrefixBenchmark :: Int -> Int -> Int -> IO ()
runPrefixBenchmark skillCount iterations samples = do
    fixture <- evaluate (force (makeFixture skillCount))
    fixtureReference <- newIORef fixture
    let names = fixtureNames fixture
    index <- evaluate (force (buildCompletionIndex names))
    normalized <- evaluate (force (normalizeNames names))
    indexReference <- newIORef index
    normalizedReference <- newIORef normalized
    -- All built-in prefixes, mixed case, misses, and skill prefixes. Equality
    -- includes result order and duplicates, not merely set membership.
    let validationQueries = "/definitely-absent" : "///" :
            concatMap (map Text.unpack . queryPrefixes) names
    forM_ validationQueries $ \query -> do
        let expected = originalCompletion fixture query
        unless (indexedCompletion index query == expected
                && normalizedCompletion normalized query == expected) $
            die ("completion mismatch for " <> show query)
    printf "# names=%d skills=%d samples=%d validation_queries=%d\n"
        (length names) skillCount samples (length validationQueries)
    putStrLn "phase,workload,implementation,operations,median_cpu_ms,median_elapsed_ms,median_allocated_bytes,checksum"
    let workloads =
            [ ("mixed", ["/", "/c", "/co", "/copy", "/copy-", "/copy-path",
                          "/m", "/mo", "/MODEL", "/re", "/resume", "/does-not-exist",
                          "/skill-", "/skill-000", "/skill-000001", "/skill-999999"])
            , ("broad", ["/", "///", "/c", "/re", "/skill-"])
            , ("selective", ["/model", "/copy-path", "/resume", "/skill-000001", "/missing"])
            ]
        implementations =
            [ ("original-list", \query -> readIORef fixtureReference >>= \current ->
                    evaluate (checksum (originalCompletion current query)))
            , ("normalized-list", \query -> readIORef normalizedReference >>= \current ->
                    evaluate (checksum (normalizedCompletion current query)))
            , ("radix-trie", \query -> readIORef indexReference >>= \current ->
                    evaluate (checksum (indexedCompletion current query)))
            ]
    forM_ workloads $ \(workload, queryCycle) -> do
        queries <- evaluate (force (take iterations (cycle queryCycle)))
        queryReference <- newIORef queries
        -- Warm all implementations, then alternate execution order by sample.
        forM_ implementations $ \(_, action) -> runQueries action queryReference >> pure ()
        rows <- forM [1 .. samples] $ \sample ->
            forM (if odd sample then implementations else reverse implementations) $
                \(name, action) -> do
                    measurement <- measure (runQueries action queryReference)
                    pure (name, measurement)
        forM_ implementations $ \(name, _) ->
            report "lookup" workload name iterations
                [measurement | row <- rows, (label, measurement) <- row, label == name]
    -- Fresh construction every sample, forced completely. Source names are
    -- already materialized; this isolates the extra index cost at catalog refresh.
    nameReference <- newIORef =<< evaluate (force names)
    forM_ (["normalized-list", "radix-trie"] :: [String]) $ \implementation -> do
        measurements <- forM [1 .. samples] $ \_ -> measure $
            if implementation == "radix-trie" then do
                current <- readIORef nameReference
                constructed <- evaluate (force (buildCompletionIndex current))
                evaluate (length (completeCommandNames constructed ""))
            else do
                current <- readIORef nameReference
                constructed <- evaluate (force (normalizeNames current))
                evaluate (length constructed)
        report "construction" "catalog" implementation 1 measurements
    putStrLn "# Additional retained heap bytes for an index; source names remain live."
    forM_ (["normalized-list", "radix-trie"] :: [String]) $ \implementation -> do
        sizes <- forM [1 .. samples] $ \_ ->
            if implementation == "radix-trie"
                then retainedBytes nameReference buildCompletionIndex
                else retainedBytes nameReference normalizeNames
        printf "# retained,%s,%d\n" implementation (median sizes)

unlessM :: IO Bool -> IO () -> IO ()
unlessM condition action = condition >>= \value -> unless value action

makeFixture :: Int -> Fixture
makeFixture count =
    ( [(command.slashName, command.slashAliases) | command <- fixtureCommands]
    , [Text.pack (printf "skill-%06d" number) | number <- [1 .. count]]
    )

fixtureCommands :: [SlashCommand]
fixtureCommands =
      [ command | command <- slashCommands
      , command.slashName /= "fast"
      , command.slashDialects == Nothing
      , null command.slashRequiredTools
      ]

fixtureNames :: Fixture -> [Text]
fixtureNames (commands, skills) = concatMap (\(name, aliases) -> name : aliases) commands <> skills

queryPrefixes :: Text -> [Text]
queryPrefixes name =
    ["/" <> Text.take count name | count <- [0 .. Text.length name]]
        <> ["/" <> Text.toUpper name, "///" <> name, "/" <> name <> "-absent"]

-- Preserve the replaced algorithm, including Text/String round trips and
-- slash attachment. NOINLINE prevents caller specialization of fixture data.
{-# NOINLINE originalCompletion #-}
originalCompletion :: Fixture -> String -> [String]
originalCompletion (commands, skills) word =
    let needle = Text.toLower (Text.dropWhile (== '/') (Text.pack word))
        names = concatMap (\(name, aliases) -> ("/" <> name) : map ("/" <>) aliases) commands
        skillNames = map ("/" <>) skills
    in filter (\name -> needle `Text.isPrefixOf` Text.drop 1 (Text.toLower (Text.pack name)))
        (map Text.unpack (names <> skillNames))

normalizeNames :: [Text] -> NormalizedNames
normalizeNames = map (\name -> (Text.toLower name, "/" <> name))

{-# NOINLINE normalizedCompletion #-}
normalizedCompletion :: NormalizedNames -> String -> [String]
normalizedCompletion names word =
    let needle = Text.toLower (Text.dropWhile (== '/') (Text.pack word))
    in [Text.unpack display | (key, display) <- names, needle `Text.isPrefixOf` key]

{-# NOINLINE indexedCompletion #-}
indexedCompletion :: CompletionIndex -> String -> [String]
indexedCompletion index word =
    let needle = Text.toLower (Text.dropWhile (== '/') (Text.pack word))
    in map Text.unpack (completeCommandNames index needle)

checksum :: [String] -> Int
checksum = foldl (\ !total name -> total + length name) 0

{-# NOINLINE runQueries #-}
runQueries :: (query -> IO Int) -> IORef [query] -> IO Int
runQueries action reference = do
    queries <- readIORef reference
    foldM (\ !total query -> action query >>= \result -> pure $! total + result) 0 queries

measure :: IO Int -> IO Measurement
measure action = do
    performGC
    before <- getRTSStats
    cpuStart <- getCPUTime
    wallStart <- getMonotonicTimeNSec
    result <- action >>= evaluate
    wallEnd <- getMonotonicTimeNSec
    cpuEnd <- getCPUTime
    -- Refresh allocated_bytes to include the final nursery. This GC is outside
    -- the timed interval, while GCs caused by the workload remain included.
    performGC
    after <- getRTSStats
    pure (Measurement (fromIntegral (cpuEnd - cpuStart) / 1e9)
        (fromIntegral (wallEnd - wallStart) / 1e6)
        (toInteger after.allocated_bytes - toInteger before.allocated_bytes) result)

retainedBytes :: NFData result => IORef [Text] -> ([Text] -> result) -> IO Integer
retainedBytes reference build = do
    performGC
    before <- getRTSStats
    names <- readIORef reference
    result <- evaluate (force (build names))
    bracket (newStablePtr result) freeStablePtr $ \_ -> do
        performGC
        after <- getRTSStats
        pure (toInteger after.gc.gcdetails_live_bytes - toInteger before.gc.gcdetails_live_bytes)

report :: String -> String -> String -> Int -> [Measurement] -> IO ()
report phase workload implementation operations measurements = do
    let checksums = [result | Measurement _ _ _ result <- measurements]
    expected <- case checksums of
        [] -> die "no benchmark measurements"
        first : _ -> pure first
    unless (all (== expected) checksums) (die "unstable benchmark checksum")
    printf "%s,%s,%s,%d,%.6f,%.6f,%d,%d\n" phase workload implementation operations
        (median [cpu | Measurement cpu _ _ _ <- measurements])
        (median [wall | Measurement _ wall _ _ <- measurements])
        (median [bytes | Measurement _ _ bytes _ <- measurements])
        expected

median :: Ord a => [a] -> a
median values = sort values !! (length values `div` 2)

-- Live menu comparison: the new function is production code, not a benchmark
-- reconstruction. The baseline below retains the original scan and scoring.
runMenuBenchmark :: Bool -> Int -> Int -> Int -> IO ()
runMenuBenchmark distributed skillCount iterations samples = do
    let skills = [SkillCommand name "Benchmark skill" Nothing "benchmark"
                 | name <- if distributed then distributedSkills skillCount
                            else snd (makeFixture skillCount)]
        source = (fixtureCommands, skills)
    _ <- evaluate (sum (map commandChecksum (fst source))
        + sum [Text.length skill.skillCommandName | skill <- skills])
    sourceReference <- newIORef source
    let catalog = menuCatalog source
    _ <- forceMenuIndex catalog
    catalogReference <- newIORef catalog
    let names = concatMap (\command -> command.slashName : command.slashAliases)
            fixtureCommands <> map (.skillCommandName) skills
        validationQueries = ["/", "/cpth", "/vwpl", "/ssn", "/0001", "/zzz", "/re/re"]
            <> concatMap queryPrefixes names
    forM_ validationQueries $ \query ->
        unless (Menu.commandMenu catalog query (Text.length query)
                == originalCommandMenu catalog query (Text.length query)) $
            die ("menu mismatch for " <> show query)
    printf "# commands=%d names=%d skills=%d samples=%d validation_queries=%d\n"
        (length fixtureCommands + skillCount) (length names) skillCount samples (length validationQueries)
    putStrLn "phase,workload,implementation,operations,median_cpu_ms,median_elapsed_ms,median_allocated_bytes,checksum"
    let workloads =
            [ ("mixed", ["/", "/c", "/co", "/copy", "/copy-", "/copy-path",
                         "/m", "/mo", "/MODEL", "/re", "/resume", "/does-not-exist",
                         "/skill-", "/skill-000", "/skill-000001", "/skill-999999"])
            , ("broad", ["/", "/c", "/re", "/s", "/skill-"])
            , ("selective", ["/model", "/copy-path", "/resume", "/skill-000001", "/missing"])
            , ("subsequence", ["/cpth", "/vwpl", "/ssn", "/0001", "/rld"])
            ]
        implementations = [("original-menu", originalCommandMenu), ("radix-menu", Menu.commandMenu)]
    forM_ workloads $ \(workload, queryCycle) -> do
        queries <- evaluate (force (take iterations (cycle queryCycle)))
        queryReference <- newIORef queries
        let action implementation query = do
                current <- readIORef catalogReference
                evaluate (menuChecksum (implementation current query (Text.length query)))
        forM_ implementations $ \(_, implementation) ->
            runQueries (action implementation) queryReference >> pure ()
        rows <- forM [1 .. samples] $ \sample ->
            forM (if odd sample then implementations else reverse implementations) $
                \(name, implementation) -> do
                    measurement <- measure (runQueries (action implementation) queryReference)
                    pure (name, measurement)
        forM_ implementations $ \(name, _) ->
            report "lookup" workload name iterations
                [measurement | row <- rows, (label, measurement) <- row, label == name]
    construction <- forM [1 .. samples] $ \_ -> measure $ do
        current <- readIORef sourceReference
        forceMenuIndex (menuCatalog current)
    report "construction" "menu-index" "radix-menu" 1 construction
    -- Include index creation in short realistic sessions, rather than claiming
    -- the warm-loop saving is free. Both functions receive fresh catalogs.
    forM_ [1, 10, 100] $ \count -> do
        queryReference <- newIORef =<< evaluate (force (take count
            (cycle ["/m", "/mo", "/model", "/c", "/cpth", "/re", "/resume"])))
        forM_ implementations $ \(name, implementation) -> do
            measurements <- forM [1 .. samples] $ \_ -> measure $ do
                current <- readIORef sourceReference
                freshReference <- newIORef (menuCatalogWithIndex (name == "radix-menu") current)
                runQueries (\query -> do
                    fresh <- readIORef freshReference
                    evaluate (menuChecksum (implementation fresh query (Text.length query)))) queryReference
            report "cold-session" "typing" name count measurements
    sizes <- forM [1 .. samples] $ \_ -> do
        performGC
        before <- getRTSStats
        current <- readIORef sourceReference
        let additional = Menu.buildMenuCompletionData (fst current) (snd current)
        _ <- evaluate (force (fst additional))
        _ <- evaluate (sum (map commandChecksum (IntMap.elems (snd additional))))
        bracket (newStablePtr additional) freeStablePtr $ \_ -> do
            performGC
            after <- getRTSStats
            pure (toInteger after.gc.gcdetails_live_bytes - toInteger before.gc.gcdetails_live_bytes)
    printf "# retained,menu-index,%d\n" (median sizes)

-- Alternative shape prevents the result depending solely on a synthetic
-- shared skill-NNNNNN prefix. Starts with repository-bundled skill names.
distributedSkills :: Int -> [Text]
distributedSkills count = take count $
    [ "add-model", "learn-about-user", "post-task-learning-review", "resume-claude"
    , "resume-codex", "resume-cursor", "resume-grok", "skill-installer"
    , "telegram-agent", "wait-for-ci"
    ] <> [ namespace <> "-" <> Text.pack (printf "%06d" number)
         | number <- [1 :: Int ..]
         , namespace <- ["repository-validate", "database-migrate", "deploy-service"
                        , "review-security", "generate-documentation", "test-integration"
                        , "analyze-performance", "configure-provider", "verify-release"
                        , "inspect-transaction"]]

menuCatalog :: ([SlashCommand], [SkillCommand]) -> SlashCatalog
menuCatalog = menuCatalogWithIndex True

menuCatalogWithIndex :: Bool -> ([SlashCommand], [SkillCommand]) -> SlashCatalog
menuCatalogWithIndex includeIndex (commands, skills) =
    let (index, entries)
            | includeIndex = Menu.buildMenuCompletionData commands skills
            | otherwise = (buildCompletionIndex [], IntMap.empty)
    in SlashCatalog
        { slashCatalogDialect = CodexDialect
        , slashCatalogToolNames = Set.empty
        , slashCatalogCommands = commands
        , slashCatalogCommandByName = Map.empty
        , slashCatalogSkills = skills
        , slashCatalogSkillByName = Map.empty
        , slashCatalogCompletionIndex = index
        , slashCatalogCompletionCommands = entries
        , slashCatalogModelIds = []
        }

forceMenuIndex :: SlashCatalog -> IO Int
forceMenuIndex catalog = do
    _ <- evaluate (force catalog.slashCatalogCompletionIndex)
    evaluate (sum (map commandChecksum (IntMap.elems catalog.slashCatalogCompletionCommands)))

commandChecksum :: SlashCommand -> Int
commandChecksum command = sum (map Text.length
    (command.slashName : command.slashUsage : command.slashSummary : command.slashAliases))
    + if command.slashTakesArguments then 1 else 0

menuChecksum :: Maybe SlashMenu -> Int
menuChecksum Nothing = 0
menuChecksum (Just menu) = menu.slashMenuReplaceStart + menu.slashMenuReplaceEnd
    + foldl' (\total row -> total + Text.length row.slashSuggestionDisplay
        + Text.length row.slashSuggestionReplacement + Text.length row.slashSuggestionSummary
        + sum row.slashSuggestionMatchPositions
        + if row.slashSuggestionTakesArguments then 1 else 0) 0 menu.slashMenuSuggestions

{-# NOINLINE originalCommandMenu #-}
originalCommandMenu :: SlashCatalog -> Text -> Int -> Maybe SlashMenu
originalCommandMenu catalog token replaceEnd =
    let query = Text.toLower (Text.drop 1 token)
        commands = catalog.slashCatalogCommands <> map Menu.skillAsSlashCommand catalog.slashCatalogSkills
        scored = mapMaybe (Menu.scoreCommand query) (zip [0 :: Int ..] commands)
        ordered
            | Text.null query = scored
            | otherwise = sortOn (\(score, order, _, _) -> (Down score, order)) scored
        rows =
            [ SlashSuggestion
                { slashSuggestionDisplay = "/" <> command.slashName
                , slashSuggestionReplacement = "/" <> command.slashName
                    <> if command.slashTakesArguments then " " else ""
                , slashSuggestionSummary = command.slashSummary
                , slashSuggestionTakesArguments = command.slashTakesArguments
                , slashSuggestionMatchPositions = map (+ 1) positions
                }
            | (_, _, command, positions) <- ordered
            ]
    in if Text.any (== '/') query || null rows then Nothing
        else Just (SlashMenu 0 replaceEnd rows)
