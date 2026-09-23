{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

-- Validate audit identifiers and citations, not product behavior or coverage.
import Control.Monad (foldM, forM, unless)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.List (sort)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import System.Directory (canonicalizePath, createDirectoryIfMissing, doesFileExist, listDirectory)
import System.Environment (getArgs, getEnv)
import System.Exit (die, exitFailure)
import System.FilePath ((</>), takeDirectory, takeExtension)
import System.IO (stderr)
import System.IO.Temp (withTempDirectory)
import Text.Regex.TDFA ((=~))

statuses, matrices :: [Text]
statuses = ["Conflict", "Covered", "Missing", "Partial"]
matrices = ["cli.md", "configuration.md", "interfaces.md", "tools.md", "website.md"]

data Audit = Audit
    { identifiers :: Set.Set Text
    , references :: Set.Set (FilePath, Int, Int)
    , errors :: [Text]
    , total :: Map.Map Text Int
    }

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> verify
        ["--self-test"] -> selfTest
        _ -> die "Usage: runghc docs/audit/VerifyAudit.hs [--self-test]"

verify :: IO ()
verify = do
    directory <- takeDirectory <$> canonicalizePath __FILE__
    let repository = takeDirectory (takeDirectory directory)
    missing <- fmap concat $ forM matrices $ \name -> do
        exists <- doesFileExist (directory </> Text.unpack name)
        pure ["Missing required matrix: " <> name | not exists]
    documents <- sort . filter ((== ".md") . takeExtension) <$> listDirectory directory
    final <- foldM (checkDocument repository directory) (Audit Set.empty Set.empty missing Map.empty) documents
    Text.putStrLn ("Total: " <> countsText (total final) " audit rows; ")
    Text.putStrLn ("Checked " <> shown (Set.size (references final)) <> " distinct repository line citations.")
    unless (null (errors final)) $ do
        Text.hPutStrLn stderr (Text.unlines (errors final))
        exitFailure
    putStrLn "Audit artifact checks passed. This is not a product behavior test."

checkDocument :: FilePath -> FilePath -> Audit -> FilePath -> IO Audit
checkDocument repository directory audit name = do
    rows <- Text.lines <$> Text.readFile (directory </> name)
    (result, counts) <- foldM checkLine (audit, Map.empty) (zip [1 :: Int ..] rows)
    if Map.null counts then
        pure $ if Text.pack name `elem` matrices
            then result { errors = errors result <> [Text.pack name <> ": no auditable rows found"] }
            else result
    else do
        Text.putStrLn (Text.pack name <> ": " <> countsText counts " rows; ")
        pure result { total = Map.unionWith (+) (total result) counts }
  where
    checkLine (state, counts) (number, line) = do
        let location = Text.pack name <> ":" <> shown number <> ": "
            parts = Text.splitOn "|" line
            cells = map (Text.dropAround (== '`') . Text.strip) (drop 1 (take (length parts - 1) parts))
            (next, counts') = case cells of
                identifier : _ | Text.unpack identifier =~ ("^[A-Z]+(-[A-Z]+)*-[A-Z]*[0-9]+$" :: String) ->
                    let found = filter (`elem` statuses) cells
                        problems = [location <> "duplicate " <> identifier | Set.member identifier (identifiers state)]
                            <> [location <> "expected one coverage status" | length found /= 1]
                        counts'' = case found of [status] -> Map.insertWith (+) status 1 counts; _ -> counts
                    in (state { identifiers = Set.insert identifier (identifiers state), errors = errors state <> problems }, counts'')
                _ -> (state, counts)
            matches = Text.unpack line =~ citationPattern :: [[String]]
        checked <- foldM (checkReference location) next matches
        pure (checked, counts')
    checkReference location state match = case match of
        [whole, path, _, first, _, lastLine] -> do
            let start = read first
                end = if null lastLine then start else read lastLine
            exists <- doesFileExist (repository </> path)
            problem <- if not exists
                then pure [location <> "missing " <> Text.pack path]
                else do
                    count <- length . Text.lines <$> Text.readFile (repository </> path)
                    pure [location <> "invalid range " <> Text.pack whole | not (1 <= start && start <= end && end <= count)]
            pure state { references = Set.insert (path, start, end) (references state), errors = errors state <> problem }
        _ -> die "Internal error: unexpected citation match"

citationPattern :: String
citationPattern = "((packages|docs|nix|scripts|tests)/[^][[:space:]`|;,:()#]+|flake\\.nix|README\\.md):([0-9]+)([-–]([0-9]+))?"

countsText :: Map.Map Text Int -> Text -> Text
countsText counts separator = shown (sum (Map.elems counts)) <> separator
    <> Text.intercalate ", " [status <> "=" <> shown (Map.findWithDefault 0 status counts) | status <- statuses]

shown :: Show a => a -> Text
shown = Text.pack . show

selfTest :: IO ()
selfTest = do
    temporary <- getEnv "TMPDIR"
    withTempDirectory temporary "documentation-audit-tests-" $ \directory -> do
        let empty = Audit Set.empty Set.empty [] Map.empty
            assert condition message = unless condition (die ("Audit self-test failed: " <> message))
        createDirectoryIfMissing True (directory </> "packages")
        Text.writeFile (directory </> "packages/Example.hs") "one\ntwo\nthree\n"
        Text.writeFile (directory </> "valid.md") $ Text.unlines
            [ "| `CLI-A15` | Covered | `packages/Example.hs:1–3` |"
            , "| IF-HTTP-01 | Partial | (packages/Example.hs:2-3) |"
            , "| Summary `IF-HTTP-01–02` | Partial | Not an inventory ID |"
            ]
        valid <- checkDocument directory directory empty "valid.md"
        assert (null (errors valid)) "valid identifiers and citations rejected"
        assert (Set.size (identifiers valid) == 2) "summary counted as inventory"
        assert (references valid == Set.fromList [("packages/Example.hs", 1, 3), ("packages/Example.hs", 2, 3)]) "citation delimiters or ranges"
        assert (total valid == Map.fromList [("Covered", 1), ("Partial", 1)]) "status totals"
        Text.writeFile (directory </> "invalid.md") $ Text.unlines
            [ "| CLI-A15 | Covered | `packages/Example.hs:0` |"
            , "| CLI-A15 | Covered | Partial |"
            , "| TOOL-007 | Unknown | `packages/Missing.hs:1` |"
            , "| TOOL-008 | Missing | `packages/Example.hs:3-2` |"
            , "| TOOL-009 | Conflict | `packages/Example.hs:4` |"
            ]
        invalid <- checkDocument directory directory empty "invalid.md"
        assert (length (errors invalid) == 7) "duplicate IDs, statuses, missing files or invalid ranges not rejected"
        Text.writeFile (directory </> "cli.md") "No inventory rows\n"
        missing <- checkDocument directory directory empty "cli.md"
        assert (errors missing == ["cli.md: no auditable rows found"]) "empty required matrix"
    putStrLn "Audit self-tests passed."
