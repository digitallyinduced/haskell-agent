{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

-- Materialize reviewed summary ranges, retaining each row's historical context.
import Control.Monad (unless)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import System.Directory (canonicalizePath)
import System.Environment (getArgs)
import System.Exit (die)
import System.FilePath ((</>), takeDirectory)
import Text.Printf (printf)
import Text.Regex.TDFA ((=~))

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> materializeFile
        ["--self-test"] -> selfTest
        _ -> die "Usage: runghc docs/scripts/MaterializeInterfaceAudit.hs [--self-test]"

materializeFile :: IO ()
materializeFile = do
    docs <- takeDirectory . takeDirectory <$> canonicalizePath __FILE__
    let path = docs </> "audit/interfaces.md"
    rows <- Text.lines <$> Text.readFile path
    let reviewed = Map.fromList (concatMap summary rows)
        materialized = map (materialize reviewed) rows
        count = length (filter fst materialized)
    unless (count == 109) $
        die ("Expected 109 inventory rows; found " <> show count <> "; no changes written")
    Text.writeFile path (Text.unlines (map (normalizeSummary . snd) materialized))
    putStrLn ("Materialized " <> show count <> " reviewed inventory rows")

summary :: Text -> [(Text, (Text, Text))]
summary line = case Text.unpack line =~ patternText :: [[String]] of
    [[_, _, prefix, first, _, lastNumber, status, evidence]] ->
        [(Text.pack (prefix <> "-" <> printf "%02d" number), (Text.pack status, Text.pack evidence))
        | number <- [read first :: Int .. read (if null lastNumber then first else lastNumber)]]
    _ -> []
  where
    patternText = "^\\| (Summary )?`?(IF-[A-Z]+)-([0-9]+)(–([0-9]+))?`? \\| (Covered|Partial|Missing|Conflict) \\| (.*) \\|$" :: String

materialize :: Map.Map Text (Text, Text) -> Text -> (Bool, Text)
materialize reviewed line = case cells line of
    [identifier, surface, source, _, prose]
        | Text.unpack line =~ ("^\\| IF-[A-Z]+-[0-9]+ \\|" :: String)
        , Just (status, evidence) <- Map.lookup identifier reviewed ->
            let marker = " Historical audit context: "
                (_, suffix) = Text.breakOn marker prose
                original = if Text.null suffix then prose else Text.drop (Text.length marker) suffix
            in (True, "| " <> Text.intercalate " | " [identifier, surface, source, status, evidence <> marker <> original] <> " |")
    _ -> (False, line)

normalizeSummary :: Text -> Text
normalizeSummary line
    | length (Text.splitOn "|" line) /= 5 = line
    | otherwise = case Text.unpack line =~ ("^\\| (Summary )?`?(IF-[A-Z]+-[0-9]+(–[0-9]+)?)`? \\|" :: String) :: (String, String, String, [String]) of
        (_, matched, rest, [_, identifier, _]) | not (null matched) ->
            "| Summary `" <> Text.pack identifier <> "` |" <> Text.pack rest
        _ -> line

cells :: Text -> [Text]
cells line = let parts = Text.splitOn "|" line
    in map Text.strip (drop 1 (take (length parts - 1) parts))

selfTest :: IO ()
selfTest = do
    let assert condition message = unless condition (die ("Materializer self-test failed: " <> message))
        summaryLine = "| IF-HTTP-01–03 | Covered | reviewed evidence |"
        reviewed = Map.fromList (summary summaryLine)
        original = "| IF-HTTP-02 | surface | source | Missing | original evidence |"
        expected = "| IF-HTTP-02 | surface | source | Covered | reviewed evidence Historical audit context: original evidence |"
    assert (Map.keys reviewed == ["IF-HTTP-01", "IF-HTTP-02", "IF-HTTP-03"]) "range expansion"
    assert (summary (normalizeSummary summaryLine) == summary summaryLine) "normalized summaries"
    assert (materialize reviewed original == (True, expected)) "inventory rewrite or historical context"
    assert (materialize reviewed expected == (True, expected)) "idempotence"
    assert (materialize reviewed "not a table" == (False, "not a table")) "unrelated text"
    assert (null (summary "| IF-HTTP-01 | unknown | evidence |")) "invalid status accepted"
    assert (normalizeSummary (normalizeSummary summaryLine) == normalizeSummary summaryLine) "summary idempotence"
    putStrLn "Materializer self-tests passed."
