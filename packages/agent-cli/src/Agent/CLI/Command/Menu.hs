-- | Command-menu candidate selection and unchanged fuzzy ranking.
module Agent.CLI.Command.Menu
    ( buildMenuCompletionData
    , commandMenu
    , skillAsSlashCommand
    , scoreCommand
    , fuzzyMatch
    ) where

import Agent.CLI.Command.Types
import Agent.CLI.Command.CompletionIndex
import qualified Data.IntMap.Strict as IntMap
import Data.List (sortOn)
import Data.Maybe (mapMaybe)
import Data.Ord (Down(..))
import Data.Text (Text)
import qualified Data.Text as Text

buildMenuCompletionData :: [SlashCommand] -> [SkillCommand]
    -> (CompletionIndex, IntMap.IntMap SlashCommand)
buildMenuCompletionData commands skills =
    ( buildGroupedCompletionIndex
        (map (\command -> command.slashName : command.slashAliases) commands
            <> map (\skill -> [skill.skillCommandName]) skills)
    , IntMap.fromDistinctAscList $
        zip [0 ..] (commands <> map skillAsSlashCommand skills)
    )

commandMenu :: SlashCatalog -> Text -> Int -> Maybe SlashMenu
commandMenu catalog token replaceEnd =
    let query = Text.toLower (Text.drop 1 token)
        commands =
            catalog.slashCatalogCommands
                <> map skillAsSlashCommand catalog.slashCatalogSkills
        candidates
            | Text.null query = zip [0 :: Int ..] commands
            | otherwise = IntMap.toAscList $
                IntMap.restrictKeys catalog.slashCatalogCompletionCommands
                    (matchingCommandGroups catalog.slashCatalogCompletionIndex query)
        scored = mapMaybe (scoreCommand query) candidates
        ordered
            | Text.null query = scored
            | otherwise = sortOn (\(score, order, _, _) -> (Down score, order)) scored
        rows =
            [ SlashSuggestion
                { slashSuggestionDisplay = "/" <> command.slashName
                , slashSuggestionReplacement =
                    "/" <> command.slashName
                        <> if command.slashTakesArguments then " " else ""
                , slashSuggestionSummary = command.slashSummary
                , slashSuggestionTakesArguments = command.slashTakesArguments
                , slashSuggestionMatchPositions = map (+ 1) positions
                }
            | (_, _, command, positions) <- ordered
            ]
    in if Text.any (== '/') query || null rows
        then Nothing
        else Just SlashMenu
            { slashMenuReplaceStart = 0
            , slashMenuReplaceEnd = replaceEnd
            , slashMenuSuggestions = rows
            }

skillAsSlashCommand :: SkillCommand -> SlashCommand
skillAsSlashCommand skill =
    SlashCommand
        { slashName = skill.skillCommandName
        , slashAliases = []
        , slashUsage =
            "/"
                <> skill.skillCommandName
                <> maybe "" (" " <>) skill.skillCommandArgumentHint
        , slashSummary =
            skill.skillCommandSummary <> " · skill · " <> skill.skillCommandSource
        , slashTakesArguments = True
        , slashDialects = Nothing
        , slashRequiredTools = []
        }

scoreCommand
    :: Text
    -> (Int, SlashCommand)
    -> Maybe (Int, Int, SlashCommand, [Int])
scoreCommand query (order, command)
    | Text.null query = Just (0, order, command, [])
    | otherwise =
        case sortOn (Down . fst) $
            mapMaybe (fuzzyMatch query . Text.toLower)
                (command.slashName : command.slashAliases) of
            [] -> Nothing
            (score, positions) : _ ->
                Just (score, order, command, positions)

-- | Small deterministic fuzzy matcher for the short command catalog.
fuzzyMatch :: Text -> Text -> Maybe (Int, [Int])
fuzzyMatch needle haystack
    | Text.null needle = Just (0, [])
    | needle == haystack =
        Just (10000, [0 .. Text.length needle - 1])
    | needle `Text.isPrefixOf` haystack =
        Just (8000 - Text.length haystack, [0 .. Text.length needle - 1])
    | otherwise = do
        positions@(firstPos:_) <- subsequencePositions needle haystack
        lastPos <- safeLast positions
        let
            gaps = lastPos - firstPos + 1 - length positions
            boundaryBonus =
                sum
                    [ if pos == 0 || Text.index haystack (pos - 1) == '-'
                        then 40
                        else 0
                    | pos <- positions
                    ]
        pure
            ( 4000
                + boundaryBonus
                - firstPos * 10
                - gaps * 20
                - Text.length haystack
            , positions
            )
  where
    safeLast = \case
        [] -> Nothing
        first : rest -> Just (foldl (\_ item -> item) first rest)

subsequencePositions :: Text -> Text -> Maybe [Int]
subsequencePositions needle haystack =
    go 0 (Text.unpack needle) (Text.unpack haystack)
  where
    go _ [] _ = Just []
    go _ _ [] = Nothing
    go index wanted@(n:ns) (h:hs)
        | n == h = (index :) <$> go (index + 1) ns hs
        | otherwise = go (index + 1) wanted hs
