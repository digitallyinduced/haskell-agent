-- | Conversation pull-request association detection and incremental indexing.
module Agent.CLI.Session.PullRequest
    ( pullRequestURLs
    , conversationPullRequestURLs
    , sessionTurnPullRequestURL
    , sessionTurnPullRequestURLs
    , advanceSessionPullRequestIndex
    ) where

import Agent.CLI.Session.Types (SessionTurn(..))
import Agent.Responses.Types
    ( ResponseItem(..)
    , FunctionCall(..)
    , FunctionCallOutput(..)
    , CustomToolCall(..)
    , CustomToolCallOutput(..)
    )
import Control.Applicative ((<|>))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Char (isAlphaNum)
import Data.Int (Int64)
import Data.List (foldl', nub)
import qualified Data.Map.Strict as Map
import Data.Maybe (listToMaybe, maybeToList)
import Data.Text (Text)
import qualified Data.Text as Text
import Text.Read (readMaybe)

-- | Advance a recency-ordered cache using ascending, bounded history pages.
-- A current cache performs no history reads. Persist each completed page so
-- interrupted indexing resumes without repeating the completed prefix.
advanceSessionPullRequestIndex
    :: Monad m
    => (Int64 -> Int64 -> m (Either Text [(Int64, [Text])]))
    -> (Int64 -> [Text] -> m (Either Text ()))
    -> Int64
    -> Maybe (Int64, [Text])
    -> m (Either Text [Text])
advanceSessionPullRequestIndex loadPage savePage total cached =
    case cached of
        Just (cursor, urls) | cursor >= 0 && cursor <= total -> scan cursor urls
        _ -> scan 0 []
  where
    scan cursor urls
        | cursor >= total = pure (Right urls)
        | otherwise = loadPage cursor total >>= \case
            Left err -> pure (Left err)
            Right [] -> pure (Left "incomplete PR association history")
            Right turns
                | map fst turns /= [cursor .. cursor + fromIntegral (length turns) - 1]
                    || fst (last turns) >= total ->
                    pure (Left "invalid PR association history page")
                | otherwise -> do
                    let next = 1 + fst (last turns)
                        associated = nub (concatMap snd (reverse turns) <> urls)
                    savePage next associated >>= \case
                        Left err -> pure (Left err)
                        Right () -> scan next associated

-- Only canonical public GitHub PR identities are accepted. Never pass arbitrary
-- conversation URLs to gh (or to a shell). Strip Markdown/query/fragment tails.
pullRequestURLs :: Text -> [Text]
pullRequestURLs = nub . go
  where
    go input = case Text.breakOn "https://github.com/" input of
        (_, rest) | Text.null rest -> []
        (prefix, rest) ->
            let suffix = Text.drop 19 rest
                token = Text.takeWhile (\c -> isAlphaNum c || c `elem` ("-._/" :: String)) suffix
                remaining = Text.drop (Text.length token) suffix
                validBoundary = Text.null prefix || not (isAlphaNum (Text.last prefix) || Text.last prefix `elem` ("/_-." :: String))
                found = case Text.splitOn "/" token of
                    owner : repo : "pull" : number : _
                        | validBoundary, validName owner, validName repo
                        , Just n <- readMaybe (Text.unpack number) :: Maybe Int
                        , n > 0, Text.all (\c -> c >= '0' && c <= '9') number ->
                            ["https://github.com/" <> Text.toCaseFold owner <> "/" <> Text.toCaseFold repo <> "/pull/" <> Text.pack (show n)]
                    _ -> []
            in found <> go remaining
    validName name = not (Text.null name) && name /= "." && name /= ".."
        && Text.all (\c -> c < '\128' && (isAlphaNum c || c `elem` ("-_." :: String))) name

-- Evidence stays local to a paragraph. Quoted references/examples do not become
-- associations. Tools must be paired with a PR operation; raw search output alone
-- cannot attach every PR it happens to mention.
conversationPullRequestURLs :: Text -> Maybe Text -> [Aeson.Value] -> [Text]
conversationPullRequestURLs user assistant items = nub $
    directUser <> concatMap evidence (maybe [] pure assistant <> [user] <> messages) <> toolURLs
  where
    directUser = case pullRequestURLs user of
        [url] | Text.toCaseFold (Text.strip user) == url -> [url]
        _ -> []
    evidence = paragraphs . Text.splitOn "\n\n"
    paragraphs (header : list : rest)
        | null (pullRequestURLs header)
        , any (`elem` ["pr", "prs", "pull"]) (Text.words (Text.map wordCharacter (Text.toCaseFold header)))
        , any (`Text.isPrefixOf` Text.stripStart list) ["- ", "* ", "1. "] =
            paragraph (header <> "\n" <> list) <> paragraphs rest
    paragraphs (content : rest) = paragraph content <> paragraphs rest
    paragraphs [] = []
    wordCharacter c = if isAlphaNum c then c else ' '
    paragraph content
        | any (`Text.isInfixOf` lower) ["for reference", "example", "unrelated", "see also", "beispiel", "referenz"] = []
        | any (`Text.isInfixOf` lower) ["created", "opened", "merged", "review", "fix", "address", "implement", "update", "check", "work on", "look at", "erstellt", "gemerg", "beheb", "prüf", "bearbeit"] =
            pullRequestURLs (Text.unlines (filter (not . Text.isPrefixOf ">" . Text.stripStart) (Text.lines content)))
        | otherwise = []
      where lower = Text.toCaseFold (Text.unwords (filter (null . pullRequestURLs) (Text.words content)))
    messages = [Text.intercalate "\n" (strings content) | Aeson.Object o <- items
        , KeyMap.lookup "type" o == Just (Aeson.String "message")
        , Just (Aeson.String role) <- [KeyMap.lookup "role" o], role `elem` ["user", "assistant"]
        , Just content <- [KeyMap.lookup "content" o]]
    calls = [callId | Aeson.Object o <- items
        , Just (Aeson.String kind) <- [KeyMap.lookup "type" o]
        , kind `elem` ["function_call", "custom_tool_call"]
        , Just (Aeson.String callId) <- [KeyMap.lookup "call_id" o]
        , let body = Text.toCaseFold (Text.intercalate " " (strings (Aeson.Object o)))
        , any (`Text.isInfixOf` body) ["gh pr create", "gh pr checkout", "gh pr merge", "gh pr review", "create_pull_request"]]
    toolURLs = concat [pullRequestURLs (Text.intercalate "\n" (strings output))
        | Aeson.Object o <- items
        , Just (Aeson.String kind) <- [KeyMap.lookup "type" o]
        , kind `elem` ["function_call_output", "custom_tool_call_output"]
        , Just (Aeson.String callId) <- [KeyMap.lookup "call_id" o], callId `elem` calls
        , Just output <- [KeyMap.lookup "output" o]]
    strings (Aeson.String value) = [value]
    strings (Aeson.Array values) = foldMap strings values
    strings (Aeson.Object values) = foldMap strings values
    strings _ = []

sessionTurnPullRequestURL :: SessionTurn -> Maybe Text
sessionTurnPullRequestURL turn =
    firstURL "" turn.turnAssistantText []
        <|> snd (foldl' inspectItem (Map.empty, Nothing)
            (turn.turnItems <> turn.turnDisplayItems))
        <|> firstURL turn.turnUserText Nothing []
  where
    firstURL :: Text -> Maybe Text -> [ResponseItem] -> Maybe Text
    firstURL user assistant items =
        listToMaybe (conversationPullRequestURLs user assistant (map Aeson.toJSON items))
    -- The shared detector returns a set of associations, not recency order.
    -- Keep the newest message/output evidence, pairing each output only with
    -- its preceding call, so an old PR in the user prompt cannot replace the
    -- PR just created during this turn.
    inspectItem current@(calls, latest) item =
        case item of
            FunctionCallItem call ->
                (Map.insert call.callId item calls, latest)
            CustomToolCallItem call ->
                (Map.insert call.callId item calls, latest)
            FunctionCallOutputItem output ->
                inspectOutput output.callId
            CustomToolCallOutputItem output ->
                inspectOutput output.callId
            MessageItem _ ->
                (calls, firstURL "" Nothing [item] <|> latest)
            _ -> current
      where
        inspectOutput callId =
            case Map.lookup callId calls of
                Nothing -> current
                Just call ->
                    (calls, firstURL "" Nothing [call, item] <|> latest)

-- | Retain every association, ordering the current one first.
sessionTurnPullRequestURLs :: SessionTurn -> [Text]
sessionTurnPullRequestURLs turn = nub $
    maybeToList (sessionTurnPullRequestURL turn)
        <> conversationPullRequestURLs turn.turnUserText turn.turnAssistantText
            (map Aeson.toJSON (turn.turnItems <> turn.turnDisplayItems))
