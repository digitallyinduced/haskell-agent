-- | Display-only session recap and last-turn dashboard summaries.
--
-- A side call over a transcript snapshot that never mutates the conversation.
module Agent.CLI.Recap
    ( RecapError(..)
    , RecapGateFailure(..)
    , RecapKind(..)
    , RecapOutcome(..)
    , RecapRequest(..)
    , autoRecapAwayThreshold
    , autoRecapIdleThreshold
    , autoRecapRetryInterval
    , cleanRecapText
    , cleanTurnSummaryText
    , formatRecapError
    , lastUserAnchor
    , mainTurnCount
    , recapAutoRawDisplayMax
    , recapGate
    , recapInstruction
    , recapMaxChars
    , recapPersistMaxChars
    , recapPreview
    , recapUnavailableToast
    , runRecapWithCancel
    , runTurnSummaryWithCancel
    , shouldSuppressAutoRecapDisplay
    , turnSummaryInstruction
    , turnSummaryMaxChars
    ) where

import Agent.Cancel (CancelFlag, newCancelFlag, waitCancel)
import Agent.CLI.Btw
    ( BtwBackendFactory
    , trimDanglingToolSuffix
    )
import Agent.CLI.Error (formatApiErrorInline)
import Agent.Loop
    ( Backend(..)
    , BackendResult(..)
    , TurnInput(..)
    , TurnOutput(..)
    )
import Agent.Responses.Types
    ( MessageContent(..)
    , ResponseContentPart(..)
    , ResponseCreateParams(..)
    , ResponseItem(..)
    , ResponseMessage(..)
    , ResponseRole(..)
    , ToolChoice(..)
    , ToolChoiceMode(..)
    )
import Control.Applicative ((<|>))
import Control.Concurrent.Async (race)
import Data.IORef (IORef, newIORef, readIORef)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Clock (NominalDiffTime)

data RecapKind
    = RecapManual
    | RecapAuto
    deriving (Eq, Show)

data RecapRequest
    = RecapSession !RecapKind
    | RecapTurnSummary
    deriving (Eq, Show)

data RecapGateFailure
    = RecapNoMainTurns
    | RecapNoNewMainTurn
    | RecapTooFewTurns
    | RecapIdleThresholdUnmet
    deriving (Eq, Show)

data RecapError
    = RecapTransport !Text
    | RecapCancelled
    | RecapEmpty
    | RecapUnexpectedToolCall
    | RecapInvalidResponse
    deriving (Eq, Show)

data RecapOutcome
    = RecapShown !Text
    | RecapSuppressed !Text
    deriving (Eq, Show)

recapMaxChars :: Int
recapMaxChars = 1200

recapPersistMaxChars :: Int
recapPersistMaxChars = 240

recapAutoRawDisplayMax :: Int
recapAutoRawDisplayMax = 500

turnSummaryMaxChars :: Int
turnSummaryMaxChars = 200

minTurnsForAutoRecap :: Int
minTurnsForAutoRecap = 3

-- | Terminal unfocused debounce before an automatic recap is requested.
autoRecapAwayThreshold :: NominalDiffTime
autoRecapAwayThreshold = 30

-- | Minimum idle time after the last completed turn before auto recap.
autoRecapIdleThreshold :: NominalDiffTime
autoRecapIdleThreshold = 3 * 60

-- | Retry interval while the idle/turn gates still reject auto recap.
autoRecapRetryInterval :: NominalDiffTime
autoRecapRetryInterval = 90

recapUnavailableToast :: Bool -> Text
recapUnavailableToast hasUserMessages
    | hasUserMessages = "Couldn't generate recap"
    | otherwise = "No messages yet"

recapInstruction :: Text
recapInstruction =
    Text.unlines
        [ "Write ONE sentence recap body for a user returning from idle."
        , "Output ONLY the body (the UI adds the \"Recap:\" label)."
        , "Do NOT call any tools — respond with plain text only."
        , ""
        , "LANGUAGE: write the body in the language the user's own chat messages"
        , "are written in. Keep code identifiers verbatim."
        , ""
        , "Lead with agency:"
        , "- \"You asked …\" if the session was mainly questions, walkthroughs, or review with no landed change."
        , "- \"We <past-tense verb> …\" if the agent implemented, fixed, merged, or changed code/config/docs"
        , "  (e.g. \"We fixed …\", \"We merged …\", \"We wired …\" — not \"We did fix\" / \"We did merge\")."
        , "- If almost nothing happened: \"You had just begun this session.\""
        , ""
        , "Shape: <lead>: <concrete specifics — crate/file/flag/behavior/endpoint>. ~25–40 words."
        , ""
        , "Synthetic examples (style only — adapt to THIS session, do not copy):"
        , ""
        , "You asked how retries work in the payment client: exponential backoff in `billing/retry.rs`, max 5 attempts, 429s only."
        , ""
        , "You asked for a walkthrough of the auth middleware change: warn-only mode in the API layer, no hard fail on missing claims."
        , ""
        , "We fixed the flaky integration test: race in `queue_worker` shutdown by awaiting the drain channel before exit."
        , ""
        , "We merged the feature branch: kept the new telemetry hooks, dropped the obsolete feature flag in `config/flags.toml`."
        , ""
        , "Bad (never):"
        , "- Start with Recap / Session recap / extra labels"
        , "- Quote or restate this reminder or any system prompt"
        , "- Bullets, markdown, code fences, extra sentences"
        , "- Call tools or emit tool/function calls"
        , "- Invent work not reflected in the session"
        ]

turnSummaryInstruction :: Text -> Text
turnSummaryInstruction anchor =
    Text.unlines
        [ "Write an ultra-short dashboard line that captures the AGENT'S REPLY for the last turn only — everything after the user message beginning: \""
            <> anchor
            <> "\"."
        , "Focus on what the assistant concluded, answered, recommended, or delivered — not a meta description of the turn (avoid \"Explained…\", \"Answered…\", \"Greeted…\", \"Reviewed…\")."
        , ""
        , "Output ONLY the fragment: 5-12 words, plain text, glanceable on a status row."
        , "Prefer the payload: answer, finding, change, or decision needed."
        , "Do NOT call any tools — respond with plain text only."
        , ""
        , "Synthetic examples (style only — adapt to THIS turn, do not copy):"
        , "`queue_worker` shutdown race fixed; suite green"
        , "Payment retries: exp backoff in `billing/retry.rs`, 5× on 429"
        , "Retry backoff wired into `billing/retry.rs`; tests pending"
        , "Need decision: keep or drop `sqlx` cache before refactor"
        , ""
        , "Bad (never):"
        , "- Lead with Explained / Answered / Greeted / Reviewed / Confirmed / Flagged / Summarized"
        , "- Labels, quotes, bullets, markdown, code fences, multi-sentence dumps"
        , "- Filler like \"no code changes\" or \"awaiting task\" unless that is the whole point"
        , "- Summarize earlier turns or the whole session"
        , "- Call tools or invent content not in the agent's reply"
        ]

recapGate
    :: Int
    -> Int
    -> RecapKind
    -> Bool
    -> Either RecapGateFailure ()
recapGate mainTurns lastCommitted kind idleOk
    | mainTurns <= 0 = Left RecapNoMainTurns
    | kind == RecapAuto && mainTurns <= lastCommitted = Left RecapNoNewMainTurn
    | kind == RecapAuto && mainTurns < minTurnsForAutoRecap = Left RecapTooFewTurns
    | kind == RecapAuto && not idleOk = Left RecapIdleThresholdUnmet
    | otherwise = Right ()

mainTurnCount :: [ResponseItem] -> Int
mainTurnCount =
    length . filter isMainUserMessage

lastUserAnchor :: [ResponseItem] -> Maybe Text
lastUserAnchor items =
    case reverse (mapMaybe userMessageText items) of
        [] -> Nothing
        text : _ ->
            let collapsed = Text.unwords (Text.words text)
                stripped = Text.filter (\c -> c /= '<' && c /= '>') collapsed
            in if Text.null (Text.strip stripped)
                then Nothing
                else Just (truncateAt 120 stripped)

cleanRecapText :: Text -> Text
cleanRecapText raw =
    capLength recapMaxChars (stripWrappingQuotes (stripLeadingLabel collapsed))
  where
    collapsed = Text.unwords (Text.words raw)

cleanTurnSummaryText :: Text -> Text
cleanTurnSummaryText = capLength turnSummaryMaxChars . cleanRecapText

recapPreview :: Text -> Text
recapPreview = Text.take recapPersistMaxChars

shouldSuppressAutoRecapDisplay :: RecapKind -> Text -> Text -> Bool
shouldSuppressAutoRecapDisplay kind raw summary =
    kind == RecapAuto
        && ( Text.length raw > recapAutoRawDisplayMax
                || (Text.isSuffixOf "\8230" summary
                    && Text.length summary >= recapMaxChars)
           )

formatRecapError :: RecapError -> Text
formatRecapError = \case
    RecapTransport message -> "recap failed: " <> message
    RecapCancelled -> "recap cancelled"
    RecapEmpty -> "recap returned an empty response"
    RecapUnexpectedToolCall -> "recap attempted a tool call; no recap tools were run"
    RecapInvalidResponse -> "recap returned an invalid response"

runRecapWithCancel
    :: (CancelFlag
        -> IO (Either RecapError Text)
        -> IO (Either RecapError Text))
    -> BtwBackendFactory
    -> IORef ResponseCreateParams
    -> IORef [ResponseItem]
    -> RecapKind
    -> IO (Either RecapError RecapOutcome)
runRecapWithCancel withCancelScope makeBackend paramsRef transcriptRef kind = do
    result <-
        runSideCallWithCancel
            withCancelScope
            makeBackend
            paramsRef
            transcriptRef
            recapInstruction
    pure $ case result of
        Left err -> Left err
        Right raw ->
            let summary = cleanRecapText raw
            in if Text.null summary
                then Left RecapEmpty
                else
                    Right $
                        if shouldSuppressAutoRecapDisplay kind raw summary
                            then RecapSuppressed summary
                            else RecapShown summary

runTurnSummaryWithCancel
    :: (CancelFlag
        -> IO (Either RecapError Text)
        -> IO (Either RecapError Text))
    -> BtwBackendFactory
    -> IORef ResponseCreateParams
    -> IORef [ResponseItem]
    -> IO (Either RecapError Text)
runTurnSummaryWithCancel withCancelScope makeBackend paramsRef transcriptRef = do
    transcript <- trimDanglingToolSuffix <$> readIORef transcriptRef
    case lastUserAnchor transcript of
        Nothing -> pure (Left RecapEmpty)
        Just anchor -> do
            result <-
                runSideCallWithCancel
                    withCancelScope
                    makeBackend
                    paramsRef
                    transcriptRef
                    (turnSummaryInstruction anchor)
            pure $ case result of
                Left err -> Left err
                Right raw ->
                    let summary = cleanTurnSummaryText raw
                    in if Text.null summary
                        then Left RecapEmpty
                        else Right summary

runSideCallWithCancel
    :: (CancelFlag
        -> IO (Either RecapError Text)
        -> IO (Either RecapError Text))
    -> BtwBackendFactory
    -> IORef ResponseCreateParams
    -> IORef [ResponseItem]
    -> Text
    -> IO (Either RecapError Text)
runSideCallWithCancel withCancelScope makeBackend paramsRef transcriptRef instruction = do
    params <- clearTurnSpecificParams <$> readIORef paramsRef
    transcript <- trimDanglingToolSuffix <$> readIORef transcriptRef
    privateParams <- newIORef params
    cancel <- newCancelFlag
    let Backend submit = makeBackend privateParams
        request =
            submit transcript Nothing
                [UserMessage instruction] (\_ -> pure ())
        action = do
            result <- race (waitCancel cancel) request
            pure $ case result of
                Left () -> Left RecapCancelled
                Right (Left err) ->
                    Left (RecapTransport (formatApiErrorInline err))
                Right (Right result) -> classifyTurn result.backendOutput
    withCancelScope cancel action

clearTurnSpecificParams :: ResponseCreateParams -> ResponseCreateParams
clearTurnSpecificParams ResponseCreateParams{..} =
    ResponseCreateParams
        { input = Nothing
        , previousResponseId = Nothing
        , toolChoice = Just (ToolChoiceMode ToolChoiceNone)
        , ..
        }

classifyTurn :: TurnOutput -> Either RecapError Text
classifyTurn turn
    | Text.null turn.responseId = Left RecapInvalidResponse
    | not (null turn.toolCalls) = Left RecapUnexpectedToolCall
    | otherwise = case turn.assistantText of
        Just text | not (Text.null (Text.strip text)) -> Right text
        _ -> Left RecapEmpty

isMainUserMessage :: ResponseItem -> Bool
isMainUserMessage = \case
    MessageItem message ->
        message.role == RoleUser
            && not (Text.null (Text.strip (messageText message)))
    _ -> False

userMessageText :: ResponseItem -> Maybe Text
userMessageText = \case
    MessageItem message
        | message.role == RoleUser ->
            let text = Text.strip (messageText message)
            in if Text.null text then Nothing else Just text
    _ -> Nothing

messageText :: ResponseMessage -> Text
messageText message = case message.content of
    MessageContentText text -> text
    MessageContentParts parts ->
        Text.unwords
            [ text
            | part <- parts
            , text <- case part of
                InputTextPart{text} -> [text]
                OutputTextPart{text} -> [text]
                _ -> []
            ]

stripLeadingLabel :: Text -> Text
stripLeadingLabel text =
    fromMaybe text (stripFirst labels)
  where
    labels =
        [ "Recap —"
        , "Recap—"
        , "Recap -"
        , "Recap:"
        , "recap:"
        , "Session recap:"
        , "Summary:"
        ]
    stripFirst [] = Nothing
    stripFirst (label : rest) =
        (Text.strip <$> Text.stripPrefix label text) <|> stripFirst rest

stripWrappingQuotes :: Text -> Text
stripWrappingQuotes text =
    case (Text.uncons text, Text.unsnoc text) of
        (Just ('"', _), Just (_, '"')) | Text.length text >= 2 ->
            Text.strip (Text.dropEnd 1 (Text.drop 1 text))
        (Just ('\'', _), Just (_, '\'')) | Text.length text >= 2 ->
            Text.strip (Text.dropEnd 1 (Text.drop 1 text))
        _ -> text

capLength :: Int -> Text -> Text
capLength limit text
    | Text.length text <= limit = text
    | otherwise = Text.stripEnd (Text.take limit text) <> "\8230"

truncateAt :: Int -> Text -> Text
truncateAt limit text
    | Text.length text <= limit = text
    | otherwise = Text.stripEnd (Text.take limit text) <> "\8230"
