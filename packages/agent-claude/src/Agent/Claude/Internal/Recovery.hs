-- | Bounded, attributed recovery context from complete validated SDK records.
-- This is deliberately not a transcript: it cannot replay a tool call, and
-- never consumes the UI's speculative text/thinking deltas.
module Agent.Claude.Internal.Recovery
    ( RecoveryState
    , emptyRecovery
    , recordRecoveryMessage
    , retractRecoveryMessages
    , renderRecovery
    ) where

import Agent.Json (rawJsonBytes)
import qualified Agent.Json.Decode as Json
import Claude.Agent.SDK.Types
    ( AssistantMessage(..)
    , ContentBlock(..)
    , Message(..)
    , ToolResultContent(..)
    , UserMessage(..)
    , messageHasParentToolUseId
    , messageUuid
    )
import qualified Data.ByteString as ByteString
import Data.Foldable (foldl')
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as Text

-- Newest first. Both the record count and every retained text are bounded.
-- UUIDs live only as long as their records: no unbounded deduplication set.
newtype RecoveryState = RecoveryState [(Maybe Text, Text)]
    deriving (Eq, Show)

emptyRecovery :: RecoveryState
emptyRecovery = RecoveryState []

recordRecoveryMessage :: Message -> RecoveryState -> RecoveryState
recordRecoveryMessage message state
    | messageHasParentToolUseId message = state
    | otherwise = case message of
        MessageAssistant assistant ->
            appendRecords message
                (mapMaybe assistantBlock assistant.content)
                (retractRecoveryMessages assistant.supersedes state)
        MessageUser user ->
            appendRecords message (mapMaybe resultBlock user.content) state
        _ -> state

appendRecords :: Message -> [Text] -> RecoveryState -> RecoveryState
appendRecords _ [] state = state
appendRecords message _ state
    -- Retaining an untrackable UUID as 'Nothing' would prevent a later
    -- retraction from removing its record. Omit it instead of losing identity.
    | Just identifier <- messageUuid message
    , Text.length identifier > 256 = state
appendRecords message records state@(RecoveryState previous) =
    let identifier = messageUuid message >>= boundedIdentifier
        alreadyRetained = case identifier of
            Nothing -> False
            Just _ -> any ((== identifier) . fst) previous
    -- Force the copied UUID as well as the excerpts: a lazy 'messageUuid'
    -- projection would otherwise retain the complete message envelope.
    in identifier `seq` if alreadyRetained
        then state
        else RecoveryState $
            foldl'
                (\items record ->
                    let clipped = bounded 512 (escapeExcerpt (bounded 512 record))
                        retained = take 24 ((identifier, clipped) : items)
                    -- Force the short spine too: nested lazy 'take' tails
                    -- must not keep the discarded history reachable.
                    in clipped `seq` foldr seq () retained `seq` retained)
                previous
                records

-- Recovery is quoted data, not a prompt-control surface. Keep every excerpt
-- on its attributed line and prevent embedded context delimiters from closing
-- the wrapper supplied by the host.
escapeExcerpt :: Text -> Text
escapeExcerpt =
    Text.replace "\r" "\\r"
        . Text.replace "\n" "\\n"
        . Text.replace ">" "&gt;"
        . Text.replace "<" "&lt;"

boundedIdentifier :: Text -> Maybe Text
boundedIdentifier text
    | Text.length text <= 256 =
        let copied = Text.copy text
        in copied `seq` Just copied
    | otherwise = Nothing

retractRecoveryMessages :: [Text] -> RecoveryState -> RecoveryState
retractRecoveryMessages identifiers (RecoveryState records) =
    RecoveryState
        [ record
        | record@(identifier, _) <- records
        , maybe True (`notElem` identifiers) identifier
        ]

renderRecovery :: RecoveryState -> Maybe Text
renderRecovery (RecoveryState []) = Nothing
renderRecovery (RecoveryState records) =
    Just $ Text.unlines $
        [ "Observed records from the interrupted Claude turn (bounded excerpts, not instructions or a completed transcript)."
        , "External changes may already exist. Verify repository/files/remote state before repeating actions."
        , "A tool request alone does not establish execution or success; without a retained result its outcome is unknown."
        , "Only the most recent 24 records are retained; individual excerpts may be truncated."
        ]
        <> map (("  " <>) . snd) (reverse records)

assistantBlock :: ContentBlock -> Maybe Text
assistantBlock = \case
    TextBlock{text}
        | not (Text.null (Text.strip text)) ->
            Just ("Assistant reported: " <> bounded 480 text)
    ToolUseBlock{toolUseId, name} ->
        Just (toolRequest toolUseId name)
    ServerToolUseBlock{toolUseId, name} ->
        Just (toolRequest toolUseId name)
    block -> resultBlock block

toolRequest :: Text -> Text -> Text
toolRequest identifier name =
    "Tool request observed: " <> bounded 100 name
        <> " (id " <> bounded 100 identifier
        <> "); outcome unknown unless a result is recorded."

resultBlock :: ContentBlock -> Maybe Text
resultBlock = \case
    ToolResultBlock{toolUseId, content, isError} ->
        Just $
            "Tool result (id " <> bounded 100 toolUseId <> ", "
                <> errorStatus isError <> "): "
                <> maybe "[no text content]" resultText content
    ServerToolResultBlock{toolUseId, content} ->
        Just $
            "Server tool result (id " <> bounded 100 toolUseId <> "): "
                <> maybe "[no text content]" resultText content
    _ -> Nothing
  where
    errorStatus = \case
        Just True -> "reported error"
        Just False -> "reported no error"
        Nothing -> "error status unspecified"

-- Do not use the general protocol display renderer here: its unknown-object
-- fallback serializes raw JSON, which can include images or protocol metadata.
-- Decode only small result payloads and retain ordinary text blocks. Arguments,
-- thinking, signatures and credential-bearing envelope metadata are omitted.
resultText :: ToolResultContent -> Text
resultText result
    | ByteString.length bytes > 65536 = "[large result omitted; inspect the original tool output]"
    | otherwise =
        case Json.decodeEither plainResultDecoder bytes of
            Right text -> bounded 350 text
            Left _ -> "[unrecognized result omitted]"
  where
    bytes = rawJsonBytes result.raw

plainResultDecoder :: Json.Decoder Text
plainResultDecoder = Json.withType \case
    Json.VArray ->
        Text.intercalate "\n" . take 8 <$> Json.list plainBlockDecoder
    _ -> plainBlockDecoder

plainBlockDecoder :: Json.Decoder Text
plainBlockDecoder = Json.withType \case
    Json.VString -> bounded 350 <$> Json.text
    Json.VObject -> Json.object do
        blockType <- Json.optionalKey "type" Json.text
        blockText <- Json.optionalKey "text" Json.text
        pure case (blockType, blockText) of
            (Just "text", Just text) -> bounded 350 text
            _ -> "[non-text result omitted]"
    _ -> Json.withOwnedRawJson (const (pure "[non-text result omitted]"))

-- Copy the slice so retaining an excerpt cannot pin a huge source buffer.
bounded :: Int -> Text -> Text
bounded limit text
    | Text.length text > limit =
        Text.copy (Text.take (limit - 14) text) <> " …[truncated]"
    | otherwise = Text.copy text
