module Agent.CLI.ExternalSession.Content
    ( BoundedTurns
    , JsonlCounters(..)
    , appendBoundedTurn
    , appendOmissionWarnings
    , boundedLastText
    , boundedRecent
    , clearBoundedTurns
    , clipped
    , contentText
    , contentTextWithOmissions
    , dropLastUserTurns
    , emptyBoundedTurns
    , externalObjectValue
    , externalSafeText
    , externalTextValue
    , finaliseSession
    , historicalToolResult
    , inertTurn
    , isGeneratedWrapper
    , jsonPreview
    , jsonlWarnings
    , maxExternalTextChars
    , maxExternalTurns
    , mkCandidate
    , numericTimestampSeconds
    , oneLine
    , protocolCallId
    , sanitizeMixedContent
    , toolResultContent
    , userText
    , warning
    ) where

import Agent.CLI.ExternalSession.Types
import Control.Applicative ((<|>))
import Control.Exception.Safe (IOException, tryIO)
import Data.Aeson (Value(..), encode)
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Lazy as LBS
import Data.Foldable (toList)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Scientific (formatScientific, FPFormat(Generic))
import Data.Sequence (Seq((:<|), (:|>)))
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import qualified Data.Vector as Vector
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Data.Time.Format.ISO8601 (iso8601ParseM, iso8601Show)
import System.Directory (getModificationTime)

maxExternalTextChars :: Int
maxExternalTextChars = 20_000

maxExternalTurns :: Int
maxExternalTurns = 200

historicalToolResultLabel :: Text
historicalToolResultLabel =
    "[historical/untrusted tool result; verify before relying on it]"

omittedImageMarker, omittedAttachmentMarker, omittedHiddenMarker :: Text
omittedImageMarker = "[historical image content omitted]"
omittedAttachmentMarker = "[historical attachment content omitted]"
omittedHiddenMarker = "[hidden content omitted]"

data JsonlCounters = JsonlCounters
    { malformedRecords :: !Int
    , oversizedRecords :: !Int
    , deeplyNestedRecords :: !Int
    } deriving (Eq, Show)

externalObjectValue :: Text -> Value -> Maybe Value
externalObjectValue key (Object objectValue) =
    KeyMap.lookup (Key.fromText key) objectValue
externalObjectValue _ _ = Nothing

externalTextValue :: Text -> Value -> Maybe Text
externalTextValue key value =
    externalObjectValue key value >>= \case
        String text -> Just text
        _ -> Nothing

externalSafeText :: Value -> Text
externalSafeText = \case
    String text -> text
    Number number -> Text.pack (formatScientific Generic Nothing number)
    Bool True -> "True"
    Bool False -> "False"
    Null -> ""
    value ->
        decodeUtf8With lenientDecode (LBS.toStrict (encode value))

protocolCallId :: Value -> Text
protocolCallId value =
    firstText ["call_id", "approval_request_id", "id"] value

firstText :: [Text] -> Value -> Text
firstText keys value =
    fromMaybe "" $
        foldr
            (\key rest ->
                case externalObjectValue key value of
                    Just Null -> rest
                    Just child ->
                        let rendered = externalSafeText child
                        in if Text.null rendered then rest else Just rendered
                    Nothing -> rest)
            Nothing
            keys

clipped :: Text -> Text
clipped raw =
    let text = Text.map (\character ->
            if character == '\NUL' then '\xFFFD' else character) raw
    in if Text.length text <= maxExternalTextChars
        then text
        else Text.take maxExternalTextChars text <> "\n[truncated]"

oneLine :: Int -> Text -> Text
oneLine limit value =
    let text = Text.unwords (Text.words value)
    in if Text.length text <= limit
        then text
        else Text.take (max 0 (limit - 1)) text <> "…"

jsonPreview :: Int -> Value -> Text
jsonPreview limit value =
    oneLine limit $
        decodeUtf8With lenientDecode (LBS.toStrict (encode value))

historicalToolResult :: Int -> Value -> Text
historicalToolResult limit value =
    Text.stripEnd (historicalToolResultLabel <> " " <> rendered)
  where
    rendered =
        case value of
            String text -> oneLine limit text
            _ -> jsonPreview limit value

warning :: Text -> Text -> ExternalWarning
warning = ExternalWarning

mkCandidate
    :: ExternalProvider
    -> Text
    -> Text
    -> FilePath
    -> Text
    -> Maybe FilePath
    -> Maybe Value
    -> Maybe Value
    -> IO ExternalCandidate
mkCandidate provider source sessionId path rawTitle cwd created updated = do
    fallback <- fileTimestamp path
    let updatedSeconds = updated >>= timestampSeconds
        sortTime = fromMaybe fallback updatedSeconds
        render value =
            case value of
                String text | not (Text.null text) -> Just text
                _ ->
                    Text.pack
                        . iso8601Show
                        . posixSecondsToUTCTime
                        . realToFrac
                    <$> timestampSeconds value
    pure ExternalCandidate
        { candidateProvider = provider
        , candidateSource = source
        , candidateSessionId = sessionId
        , candidatePath = path
        , candidateTitle =
            oneLine 160 $
                if Text.null rawTitle then "(untitled)" else rawTitle
        , candidateCwd = cwd
        , candidateCreatedAt = created >>= render
        , candidateUpdatedAt =
            (updated >>= render)
                <|> if fallback > 0
                    then Just
                        ( Text.pack
                            (iso8601Show
                                (posixSecondsToUTCTime (realToFrac fallback)))
                        )
                    else Nothing
        , candidateSortTime = sortTime
        }

fileTimestamp :: FilePath -> IO Double
fileTimestamp path =
    tryIO (getModificationTime path) >>= \case
        Left (_ :: IOException) -> pure 0
        Right timestamp ->
            pure (realToFrac (utcTimeToPOSIXSeconds timestamp))

numericTimestampSeconds :: Value -> Maybe Double
numericTimestampSeconds = timestampSeconds

timestampSeconds :: Value -> Maybe Double
timestampSeconds = \case
    Number scientific ->
        let original = realToFrac scientific
            seconds = if original > 10_000_000_000
                then original / 1000
                else original
        in if isNaN seconds || isInfinite seconds || seconds < -62_135_596_800
                || seconds > 253_402_300_799
            then Nothing
            else Just seconds
    String text ->
        realToFrac . utcTimeToPOSIXSeconds
            <$> (iso8601ParseM (Text.unpack text) :: Maybe UTCTime)
    _ -> Nothing

appendOmissionWarnings
    :: ContentOmissions
    -> [ExternalWarning]
    -> [ExternalWarning]
appendOmissionWarnings omissions warnings =
    warnings
        <> [ warning
                "image_content_omitted"
                ( "Omitted "
                    <> Text.pack (show omissions.omittedImages)
                    <> " image content block(s); visual content is unavailable."
                )
           | omissions.omittedImages > 0
           ]
        <> [ warning
                "attachment_content_omitted"
                ( "Omitted "
                    <> Text.pack (show omissions.omittedAttachments)
                    <> " file/audio/resource attachment block(s); "
                    <> "attachment content is unavailable."
                )
           | omissions.omittedAttachments > 0
           ]

jsonlWarnings :: JsonlCounters -> [ExternalWarning]
jsonlWarnings counters =
    [ warning
        "malformed_records"
        ("Skipped " <> count counters.malformedRecords
            <> " malformed record(s).")
    | counters.malformedRecords > 0
    ]
    <> [ warning
        "oversized_records"
        ("Skipped " <> count counters.oversizedRecords
            <> " oversized record(s).")
       | counters.oversizedRecords > 0
       ]
    <> [ warning
        "deeply_nested_records"
        ("Skipped " <> count counters.deeplyNestedRecords
            <> " excessively nested record(s).")
       | counters.deeplyNestedRecords > 0
       ]
  where
    count = Text.pack . show

imageContentTypes, attachmentContentTypes, ignoredContentTypes,
    textContentTypes :: [Text]
imageContentTypes = ["input_image", "image", "computer_screenshot"]
attachmentContentTypes = ["input_file", "input_audio", "audio", "resource"]
ignoredContentTypes =
    [ "thinking"
    , "reasoning"
    , "reasoning_text"
    , "summary_text"
    , "redacted_thinking"
    , "encrypted_content"
    , "signature"
    ]
textContentTypes = ["text", "input_text", "output_text"]

contentText :: Value -> Text
contentText = fst . contentTextWithOmissions

contentTextWithOmissions :: Value -> (Text, ContentOmissions)
contentTextWithOmissions content =
    let blocks = case content of
            Object{} -> [content]
            Array values -> Vector.toList values
            String text -> [String text]
            _ -> []
        step (parts, omissions) block =
            case block of
                String text -> (parts <> [text], omissions)
                Object{} ->
                    let kind =
                            Text.toLower
                                (fromMaybe "" (externalTextValue "type" block))
                    in if validImageContentBlock block
                        then
                            ( parts
                            , omissions
                                { omittedImages =
                                    omissions.omittedImages + 1
                                }
                            )
                        else if validAttachmentContentBlock block
                            then
                                ( parts
                                , omissions
                                    { omittedAttachments =
                                        omissions.omittedAttachments + 1
                                    }
                                )
                            else if kind `elem` ignoredContentTypes
                                then (parts, omissions)
                                else if kind `elem` textContentTypes
                                    then case
                                        externalTextValue "text" block
                                            <|> externalTextValue "content" block
                                        of
                                            Just text -> (parts <> [text], omissions)
                                            Nothing -> (parts, omissions)
                                    else if kind == "refusal"
                                        then case externalTextValue "refusal" block of
                                            Just text -> (parts <> [text], omissions)
                                            Nothing -> (parts, omissions)
                                        else (parts, omissions)
                _ -> (parts, omissions)
        (parts, omissions) = foldl step ([], mempty) blocks
    in (clipped (Text.intercalate "\n" parts), omissions)

validImageContentBlock :: Value -> Bool
validImageContentBlock block =
    case Text.toLower <$> externalTextValue "type" block of
        Just "input_image" ->
            any (`hasStringOrObject` block) ["image_url", "file_id"]
        Just "image" ->
            hasStringOrObject "source" block
                || any (`hasStringOrObject` block)
                    ["data", "url", "image_url", "file_id"]
        Just "computer_screenshot" ->
            hasStringOrObject "image_url" block
        _ -> False

validAttachmentContentBlock :: Value -> Bool
validAttachmentContentBlock block =
    case Text.toLower <$> externalTextValue "type" block of
        Just "input_file" ->
            any (`hasString` block)
                ["file_data", "file_id", "file_url", "filename"]
        Just "input_audio" -> hasStringOrObject "input_audio" block
        Just "audio" -> hasString "data" block
        Just "resource" ->
            case externalObjectValue "resource" block of
                Just resource@Object{} -> hasString "blob" resource
                _ -> False
        _ -> False

validStructuredContentBlock :: Value -> Bool
validStructuredContentBlock block =
    case Text.toLower <$> externalTextValue "type" block of
        Just kind
            | kind `elem` textContentTypes ->
                hasString "text" block || hasString "content" block
            | kind `elem` imageContentTypes -> validImageContentBlock block
            | kind `elem` attachmentContentTypes ->
                validAttachmentContentBlock block
            | kind == "refusal" -> hasString "refusal" block
            | kind `elem` ["thinking", "reasoning", "reasoning_text", "summary_text"] ->
                any (`hasStringOrArray` block) ["thinking", "text", "summary"]
            | kind `elem` ["redacted_thinking", "encrypted_content", "signature"] ->
                any (`hasString` block)
                    ["data", "encrypted_content", "signature"]
        _ -> False

hasString :: Text -> Value -> Bool
hasString key value =
    case externalObjectValue key value of
        Just String{} -> True
        _ -> False

hasStringOrObject :: Text -> Value -> Bool
hasStringOrObject key value =
    case externalObjectValue key value of
        Just String{} -> True
        Just Object{} -> True
        _ -> False

hasStringOrArray :: Text -> Value -> Bool
hasStringOrArray key value =
    case externalObjectValue key value of
        Just String{} -> True
        Just Array{} -> True
        _ -> False

sanitizeMixedContent :: Value -> (Value, ContentOmissions)
sanitizeMixedContent = go 0
  where
    go :: Int -> Value -> (Value, ContentOmissions)
    go depth value
        | depth > 128 =
            (String "[nested value omitted]", mempty)
        | validImageContentBlock value =
            ( omittedObject value omittedImageMarker
            , ContentOmissions 1 0
            )
        | validAttachmentContentBlock value =
            ( omittedObject value omittedAttachmentMarker
            , ContentOmissions 0 1
            )
        | validStructuredContentBlock value
            && maybe False
                ((`elem` ignoredContentTypes) . Text.toLower)
                (externalTextValue "type" value) =
            (omittedObject value omittedHiddenMarker, mempty)
        | otherwise = case value of
            Object objectValue ->
                let pairs =
                        [ let (sanitized, omissions) = go (depth + 1) child
                          in ((key, sanitized), omissions)
                        | (key, child) <- KeyMap.toList objectValue
                        ]
                in
                    ( Object (KeyMap.fromList (map fst pairs))
                    , mconcat (map snd pairs)
                    )
            Array values ->
                let children = map (go (depth + 1)) (Vector.toList values)
                in
                    ( Array (Vector.fromList (map fst children))
                    , mconcat (map snd children)
                    )
            _ -> (value, mempty)

omittedObject :: Value -> Text -> Value
omittedObject value marker =
    Object $ KeyMap.fromList
        [ (Key.fromText "type", String kind)
        , (Key.fromText "omitted", String marker)
        ]
  where
    kind =
        Text.toLower (fromMaybe "" (externalTextValue "type" value))

toolResultContent :: Value -> (Value, ContentOmissions)
toolResultContent value =
    let blocks = case value of
            Object{} -> Just [value]
            Array values -> Just (Vector.toList values)
            _ -> Nothing
    in case blocks of
        Just values
            | not (null values)
            , all validStructuredContentBlock values ->
                let (text, omissions) = contentTextWithOmissions value
                in (String text, omissions)
        _ -> sanitizeMixedContent value

inertTurn
    :: Text
    -> Text
    -> [HistoricalToolCall]
    -> [HistoricalToolResult]
    -> Maybe ExternalTurn
inertTurn rawRole rawText calls results
    | role `notElem` ["user", "assistant"] = Nothing
    | Text.null text && null calls && null results = Nothing
    | otherwise = Just ExternalTurn
        { externalTurnRole = role
        , externalTurnText = text
        , externalTurnToolCalls = calls
        , externalTurnToolResults = results
        }
  where
    role = Text.toLower rawRole
    initialText = clipped (Text.strip rawText)
    text = if role == "user" then userText initialText else initialText

generatedContextPrefixes :: [Text]
generatedContextPrefixes =
    [ "# Skill instructions: "
    , "Plan mode is active. Do not make any edits or writes to the system "
        <> "except for the plan file."
    , "The user approved the plan. Plan mode is now off."
    , "<subagent_notification>"
    , "## Skills\nThe following reusable skills are available in this session."
    , "<learned-skills>\nThese are durable, reusable instructions learned from "
        <> "earlier sessions."
    ]

generatedTags :: [Text]
generatedTags =
    [ "system-reminder"
    , "environment_context"
    , "system"
    , "developer"
    , "instructions"
    , "user_instructions"
    , "manually_attached_skills"
    , "timestamp"
    , "local-command-caveat"
    , "harness_instructions"
    , "prior_conversation"
    , "current_request"
    , "user_query"
    ]

isGeneratedWrapper :: Text -> Bool
isGeneratedWrapper raw =
    any (`Text.isPrefixOf` stripped) generatedContextPrefixes
        || Text.isPrefixOf "# agents.md instructions for" lower
        || any (\tag ->
            Text.isPrefixOf ("<" <> tag <> ">") lower
                || Text.isPrefixOf ("<" <> tag <> " ") lower)
            generatedTags
  where
    stripped = Text.stripStart raw
    lower = Text.toLower stripped

isOuterHarness :: Text -> Bool
isOuterHarness raw =
    any (`Text.isPrefixOf` Text.toLower (Text.stripStart raw))
        [ "instructions supplied by the outer agent harness:"
        , "prior conversation imported from the outer agent harness."
        , "current request:"
        ]

extractTagged :: Text -> Text -> Maybe Text
extractTagged tag raw = do
    let stripped = Text.stripStart raw
        lower = Text.toLower stripped
        opening = "<" <> Text.toLower tag <> ">"
        closing = "</" <> Text.toLower tag <> ">"
    guardTextPrefix opening lower
    let body = Text.drop (Text.length opening) stripped
        lowerBody = Text.drop (Text.length opening) lower
        (beforeClose, fromClose) = Text.breakOn closing lowerBody
    if Text.null fromClose
        then Nothing
        else Just (Text.take (Text.length beforeClose) body)

guardTextPrefix :: Text -> Text -> Maybe ()
guardTextPrefix prefix value
    | prefix `Text.isPrefixOf` value = Just ()
    | otherwise = Nothing

taggedUserRequest :: Text -> Maybe Text
taggedUserRequest text =
    firstNonEmpty
        [ clipped . Text.strip <$> extractTagged "user_query" text
        , clipped . Text.strip <$> extractTagged "current_request" text
        ]
        <|> if isGeneratedWrapper text || isOuterHarness text
            then lastOuterRequest text
            else Nothing

lastOuterRequest :: Text -> Maybe Text
lastOuterRequest raw =
    let marker = "current request:"
        lower = Text.toLower raw
        starts = occurrences marker lower
        candidates =
            [ let suffix = Text.drop (offset + Text.length marker) raw
              in extractTagged "current_request" suffix
            | offset <- starts
            ]
    in clipped . Text.strip <$> lastMaybe (mapMaybe id candidates)

occurrences :: Text -> Text -> [Int]
occurrences needle haystack
    | Text.null needle = []
    | otherwise = go 0 haystack
  where
    go offset remaining =
        let (before, after) = Text.breakOn needle remaining
        in if Text.null after
            then []
            else
                let found = offset + Text.length before
                    consumed = Text.length before + Text.length needle
                in found : go (offset + consumed)
                    (Text.drop (Text.length needle) after)

priorConversationUserText :: Text -> Text
priorConversationUserText raw =
    fromMaybe "" $
        lastMaybe $
            mapMaybe latestUser $
                taggedSections "prior_conversation" raw
  where
    latestUser conversation =
        lastMaybe
            [ candidate
            | block <- splitConversationBlocks conversation
            , Just body <- [Text.stripPrefix "User:\n" block]
            , let nested = taggedUserRequest body
                  candidate = clipped (Text.strip (fromMaybe body nested))
            , not (Text.null candidate)
            , not (isGeneratedWrapper candidate)
            , not (isOuterHarness candidate)
            ]

taggedSections :: Text -> Text -> [Text]
taggedSections tag = go
  where
    opening = "<" <> Text.toLower tag <> ">"
    closing = "</" <> Text.toLower tag <> ">"
    go remaining =
        let lower = Text.toLower remaining
            (beforeOpen, fromOpen) = Text.breakOn opening lower
        in if Text.null fromOpen
            then []
            else
                let originalFromOpen = Text.drop (Text.length beforeOpen) remaining
                    body = Text.drop (Text.length opening) originalFromOpen
                    lowerBody = Text.drop (Text.length opening) fromOpen
                    (beforeClose, fromClose) = Text.breakOn closing lowerBody
                in if Text.null fromClose
                    then []
                    else
                        let section = Text.take (Text.length beforeClose) body
                            rest = Text.drop
                                (Text.length beforeClose + Text.length closing)
                                body
                        in section : go rest

splitConversationBlocks :: Text -> [Text]
splitConversationBlocks text =
    filter (not . Text.null) $
        Text.splitOn "\n\n" (Text.strip text)

firstNonEmpty :: [Maybe Text] -> Maybe Text
firstNonEmpty = foldr choose Nothing
  where
    choose (Just value) rest
        | not (Text.null value) = Just value
        | otherwise = rest
    choose Nothing rest = rest

lastMaybe :: [value] -> Maybe value
lastMaybe [] = Nothing
lastMaybe values = Just (last values)

userText :: Text -> Text
userText text =
    case taggedUserRequest text of
        Just tagged
            | not (Text.null tagged) -> tagged
            | otherwise -> ""
        Nothing ->
            let prior = priorConversationUserText text
            in if not (Text.null prior)
                then prior
                else if isGeneratedWrapper text || isOuterHarness text
                    then ""
                    else clipped text

data BoundedTurns = BoundedTurns
    { boundedTurns :: !(Seq ExternalTurn)
    , boundedPrefixUser :: !(Maybe Text)
    , boundedPrefixAssistant :: !(Maybe Text)
    , boundedWasTruncated :: !Bool
    } deriving (Eq, Show)

emptyBoundedTurns :: BoundedTurns
emptyBoundedTurns = BoundedTurns Seq.empty Nothing Nothing False

clearBoundedTurns :: BoundedTurns -> BoundedTurns
clearBoundedTurns _ = emptyBoundedTurns

appendBoundedTurn :: ExternalTurn -> BoundedTurns -> BoundedTurns
appendBoundedTurn turn state =
    trim state { boundedTurns = state.boundedTurns :|> turn }
  where
    trim current
        | Seq.length current.boundedTurns <= maxExternalTurns + 1 = current
        | removed :<| rest <- current.boundedTurns =
            let text = removed.externalTurnText
                withPrefix
                    | Text.null text = current
                    | removed.externalTurnRole == "user" =
                        current { boundedPrefixUser = Just text }
                    | removed.externalTurnRole == "assistant" =
                        current { boundedPrefixAssistant = Just text }
                    | otherwise = current
            in withPrefix
                { boundedTurns = rest
                , boundedWasTruncated = True
                }
        | otherwise = current

dropLastUserTurns :: Int -> BoundedTurns -> Maybe BoundedTurns
dropLastUserTurns number state
    | number <= 0 = Just state
    | state.boundedWasTruncated = Nothing
    | otherwise =
        case targetIndex of
            Nothing -> Just state
            Just index -> Just state
                { boundedTurns = Seq.take index state.boundedTurns }
  where
    indexed = zip [0..] (toList state.boundedTurns)
    userIndexes =
        [ index
        | (index, turn) <- indexed
        , turn.externalTurnRole == "user"
        ]
    targetIndex =
        case reverse userIndexes of
            [] -> Nothing
            reversed ->
                Just (reversed !! min (number - 1) (length reversed - 1))

boundedRecent :: BoundedTurns -> [ExternalTurn]
boundedRecent = toList . (.boundedTurns)

boundedLastText :: Text -> BoundedTurns -> Maybe Text
boundedLastText role state =
    lastMaybe
        [ turn.externalTurnText
        | turn <- toList state.boundedTurns
        , turn.externalTurnRole == role
        , not (Text.null turn.externalTurnText)
        ]
        <|> if role == "user"
            then state.boundedPrefixUser
            else state.boundedPrefixAssistant

finaliseSession
    :: ExternalCandidate
    -> [ExternalTurn]
    -> [ExternalWarning]
    -> Maybe Text
    -> Maybe Text
    -> ExternalSession
finaliseSession candidate turns initialWarnings knownUser knownAssistant =
    ExternalSession
        { externalSessionCandidate = candidate
        , externalSessionTurns = recent
        , externalSessionWarnings =
            initialWarnings
                <> [ warning
                        "turns_truncated"
                        ( "Only the last "
                            <> Text.pack (show maxExternalTurns)
                            <> " turns were returned."
                        )
                   | length turns > maxExternalTurns
                   ]
        , externalSessionLastUserRequest =
            knownUser <|> lastText "user" turns
        , externalSessionLastAssistantAction =
            knownAssistant <|> lastText "assistant" turns
        }
  where
    recent = drop (max 0 (length turns - maxExternalTurns)) turns
    lastText role =
        lastMaybe
            . map (.externalTurnText)
            . filter
                (\turn ->
                    turn.externalTurnRole == role
                        && not (Text.null turn.externalTurnText))
