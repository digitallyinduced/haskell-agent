-- | Codex's frameless bidirectional voice protocol, not the public
-- @/v1/live/sessions@ protocol. Reference: openai/codex at
-- 818f1cca8ccf8899f0f4d59336baebaccf358eed.
module Agent.OpenAI.Live.Protocol
    ( LiveEvent(..)
    , LiveRole(..)
    , ContextChannel(..)
    , decodeLiveEvent
    , sessionUpdate
    , inputAudioAppend
    , contextAppend
    , sessionClose
    , contextChunks
    , liveModel
    , liveSampleRate
    ) where

import qualified Agent.Json.Decode as Json
import Data.Aeson (Value, object, (.=))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as Base64
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

data LiveRole = LiveUser | LiveAssistant | LiveDeveloper
    deriving (Eq, Show)

data ContextChannel = Speakable | Commentary
    deriving (Eq, Show)

data LiveEvent
    = LiveStarted
    | LiveUpdated
    | LiveAudio !BS.ByteString
    | LiveTranscript !LiveRole !Bool !Text
    | LiveDelegation !Text !Text
    | LiveError !Text
    | LiveUnknown
    deriving (Eq, Show)

liveModel :: Text
liveModel = "gpt-live-1-codex"

-- | Signed PCM16 little endian, mono, in both directions.
liveSampleRate :: Int
liveSampleRate = 24_000

decodeLiveEvent :: BS.ByteString -> Either Text LiveEvent
decodeLiveEvent bytes =
    case Json.decodeEither eventDecoder bytes of
        Left err -> Left err.jsonErrorMessage
        Right event -> Right event

eventDecoder :: Json.Decoder LiveEvent
eventDecoder = Json.discriminatedObject "type" \case
    "session.started" -> pure LiveStarted
    "session.updated" -> pure LiveUpdated
    "output_audio.delta" -> Json.object do
        encoded <- Json.atKey "audio" Json.text
        case Base64.decode (Text.encodeUtf8 encoded) of
            Left _ -> fail "Invalid Live audio encoding"
            Right pcm
                | odd (BS.length pcm) -> fail "Incomplete Live PCM16 sample"
                | otherwise -> pure (LiveAudio pcm)
    "input_transcript.added" -> transcript LiveUser
    "output_transcript.added" -> transcript LiveAssistant
    "turn.done" -> Json.object $ Json.atKey "turn" $ Json.object do
        role <- Json.atKey "role" Json.text
        content <- Json.atKey "transcript" Json.text
        pure $ case role of
            "user" -> LiveTranscript LiveUser True content
            "assistant" -> LiveTranscript LiveAssistant True content
            _ -> LiveUnknown
    "delegation.created" -> Json.object $ Json.atKey "item" $ Json.object do
        kind <- Json.atKey "type" Json.text
        target <- Json.atKey "target" Json.text
        if kind /= "delegation" || target /= "client"
            then pure LiveUnknown
            else do
                identifier <- Json.atKey "id" Json.text
                content <- Json.atKey "content" $ Json.list $
                    Json.discriminatedObject "type" \case
                        "input_text" -> Json.object (Json.atKey "text" Json.text)
                        _ -> pure ""
                if Text.null identifier || Text.null (Text.strip (Text.concat content))
                    then fail "Missing Live delegation identifier or task"
                    else pure (LiveDelegation identifier (Text.concat content))
    "error" -> Json.object do
        message <- Json.optionalKey "error" $ Json.object $
            Json.defaultKey "Live voice service error" "message" Json.text
        pure (LiveError (maybe "Live voice service error" id message))
    _ -> pure LiveUnknown
  where
    transcript role = Json.object $ Json.atKey "item" $ Json.object $
        LiveTranscript role False <$> Json.atKey "text" Json.text

sessionUpdate :: Text -> Text -> [(LiveRole, Text)] -> Value
sessionUpdate instructions voice history = object
    [ "type" .= ("session.update" :: Text)
    , "session" .= object
        ([ "instructions" .= instructions
         , "audio" .= object ["output" .= object ["voice" .= voice]]
         , "delegation" .= object ["type" .= ("client" :: Text)]
         ] <> ["initial_items" .= map historyItem history | not (null history)])
    ]
  where
    historyItem (role, content) = object
        [ "type" .= ("message" :: Text)
        , "role" .= roleText role
        , "content" .= [object
            [ "type" .= (if role == LiveAssistant then "output_text" else "input_text" :: Text)
            , "text" .= content
            ]]
        ]

roleText :: LiveRole -> Text
roleText LiveUser = "user"
roleText LiveAssistant = "assistant"
roleText LiveDeveloper = "developer"

inputAudioAppend :: BS.ByteString -> Value
inputAudioAppend pcm = object
    [ "type" .= ("input_audio.append" :: Text)
    , "audio" .= Text.decodeUtf8 (Base64.encode pcm)
    ]

-- | Delegated results retain their identifier; unrelated session context does
-- not. No legacy response.create or synthetic final-message marker is sent.
contextAppend :: Maybe Text -> ContextChannel -> Text -> [Value]
contextAppend delegation channel = map message . contextChunks
  where
    message content = object $
        [ "type" .= (maybe "session.context.append" (const "delegation.context.append") delegation :: Text)
        , "channel" .= (case channel of Speakable -> "speakable"; Commentary -> "commentary" :: Text)
        , "content" .= [object ["type" .= ("input_text" :: Text), "text" .= content]]
        ] <> maybe [] (\identifier -> ["delegation_item_id" .= identifier]) delegation

sessionClose :: Value
sessionClose = object ["type" .= ("session.close" :: Text)]

-- | Codex limits context appends to 500 UTF-8 bytes, not 500 characters.
contextChunks :: Text -> [Text]
contextChunks text
    | Text.null text = [""]
    | otherwise = go text
  where
    go remaining
        | Text.null remaining = []
        | otherwise =
            let chunk = Text.pack (takeBytes 500 (Text.unpack (Text.take 500 remaining)))
            in chunk : go (Text.drop (Text.length chunk) remaining)
    takeBytes _ [] = []
    takeBytes budget (character : rest)
        | size > budget = []
        | otherwise = character : takeBytes (budget - size) rest
      where
        size = BS.length (Text.encodeUtf8 (Text.singleton character))
