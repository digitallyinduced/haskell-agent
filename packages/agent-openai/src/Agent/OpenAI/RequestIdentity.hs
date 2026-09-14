-- | Canonical Codex request attribution.
--
-- The official Codex clients attach the session, thread, turn, and
-- context-window identity of every Responses request as HTTP headers and as
-- a @client_metadata@ object. The backend attributes usage to turns from that
-- metadata rather than from individual requests, so tool continuations and
-- retries that share one turn identifier are accounted as one turn.
--
-- This module contains only the pure wire encoding. Lifecycle (which turn a
-- request belongs to, when the window advances) lives in
-- "Agent.OpenAI.TurnState".
module Agent.OpenAI.RequestIdentity
    ( CodexRequestKind(..)
    , codexRequestKindText
    , CodexRequestIdentity(..)
    , codexOriginator
    , codexWindowId
    , codexAttributionHeaders
    , codexClientMetadata
    , codexTurnMetadataJson
    , asciiJsonText
    , isHeaderSafeIdentifier
    ) where

import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Bits ((.&.), shiftR)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Char (ord)
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Numeric (showHex)

-- | Why a request is being made. Codex distinguishes foreground turns from
-- internal requests so analytics and quota accounting can tell them apart.
data CodexRequestKind
    = CodexTurnRequest
    | CodexCompactionRequest
    deriving (Eq, Show)

codexRequestKindText :: CodexRequestKind -> Text
codexRequestKindText = \case
    CodexTurnRequest -> "turn"
    CodexCompactionRequest -> "compaction"

-- | The identity attached to one request.
--
-- * 'sessionId' and 'threadId' identify the durable conversation. Codex sets
--   its session header from the prompt cache key, and our cache key is the
--   persisted session identifier, so both carry the same value.
-- * 'turnId' is shared by every request of one logical turn: the initial
--   model request, tool-output continuations, and transport retries.
-- * 'windowNumber' counts committed compactions; the window identifier sent
--   on the wire is @thread:number@, and 'contextWindowId' is a fresh UUID for
--   each window.
data CodexRequestIdentity = CodexRequestIdentity
    { sessionId :: !Text
    , threadId :: !Text
    , turnId :: !Text
    , windowNumber :: !Int
    , contextWindowId :: !Text
    , requestKind :: !CodexRequestKind
    , turnStartedAtUnixMs :: !Int64
    } deriving (Eq, Show)

-- | Client identity reported to the Codex backend, matching the value used
-- by the image-generation and Live voice endpoints.
codexOriginator :: ByteString
codexOriginator = "haskell-agent"

codexWindowId :: CodexRequestIdentity -> Text
codexWindowId identity =
    identity.threadId <> ":" <> Text.pack (show identity.windowNumber)

-- | Headers sent on HTTP requests and WebSocket handshakes. The turn
-- metadata header repeats the JSON carried in @client_metadata@ so proxies
-- that only forward headers still attribute the request.
codexAttributionHeaders :: CodexRequestIdentity -> [(ByteString, ByteString)]
codexAttributionHeaders identity =
    [ ("originator", codexOriginator)
    , ("session-id", encode identity.sessionId)
    , ("thread-id", encode identity.threadId)
    , ("x-client-request-id", encode identity.threadId)
    , ("x-codex-window-id", encode (codexWindowId identity))
    , ("x-codex-turn-metadata", encode (codexTurnMetadataJson identity))
    ]
  where
    encode = Text.encodeUtf8

-- | The @client_metadata@ object carried in request bodies and WebSocket
-- @response.create@ frames. The nested turn metadata is the canonical
-- attribution record; the flat fields mirror it for older consumers.
codexClientMetadata :: CodexRequestIdentity -> KeyMap.KeyMap Aeson.Value
codexClientMetadata identity =
    KeyMap.fromList
        [ ("session_id", Aeson.String identity.sessionId)
        , ("thread_id", Aeson.String identity.threadId)
        , ("turn_id", Aeson.String identity.turnId)
        , ("x-codex-window-id", Aeson.String (codexWindowId identity))
        , ("x-codex-turn-metadata"
          , Aeson.String (codexTurnMetadataJson identity))
        ]

-- | The turn metadata record as an ASCII-only JSON string so it can be sent
-- as a header value verbatim.
codexTurnMetadataJson :: CodexRequestIdentity -> Text
codexTurnMetadataJson identity =
    asciiJsonText $ Aeson.object
        [ "session_id" .= identity.sessionId
        , "thread_id" .= identity.threadId
        , "turn_id" .= identity.turnId
        , "window_id" .= codexWindowId identity
        , "window_number" .= identity.windowNumber
        , "context_window_id" .= identity.contextWindowId
        , "request_kind" .= codexRequestKindText identity.requestKind
        , "turn_started_at_unix_ms" .= identity.turnStartedAtUnixMs
        ]

-- | Encode JSON with every character outside printable ASCII escaped as
-- @\\uXXXX@ (surrogate pairs above the basic multilingual plane), the same
-- normalization the Codex client applies before placing JSON in a header.
asciiJsonText :: Aeson.Value -> Text
asciiJsonText value =
    Text.concatMap escape
        (Text.decodeUtf8 (LBS.toStrict (Aeson.encode value)))
  where
    escape character
        | code <= 0x7e = Text.singleton character
        | code <= 0xffff = unicodeEscape code
        | otherwise =
            let offset = code - 0x10000
            in unicodeEscape (0xd800 + (offset `shiftR` 10))
                <> unicodeEscape (0xdc00 + (offset .&. 0x3ff))
      where
        code = ord character
    unicodeEscape code =
        "\\u" <> Text.justifyRight 4 '0' (Text.pack (showHex code ""))

-- | Whether a caller-supplied identifier can be sent as a header value
-- without transformation: non-empty, visible ASCII, no whitespace.
isHeaderSafeIdentifier :: Text -> Bool
isHeaderSafeIdentifier value =
    not (Text.null value)
        && Text.all (\character -> character >= '!' && character <= '~') value
