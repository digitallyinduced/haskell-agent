{-# LANGUAGE GHC2021 #-}

module Agent.CLI.MacOS.NativeLoopEvent
    ( encodeNativeLoopEvent
    , encodeNativeUsageEvent
    ) where

import Agent.CLI.Render (summarizeToolCall)
import Agent.Loop (LoopEvent(..), TokenUsage(..), TurnOutput(..))
import Agent.ToolDispatch (ToolCall(..), ToolCallResult(..))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import Data.Word (Word16, Word8)

-- | Binary events are framed individually by the C callback.  HAEV version 1
-- uses a common header followed by a turn id and kind-specific length-prefixed
-- UTF-8 fields:
--
--   magic (4), version (1), kind (1), flags (u16 BE),
--   turn-id length (u32 BE), fields...
--
-- A field length of maxBound denotes an absent optional field.  The callback
-- already supplies the complete frame length, so no outer payload length is
-- needed.
encodeNativeLoopEvent :: Text -> LoopEvent -> Maybe BS.ByteString
encodeNativeLoopEvent turnId event =
    case event of
        ReasoningDelta text -> textEvent 1 turnId text
        TextDelta text -> textEvent 2 turnId text
        ActivityUpdated status -> textEvent 3 turnId status
        WarningRaised warning -> textEvent 3 turnId warning
        ResponseRestarted message -> textEvent 3 turnId message
        ToolStarted call ->
            toolEvent 4 flags startedFields
          where
            (arguments, truncated) =
                boundedEventText
                    (if call.argumentsEncrypted then "" else call.arguments)
            flags =
                (if call.argumentsEncrypted then 1 else 0)
                    + (if truncated then 2 else 0)
            startedFields =
                [ Just call.callId
                , Just call.name
                , Just (summarizeToolCall call)
                , Just arguments
                ]
        ToolFinished result ->
            toolEvent 5 flags finishedFields
          where
            (output, truncated) = boundedEventText result.output
            flags = if truncated then 2 else 0
            finishedFields = [Just result.callId, Just output]
        TurnFinished output ->
            encodeNativeUsageEvent
                False
                turnId
                output.tokenUsage
                Nothing
        _ -> Nothing
  where
    textEvent kind id text = frame kind 0 id [Just text]
    toolEvent kind flags fields = frame kind flags turnId fields

encodeNativeUsageEvent
    :: Bool
    -> Text
    -> TokenUsage
    -> Maybe Double
    -> Maybe BS.ByteString
encodeNativeUsageEvent aggregate turnId usage providerCost =
    frame
        (if aggregate then 7 else 6)
        0
        turnId
        [ Just (Text.pack (show usage.inputTokens))
        , Just (Text.pack (show usage.outputTokens))
        , Just (Text.pack (show usage.cachedTokens))
        , Text.pack . show <$> providerCost
        ]

frame :: Word8 -> Word16 -> Text -> [Maybe Text] -> Maybe BS.ByteString
frame kind flags turnId fields =
    fmap
        (LBS.toStrict . Builder.toLazyByteString)
        ( do
            turnIdField <- field (Just turnId)
            fields' <- traverse field fields
            pure $
                Builder.byteString "HAEV"
                    <> Builder.word8 1
                    <> Builder.word8 kind
                    <> Builder.word16BE flags
                    <> turnIdField
                    <> foldMap id fields'
        )

maxNativeFieldBytes :: Int
-- Keep malformed callback frames from requesting unbounded allocations in
-- consumers, while leaving room for bounded tool output and future fields.
maxNativeFieldBytes = 16 * 1024 * 1024

field :: Maybe Text -> Maybe Builder.Builder
field Nothing = Just (Builder.word32BE maxBound)
field (Just value) =
    let bytes = TextEncoding.encodeUtf8 value
    in if BS.length bytes > maxNativeFieldBytes
        then Nothing
        else Just $
            Builder.word32BE (fromIntegral (BS.length bytes))
                <> Builder.byteString bytes

boundedEventText :: Text -> (Text, Bool)
boundedEventText value =
    let (visible, remainder) = Text.splitAt 8192 value
    in (visible, not (Text.null remainder))
