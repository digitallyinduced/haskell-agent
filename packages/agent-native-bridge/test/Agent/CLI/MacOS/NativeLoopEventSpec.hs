module Agent.CLI.MacOS.NativeLoopEventSpec (spec) where

import Agent.CLI.MacOS.NativeLoopEvent
    ( encodeNativeLoopEvent
    , encodeNativeUsageEvent
    )
import Agent.Loop
    ( LoopEvent(..)
    , TokenUsage(..)
    , TurnOutput(..)
    , emptyTurnOutput
    )
import Agent.ToolDispatch (ToolCall(..), ToolCallKind(..), ToolCallResult(..))
import qualified Data.ByteString as BS
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import Data.Word (Word8, Word32)
import Test.Hspec

spec :: Spec
spec = describe "native loop event binary encoding" do
    it "encodes text deltas with a versioned HAEV frame" do
        encodeNativeLoopEvent "turn" (TextDelta "hé")
            `shouldBe` Just (frame 2 0 ["turn", "hé"])

    it "re-emits updated tool metadata through the keyed tool frame" do
        let call = ToolCall
                { callId = "call-1"
                , name = "shell_command"
                , arguments = "{\"command\":\"git status\"}"
                , callKind = FunctionCallKind
                , argumentsEncrypted = False
                }
        encodeNativeLoopEvent "turn" (ToolUpdated call)
            `shouldSatisfy` hasKind 4
        encodeNativeLoopEvent "turn" (ToolArgumentsUpdated call)
            `shouldSatisfy` hasKind 4

    it "encodes reasoning and status events as distinct kinds" do
        encodeNativeLoopEvent "turn" (ReasoningDelta "checking")
            `shouldSatisfy` hasKind 1
        encodeNativeLoopEvent "turn" (ActivityUpdated "working")
            `shouldSatisfy` hasKind 3

    it "preserves tool flags and fields" do
        let call = ToolCall
                { callId = "call-1"
                , name = "read_file"
                , arguments = "{\"path\":\"/tmp/a\"}"
                , callKind = FunctionCallKind
                , argumentsEncrypted = True
                }
        case encodeNativeLoopEvent "turn" (ToolStarted call) of
            Nothing -> expectationFailure "native tool event failed to encode"
            Just encoded -> do
                BS.take 8 encoded `shouldBe` header 4 1
                BS.isInfixOf "call-1" encoded `shouldBe` True
                BS.isInfixOf "read_file" encoded `shouldBe` True
                BS.index encoded 7 `shouldBe` 1

    it "encodes truncated tool output with its flag" do
        let result = ToolCallResult
                { callId = "call-1"
                , output = Text.replicate 8193 "x"
                , callKind = FunctionCallKind
                }
        case encodeNativeLoopEvent "turn" (ToolFinished result) of
            Nothing -> expectationFailure "native tool event failed to encode"
            Just encoded -> BS.take 8 encoded `shouldBe` header 5 2

    it "redacts computer screenshots from tool-finish frames" do
        let secret = "data:image/png;base64,private-screenshot" :: Text.Text
            result = ToolCallResult
                { callId = "computer-1"
                , output = secret
                , callKind = ComputerFunctionCallKind
                }
        case encodeNativeLoopEvent "turn" (ToolFinished result) of
            Nothing -> expectationFailure "native tool event failed to encode"
            Just encoded -> do
                encoded `shouldSatisfy` BS.isInfixOf "Screenshot captured"
                encoded `shouldNotSatisfy`
                    BS.isInfixOf (TextEncoding.encodeUtf8 secret)

    it "encodes terminal provider token usage without inventing cost" do
        let output =
                (emptyTurnOutput "response" [] Nothing)
                    { tokenUsage = TokenUsage 120 34 56 }
        encodeNativeLoopEvent "turn" (TurnFinished output)
            `shouldBe`
                Just
                    (frame 6 0 ["turn", "120", "34", "56"]
                        <> word32BE maxBound)

    it "encodes aggregate usage and exact provider-reported cost" do
        encodeNativeUsageEvent
            True
            "turn"
            (TokenUsage 200 50 80)
            (Just 0.0125)
            `shouldBe`
                Just
                    (frame 7 0
                        ["turn", "200", "50", "80", "1.25e-2"])

    it "does not encode turn-start lifecycle events as native loop frames" do
        encodeNativeLoopEvent "turn" TurnStarted `shouldBe` Nothing

frame :: Word8 -> Word8 -> [String] -> BS.ByteString
frame kind flags fields =
    header kind flags
        <> BS.concat
            [ field (TextEncoding.encodeUtf8 (Text.pack value))
            | value <- fields
            ]

header :: Word8 -> Word8 -> BS.ByteString
header kind flags =
    "HAEV" <> BS.pack [1, kind, 0, flags]

field :: BS.ByteString -> BS.ByteString
field bytes = word32BE (fromIntegral (BS.length bytes)) <> bytes

word32BE :: Word32 -> BS.ByteString
word32BE value = BS.pack
    [ fromIntegral (value `div` 16777216)
    , fromIntegral (value `div` 65536)
    , fromIntegral (value `div` 256)
    , fromIntegral value
    ]

hasKind :: Word8 -> Maybe BS.ByteString -> Bool
hasKind kind (Just encoded) =
    BS.take 8 encoded == header kind 0
hasKind _ Nothing = False
