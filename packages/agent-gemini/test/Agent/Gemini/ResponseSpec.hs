module Agent.Gemini.ResponseSpec (spec) where

import Agent.Gemini.Response
import Agent.Gemini.Types
import Agent.Responses.Types
import qualified Data.Aeson
import qualified Data.Set as Set
import Test.Hspec

spec :: Spec
spec = describe "Gemini response assembly" do
    it "assembles text, thoughts, signatures, calls, and usage" do
        let thought = Part (Just "thinking") True (Just "sig") Nothing
            text = Part (Just "hello") False Nothing Nothing
            call = Part Nothing False Nothing
                (Just (NativeFunctionCall (Just "call-1") "lookup" (objectValue)))
            usage = UsageMetadata (Just 3) (Just 4) Nothing (Just 1) (Just 8)
            chunk = GenerateContentResponse (Just "resp-1") (Just "gemini-test")
                [Candidate (Just (Content (Just "model") [thought, text, call]))
                    (Just "STOP") (Just 0)]
                (Just usage) Nothing
            (state, events) = applyChunk
                (initialStreamState "gemini-test" "fallback") chunk
            response = buildResponse defaultResponseCreateParams state
        events `shouldSatisfy` (not . null)
        response.responseId `shouldBe` "resp-1"
        length response.output `shouldBe` 3
        fmap (.inputTokens) response.usage `shouldBe` Just 3
        fmap (.outputTokens) response.usage `shouldBe` Just 5
        fmap (.totalTokens) response.usage `shouldBe` Just 8
        (response.usage >>= (.outputTokensDetails)
            >>= (.reasoningTokens))
            `shouldBe` Just 1

    it "preserves whitespace in streamed text deltas" do
        let parts =
                [ Part (Just "hello ") False Nothing Nothing
                , Part (Just " ") False Nothing Nothing
                , Part (Just "world") False Nothing Nothing
                ]
            chunk = GenerateContentResponse Nothing Nothing
                [Candidate (Just (Content (Just "model") parts))
                    Nothing (Just 0)]
                Nothing Nothing
            (_, events) = applyChunk
                (initialStreamState "gemini-test" "fallback") chunk
        events `shouldBe`
            [ GeminiTextDelta "hello "
            , GeminiTextDelta " "
            , GeminiTextDelta "world"
            ]

    it "assembles adjacent wire deltas into one canonical assistant message" do
        let first = GenerateContentResponse Nothing Nothing
                [Candidate
                    (Just (Content (Just "model")
                        [Part (Just "hello ") False Nothing Nothing]))
                    Nothing
                    (Just 0)]
                Nothing Nothing
            second = GenerateContentResponse Nothing Nothing
                [Candidate
                    (Just (Content (Just "model")
                        [Part (Just "world") False Nothing Nothing]))
                    (Just "STOP")
                    (Just 0)]
                Nothing Nothing
            (afterFirst, _) = applyChunk
                (initialStreamState "gemini-test" "fallback")
                first
            (afterSecond, _) = applyChunk afterFirst second
            response = buildResponse defaultResponseCreateParams afterSecond
        response.output `shouldBe`
            [ MessageItem ResponseMessage
                { messageId = Nothing
                , content = MessageContentParts
                    [OutputTextPart "hello world" Nothing Nothing]
                , role = RoleAssistant
                , status = Just ItemCompleted
                , phase = Nothing
                , passthrough = Nothing
                }
            ]

    it "unwraps raw input from adapted custom tool calls" do
        let nativeCall = NativeFunctionCall
                (Just "call-1")
                "apply_patch"
                (Data.Aeson.object
                    ["input" Data.Aeson..= ("*** Begin Patch" :: String)])
            chunk = GenerateContentResponse Nothing Nothing
                [ Candidate
                    (Just (Content (Just "model")
                        [Part Nothing False Nothing (Just nativeCall)]))
                    (Just "STOP")
                    (Just 0)
                ]
                Nothing Nothing
            (_, events) = applyChunk
                (initialStreamStateWithCustomTools
                    "gemini-test"
                    "fallback"
                    (Set.singleton "apply_patch"))
                chunk
        events `shouldBe`
            [ GeminiFunctionCallReady FunctionCall
                { itemId = Just "call-1"
                , callId = "call-1"
                , name = "apply_patch"
                , namespace = Nothing
                , provider = Just "gemini"
                , arguments = "*** Begin Patch"
                , encryptedFunctionArgs = Nothing
                , status = Just ItemCompleted
                }
            ]
  where
    objectValue = Data.Aeson.object ["q" Data.Aeson..= ("x" :: String)]
