module Agent.XAI.TranscriptionSpec (spec) where

import Agent.XAI.Transcription
import Test.Hspec

spec :: Spec
spec = describe "xAI transcription events" do
    it "decodes partial and final transcript events" do
        decodeTranscriptEvent
            "{\"type\":\"transcript.partial\",\"text\":\"hello\",\"is_final\":false,\"speech_final\":false}"
            `shouldBe` Right TranscriptPartial
                { transcriptText = "hello"
                , transcriptIsFinal = False
                , transcriptSpeechFinal = False
                }
        decodeTranscriptEvent
            "{\"type\":\"transcript.done\",\"text\":\"hello world\",\"duration\":1.2}"
            `shouldBe` Right TranscriptDone
                { transcriptText = "hello world"
                }

    it "ignores forward-compatible server events" do
        decodeTranscriptEvent "{\"type\":\"usage.updated\",\"tokens\":1}"
            `shouldBe` Right TranscriptUnknown
