module Agent.CLI.VoiceAudioSpec (spec) where

import Agent.CLI.Voice.Audio (playbackHeader, playbackSamples)
import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as B
import Data.ByteString.Lazy qualified as LBS
import Data.Int (Int16)
import Test.Hspec

spec :: Spec
spec = describe "Live playback framing" do
    it "preserves every PCM16 value in little-endian IEEE float" do
        let samples = [minBound .. maxBound] :: [Int16]
            packed = LBS.toStrict . B.toLazyByteString . mconcat
            pcm = packed (map B.int16LE samples)
            floats = packed (map (B.floatLE . (/ 32768) . fromIntegral) samples)
        playbackSamples pcm `shouldBe` Just floats
    it "does not buffer complete samples between chunks" do
        playbackSamples (BS.pack [0, 128]) `shouldBe` Just (BS.pack [0, 0, 128, 191])
        playbackSamples BS.empty `shouldBe` Just BS.empty
    it "rejects incomplete samples" do
        playbackSamples (BS.pack [0]) `shouldBe` Nothing
    it "declares float mono 24 kHz and unknown stream lengths" do
        BS.length playbackHeader `shouldBe` 44
        BS.take 12 playbackHeader `shouldBe` BS.pack [82,73,70,70,255,255,255,255,87,65,86,69]
        BS.take 16 (BS.drop 20 playbackHeader) `shouldBe`
            BS.pack [3,0,1,0,192,93,0,0,0,119,1,0,4,0,32,0]
        BS.drop 36 playbackHeader `shouldBe` BS.pack [100,97,116,97,255,255,255,255]
