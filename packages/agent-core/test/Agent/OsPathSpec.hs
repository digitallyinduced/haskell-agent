{-# LANGUAGE CPP #-}

module Agent.OsPathSpec (spec) where

import Agent.OsPath (fromText, toText)
import qualified Data.Text as Text
import System.OsPath (decodeUtf, pack, unsafeFromChar)
import Test.Hspec

spec :: Spec
spec = describe "Agent.OsPath" do
    it "converts Text to and from a UTF path" do
        let text = "/tmp/日本語"
            path = fromText text
        decodeUtf path `shouldReturn` Text.unpack text
        toText path `shouldBe` text

#ifndef mingw32_HOST_OS
    it "uses an escaped representation only for human-readable output" do
        let path = pack [unsafeFromChar '\xff']
        toText path `shouldBe` Text.pack (show path)
#endif
