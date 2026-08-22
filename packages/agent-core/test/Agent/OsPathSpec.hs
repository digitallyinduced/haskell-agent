{-# LANGUAGE CPP #-}

module Agent.OsPathSpec (spec) where

import Agent.OsPath (directoryChain, fromText, toText, unsafeToFilePath)
import qualified Data.Text as Text
import System.OsPath (decodeUtf, pack, unsafeFromChar)
import Test.Hspec

spec :: Spec
spec = describe "Agent.OsPath" do
    describe "directoryChain" do
        it "returns the root itself when root and cwd match" do
            directoryChain (fromText "/repo") (fromText "/repo")
                `shouldBe` [fromText "/repo"]

        it "returns directories from root through a descendant cwd" do
            directoryChain (fromText "/repo") (fromText "/repo/pkg/src")
                `shouldBe`
                    [ fromText "/repo"
                    , fromText "/repo/pkg"
                    , fromText "/repo/pkg/src"
                    ]

        it "falls back to cwd when cwd is outside root" do
            directoryChain (fromText "/repo") (fromText "/other/pkg")
                `shouldBe` [fromText "/other/pkg"]

    it "converts Text to and from a UTF path" do
        let text = "/tmp/日本語"
            path = fromText text
        decodeUtf path `shouldReturn` Text.unpack text
        toText path `shouldBe` text
        unsafeToFilePath path `shouldBe` Text.unpack text

#ifndef mingw32_HOST_OS
    it "uses an escaped representation only for human-readable output" do
        let path = pack [unsafeFromChar '\xff']
        toText path `shouldBe` Text.pack (show path)
#endif
