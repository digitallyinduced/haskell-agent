{-# LANGUAGE CPP #-}

module Agent.OsPathSpec (spec) where

import Agent.OsPath
    ( directoryChain
    , fromText
    , normalizeLexically
    , relativeDisplayPath
    , toText
    , unsafeToFilePath
    )
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

    describe "normalizeLexically" do
        it "collapses dots and parent directories" do
            toText (normalizeLexically (fromText "/repo/./src/../nix/foo.nix"))
                `shouldBe` "/repo/nix/foo.nix"
            toText (normalizeLexically (fromText "/repo/"))
                `shouldBe` "/repo"
            toText (normalizeLexically (fromText "src/../nix/foo.nix"))
                `shouldBe` "nix/foo.nix"

        it "does not climb above an absolute root" do
            toText (normalizeLexically (fromText "/repo/../etc"))
                `shouldBe` "/etc"

    describe "relativeDisplayPath" do
        it "shows workspace-relative paths after lexical normalization" do
            let workspace = fromText "/Users/marc/.haskell-agent/worktrees/haskell-agent/wt"
            relativeDisplayPath
                workspace
                (fromText "/Users/marc/.haskell-agent/worktrees/haskell-agent/wt/./nix/../nix/modules/telegram.nix")
                `shouldBe` "nix/modules/telegram.nix"
            relativeDisplayPath workspace workspace `shouldBe` "."
            relativeDisplayPath workspace (fromText "src/../nix/foo.nix")
                `shouldBe` "nix/foo.nix"
            relativeDisplayPath workspace (fromText "/tmp/outside.hs")
                `shouldBe` "/tmp/outside.hs"
            relativeDisplayPath
                (fromText "/worktree")
                (fromText "/worktree-other/Foo.hs")
                `shouldBe` "/worktree-other/Foo.hs"

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
