module Agent.CLI.FileUriSpec (spec) where

import Agent.CLI.FileUri
import Test.Hspec

spec :: Spec
spec =
    describe "file URI codec" do
        it "percent-encodes reserved and non-ASCII path bytes" do
            fileUri "/tmp/a b#?%/é:cache"
                `shouldBe` "file:///tmp/a%20b%23%3F%25/%C3%A9:cache"

        it "round-trips representative absolute paths" do
            mapM_
                (\path -> fileUriPath (fileUri path) `shouldBe` Just path)
                [ "/tmp/plain"
                , "/tmp/a b"
                , "/tmp/hash#query?percent%"
                , "/tmp/café/日本語"
                , "/tmp/cache:key"
                ]

        it "rejects non-file URI schemes" do
            fileUriPath "https://example.com/file" `shouldBe` Nothing
