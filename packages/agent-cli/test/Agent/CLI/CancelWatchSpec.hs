module Agent.CLI.CancelWatchSpec (spec) where

import Agent.CLI.CancelWatch (drainEscapeContinuation, isBareEscape)
import System.IO (hClose)
import System.Posix.IO (createPipe, fdToHandle)
import System.Timeout (timeout)
import Test.Hspec

spec :: Spec
spec = do
    describe "isBareEscape" do
        it "accepts a lone ESC" do
            isBareEscape "\ESC" `shouldBe` True

        it "rejects CSI / empty / other" do
            isBareEscape "\ESC[" `shouldBe` False
            isBareEscape "" `shouldBe` False
            isBareEscape "a" `shouldBe` False

    describe "drainEscapeContinuation" do
        it "does not block forever on an incomplete SS3 sequence" do
            (readFd, writeFd) <- createPipe
            readHandle <- fdToHandle readFd
            writeHandle <- fdToHandle writeFd
            result <- timeout 500000 $
                drainEscapeContinuation readHandle 'O'
            hClose readHandle
            hClose writeHandle
            result `shouldBe` Just ()
