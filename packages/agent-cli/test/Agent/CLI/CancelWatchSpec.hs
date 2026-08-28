module Agent.CLI.CancelWatchSpec (spec) where

import Agent.CLI.CancelWatch
    ( EscapeContinuation(..)
    , escapeContinuationCancels
    , isBareEscape
    , readEscapeContinuation
    )
import System.IO (BufferMode(..), hClose, hPutStr, hFlush, hSetBuffering)
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

    describe "readEscapeContinuation" do
        it "does not block forever on an incomplete SS3 sequence" do
            (readFd, writeFd) <- createPipe
            readHandle <- fdToHandle readFd
            writeHandle <- fdToHandle writeFd
            result <- timeout 500000 $
                readEscapeContinuation readHandle 'O'
            hClose readHandle
            hClose writeHandle
            result `shouldBe` Just EscapeOther

        it "consumes a complete CSI body including the final byte" do
            (readFd, writeFd) <- createPipe
            readHandle <- fdToHandle readFd
            writeHandle <- fdToHandle writeFd
            hSetBuffering writeHandle NoBuffering
            hPutStr writeHandle "27u"
            hFlush writeHandle
            result <- timeout 1000000 $
                readEscapeContinuation readHandle '['
            hClose readHandle
            hClose writeHandle
            result `shouldBe` Just (EscapeCsi "27u")

        it "returns a fragmented CSI tail instead of leaving it buffered" do
            (readFd, writeFd) <- createPipe
            readHandle <- fdToHandle readFd
            writeHandle <- fdToHandle writeFd
            hSetBuffering writeHandle NoBuffering
            hPutStr writeHandle "27"
            hFlush writeHandle
            result <- timeout 1000000 $
                readEscapeContinuation readHandle '['
            hClose readHandle
            hClose writeHandle
            result `shouldBe` Just (EscapeCsi "27")

    describe "escapeContinuationCancels" do
        it "cancels on a Kitty-encoded Esc key press" do
            escapeContinuationCancels (EscapeCsi "27u") `shouldBe` True
            escapeContinuationCancels (EscapeCsi "27;1u") `shouldBe` True
            escapeContinuationCancels (EscapeCsi "27;1:1u") `shouldBe` True

        it "ignores Esc key releases" do
            escapeContinuationCancels (EscapeCsi "27;1:3u") `shouldBe` False

        it "ignores arrows, other keys, and non-CSI tails" do
            escapeContinuationCancels (EscapeCsi "A") `shouldBe` False
            escapeContinuationCancels (EscapeCsi "13;2u") `shouldBe` False
            escapeContinuationCancels (EscapeCsi "27") `shouldBe` False
            escapeContinuationCancels EscapeOther `shouldBe` False
