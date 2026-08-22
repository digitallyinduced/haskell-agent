module Agent.CLI.CancelWatchSpec (spec) where

import Agent.CLI.CancelWatch
    ( isBareEscape
    , newStdinGate
    , readBareEscape
    , withStdinPaused
    )
import Control.Concurrent.MVar
    ( newEmptyMVar
    , putMVar
    , takeMVar
    )
import Control.Concurrent.Async (wait, withAsync)
import Control.Exception.Safe (bracket)
import qualified Data.ByteString as BS
import System.IO (Handle, hClose, hFlush)
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

    describe "readBareEscape" do
        it "treats a lone ESC as bare after a bounded wait" $
            withPipe \readHandle _writeHandle -> do
                completed <- timeout 500_000 (readBareEscape readHandle)
                completed `shouldBe` Just True

        it "does not block on an incomplete SS3 sequence" $
            withPipe \readHandle writeHandle -> do
                BS.hPut writeHandle (BS.singleton 0x4f)
                hFlush writeHandle
                completed <- timeout 500_000 (readBareEscape readHandle)
                completed `shouldBe` Just False

    describe "withStdinPaused" do
        it "serializes concurrent stdin users for the full prompt" do
            stdinGate <- newStdinGate
            firstEntered <- newEmptyMVar
            releaseFirst <- newEmptyMVar
            secondAttempting <- newEmptyMVar
            secondEntered <- newEmptyMVar
            withAsync
                (withStdinPaused stdinGate do
                    putMVar firstEntered ()
                    takeMVar releaseFirst)
                \first -> do
                    takeMVar firstEntered
                    withAsync
                        (do
                            putMVar secondAttempting ()
                            withStdinPaused stdinGate $
                                putMVar secondEntered ())
                        \second -> do
                            takeMVar secondAttempting
                            enteredEarly <- timeout 100_000 $
                                takeMVar secondEntered
                            enteredEarly `shouldBe` Nothing
                            putMVar releaseFirst ()
                            wait first
                            enteredAfterRelease <- timeout 500_000 $
                                takeMVar secondEntered
                            enteredAfterRelease `shouldBe` Just ()
                            wait second

        it "allows nested prompt helpers on the owning thread" do
            stdinGate <- newStdinGate
            completed <- timeout 500_000 $
                withStdinPaused stdinGate $
                    withStdinPaused stdinGate $
                        pure ()
            completed `shouldBe` Just ()

withPipe :: (Handle -> Handle -> IO a) -> IO a
withPipe action =
    bracket acquire release (uncurry action)
  where
    acquire = do
        (readFd, writeFd) <- createPipe
        (,) <$> fdToHandle readFd <*> fdToHandle writeFd
    release (readHandle, writeHandle) = do
        hClose readHandle
        hClose writeHandle
