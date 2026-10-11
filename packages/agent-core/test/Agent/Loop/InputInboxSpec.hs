{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
module Agent.Loop.InputInboxSpec (spec) where

import Prelude
import Agent.Loop.InputInbox
import Control.Concurrent.Async (cancel, withAsync)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Monad (when)
import Data.IORef
import Data.Text (Text)
import System.Timeout (timeout)
import Test.Hspec

spec :: Spec
spec = describe "Input inbox" do
    it "never acknowledges an arrival after the submitted batch" do
        rows <- newIORef [(1 :: Int, "initial" :: Text), (2, "steering")]
        inbox <- newInputInbox [1] (readIORef rows)
        readInputInbox inbox `shouldReturn` ["steering"]
        modifyIORef' rows (<> [(3, "later")])
        submittedInputInboxIds inbox `shouldReturn` [1, 2]
        acknowledgeInputInbox inbox 1
        acceptedInputInboxIds inbox `shouldReturn` [1, 2]
        readInputInbox inbox `shouldReturn` ["later"]

    it "extends an unaccepted batch behind its unchanged prefix" do
        rows <- newIORef [(1 :: Int, "initial" :: Text), (2, "steering")]
        inbox <- newInputInbox [1] (readIORef rows)
        readInputInbox inbox `shouldReturn` ["steering"]
        -- A request abandoned for newer input is resent with it.
        modifyIORef' rows (<> [(3, "newer")])
        readInputInbox inbox `shouldReturn` ["steering", "newer"]
        outstandingInputInboxIds inbox `shouldReturn` [2, 3]
        submittedInputInboxIds inbox `shouldReturn` [1, 2, 3]
        acknowledgeInputInbox inbox 2
        outstandingInputInboxIds inbox `shouldReturn` []
        readInputInbox inbox `shouldReturn` []

    it "preserves accepted and unaccepted steering across provider retries" do
        rows <- newIORef [(2 :: Int, "accepted" :: Text)]
        inbox <- newInputInbox [1] (readIORef rows)
        _ <- readInputInbox inbox
        acknowledgeInputInbox inbox 1
        modifyIORef' rows (<> [(3, "failed request")])
        readInputInbox inbox `shouldReturn` ["failed request"]
        -- A failed request has no acknowledgement. A new loop uses
        -- accepted items in replay and re-reads the same outstanding batch.
        acceptedInputInboxItems inbox `shouldReturn` ["accepted"]
        readInputInbox inbox `shouldReturn` ["failed request"]
        acknowledgeInputInbox inbox 1
        readInputInbox inbox `shouldReturn` []

    it "does not durably consume input until delivery completes" do
        rows <- newIORef [(1 :: Int, "initial" :: Text), (2, "steering")]
        delivered <- newIORef []
        inbox <- newInputInbox [1] (readIORef rows)
        _ <- readInputInbox inbox
        acknowledgeInputInbox inbox 1
        readIORef delivered `shouldReturn` []
        -- Reconstructing after a crash still sees every durable queue entry.
        recovered <- newInputInbox [] (readIORef rows)
        readInputInbox recovered `shouldReturn` ["initial", "steering"]
        completeInputInbox inbox (writeIORef delivered)
        readIORef delivered `shouldReturn` [1, 2]

    it "does not include an unaccepted batch in a completion receipt" do
        inbox <- newInputInbox [1 :: Int] (pure [(2, "not yet submitted" :: Text)])
        _ <- readInputInbox inbox
        receipt <- newIORef []
        completeInputInbox inbox (writeIORef receipt)
        readIORef receipt `shouldReturn` [1]

    it "retains input when durable completion fails" do
        inbox <- newInputInbox [1 :: Int] (pure [(2, "steering" :: Text)])
        _ <- readInputInbox inbox
        acknowledgeInputInbox inbox 1
        completeInputInbox inbox (\_ -> fail "database unavailable")
            `shouldThrow` anyIOException
        acceptedInputInboxIds inbox `shouldReturn` [1, 2]

    it "rejects invalid acknowledgement counts without changing the batch" do
        inbox <- newInputInbox [] (pure [(1 :: Int, "steering" :: Text)])
        _ <- readInputInbox inbox
        acknowledgeInputInbox inbox 2 `shouldThrow` anyIOException
        acknowledgeInputInbox inbox (-1) `shouldThrow` anyIOException
        readInputInbox inbox `shouldReturn` ["steering"]
        acceptedInputInboxIds inbox `shouldReturn` []

    it "uses identity rather than arrival ordering for newly published rows" do
        rows <- newIORef [(20 :: Int, "initial" :: Text)]
        inbox <- newInputInbox [20] (readIORef rows)
        modifyIORef' rows (<> [(10, "older transaction committed later")])
        readInputInbox inbox `shouldReturn` ["older transaction committed later"]

    it "acknowledges only the submitted prefix after a later read extends it" do
        rows <- newIORef [(2 :: Int, "submitted" :: Text)]
        inbox <- newInputInbox [1] (readIORef rows)
        submitted <- readInputInbox inbox
        modifyIORef' rows (<> [(3, "new arrival")])
        readInputInbox inbox `shouldReturn` ["submitted", "new arrival"]
        acknowledgeInputInbox inbox (length submitted)
        acceptedInputInboxItems inbox `shouldReturn` ["submitted"]
        acceptedSteeringIds inbox `shouldReturn` [2]
        outstandingInputInboxIds inbox `shouldReturn` [3]
        readInputInbox inbox `shouldReturn` ["new arrival"]
        acknowledgeInputInbox inbox 0
        outstandingInputInboxIds inbox `shouldReturn` [3]

    it "restores the accepted and pending state when loading fails" do
        rows <- newIORef [(2 :: Int, "accepted" :: Text)]
        shouldFail <- newIORef False
        inbox <- newInputInbox [1] do
            failing <- readIORef shouldFail
            when failing (fail "input source unavailable")
            readIORef rows
        _ <- readInputInbox inbox
        acknowledgeInputInbox inbox 1
        modifyIORef' rows (<> [(3, "pending")])
        _ <- readInputInbox inbox
        writeIORef shouldFail True
        readInputInbox inbox `shouldThrow` anyIOException
        writeIORef shouldFail False
        timeout 1000000 (acceptedInputInboxIds inbox) `shouldReturn` Just [1, 2]
        timeout 1000000 (readInputInbox inbox) `shouldReturn` Just ["pending"]

    it "restores the accepted and pending state when loading is cancelled" do
        rows <- newIORef [(2 :: Int, "accepted" :: Text)]
        shouldBlock <- newIORef False
        entered <- newEmptyMVar
        release <- newEmptyMVar
        inbox <- newInputInbox [1] do
            blocking <- readIORef shouldBlock
            when blocking do
                putMVar entered ()
                takeMVar release
            readIORef rows
        _ <- readInputInbox inbox
        acknowledgeInputInbox inbox 1
        modifyIORef' rows (<> [(3, "pending")])
        _ <- readInputInbox inbox
        writeIORef shouldBlock True
        withAsync (readInputInbox inbox) \reader -> do
            timeout 1000000 (takeMVar entered) `shouldReturn` Just ()
            cancel reader
        writeIORef shouldBlock False
        timeout 1000000 (acceptedInputInboxIds inbox) `shouldReturn` Just [1, 2]
        timeout 1000000 (readInputInbox inbox) `shouldReturn` Just ["pending"]
