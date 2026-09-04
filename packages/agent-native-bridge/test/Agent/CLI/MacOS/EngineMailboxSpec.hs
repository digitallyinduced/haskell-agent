module Agent.CLI.MacOS.EngineMailboxSpec (spec) where

import Agent.CLI.MacOS.EngineMailbox
    ( EngineMailbox
    , acceptEngineCommand
    , closeEngineMailbox
    , newEngineMailboxIO
    , readEngineCommand
    )
import Control.Concurrent.Async (concurrently)
import Control.Concurrent.STM (atomically)
import Control.Monad (replicateM_)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "native engine mailbox" do
    it "orders every accepted callback command before concurrent shutdown" $
        replicateM_ 1000 do
            mailbox <- newEngineMailboxIO :: IO (EngineMailbox String)
            (accepted, closed) <- concurrently
                (atomically (acceptEngineCommand mailbox "restart"))
                (atomically (closeEngineMailbox mailbox "stop"))
            closed `shouldBe` True
            first <- atomically (readEngineCommand mailbox)
            if accepted
                then do
                    second <- atomically (readEngineCommand mailbox)
                    [first, second] `shouldBe` ["restart", "stop"]
                else first `shouldBe` "stop"

    it "rejects callback commands after shutdown wins" do
        mailbox <- newEngineMailboxIO :: IO (EngineMailbox String)
        closed <- atomically (closeEngineMailbox mailbox "stop")
        accepted <- atomically (acceptEngineCommand mailbox "restart")
        command <- atomically (readEngineCommand mailbox)
        closed `shouldBe` True
        accepted `shouldBe` False
        command `shouldBe` "stop"
