module Agent.Runtime.SessionOwnerSpec (spec) where

import Agent.Runtime.SessionOwner
import Control.Concurrent.Async (concurrently_, withAsync, wait)
import Control.Concurrent.MVar
    (newEmptyMVar, putMVar, takeMVar, tryTakeMVar)
import Control.Exception.Safe (finally)
import qualified Data.Map.Strict as Map
import System.Timeout (timeout)
import Test.Hspec

spec :: Spec
spec = describe "runtime session owner" do
    it "rejects duplicate and excess work without queuing or executing it" $
        withSessionOwner 1 \owner -> do
            started <- newEmptyMVar
            release <- newEmptyMVar
            rejected <- newEmptyMVar
            submitSessionTurn owner "one" silent
                (putMVar started () >> takeMVar release >> pure (Right ()))
                `shouldReturn` Right ()
            within (takeMVar started)
            submitSessionTurn owner "one" silent
                (putMVar rejected () >> pure (Right ()))
                `shouldReturn` Left SessionBusy
            submitSessionTurn owner "two" silent
                (putMVar rejected () >> pure (Right ()))
                `shouldReturn` Left OwnerAtCapacity
            Just (_, wait) <- prepareSessionWait owner "one"
            putMVar release ()
            within wait `shouldReturn` SessionCompleted
            tryTakeMVar rejected `shouldReturn` Nothing
            submitSessionTurn owner "two" silent (pure (Right ()))
                `shouldReturn` Right ()

    it "captures the exact generation across a quick retry" $
        withSessionOwner 1 \owner -> do
            release <- newEmptyMVar
            submitSessionTurn owner "one" silent
                (takeMVar release >> pure (Left "first"))
                `shouldReturn` Right ()
            Just (_, firstWait) <- prepareSessionWait owner "one"
            putMVar release ()
            within firstWait `shouldReturn` SessionFailed "first"
            secondRelease <- newEmptyMVar
            submitSessionTurn owner "one" silent
                (takeMVar secondRelease >> pure (Right ()))
                `shouldReturn` Right ()
            within firstWait `shouldReturn` SessionFailed "first"
            cancelSessionTurn owner "one" `shouldReturn` True

    it "joins cancellation cleanup before allowing another generation" $
        withSessionOwner 1 \owner -> do
            started <- newEmptyMVar
            release <- newEmptyMVar
            finished <- newEmptyMVar
            submitSessionTurn owner "one" silent
                ((putMVar started () >> takeMVar release >> pure (Right ()))
                    `finally` putMVar finished ())
                `shouldReturn` Right ()
            within (takeMVar started)
            cancelSessionTurn owner "one" `shouldReturn` True
            tryTakeMVar finished `shouldReturn` Just ()
            sessionOwnerSnapshot owner `shouldReturn`
                (False, Map.singleton "one" (SessionFinished SessionCancelled))
            submitSessionTurn owner "one" silent (pure (Right ()))
                `shouldReturn` Right ()

    it "publishes completion before a captured waiter returns" $
        withSessionOwner 1 \owner -> do
            notified <- newEmptyMVar
            submitSessionTurn owner "one" (putMVar notified) (pure (Left "provider"))
                `shouldReturn` Right ()
            Just (_, wait) <- prepareSessionWait owner "one"
            within wait `shouldReturn` SessionFailed "provider"
            tryTakeMVar notified `shouldReturn` Just (SessionFailed "provider")

    it "does not replace the outcome when a notification fails" $
        withSessionOwner 1 \owner -> do
            submitSessionTurn owner "one" (const (fail "notification")) (pure (Right ()))
                `shouldReturn` Right ()
            Just (_, captured) <- prepareSessionWait owner "one"
            within captured `shouldReturn` SessionCompleted

    it "does not inject repeated cancellation into cleanup" $
        withSessionOwner 1 \owner -> do
            started <- newEmptyMVar
            release <- newEmptyMVar
            cleaning <- newEmptyMVar
            finishCleanup <- newEmptyMVar
            cleaned <- newEmptyMVar
            submitSessionTurn owner "one" silent
                ((putMVar started () >> takeMVar release >> pure (Right ()))
                    `finally` (putMVar cleaning () >> takeMVar finishCleanup >> putMVar cleaned ()))
                `shouldReturn` Right ()
            within (takeMVar started)
            withAsync (cancelSessionTurn owner "one") \canceller -> do
                within (takeMVar cleaning)
                withAsync (closeSessionOwner owner) \closer -> do
                    putMVar finishCleanup ()
                    wait canceller `shouldReturn` True
                    wait closer
            tryTakeMVar cleaned `shouldReturn` Just ()

    it "concurrent closes join all workers and reject future admission" $
        withSessionOwner 1 \owner -> do
            started <- newEmptyMVar
            release <- newEmptyMVar
            finished <- newEmptyMVar
            submitSessionTurn owner "one" silent
                ((putMVar started () >> takeMVar release >> pure (Right ()))
                    `finally` putMVar finished ())
                `shouldReturn` Right ()
            within (takeMVar started)
            concurrently_ (closeSessionOwner owner) (closeSessionOwner owner)
            tryTakeMVar finished `shouldReturn` Just ()
            sessionOwnerSnapshot owner `shouldReturn` (True, Map.empty)
            submitSessionTurn owner "two" silent (pure (Right ()))
                `shouldReturn` Left OwnerClosed

    it "supports a zero-capacity owner without launching workers" $
        withSessionOwner 0 \owner ->
            submitSessionTurn owner "one" silent (pure (Right ()))
                `shouldReturn` Left OwnerAtCapacity
  where
    silent _ = pure ()

within :: IO a -> IO a
within action =
    timeout 5000000 action >>= maybe (fail "session owner handshake timed out") pure
