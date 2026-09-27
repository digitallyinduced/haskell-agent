{-# LANGUAGE OverloadedStrings #-}
module Agent.Tools.CodeMode.Haskell.HostSpec (spec) where

import Agent.Tools.CodeMode.Haskell.Host
import Agent.Tools.CodeMode.Host.Types
import Agent.ToolDispatch (ToolResultFile(..))
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Concurrent.Async (AsyncCancelled)
import qualified Control.Exception as Exception
import Control.Exception.Safe (bracket)
import Data.Aeson (Value(..))
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString as BS
import qualified Data.Vector as Vector
import Data.IORef
import qualified Data.Text as Text
import System.Directory (doesFileExist)
import System.Timeout (timeout)
import Test.Hspec

nativeFiles :: Either CodeModeError CodeModeResult -> [Value]
nativeFiles result = case result of
    Right (CodeModeFinished _ value) -> extract value
    Right (CodeModeFailed _ value _) -> extract value
    Right (CodeModeRunning _ value) -> extract value
    _ -> []
  where
    extract (Object fields)
        | Just (Array values) <- KeyMap.lookup "content" fields =
            [value | value@(Object part) <- Vector.toList values,
                KeyMap.lookup "type" part == Just (String "native_file")]
    extract _ = []

spec :: Spec
spec = describe "GHCi code-mode host" $ aroundAll withHost $ do
    it "executes complete cells, captures Unicode and preserves explicit state" $ \host -> do
        result <- execute host "store \"answer\" (Number 42) >> text \"λ-calculus\""
        result `shouldSatisfy` succeeded
        resultText result `shouldSatisfy` Text.isInfixOf "\\955"
        loaded <- execute host "load \"answer\" >>= json"
        resultText loaded `shouldSatisfy` Text.isInfixOf "42"

    it "forwards native files outside callback values and the text clipping budget" $ \host -> do
        let file = ToolResultFile "report.pdf" "application/pdf" (BS.replicate (2 * 1024 * 1024) 65)
        result <- execHaskellCellWithFilesAndRepair Nothing host
            "callTool \"read_file\" Null >>= json" emptyBindings
            (\_ _ -> pure (Right (String "document attached", [file]))) 10000
        result `shouldSatisfy` succeeded
        length (nativeFiles result) `shouldBe` 1
        resultText result `shouldSatisfy` Text.isInfixOf "document attached"

    it "bounds concurrent native files per cell and resets the budget for the next cell" $ \host -> do
        let file = ToolResultFile "report.pdf" "application/pdf" "pdf"
            handler _ _ = pure (Right (Null, [file]))
        result <- execHaskellCellWithFilesAndRepair Nothing host
            "mapConcurrently_ (const (callTool \"read_file\" Null)) [1..9 :: Int]"
            emptyBindings handler 10000
        result `shouldSatisfy` failed
        length (nativeFiles result) `shouldSatisfy` (<= 8)
        resultText result `shouldSatisfy` Text.isInfixOf "native file output exceeds"
        recovery <- execHaskellCellWithFilesAndRepair Nothing host
            "void (callTool \"read_file\" Null)" emptyBindings handler 10000
        recovery `shouldSatisfy` succeeded
        length (nativeFiles recovery) `shouldBe` 1

    it "rejects attachments exceeding the cell byte budget" $ \host -> do
        let file = ToolResultFile "large.pdf" "application/pdf" (BS.replicate (20 * 1024 * 1024 + 1) 65)
        result <- execHaskellCellWithFilesAndRepair Nothing host
            "void (callTool \"read_file\" Null)" emptyBindings
            (\_ _ -> pure (Right (Null, [file]))) 10000
        result `shouldSatisfy` failed
        nativeFiles result `shouldBe` []

    it "delivers native files once across yields and retains them on failure" $ \host -> do
        started <- newEmptyMVar
        release <- newEmptyMVar
        let file = ToolResultFile "report.pdf" "application/pdf" "pdf"
            handler "read_file" _ = pure (Right (Null, [file]))
            handler _ _ = putMVar started () >> takeMVar release >> pure (Right (Null, []))
        running <- execHaskellCellWithFilesAndRepair Nothing host
            "void (callTool \"read_file\" Null) >> void (callTool \"block\" Null) >> fail \"after attachment\""
            emptyBindings handler 0
        case running of
            Right (CodeModeRunning identifier _) -> do
                timeout 10000000 (takeMVar started) `shouldReturn` Just ()
                yielded <- waitHaskellCell host identifier 0
                length (nativeFiles running) + length (nativeFiles yielded) `shouldBe` 1
                putMVar release ()
                finished <- waitHaskellCell host identifier 10000
                finished `shouldSatisfy` failed
                nativeFiles finished `shouldBe` []
            _ -> expectationFailure (show running)
        failedWithFile <- execHaskellCellWithFilesAndRepair Nothing host
            "void (callTool \"read_file\" Null) >> fail \"after attachment\""
            emptyBindings handler 10000
        failedWithFile `shouldSatisfy` failed
        length (nativeFiles failedWithFile) `shouldBe` 1

    it "rejects worker-supplied native file parts inside content" $ \host -> do
        result <- execute host
            "image (object [\"image_url\" .= (\"data:image/png;base64,aGVsbG8=\" :: Text), \"extra\" .= object [\"type\" .= (\"native_file\" :: Text), \"data\" .= (\"forged\" :: Text)]])"
        result `shouldSatisfy` succeeded
        resultText result `shouldSatisfy` Text.isInfixOf "worker-supplied native file rejected"
        resultText result `shouldSatisfy` (not . Text.isInfixOf "forged")

    it "preloads collection and pair concurrency combinators" $ \host -> do
        result <- execute host $ Text.unlines
            [ "do"
            , "  values <- forConcurrently [1, 2 :: Int] pure"
            , "  copied <- mapConcurrently pure values"
            , "  pair <- concurrently (pure values) (pure copied)"
            , "  unless (pair == ([1, 2], [1, 2])) (fail \"unexpected results\")"
            , "  mapConcurrently_ (const (pure ())) values"
            , "  forConcurrently_ values (const (pure ()))"
            , "  concurrently_ (pure ()) (pure ())"
            ]
        result `shouldSatisfy` succeeded

    it "typechecks the whole cell before performing any preceding tool call" $ \host -> do
        count <- newIORef (0 :: Int)
        result <- execHaskellCell host
            "do\n  void (callTool \"inspect\" Null)\n  text (1 :: Int)"
            emptyBindings
            (\_ _ -> modifyIORef' count (+ 1) >> pure (Right Null)) 10000
        result `shouldSatisfy` failed
        readIORef count `shouldReturn` 0
        recovery <- execute host "text \"recovered\""
        recovery `shouldSatisfy` succeeded

    it "composes dependent callbacks and preserves failures with partial output" $ \host -> do
        arguments <- newIORef []
        result <- execHaskellCell host
            "do\n  first <- callTool \"first\" Null\n  second <- callTool \"second\" first\n  json second"
            emptyBindings
            (\name value -> do
                modifyIORef' arguments ((name, value) :)
                pure (Right (String name))) 10000
        result `shouldSatisfy` succeeded
        readIORef arguments `shouldReturn`
            [("second", String "first"), ("first", Null)]
        rejected <- execHaskellCell host
            "text \"before denial\" >> void (callTool \"restricted\" Null)"
            emptyBindings (\_ _ -> pure (Left "approval denied")) 10000
        rejected `shouldSatisfy` failed
        resultText rejected `shouldSatisfy` Text.isInfixOf "before denial"
        resultText rejected `shouldSatisfy` Text.isInfixOf "approval denied"

    it "repairs compiler errors in isolation before executing effects once" $ \host -> do
        requests <- newIORef []
        count <- newIORef (0 :: Int)
        let original = "void (callTool \"inspect\" Null) >> text (1 :: Int)"
            revised = "void (callTool \"inspect\" Null) >> text \"1\""
            repair request = do
                readIORef count `shouldReturn` 0
                modifyIORef' requests (request :)
                pure (Just revised)
        result <- execHaskellCellWithRepair (Just repair) host original emptyBindings
            (\_ _ -> modifyIORef' count (+ 1) >> pure (Right Null)) 10000
        result `shouldSatisfy` succeeded
        readIORef count `shouldReturn` 1
        observed <- readIORef requests
        length observed `shouldBe` 1
        let request = head observed
        request.repairOriginalSource `shouldBe` original
        request.repairCurrentSource `shouldBe` original
        request.repairBindings `shouldBe` emptyBindings
        request.repairAttempt `shouldBe` 1
        request.repairDiagnostics `shouldSatisfy` Text.isInfixOf "Int"
        request.repairEnvironment `shouldSatisfy` Text.isInfixOf "mapConcurrently"
        request.repairEnvironment `shouldSatisfy` Text.isInfixOf "text :: Text -> IO ()"
        resultText result `shouldSatisfy` Text.isInfixOf "repaired before execution"
        resultText result `shouldSatisfy` (not . Text.isInfixOf "Couldn't match")

    it "limits repair to two attempts and returns the last compiler diagnostics" $ \host -> do
        requests <- newIORef []
        let repair request = do
                modifyIORef' requests (request :)
                pure (Just ("text (" <> Text.pack (show request.repairAttempt) <> " :: Bool)"))
        result <- execHaskellCellWithRepair (Just repair) host "text (0 :: Int)"
            emptyBindings (\_ _ -> expectationFailure "unexpected effect" >> pure (Right Null)) 10000
        result `shouldSatisfy` failed
        observed <- readIORef requests
        map (.repairAttempt) (reverse observed) `shouldBe` [1, 2]
        map (.repairOriginalSource) observed `shouldBe` replicate 2 "text (0 :: Int)"
        resultText result `shouldSatisfy` Text.isInfixOf "Bool"

    it "does not repair runtime failures after effects have occurred" $ \host -> do
        repairs <- newIORef (0 :: Int)
        effects <- newIORef (0 :: Int)
        let repair _ = modifyIORef' repairs (+ 1) >> pure (Just "pure ()")
        result <- execHaskellCellWithRepair (Just repair) host
            "void (callTool \"inspect\" Null) >> fail \"runtime failure\""
            emptyBindings (\_ _ -> modifyIORef' effects (+ 1) >> pure (Right Null)) 10000
        result `shouldSatisfy` failed
        readIORef effects `shouldReturn` 1
        readIORef repairs `shouldReturn` 0
        resultText result `shouldSatisfy` Text.isInfixOf "runtime failure"

    it "returns compiler diagnostics when the repair callback fails or declines" $ \host -> do
        let unavailable _ = fail "repair provider unavailable"
            declined _ = pure Nothing
        mapM_ (\repair -> do
            result <- execHaskellCellWithRepair (Just repair) host "text (0 :: Int)"
                emptyBindings (\_ _ -> pure (Right Null)) 10000
            result `shouldSatisfy` failed
            resultText result `shouldSatisfy` Text.isInfixOf "Int") [unavailable, declined]
        execute host "pure ()" >>= (`shouldSatisfy` succeeded)

    it "does not repair failures in generated bindings" $ \host -> do
        repairs <- newIORef (0 :: Int)
        let repair _ = modifyIORef' repairs (+ 1) >> pure (Just "pure ()")
        result <- execHaskellCellWithRepair (Just repair) host "text (0 :: Int)"
            "module Tools where\ninvalid :: Int\ninvalid = True\n"
            (\_ _ -> pure (Right Null)) 10000
        result `shouldSatisfy` failed
        readIORef repairs `shouldReturn` 0
        resultText result `shouldSatisfy` Text.isInfixOf "invalid"
        execute host "pure ()" >>= (`shouldSatisfy` succeeded)

    it "cancels an outstanding repair when its cell is terminated" $ \host -> do
        entered <- newEmptyMVar
        blocked <- newEmptyMVar
        cancelled <- newEmptyMVar
        let repair _ = (putMVar entered () >> takeMVar blocked)
                `Exception.finally` putMVar cancelled ()
        running <- execHaskellCellWithRepair (Just repair) host "text (0 :: Int)"
            emptyBindings (\_ _ -> pure (Right Null)) 1
        identifier <- runningIdentifier running
        timeout 10000000 (takeMVar entered) `shouldReturn` Just ()
        terminated <- terminateHaskellCell host identifier
        terminated `shouldSatisfy` \case
            Right CodeModeTerminated{} -> True
            _ -> False
        timeout 1000000 (takeMVar cancelled) `shouldReturn` Just ()
        execute host "pure ()" >>= (`shouldSatisfy` succeeded)

    it "overlaps callbacks and demultiplexes their replies" $ \host -> do
        entered <- newTVarIO (0 :: Int)
        release <- newEmptyMVar
        running <- execHaskellCell host
            "do\n  replies <- mapConcurrently (\\name -> callTool name Null) [\"first\", \"second\"]\n  unless (replies == [String \"first\", String \"second\"]) (fail \"replies crossed\")\n  json (toJSON replies)"
            emptyBindings
            (\name _ -> do
                atomically (modifyTVar' entered (+ 1))
                readMVar release
                pure (Right (String name))) 1
        identifier <- runningIdentifier running
        overlapping <- timeout 10000000 $
            atomically (readTVar entered >>= check . (== 2))
        putMVar release ()
        overlapping `shouldBe` Just ()
        finished <- waitHaskellCell host identifier 10000
        finished `shouldSatisfy` succeeded
        resultText finished `shouldSatisfy` Text.isInfixOf "first"
        resultText finished `shouldSatisfy` Text.isInfixOf "second"

    it "yields without stopping and rejects another cell while busy" $ \host -> do
        entered <- newEmptyMVar
        release <- newEmptyMVar
        running <- execHaskellCell host
            "text \"before wait\" >> void (callTool \"blocked\" Null) >> text \"after wait\""
            emptyBindings
            (\_ _ -> putMVar entered () >> takeMVar release >> pure (Right Null)) 1
        identifier <- runningIdentifier running
        takeMVar entered
        busy <- execute host "pure ()"
        busy `shouldSatisfy` resourceFailure
        first <- waitHaskellCell host identifier 0
        resultText first `shouldSatisfy` Text.isInfixOf "before wait"
        putMVar release ()
        finished <- waitHaskellCell host identifier 10000
        finished `shouldSatisfy` succeeded
        resultText finished `shouldSatisfy` Text.isInfixOf "after wait"
        resultText finished `shouldSatisfy` (not . Text.isInfixOf "before wait")

    it "does not deliver an abandoned callback reply to the next cell" $ \host -> do
        entered <- newEmptyMVar
        blocked <- newEmptyMVar
        cancelled <- newEmptyMVar
        let bindings = Text.unlines
                [ "{-# LANGUAGE OverloadedStrings #-}"
                , "module Tools where"
                , "import CodeModeSupport"
                , "import Control.Concurrent.Async (race)"
                , "import Control.Monad (void)"
                , "import Data.Aeson"
                , "abandon :: IO ()"
                , "abandon = void $ race (callTool \"stale\" Null) (callTool \"ready\" Null)"
                ]
            handler "stale" _ =
                (putMVar entered () >> takeMVar blocked >> pure (Right Null))
                    `Exception.catch` \(_ :: AsyncCancelled) -> do
                        -- Return during barrier unwinding, after its acknowledgement.
                        putMVar cancelled ()
                        pure (Right (String "stale reply"))
            handler _ _ = readMVar entered >> pure (Right Null)
        result <- execHaskellCell host "Tools.abandon" bindings handler 10000
        result `shouldSatisfy` succeeded
        timeout 1000000 (takeMVar cancelled) `shouldReturn` Just ()
        next <- execHaskellCell host
            "do\n  value <- callTool \"fresh\" Null\n  unless (value == String \"fresh reply\") (fail \"stale reply crossed cell boundary\")"
            emptyBindings (\_ _ -> pure (Right (String "fresh reply"))) 10000
        next `shouldSatisfy` succeeded

    it "terminates a blocked callback and permits a fresh worker afterward" $ \host -> do
        stored <- execute host "store \"restartState\" (Number 42)"
        stored `shouldSatisfy` succeeded
        entered <- newEmptyMVar
        blocked <- newEmptyMVar
        running <- execHaskellCell host "void (callTool \"blocked\" Null)"
            emptyBindings
            (\_ _ -> putMVar entered () >> takeMVar blocked >> pure (Right Null)) 1
        identifier <- runningIdentifier running
        takeMVar entered
        terminated <- terminateHaskellCell host identifier
        terminated `shouldSatisfy` \case
            Right CodeModeTerminated{} -> True
            _ -> False
        result <- execute host "load \"restartState\" >>= json"
        result `shouldSatisfy` succeeded
        resultText result `shouldSatisfy` Text.isInfixOf "42"

    it "captures stdout without a trailing newline" $ \host -> do
        result <- execute host "putStr \"unterminated output\""
        result `shouldSatisfy` succeeded
        resultText result `shouldSatisfy` Text.isInfixOf "unterminated output"

    it "loads fresh tool bindings without retaining a removed function" $ \host -> do
        let bindings = Text.unlines
                [ "{-# LANGUAGE OverloadedStrings #-}"
                , "module Tools where"
                , "import qualified CodeModeSupport"
                , "import Data.Aeson"
                , "inspect :: IO Value"
                , "inspect = CodeModeSupport.callTool \"inspect\" Null"
                ]
        result <- execHaskellCell host "Tools.inspect >>= json" bindings
            (\name _ -> pure (Right (String name))) 10000
        result `shouldSatisfy` succeeded
        resultText result `shouldSatisfy` Text.isInfixOf "inspect"
        removed <- execute host "Tools.inspect >>= json"
        removed `shouldSatisfy` failed

    it "validates bindings before publication and recovers after invalid bindings" $ \host -> do
        prepareHaskellBindings host emptyBindings `shouldReturn` Right ()
        rejected <- prepareHaskellBindings host
            "module Tools where\ninvalid :: Int\ninvalid = True\n"
        rejected `shouldSatisfy` either (const True) (const False)
        prepareHaskellBindings host emptyBindings `shouldReturn` Right ()

    it "reports worker death and starts another worker without replaying effects" $ \host -> do
        let bindings = Text.unlines
                [ "module Tools where"
                , "import System.Exit (ExitCode(..))"
                , "import System.Posix.Process (exitImmediately)"
                , "terminate :: IO ()"
                , "terminate = exitImmediately (ExitFailure 1)"
                ]
        result <- execHaskellCell host "Tools.terminate" bindings
            (\_ _ -> pure (Right Null)) 10000
        result `shouldSatisfy` failed
        recovery <- execute host "text \"new worker\""
        recovery `shouldSatisfy` succeeded

    it "bounds oversized output and forwards image envelopes" $ \host -> do
        truncated <- execute host "text (Text.replicate 1100000 \"a\")"
        truncated `shouldSatisfy` succeeded
        resultText truncated `shouldSatisfy` Text.isInfixOf "truncated"
        Text.length (resultText truncated) `shouldSatisfy` (< 2048)
        media <- execute host "image (object [\"image_url\" .= (\"data:image/png;base64,AAAA\" :: Text)])"
        media `shouldSatisfy` succeeded
        resultText media `shouldSatisfy` Text.isInfixOf "image_url"

withHost :: (HaskellHost -> IO ()) -> IO ()
withHost action = do
    let repositoryPath = "packages/agent-tools/data/code-mode/CodeModeSupport.hs"
    repositoryLayout <- doesFileExist repositoryPath
    let support = if repositoryLayout then repositoryPath else "data/code-mode/CodeModeSupport.hs"
    bracket
        (newHaskellHost support >>= either (fail . Text.unpack) pure)
        closeHaskellHost
        action

emptyBindings :: Text.Text
emptyBindings = "module Tools where\n"

execute :: HaskellHost -> Text.Text -> IO (Either CodeModeError CodeModeResult)
execute host source =
    execHaskellCell host source emptyBindings (\_ _ -> pure (Right Null)) 10000

succeeded :: Either CodeModeError CodeModeResult -> Bool
succeeded (Right CodeModeFinished{}) = True
succeeded _ = False

failed :: Either CodeModeError CodeModeResult -> Bool
failed (Right CodeModeFailed{}) = True
failed _ = False

resourceFailure :: Either CodeModeError CodeModeResult -> Bool
resourceFailure (Left CodeModeResourceError{}) = True
resourceFailure _ = False

runningIdentifier :: Either CodeModeError CodeModeResult -> IO Text.Text
runningIdentifier (Right (CodeModeRunning identifier _)) = pure identifier
runningIdentifier result = fail ("expected running cell, received " <> show result)

resultText :: Either CodeModeError CodeModeResult -> Text.Text
resultText = Text.pack . show
