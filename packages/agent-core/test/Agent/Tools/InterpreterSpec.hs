module Agent.Tools.InterpreterSpec (spec) where

import Agent.ToolArgs (objectArgs, reqText)
import Agent.ToolDispatch
    ( ToolArgumentStreamEvent(..)
    , ToolCall(..)
    , ToolCallStreamRef(..)
    , functionToolCall
    , noArgsTool
    , typedStreamingTool
    , typedTool
    , typedToolWithCall
    )
import Agent.Tools.Speculation
    ( ToolSpeculationRuntime
    , closeToolSpeculationRuntime
    , newToolSpeculationRuntime
    , observeToolArgumentEvent
    , retainToolSpeculation
    , takeToolSpeculation
    , takeToolSpeculationEmitting
    )
import Agent.Tools.Types
    ( AppTool
    , ToolExecutionPolicy(..)
    , jsonTool
    )
import Control.Exception.Safe (bracket)
import Control.Monad (forM_)
import Data.Aeson (FromJSON(..), ToJSON, object, (.=))
import qualified Data.Aeson as Aeson
import Data.ByteString.Lazy (toStrict)
import Data.IORef
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import Test.Hspec
import Test.Hspec.QuickCheck (modifyMaxSuccess, prop)
import Test.QuickCheck
    ( Arbitrary(..)
    , Gen
    , Property
    , chooseInt
    , elements
    , generate
    , ioProperty
    , listOf
    , scale
    , property
    , (===)
    , (.&&.)
    )

newtype EchoArgs = EchoArgs { message :: Text }
    deriving (Eq, Show)

instance FromJSON EchoArgs where
    parseJSON = objectArgs $ \object ->
        EchoArgs <$> reqText object "message"

instance ToJSON EchoArgs where
    toJSON (EchoArgs message) = object ["message" .= message]

newtype EchoMessage = EchoMessage { echoMessage :: Text }
    deriving (Eq, Show)

instance Arbitrary EchoMessage where
    arbitrary = EchoMessage . Text.pack <$> scale (min 48) (listOf echoChar)
      where
        echoChar = elements (['a' .. 'z'] <> ['0' .. '9'] <> " _-./")

spec :: Spec
spec = describe "streamed tool interpreters" do
    describe "typedTool" do
        modifyMaxSuccess (const 150) $
            prop "chunked prefixes match a single ToolDone" $ \msg ->
                ioProperty (chunkedMatchesWhole typedEcho (echoJson msg))
        modifyMaxSuccess (const 150) $
            prop "prefixes never run the handler" $ \msg ->
                ioProperty (prefixesDoNotRunHandler typedEcho (echoJson msg))
        modifyMaxSuccess (const 80) $
            prop "incomplete JSON is a miss" $ \msg ->
                ioProperty (incompleteJsonMisses typedEcho (echoJson msg))
        it "runs the handler once on ToolDone" do
            (runs, result) <- runCounting typedEcho "{\"message\":\"once\"}"
            runs `shouldBe` 1
            result `shouldBe` Just (Right "echo:once")

    describe "typedToolWithCall" do
        modifyMaxSuccess (const 80) $
            prop "consume sees the original call id" $ \msg ->
                ioProperty do
                    let json = echoJson msg
                    result <- evalStreamed callAwareEcho [] json
                    pure $ result === Just (Right ("call-1:" <> msg.echoMessage))

    describe "typedStreamingTool" do
        it "forwards snapshots from consume" do
            snapshots <- newIORef []
            result <- evalStreamedEmitting
                streamingEcho
                "{\"message\":\"snap\"}"
                (\chunk -> modifyIORef' snapshots (<> [chunk]))
            result `shouldBe` Just (Right "full:snap")
            readIORef snapshots `shouldReturn` ["part:snap"]

    describe "noArgsTool" do
        it "runs on ToolDone even with empty arguments" do
            (runs, result) <- runCounting noArgsEcho ""
            runs `shouldBe` 1
            result `shouldBe` Just (Right "ok")
        modifyMaxSuccess (const 80) $
            prop "prefixes never run a no-args handler" $ \msg ->
                ioProperty (prefixesDoNotRunHandler noArgsEcho (echoJson msg))

    describe "inits of a valid payload" do
        modifyMaxSuccess (const 80) $
            prop "every strict prefix misses and the full payload hits" $ \msg ->
                ioProperty do
                    let json = echoJson msg
                        prefixes = init (Text.inits json)
                    misses <- traverse (fmap fst . runCounting typedEcho) prefixes
                    (runs, result) <- runCounting typedEcho json
                    pure $
                        (all (== 0) misses === True)
                            .&&. (runs === 1)
                            .&&. (result === Just (Right ("echo:" <> msg.echoMessage)))

chunkedMatchesWhole :: (IORef Int -> AppTool) -> Text -> IO Property
chunkedMatchesWhole mkTool json = do
    chunks <- generateChunks json
    whole <- evalStreamed mkTool [] json
    chunked <- evalStreamed mkTool chunks json
    pure $ chunked === whole

prefixesDoNotRunHandler :: (IORef Int -> AppTool) -> Text -> IO Property
prefixesDoNotRunHandler mkTool json = do
    chunks <- generateChunks json
    runs <- newIORef (0 :: Int)
    bracket
        (newToolSpeculationRuntime [mkTool runs])
        closeToolSpeculationRuntime
        \runtime -> do
            streamPrefixes runtime "echo" chunks
            count <- readIORef runs
            pure $ count === 0

incompleteJsonMisses :: (IORef Int -> AppTool) -> Text -> IO Property
incompleteJsonMisses mkTool json
    | Text.length json < 2 = pure (property True)
    | otherwise = do
        let truncated = Text.dropEnd 1 json
        (runs, result) <- runCounting mkTool truncated
        pure $ (runs === 0) .&&. (result === Nothing)

evalStreamed
    :: (IORef Int -> AppTool)
    -> [Text]
    -> Text
    -> IO (Maybe (Either Text Text))
evalStreamed mkTool chunks json = do
    runs <- newIORef (0 :: Int)
    bracket
        (newToolSpeculationRuntime [mkTool runs])
        closeToolSpeculationRuntime
        \runtime -> do
            let call = functionToolCall "call-1" "echo" json
            streamTool runtime "echo" chunks json
            retainToolSpeculation runtime [call]
            takeToolSpeculation runtime call

evalStreamedEmitting
    :: (IORef Int -> AppTool)
    -> Text
    -> (Text -> IO ())
    -> IO (Maybe (Either Text Text))
evalStreamedEmitting mkTool json emit = do
    runs <- newIORef (0 :: Int)
    bracket
        (newToolSpeculationRuntime [mkTool runs])
        closeToolSpeculationRuntime
        \runtime -> do
            let call = functionToolCall "call-1" "echo" json
            streamTool runtime "echo" [] json
            retainToolSpeculation runtime [call]
            takeToolSpeculationEmitting runtime call emit

runCounting
    :: (IORef Int -> AppTool)
    -> Text
    -> IO (Int, Maybe (Either Text Text))
runCounting mkTool json = do
    runs <- newIORef (0 :: Int)
    result <-
        bracket
            (newToolSpeculationRuntime [mkTool runs])
            closeToolSpeculationRuntime
            \runtime -> do
                let call = functionToolCall "call-1" "echo" json
                streamTool runtime "echo" [] json
                retainToolSpeculation runtime [call]
                takeToolSpeculation runtime call
    count <- readIORef runs
    pure (count, result)

streamTool
    :: ToolSpeculationRuntime
    -> Text
    -> [Text]
    -> Text
    -> IO ()
streamTool runtime name chunks json = do
    streamPrefixes runtime name chunks
    observeToolArgumentEvent runtime $
        ToolArgumentsDone
            { argumentStreamRefs = [itemRef]
            , argumentStreamName = Just name
            , argumentStreamArguments = json
            }

streamPrefixes
    :: ToolSpeculationRuntime
    -> Text
    -> [Text]
    -> IO ()
streamPrefixes runtime name chunks = do
    observeToolArgumentEvent runtime $
        ToolArgumentsStarted
            { argumentStreamRefs = [itemRef]
            , argumentStreamCallId = "call-1"
            , argumentStreamName = Just name
            , argumentStreamArguments = ""
            }
    forM_ chunks \chunk ->
        observeToolArgumentEvent runtime $
            ToolArgumentsDelta
                { argumentStreamRefs = [itemRef]
                , argumentStreamDelta = chunk
                }

itemRef :: ToolCallStreamRef
itemRef = ToolCallStreamItem "item-1"

typedEcho :: IORef Int -> AppTool
typedEcho runs =
    jsonTool "echo" "echo" [] True ParallelSafe $
        typedTool "echo" $ \EchoArgs{message} -> do
            modifyIORef' runs (+ 1)
            pure (Right ("echo:" <> message))

callAwareEcho :: IORef Int -> AppTool
callAwareEcho runs =
    jsonTool "echo" "echo" [] True ParallelSafe $
        typedToolWithCall "echo" $ \call (EchoArgs message) -> do
            modifyIORef' runs (+ 1)
            pure (Right (call.callId <> ":" <> message))

streamingEcho :: IORef Int -> AppTool
streamingEcho runs =
    jsonTool "echo" "echo" [] True ParallelSafe $
        typedStreamingTool "echo" $ \emit EchoArgs{message} -> do
            modifyIORef' runs (+ 1)
            emit ("part:" <> message)
            pure (Right ("full:" <> message))

noArgsEcho :: IORef Int -> AppTool
noArgsEcho runs =
    jsonTool "echo" "echo" [] True ParallelSafe $
        noArgsTool "echo" $ do
            modifyIORef' runs (+ 1)
            pure (Right "ok")

echoJson :: EchoMessage -> Text
echoJson (EchoMessage message) =
    TextEncoding.decodeUtf8 $
        toStrict $
            Aeson.encode (EchoArgs message)

generateChunks :: Text -> IO [Text]
generateChunks json
    | Text.null json = pure []
    | otherwise = generate (chunkText json)

chunkText :: Text -> Gen [Text]
chunkText text
    | Text.null text = pure []
    | otherwise = do
        n <- chooseInt (1, Text.length text)
        let (chunk, rest) = Text.splitAt n text
        (chunk :) <$> chunkText rest
