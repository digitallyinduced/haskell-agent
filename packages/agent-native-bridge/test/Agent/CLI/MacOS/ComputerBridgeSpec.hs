{-# LANGUAGE ForeignFunctionInterface #-}

module Agent.CLI.MacOS.ComputerBridgeSpec (spec) where

import Agent.CLI.ComputerUse.Accessibility
    ( AccessibilityObservation(..)
    )
import Agent.CLI.MacOS.ComputerBridge
    ( ComputerCallback
    , ComputerHost
    , ComputerRegistration(..)
    , NativeComputerResult(..)
    , closeComputerSession
    , computerToolSessionWhenEnabled
    , invokeComputerSessionRequest
    , newComputerHost
    , newComputerSession
    , replaceComputerRegistration
    , resetComputerSessionAccessibility
    )
import Agent.ComputerUse.Protocol
    ( SemanticComputerAction(..)
    , SemanticComputerRequest(..)
    , SemanticComputerScalar(..)
    , semanticComputerRequestWireValue
    )
import Agent.ToolDispatch
    ( ToolCall(..)
    , ToolCallKind(..)
    , ToolDispatchConfig(..)
    , ToolDispatchOutcome(..)
    , ToolResultImage(..)
    , dispatchToolHandlerDetailed
    , toolCallResultImages
    )
import Agent.Tools.Types
    ( AppTool(..)
    , ApprovalRule(..)
    , ToolExecutionPolicy(..)
    , ToolSchema(..)
    )
import Control.Concurrent.Async (concurrently)
import Control.Concurrent.MVar
    ( MVar
    , newEmptyMVar
    , putMVar
    , takeMVar
    )
import Control.Exception.Safe (bracket)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString as BS
import Data.Either (isRight)
import Data.IORef
    ( IORef
    , atomicModifyIORef'
    , modifyIORef'
    , newIORef
    , readIORef
    )
import qualified Data.List.NonEmpty as NonEmpty
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import Data.Word (Word8)
import Foreign
    ( FunPtr
    , Ptr
    , castPtr
    , copyBytes
    , freeHaskellFunPtr
    , nullPtr
    , poke
    )
import Foreign.C.Types (CInt(..), CSize(..))
import Test.Hspec
    ( Spec
    , describe
    , expectationFailure
    , it
    , shouldBe
    , shouldReturn
    , shouldSatisfy
    )

foreign import ccall "ha_computer_callback_abi_smoke"
    computerCallbackABISmoke :: IO CInt

foreign import ccall "wrapper"
    wrapComputerCallback :: ComputerCallback -> IO (FunPtr ComputerCallback)

spec :: Spec
spec = describe "native AX-first computer bridge" do
    it "matches the authoritative v3 C ABI" do
        computerCallbackABISmoke `shouldReturn` 0

    it "publishes only semantic operations and defaults screenshots off" do
        requests <- newIORef []
        closes <- newIORef 0
        withHost (recordingCallback "current" requests closes) \host -> do
            computerToolSessionWhenEnabled host >>= \case
                Left err -> expectationFailure (Text.unpack err)
                Right Nothing -> expectationFailure "native computer tool was disabled"
                Right (Just (tool, _, close)) -> do
                    case tool.appToolSchema of
                        HostedComputerFunctionSchema parameters -> do
                            let serialized = Aeson.encode parameters
                            Aeson.decode serialized `shouldBe` Just parameters
                            parameters `shouldBe` expectedComputerParameters
                        schema -> expectationFailure
                            ("unexpected computer schema: " <> show schema)
                    case tool.appToolApproval of
                        AlwaysPrompt -> pure ()
                        _ -> expectationFailure
                            "native computer tool must always require approval"
                    case tool.appToolExecution of
                        TurnSequential -> pure ()
                        _ -> expectationFailure
                            "native computer tool must run sequentially"
                    result <- runTool tool
                        (ToolCall
                            "call"
                            "computer"
                            ( "{\"operation\":\"observe\",\"target_id\":null,"
                            <> "\"actions\":null,\"include_screenshot\":false}"
                            )
                            ComputerFunctionCallKind
                            False)
                    result.toolDispatchSucceeded `shouldBe` True
                    toolCallResultImages result.toolDispatchResult `shouldBe` []
                    semantic <- runTool tool
                        (ToolCall
                            "semantic"
                            "computer"
                            ( "{\"operation\":\"act\",\"target_id\":null,"
                            <> "\"actions\":["
                            <> "{\"type\":\"perform\",\"element_id\":\"e1\","
                            <> "\"action\":\"AXPress\",\"value\":null,\"text\":null},"
                            <> "{\"type\":\"set_value\",\"element_id\":\"e2\","
                            <> "\"action\":null,\"value\":42,\"text\":null},"
                            <> "{\"type\":\"replace_selected_text\","
                            <> "\"element_id\":\"e3\",\"action\":null,\"value\":null,"
                            <> "\"text\":\"hello\"}],\"include_screenshot\":false}"
                            )
                            ComputerFunctionCallKind
                            False)
                    semantic.toolDispatchSucceeded `shouldBe` True
                    coordinates <- runTool tool
                        (ToolCall
                            "coordinates"
                            "computer"
                            ( "{\"operation\":\"act\",\"target_id\":null,"
                                <> "\"actions\":["
                                <> "{\"type\":\"perform\",\"element_id\":\"e1\","
                                <> "\"action\":\"AXPress\",\"value\":null,"
                                <> "\"text\":null,\"x\":20,\"y\":30}],"
                                <> "\"include_screenshot\":false}"
                            )
                            ComputerFunctionCallKind
                            False)
                    coordinates.toolDispatchSucceeded `shouldBe` False
                    nullScreenshot <- runTool tool
                        (ToolCall
                            "null-screenshot"
                            "computer"
                            ( "{\"operation\":\"observe\",\"target_id\":null,"
                            <> "\"actions\":null,\"include_screenshot\":null}"
                            )
                            ComputerFunctionCallKind
                            False)
                    nullScreenshot.toolDispatchSucceeded `shouldBe` False
                    missingScreenshot <- runTool tool
                        (ToolCall
                            "missing-screenshot"
                            "computer"
                            ( "{\"operation\":\"observe\",\"target_id\":null,"
                            <> "\"actions\":null}"
                            )
                            ComputerFunctionCallKind
                            False)
                    missingScreenshot.toolDispatchSucceeded `shouldBe` False
                    stringScreenshot <- runTool tool
                        (ToolCall
                            "string-screenshot"
                            "computer"
                            ( "{\"operation\":\"observe\",\"target_id\":null,"
                            <> "\"actions\":null,\"include_screenshot\":\"false\"}"
                            )
                            ComputerFunctionCallKind
                            False)
                    stringScreenshot.toolDispatchSucceeded `shouldBe` False
                    listScreenshot <- runTool tool
                        (ToolCall
                            "list-screenshot"
                            "computer"
                            ( "{\"operation\":\"list_targets\",\"target_id\":null,"
                            <> "\"actions\":null,\"include_screenshot\":true}"
                            )
                            ComputerFunctionCallKind
                            False)
                    listScreenshot.toolDispatchSucceeded `shouldBe` False
                    close
            readIORef requests `shouldReturn`
                map semanticComputerRequestWireValue
                    [ ObserveComputerTarget False
                    , ActOnComputerTarget
                        ( PerformComputerAction "e1" "AXPress"
                            NonEmpty.:|
                                [ SetComputerValue
                                    "e2"
                                    (ComputerNumber 42)
                                , ReplaceComputerSelectedText "e3" "hello"
                                ]
                        )
                        False
                    ]

    it "returns an image only when explicitly requested" do
        requests <- newIORef []
        closes <- newIORef 0
        withHost (recordingCallback "image" requests closes) \host -> do
            Right session <- newComputerSession host
            withoutImage <- invokeComputerSessionRequest session
                (ObserveComputerTarget False)
            withImage <- invokeComputerSessionRequest session
                (ObserveComputerTarget True)
            fmap (.nativeComputerImage) withoutImage `shouldBe` Right Nothing
            case fmap (.nativeComputerImage) withImage of
                Right (Just image) ->
                    Text.isPrefixOf "data:image/png;base64,"
                        image.imageUrl `shouldBe` True
                value -> expectationFailure
                    ("unexpected screenshot result: " <> show value)
            closeComputerSession session

    it "rejects malformed screenshots and embedded image data" do
        withHost
            (fixedCallback
                "{\"ok\":true}"
                (Just pngSignature)
                False)
            \host -> do
                Right session <- newComputerSession host
                result <- invokeComputerSessionRequest session
                    (ObserveComputerTarget True)
                result `shouldBe`
                    Left "The native computer host returned malformed PNG data."
                closeComputerSession session
        withHost
            (fixedCallback
                "{\"ok\":true}"
                (Just corruptPngCrc)
                False)
            \host -> do
                Right session <- newComputerSession host
                result <- invokeComputerSessionRequest session
                    (ObserveComputerTarget True)
                result `shouldBe`
                    Left "The native computer host returned malformed PNG data."
                closeComputerSession session
        withHost
            (fixedCallback
                "{\"ok\":true}"
                (Just corruptPngImageData)
                False)
            \host -> do
                Right session <- newComputerSession host
                result <- invokeComputerSessionRequest session
                    (ObserveComputerTarget True)
                result `shouldBe`
                    Left "The native computer host returned malformed PNG data."
                closeComputerSession session
        withHost
            (fixedImageCallback
                "{\"ok\":true}"
                (Just (2, invalidJpeg))
                False)
            \host -> do
                Right session <- newComputerSession host
                result <- invokeComputerSessionRequest session
                    (ObserveComputerTarget True)
                result `shouldBe`
                    Left "The native computer host returned malformed JPEG data."
                closeComputerSession session
        withHost
            (fixedCallback
                "{\"nested\":{\"preview\":\"DATA:IMAGE/png;base64,secret\"}}"
                Nothing
                False)
            \host -> do
                Right session <- newComputerSession host
                result <- invokeComputerSessionRequest session
                    (ObserveComputerTarget False)
                result `shouldBe`
                    Left
                        "The native computer host embedded screenshot data in result JSON."
                closeComputerSession session

    it "rejects image lengths reported without an allocated image buffer" do
        withHost (fixedCallback "{\"ok\":true}" Nothing True) \host -> do
            Right session <- newComputerSession host
            result <- invokeComputerSessionRequest session
                (ObserveComputerTarget False)
            result `shouldBe`
                Left
                    "The native computer host reported image output beyond its buffer."
            closeComputerSession session

    it "keeps old generations alive while replacements serve new sessions" do
        oldRequests <- newIORef []
        newRequests <- newIORef []
        oldCloses <- newIORef 0
        newCloses <- newIORef 0
        withCallback (recordingCallback "old" oldRequests oldCloses) \old -> do
          withCallback (recordingCallback "new" newRequests newCloses) \new -> do
            host <- newComputerHost
            replaceComputerRegistration host
                (Just (ComputerRegistration old nullPtr))
            Right oldSession <- newComputerSession host
            replaceComputerRegistration host
                (Just (ComputerRegistration new nullPtr))
            Right newSession <- newComputerSession host
            oldResult <- invokeComputerSessionRequest oldSession
                ListComputerTargets
            newResult <- invokeComputerSessionRequest newSession
                ListComputerTargets
            resultHost oldResult `shouldBe` Just "old"
            resultHost newResult `shouldBe` Just "new"
            closeComputerSession oldSession
            closeComputerSession oldSession
            closeComputerSession newSession
            readIORef oldCloses `shouldReturn` 1
            readIORef newCloses `shouldReturn` 1

    it "propagates native OPEN failures instead of disabling the tool" do
        withHost openFailureCallback \host ->
            computerToolSessionWhenEnabled host >>= \case
                Left err -> err `shouldBe` "Accessibility permission is required."
                Right _ -> expectationFailure "OPEN failure was silently ignored"

    it "serializes calls per session but permits distinct sessions to overlap" do
        release <- newEmptyMVar
        active <- newIORef (0 :: Int)
        maximumActive <- newIORef (0 :: Int)
        let callback = blockingCallback release active maximumActive
        withHost callback \host -> do
            Right first <- newComputerSession host
            Right second <- newComputerSession host
            outcome <- concurrently
                (invokeComputerSessionRequest first
                    (ObserveComputerTarget False))
                (invokeComputerSessionRequest second
                    (ObserveComputerTarget False))
            fst outcome `shouldSatisfy` isRight
            snd outcome `shouldSatisfy` isRight
            readIORef maximumActive `shouldReturn` 2
            closeComputerSession first
            closeComputerSession second

    it "resets only accessibility delta history for a live session" do
        requests <- newIORef []
        closes <- newIORef 0
        withHost (recordingCallback "ax" requests closes) \host -> do
            Right session <- newComputerSession host
            first <- invokeComputerSessionRequest session observeRequest
            second <- invokeComputerSessionRequest session observeRequest
            resetComputerSessionAccessibility session
            third <- invokeComputerSessionRequest session observeRequest
            accessibilityKind first `shouldBe` Just "full"
            accessibilityKind second `shouldBe` Just "delta"
            accessibilityKind third `shouldBe` Just "full"
            closeComputerSession session

observeRequest :: SemanticComputerRequest
observeRequest = ObserveComputerTarget False

runTool :: AppTool -> ToolCall -> IO ToolDispatchOutcome
runTool tool =
    dispatchToolHandlerDetailed
        ToolDispatchConfig
            { toolDispatchUnknownTool = ("unknown tool: " <>)
            , toolDispatchFormatResult = either id id
            , toolDispatchFormatException = \_ exception ->
                Text.pack (show exception)
            , toolDispatchOnException = \_ _ -> pure ()
            , toolDispatchOnOutput = \_ _ -> pure ()
            , toolDispatchFinalizeOutput = \_ output -> pure output
            }
        (Just tool.appToolHandler)

accessibilityKind
    :: Either Text NativeComputerResult
    -> Maybe Text
accessibilityKind (Right result) =
    case result.nativeComputerAccessibility of
        Just AccessibilityFull{} -> Just "full"
        Just AccessibilityDelta{} -> Just "delta"
        Just AccessibilityUnavailable{} -> Just "unavailable"
        Nothing -> Nothing
accessibilityKind _ = Nothing

resultHost :: Either Text NativeComputerResult -> Maybe Text
resultHost (Right result) =
    case result.nativeComputerResultValue of
        Aeson.Object object ->
            case KeyMap.lookup "host" object of
                Just (Aeson.String value) -> Just value
                _ -> Nothing
        _ -> Nothing
resultHost _ = Nothing

recordingCallback
    :: Text
    -> IORef [Aeson.Value]
    -> IORef Int
    -> ComputerCallback
recordingCallback label requests closes _context abi operation _token
        request requestLength result resultCapacity resultLength
        accessibility accessibilityCapacity accessibilityLength
        image imageCapacity imageLength _err _errorCapacity errorLength
        outputToken outputImageFormat
    | abi /= 3 = pure 1
    | operation == 1 = do
        poke outputToken 73
        zeroLengths resultLength accessibilityLength imageLength errorLength
        poke outputImageFormat 0
        pure 0
    | operation == 5 = do
        modifyIORef' closes (+ 1)
        zeroLengths resultLength accessibilityLength imageLength errorLength
        poke outputImageFormat 0
        pure 0
    | otherwise = do
        requestBytes <- peekBytes request requestLength
        case Aeson.decodeStrict' requestBytes of
            Just value -> modifyIORef' requests (<> [value])
            Nothing -> pure ()
        resultStatus <- writeBytes
            (TextEncoding.encodeUtf8
                ("{\"host\":\"" <> label <> "\",\"ok\":true}"))
            result
            resultCapacity
            resultLength
        accessibilityStatus <-
            if operation == 2
                then poke accessibilityLength 0 >> pure 0
                else writeBytes snapshotBytes accessibility
                    accessibilityCapacity accessibilityLength
        let wantsImage =
                maybe False requestIncludesScreenshot
                    (Aeson.decodeStrict' requestBytes)
        imageStatus <-
            if wantsImage
                then do
                    poke outputImageFormat 1
                    writeBytes minimalPng image imageCapacity imageLength
                else if image /= nullPtr || imageCapacity /= 0
                    then pure 9
                else do
                    poke outputImageFormat 0
                    poke imageLength 0
                    pure 0
        poke errorLength 0
        pure (maximum [resultStatus, accessibilityStatus, imageStatus])

blockingCallback
    :: MVar ()
    -> IORef Int
    -> IORef Int
    -> ComputerCallback
blockingCallback release active maximumActive _context abi operation
        _token _request _requestLength result resultCapacity resultLength
        accessibility accessibilityCapacity accessibilityLength
        _image _imageCapacity imageLength _err _errorCapacity errorLength
        outputToken outputImageFormat
    | abi /= 3 = pure 1
    | operation == 1 = do
        poke outputToken 91
        zeroLengths resultLength accessibilityLength imageLength errorLength
        poke outputImageFormat 0
        pure 0
    | operation == 5 = do
        zeroLengths resultLength accessibilityLength imageLength errorLength
        poke outputImageFormat 0
        pure 0
    | otherwise = do
        now <- atomicModifyIORef' active \value ->
            let next = value + 1 in (next, next)
        atomicModifyIORef' maximumActive \value -> (max value now, ())
        if now == 1
            then takeMVar release
            else putMVar release ()
        atomicModifyIORef' active \value -> (value - 1, ())
        resultStatus <- writeBytes "{\"ok\":true}"
            result resultCapacity resultLength
        accessibilityStatus <- writeBytes snapshotBytes accessibility
            accessibilityCapacity accessibilityLength
        poke imageLength 0
        poke errorLength 0
        poke outputImageFormat 0
        pure (max resultStatus accessibilityStatus)

requestIncludesScreenshot :: Aeson.Value -> Bool
requestIncludesScreenshot (Aeson.Object object) =
    KeyMap.lookup "include_screenshot" object == Just (Aeson.Bool True)
requestIncludesScreenshot _ = False

fixedCallback
    :: BS.ByteString
    -> Maybe BS.ByteString
    -> Bool
    -> ComputerCallback
fixedCallback resultBytes imageBytes =
    fixedImageCallback
        resultBytes
        (fmap (\bytes -> (1, bytes)) imageBytes)

fixedImageCallback
    :: BS.ByteString
    -> Maybe (CInt, BS.ByteString)
    -> Bool
    -> ComputerCallback
fixedImageCallback resultBytes imageBytes reportImageWithoutBuffer
        _context abi operation _token _request _requestLength
        result resultCapacity resultLength
        accessibility accessibilityCapacity accessibilityLength
        image imageCapacity imageLength
        _err _errorCapacity errorLength outputToken outputImageFormat
    | abi /= 3 = pure 1
    | operation == 1 = do
        poke outputToken 101
        zeroLengths resultLength accessibilityLength imageLength errorLength
        poke outputImageFormat 0
        pure 0
    | operation == 5 = do
        zeroLengths resultLength accessibilityLength imageLength errorLength
        poke outputImageFormat 0
        pure 0
    | otherwise = do
        resultStatus <-
            writeBytes resultBytes result resultCapacity resultLength
        accessibilityStatus <-
            if operation == 2
                then poke accessibilityLength 0 >> pure 0
                else writeBytes snapshotBytes accessibility
                    accessibilityCapacity accessibilityLength
        imageStatus <- case imageBytes of
            Just (imageFormat, bytes) -> do
                poke outputImageFormat imageFormat
                writeBytes bytes image imageCapacity imageLength
            Nothing
                | reportImageWithoutBuffer -> do
                    poke outputImageFormat 1
                    poke imageLength 1
                    pure 0
                | otherwise -> do
                    poke outputImageFormat 0
                    poke imageLength 0
                    pure 0
        poke errorLength 0
        pure (maximum [resultStatus, accessibilityStatus, imageStatus])

openFailureCallback :: ComputerCallback
openFailureCallback _context _abi operation _token _request _requestLength
        _result _resultCapacity resultLength
        _accessibility _accessibilityCapacity accessibilityLength
        image imageCapacity imageLength
        _err _errorCapacity errorLength outputToken outputImageFormat = do
    zeroLengths resultLength accessibilityLength imageLength errorLength
    poke outputToken 0
    poke outputImageFormat 0
    if operation == 1 && image == nullPtr && imageCapacity == 0
        then pure 4
        else pure 1

withHost :: ComputerCallback -> (ComputerHost -> IO value) -> IO value
withHost callback action =
    withCallback callback \pointer -> do
        host <- newComputerHost
        replaceComputerRegistration host
            (Just (ComputerRegistration pointer nullPtr))
        action host

withCallback
    :: ComputerCallback
    -> (FunPtr ComputerCallback -> IO value)
    -> IO value
withCallback callback =
    bracket (wrapComputerCallback callback) freeHaskellFunPtr

peekBytes :: Ptr value -> CSize -> IO BS.ByteString
peekBytes _ (CSize 0) = pure BS.empty
peekBytes pointer (CSize lengthValue) =
    BS.packCStringLen (castPtr pointer, fromIntegral lengthValue)

writeBytes :: BS.ByteString -> Ptr Word8 -> CSize -> Ptr CSize -> IO CInt
writeBytes bytes output (CSize capacity) outputLength
    | BS.length bytes > fromIntegral capacity = pure 7
    | otherwise = do
        BS.useAsCStringLen bytes \(source, lengthValue) ->
            copyBytes output (castPtr source) lengthValue
        poke outputLength (fromIntegral (BS.length bytes))
        pure 0

zeroLengths :: Ptr CSize -> Ptr CSize -> Ptr CSize -> Ptr CSize -> IO ()
zeroLengths a b c d = do
    poke a 0
    poke b 0
    poke c 0
    poke d 0

pngSignature :: BS.ByteString
pngSignature = BS.pack [137, 80, 78, 71, 13, 10, 26, 10]

minimalPng :: BS.ByteString
minimalPng = BS.pack
    [ 137, 80, 78, 71, 13, 10, 26, 10
    , 0, 0, 0, 13, 73, 72, 68, 82
    , 0, 0, 0, 1, 0, 0, 0, 1
    , 8, 4, 0, 0, 0, 181, 28, 12, 2
    , 0, 0, 0, 11, 73, 68, 65, 84
    , 120, 218, 99, 100, 248, 15, 0, 1
    , 5, 1, 1, 39, 24, 227, 102
    , 0, 0, 0, 0, 73, 69, 78, 68
    , 174, 66, 96, 130
    ]

corruptPngCrc :: BS.ByteString
corruptPngCrc =
    BS.take 32 minimalPng <> BS.singleton 3 <> BS.drop 33 minimalPng

-- The IDAT checksum is valid, but the zlib header is not. Envelope-only PNG
-- validation used to accept this payload.
corruptPngImageData :: BS.ByteString
corruptPngImageData = BS.pack
    [ 137, 80, 78, 71, 13, 10, 26, 10
    , 0, 0, 0, 13, 73, 72, 68, 82
    , 0, 0, 0, 1, 0, 0, 0, 1
    , 8, 4, 0, 0, 0, 181, 28, 12, 2
    , 0, 0, 0, 11, 73, 68, 65, 84
    , 121, 218, 99, 100, 248, 15, 0, 1
    , 5, 1, 1, 230, 150, 60, 166
    , 0, 0, 0, 0, 73, 69, 78, 68
    , 174, 66, 96, 130
    ]

-- The marker envelope and one-pixel SOF are present, but there is no valid
-- component table or compressed scan.
invalidJpeg :: BS.ByteString
invalidJpeg = BS.pack
    [ 255, 216
    , 255, 192, 0, 8, 8, 0, 1, 0, 1, 1
    , 255, 218, 0, 2
    , 255, 217
    ]

snapshotBytes :: BS.ByteString
snapshotBytes =
    "{\"schema_version\":1,\"scope\":{\"bundle_id\":\"test\"},\"contents\":{\"role\":\"AXWindow\"}}"

expectedComputerParameters :: Aeson.Value
expectedComputerParameters = strictObjectSchema
    [ ("operation", Aeson.object
        [ "type" Aeson..= ("string" :: Text)
        , "enum" Aeson..=
            (["list_targets", "bind", "observe", "act"] :: [Text])
        ])
    , ("target_id", nullableStringSchema 1024)
    , ("actions", Aeson.object
        [ "type" Aeson..= (["array", "null"] :: [Text])
        , "minItems" Aeson..= (1 :: Int)
        , "maxItems" Aeson..= (64 :: Int)
        , "items" Aeson..= strictObjectSchema
            [ ("type", Aeson.object
                [ "type" Aeson..= ("string" :: Text)
                , "enum" Aeson..=
                    ( [ "perform"
                      , "set_value"
                      , "replace_selected_text"
                      ] :: [Text]
                    )
                ])
            , ("element_id", boundedStringSchema False 1024)
            , ("action", nullableStringSchema 1024)
            , ("value", Aeson.object
                [ "type" Aeson..=
                    (["string", "number", "boolean", "null"] :: [Text])
                , "maxLength" Aeson..= (65536 :: Int)
                ])
            , ("text", nullableStringSchema 65536)
            ]
            ["type", "element_id", "action", "value", "text"]
        ])
    , ("include_screenshot", Aeson.object
        [ "type" Aeson..= ("boolean" :: Text)
        , "description" Aeson..=
            ( "Set false unless visual evidence is necessary; screenshots are "
            <> "never returned implicitly."
            :: Text
            )
        ])
    ]
    ["operation", "target_id", "actions", "include_screenshot"]

strictObjectSchema :: [(Text, Aeson.Value)] -> [Text] -> Aeson.Value
strictObjectSchema properties requiredFields = Aeson.object
    [ "type" Aeson..= ("object" :: Text)
    , "additionalProperties" Aeson..= False
    , "properties" Aeson..= Aeson.object
        [ Key.fromText name Aeson..= schema | (name, schema) <- properties ]
    , "required" Aeson..= requiredFields
    ]

nullableStringSchema :: Int -> Aeson.Value
nullableStringSchema maximumLength = Aeson.object
    [ "type" Aeson..= (["string", "null"] :: [Text])
    , "maxLength" Aeson..= maximumLength
    ]

boundedStringSchema :: Bool -> Int -> Aeson.Value
boundedStringSchema allowEmpty maximumLength = Aeson.object $
    [ "type" Aeson..= ("string" :: Text)
    , "maxLength" Aeson..= maximumLength
    ]
    <> [ "minLength" Aeson..= (1 :: Int) | not allowEmpty ]
