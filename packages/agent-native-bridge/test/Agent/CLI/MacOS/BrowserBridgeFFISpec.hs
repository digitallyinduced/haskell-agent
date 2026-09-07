{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CPP #-}

module Agent.CLI.MacOS.BrowserBridgeFFISpec (spec) where

#ifdef darwin_HOST_OS
import Agent.CLI.BrowserTools
    ( BrowserCommand(..)
    , BrowserInvocation(..)
    )
import qualified Data.Map.Strict as Map
import Agent.CLI.MacOS.Bridge
    ( BrowserCallback
    , BrowserCancelCallback
    , BrowserCompletion
    , BrowserHost(..)
    , BrowserRegistration(..)
    , browserCommandABI
    , browserOutputCapacity
    , browserStatusMessage
    , browserToolsWhenEnabled
    , invokeBrowserCommand
    , nextAvailableBrowserPendingId
    )
import Agent.Loop (defaultLoopDispatch)
import Agent.ToolDispatch
    ( ToolCallResult(..)
    , ToolHandlerResult(..)
    , ToolResultImage(..)
    , dispatchToolCall
    , functionToolCall
    , toolCallResultImages
    )
import Agent.Tools.Types
    ( AppTool(..)
    , appToolHandlers
    )
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.Async
    ( cancel
    , poll
    , wait
    , waitCatch
    , withAsync
    )
import Control.Concurrent.MVar
    ( modifyMVar_
    , newEmptyMVar
    , newMVar
    , putMVar
    , readMVar
    , takeMVar
    )
import Control.Exception.Safe (bracket, uninterruptibleMask_)
import Control.Monad (void)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as Base64
import Data.IORef
    ( IORef
    , newIORef
    , readIORef
    , writeIORef
    )
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import Data.Word (Word8, Word32, Word64)
import Foreign
    ( FunPtr
    , Ptr
    , allocaBytes
    , castPtr
    , freeHaskellFunPtr
    , nullPtr
    , peekByteOff
    , pokeByteOff
    )
import Foreign.C.Types
    ( CDouble(..)
    , CInt(..)
    , CSize(..)
    )
import Foreign.Marshal.Utils (fillBytes)
import Test.Hspec
    ( Spec
    , describe
    , expectationFailure
    , it
    , shouldBe
    , shouldReturn
    , shouldSatisfy
    )

foreign import ccall "ha_browser_callback_abi_smoke"
    browserCallbackAbiSmoke :: IO CInt

foreign import ccall "wrapper"
    wrapBrowserCallback :: BrowserCallback -> IO (FunPtr BrowserCallback)

foreign import ccall "wrapper"
    wrapBrowserCancelCallback
        :: BrowserCancelCallback
        -> IO (FunPtr BrowserCancelCallback)

foreign import ccall "dynamic"
    invokeBrowserCompletion
        :: FunPtr BrowserCompletion
        -> BrowserCompletion

spec :: Spec
spec = describe "native browser callback ABI" do
    it "matches the C ABI and installs/disables the current callback" do
        browserCallbackAbiSmoke `shouldReturn` 0

    it "maps every typed command to current ABI fields" do
        browserCommandABI (BrowserNavigate "https://example.com")
            `shouldBe` (1, "https://example.com", "", 0, 0, 0)
        browserCommandABI BrowserSnapshot
            `shouldBe` (2, "", "", 0, 0, 0)
        browserCommandABI (BrowserClick "ref-continue")
            `shouldBe` (3, "ref-continue", "", 0, 0, 0)
        browserCommandABI (BrowserType "ref-query" "λ" True)
            `shouldBe` (4, "ref-query", "λ", 0, 0, 1)
        browserCommandABI BrowserBack `shouldBe` (5, "", "", 0, 0, 0)
        browserCommandABI BrowserForward `shouldBe` (6, "", "", 0, 0, 0)
        browserCommandABI BrowserReload `shouldBe` (7, "", "", 0, 0, 0)
        browserCommandABI (BrowserKey "Enter")
            `shouldBe` (8, "Enter", "", 0, 0, 0)
        browserCommandABI (BrowserScroll 12.5 (-800))
            `shouldBe` (9, "", "", 12.5, -800, 0)
        browserCommandABI BrowserScreenshot
            `shouldBe` (10, "", "", 0, 0, 0)
        browserCommandABI BrowserListTabs
            `shouldBe` (11, "", "", 0, 0, 0)
        browserCommandABI (BrowserSwitchTab "tab-2")
            `shouldBe` (12, "tab-2", "", 0, 0, 0)
        browserCommandABI BrowserListDownloads
            `shouldBe` (13, "", "", 0, 0, 0)
        browserOutputCapacity `shouldBe` 262144

    it "passes copied UTF-8 IDs, refs, text, flags, and scroll fields" do
        observed <- newIORef Nothing
        withBrowserHost (recordingCallback observed)
                noOpCancelCallback \host -> do
            invokeBrowserCommand host
                (invocation "scope-λ" "call-type"
                    (BrowserType "ref-query" "Haskell λ" True))
                `shouldReturn` successResult "native ✓"
            readIORef observed `shouldReturn`
                Just (4, "scope-λ", "call-type", "ref-query",
                    "Haskell λ", 0, 0, 1)
            invokeBrowserCommand host
                (invocation "scope-λ" "call-scroll"
                    (BrowserScroll 12.5 (-800)))
                `shouldReturn` successResult "native ✓"
            readIORef observed `shouldReturn`
                Just (9, "scope-λ", "call-scroll", "", "",
                    12.5, -800, 0)

    it "uses NULL/zero for unused request argument slices" do
        observed <- newIORef False
        let callback _ request completion completionContext = do
                firstPtr <- peekByteOff request 64 :: IO (Ptr Word8)
                firstLength <- peekByteOff request 72 :: IO CSize
                secondPtr <- peekByteOff request 80 :: IO (Ptr Word8)
                secondLength <- peekByteOff request 88 :: IO CSize
                writeIORef observed
                    (firstPtr == nullPtr && firstLength == 0
                        && secondPtr == nullPtr && secondLength == 0)
                completeResult completion completionContext
                    0 "ok" 0 0 0 BS.empty
                pure 0
        withBrowserHost callback noOpCancelCallback \host ->
            invokeBrowserCommand host
                (invocation "scope" "snapshot" BrowserSnapshot)
                `shouldReturn` successResult "ok"
        readIORef observed `shouldReturn` True

    it "returns screenshots through rich images" do
        let png = tinyPng
            callback _ _ completion completionContext = do
                completeResult completion completionContext
                    0 "viewport" 1 1 1 png
                pure 0
        withBrowserHost callback noOpCancelCallback \host -> do
            result <- invokeBrowserCommand host
                (invocation "scope" "shot" BrowserScreenshot)
            case result of
                Left err -> expectationFailure (Text.unpack err)
                Right rich -> do
                    rich.resultText `shouldBe` "viewport"
                    map (.imageUrl) rich.resultImages `shouldBe`
                        ["data:image/png;base64,"
                            <> TextEncoding.decodeUtf8 (Base64.encode tinyPng)]

    it "exposes tools only while callbacks are registered" do
        registration <- newMVar Nothing
        let host = BrowserHost registration
        disabledTools <- browserToolsWhenEnabled host "turn"
        map (.appToolName) disabledTools `shouldBe` []
        withCallbacks (successCallback "ok") noOpCancelCallback
            \callback cancelCallback -> do
                modifyMVar_ registration \_ ->
                    pure (Just BrowserRegistration
                        { browserCallback = callback
                        , browserCancelCallback = cancelCallback
                        , browserContext = nullPtr
                        })
                tools <- browserToolsWhenEnabled host "turn"
                map (.appToolName) tools `shouldBe`
                    [ "browser_navigate"
                    , "browser_snapshot"
                    , "browser_click"
                    , "browser_type"
                    , "browser_key"
                    , "browser_scroll"
                    , "browser_back"
                    , "browser_forward"
                    , "browser_reload"
                    , "browser_screenshot"
                    , "browser_list_tabs"
                    , "browser_switch_tab"
                    , "browser_list_downloads"
                    ]
                modifyMVar_ registration (const (pure Nothing))
                retained <- dispatchToolCall defaultLoopDispatch
                    (appToolHandlers tools)
                    (functionToolCall "call" "browser_snapshot" "{}")
                retained.output `shouldBe`
                    "Error: The browser view is not active."
                toolCallResultImages retained `shouldBe` []

    it "rejects oversized text, invalid UTF-8, and malformed images" do
        withBrowserHost oversizedTextCallback noOpCancelCallback \host ->
            invokeBrowserCommand host
                (invocation "scope" "call" BrowserSnapshot)
                `shouldReturn`
                    Left "The browser returned text data above its size limit."
        withBrowserHost invalidUtf8Callback noOpCancelCallback \host ->
            invokeBrowserCommand host
                (invocation "scope" "call" BrowserSnapshot)
                `shouldReturn`
                    Left "The browser returned invalid UTF-8."
        withBrowserHost (screenshotCallback "not png")
                noOpCancelCallback \host ->
            invokeBrowserCommand host
                (invocation "scope" "call" BrowserScreenshot)
                `shouldReturn`
                    Left "The browser screenshot was not a valid PNG byte sequence."
        withBrowserHost (screenshotWithDimensionsCallback 2 1 tinyPng)
                noOpCancelCallback \host -> do
            result <- invokeBrowserCommand host
                (invocation "scope" "call" BrowserScreenshot)
            case result of
                Left err -> expectationFailure (Text.unpack err)
                Right rich ->
                    rich.resultText `shouldBe`
                        "Captured browser viewport (2×1)."
        withBrowserHost (screenshotCallback oversizedPngHeader)
                noOpCancelCallback \host ->
            invokeBrowserCommand host
                (invocation "scope" "call" BrowserScreenshot)
                `shouldReturn`
                    Left
                        "The browser screenshot PNG pixel dimensions were invalid."
        withBrowserHost nonNullEmptyTextCallback noOpCancelCallback \host ->
            invokeBrowserCommand host
                (invocation "scope" "call" BrowserSnapshot)
                `shouldReturn`
                    Left
                        "The browser returned a non-null text pointer for an empty slice."
        withBrowserHost nonNullEmptyImageCallback noOpCancelCallback \host ->
            invokeBrowserCommand host
                (invocation "scope" "call" BrowserSnapshot)
                `shouldReturn`
                    Left
                        "The browser returned a non-null image pointer for an empty slice."
        withBrowserHost zeroWidthScreenshotCallback noOpCancelCallback \host ->
            invokeBrowserCommand host
                (invocation "scope" "call" BrowserScreenshot)
                `shouldReturn`
                    Left "The browser screenshot dimensions were invalid."

    it "skips occupied pending IDs across wraparound" do
        let occupied :: Map.Map Word64 ()
            occupied = Map.fromList [(maxBound, ()), (1, ()), (2, ())]
        nextAvailableBrowserPendingId maxBound occupied `shouldBe` 3

    it "accepts only the first terminal completion" do
        let callback _ _ completion completionContext = do
                completeResult completion completionContext
                    0 "first" 0 0 0 BS.empty
                completeResult completion completionContext
                    0 "second" 0 0 0 BS.empty
                pure 0
        withBrowserHost callback noOpCancelCallback \host ->
            invokeBrowserCommand host
                (invocation "scope" "call" BrowserSnapshot)
                `shouldReturn` successResult "first"

    it "renders stable fallback errors for all host statuses" do
        browserStatusMessage 1 `shouldBe`
            "The browser host rejected an invalid argument."
        browserStatusMessage 2 `shouldBe`
            "The native browser bridge is unavailable."
        browserStatusMessage 3 `shouldBe`
            "The browser command timed out."
        browserStatusMessage 4 `shouldBe`
            "The browser host denied website or extension access."
        browserStatusMessage 5 `shouldBe`
            "The browser host does not support this operation."
        browserStatusMessage 6 `shouldBe`
            "The native browser bridge failed internally."
        browserStatusMessage 7 `shouldBe`
            "The browser host returned a result that was too large."
        browserStatusMessage 8 `shouldBe`
            "The browser command was cancelled."
        browserStatusMessage 99 `shouldSatisfy`
            (== "The browser command failed (status 99).")

    it "waits for an async completion before disabling its context" do
        entered <- newEmptyMVar
        release <- newEmptyMVar
        let callback _ _ completion completionContext = do
                putMVar entered ()
                void $ forkIO do
                    takeMVar release
                    completeResult completion completionContext
                        0 "done" 0 0 0 BS.empty
                pure 0
        withCallbacks callback noOpCancelCallback
            \callbackPtr cancelPtr -> do
                registration <- newMVar
                    (Just BrowserRegistration
                        { browserCallback = callbackPtr
                        , browserCancelCallback = cancelPtr
                        , browserContext = nullPtr
                        })
                let host = BrowserHost registration
                withAsync
                    (invokeBrowserCommand host
                        (invocation "scope" "call" BrowserSnapshot))
                    \running -> do
                        takeMVar entered
                        withAsync
                            (modifyMVar_ registration
                                (const (pure Nothing)))
                            \disabling -> do
                                threadDelay 20000
                                poll disabling >>= \case
                                    Nothing -> pure ()
                                    Just _ -> expectationFailure
                                        "disabling returned before completion"
                                putMVar release ()
                                wait running `shouldReturn`
                                    successResult "done"
                                wait disabling

    it "copies cancellation IDs and drains the terminal completion" do
        accepted <- newEmptyMVar
        stored <- newEmptyMVar
        cancelled <- newEmptyMVar
        let callback _ request completion completionContext = do
                scope <- requestText request 32 40
                callId <- requestText request 48 56
                putMVar stored
                    (scope, callId, completion, completionContext)
                putMVar accepted ()
                pure 0
            cancelCallback _ scopePtr scopeLength callPtr callLength = do
                scope <- decodeBytes scopePtr scopeLength
                callId <- decodeBytes callPtr callLength
                putMVar cancelled (scope, callId)
                (_, _, completion, completionContext) <- readMVar stored
                completeResult completion completionContext
                    8 "" 0 0 0 BS.empty
        withBrowserHost callback cancelCallback \host ->
            withAsync
                (invokeBrowserCommand host
                    (invocation "turn-cancel" "call-cancel"
                        BrowserSnapshot))
                \running -> do
                    takeMVar accepted
                    cancel running
                    _ <- waitCatch running
                    takeMVar cancelled `shouldReturn`
                        ("turn-cancel", "call-cancel")

    it "covers cancellation after host acceptance but before callback return" do
        accepted <- newEmptyMVar
        releaseCallback <- newEmptyMVar
        stored <- newEmptyMVar
        cancelled <- newEmptyMVar
        let callback _ request completion completionContext = do
                scope <- requestText request 32 40
                callId <- requestText request 48 56
                putMVar stored
                    (scope, callId, completion, completionContext)
                putMVar accepted ()
                uninterruptibleMask_ (takeMVar releaseCallback)
                pure 0
            cancelCallback _ scopePtr scopeLength callPtr callLength = do
                scope <- decodeBytes scopePtr scopeLength
                callId <- decodeBytes callPtr callLength
                putMVar cancelled (scope, callId)
                (_, _, completion, completionContext) <- readMVar stored
                completeResult completion completionContext
                    8 "" 0 0 0 BS.empty
        withBrowserHost callback cancelCallback \host ->
            withAsync
                (invokeBrowserCommand host
                    (invocation "turn-window" "call-window"
                        BrowserSnapshot))
                \running -> do
                    takeMVar accepted
                    withAsync (cancel running) \cancelling -> do
                        threadDelay 20000
                        poll cancelling >>= \case
                            Nothing -> pure ()
                            Just _ -> expectationFailure
                                "cancellation completed before callback return"
                        poll running >>= \case
                            Nothing -> pure ()
                            Just _ -> expectationFailure
                                "request completed before callback return"
                        putMVar releaseCallback ()
                        _ <- waitCatch running
                        takeMVar cancelled `shouldReturn`
                            ("turn-window", "call-window")
                        wait cancelling

type ObservedCall =
    (CInt, Text, Text, Text, Text, CDouble, CDouble, CInt)

recordingCallback
    :: IORef (Maybe ObservedCall)
    -> BrowserCallback
recordingCallback observed _ request completion completionContext = do
    command <- peekByteOff request 4
    flags <- peekByteOff request 8
    deltaX <- peekByteOff request 16
    deltaY <- peekByteOff request 24
    scope <- requestText request 32 40
    callId <- requestText request 48 56
    first <- requestText request 64 72
    second <- requestText request 80 88
    writeIORef observed
        (Just (command, scope, callId, first, second, deltaX, deltaY, flags))
    completeResult completion completionContext
        0 (TextEncoding.encodeUtf8 "native ✓") 0 0 0 BS.empty
    pure 0

nonNullEmptyTextCallback :: BrowserCallback
nonNullEmptyTextCallback _ _ completion completionContext =
    allocaBytes 1 \byte -> do
        withResult 0 0 0 0 byte 0 nullPtr 0 \result ->
            invokeBrowserCompletion completion completionContext result
        pure 0

nonNullEmptyImageCallback :: BrowserCallback
nonNullEmptyImageCallback _ _ completion completionContext =
    allocaBytes 1 \byte -> do
        withResult 0 0 0 0 nullPtr 0 byte 0 \result ->
            invokeBrowserCompletion completion completionContext result
        pure 0

zeroWidthScreenshotCallback :: BrowserCallback
zeroWidthScreenshotCallback _ _ completion completionContext = do
    completeResult completion completionContext
        0 "" 1 0 1 (BS.pack [137, 80, 78, 71, 13, 10, 26, 10])
    pure 0

successCallback :: ByteString -> BrowserCallback
successCallback text _ _ completion completionContext = do
    completeResult completion completionContext 0 text 0 0 0 BS.empty
    pure 0

screenshotCallback :: ByteString -> BrowserCallback
screenshotCallback bytes _ _ completion completionContext = do
    completeResult completion completionContext 0 "" 1 1 1 bytes
    pure 0

screenshotWithDimensionsCallback :: CInt -> CInt -> ByteString -> BrowserCallback
screenshotWithDimensionsCallback width height bytes
        _ _ completion completionContext = do
    completeResult completion completionContext
        0 "" 1 width height bytes
    pure 0

tinyPng :: ByteString
tinyPng = Base64.decodeLenient
    "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAQAAAC1HAwCAAAAC0lEQVR42mNk+A8AAQUBAScY42YAAAAASUVORK5CYII="

oversizedPngHeader :: ByteString
oversizedPngHeader =
    BS.take 16 tinyPng
        <> BS.pack [0, 0, 64, 1]
        <> BS.drop 20 tinyPng

oversizedTextCallback :: BrowserCallback
oversizedTextCallback _ _ completion completionContext = do
    withResult 0 0 0 0 nullPtr
            (fromIntegral browserOutputCapacity + 1)
            nullPtr 0 \result ->
        invokeBrowserCompletion completion completionContext result
    pure 0

invalidUtf8Callback :: BrowserCallback
invalidUtf8Callback _ _ completion completionContext = do
    completeResult completion completionContext
        0 (BS.pack [0xff]) 0 0 0 BS.empty
    pure 0

noOpCancelCallback :: BrowserCancelCallback
noOpCancelCallback _ _ _ _ _ = pure ()

completeResult
    :: FunPtr BrowserCompletion
    -> Ptr ()
    -> CInt
    -> ByteString
    -> CInt
    -> CInt
    -> CInt
    -> ByteString
    -> IO ()
completeResult completion completionContext status text imageFormat
        width height image =
    withNullableBytes text \textPtr textLength ->
        withNullableBytes image \imagePtr imageLength ->
            withResult status imageFormat width height
                textPtr textLength imagePtr imageLength \result ->
                    invokeBrowserCompletion
                        completion completionContext result

withNullableBytes
    :: ByteString
    -> (Ptr Word8 -> CSize -> IO value)
    -> IO value
withNullableBytes bytes action
    | BS.null bytes = action nullPtr 0
    | otherwise =
        BS.useAsCStringLen bytes \(pointer, length) ->
            action (castPtr pointer) (fromIntegral length)

withResult
    :: CInt
    -> CInt
    -> CInt
    -> CInt
    -> Ptr a
    -> CSize
    -> Ptr b
    -> CSize
    -> (Ptr () -> IO value)
    -> IO value
withResult status imageFormat width height textPtr textLength
        imagePtr imageLength action =
    allocaBytes 56 \result -> do
        fillBytes result 0 56
        pokeByteOff result 0 (56 :: Word32)
        pokeByteOff result 4 status
        pokeByteOff result 8 imageFormat
        pokeByteOff result 12 width
        pokeByteOff result 16 height
        pokeByteOff result 24 textPtr
        pokeByteOff result 32 textLength
        pokeByteOff result 40 imagePtr
        pokeByteOff result 48 imageLength
        action (castPtr result)

requestText :: Ptr () -> Int -> Int -> IO Text
requestText request pointerOffset lengthOffset = do
    pointer <- peekByteOff request pointerOffset
    length <- peekByteOff request lengthOffset
    decodeBytes pointer length

decodeBytes :: Ptr a -> CSize -> IO Text
decodeBytes _ 0 = pure ""
decodeBytes pointer (CSize length) = do
    bytes <- BS.packCStringLen
        (castPtr pointer, fromIntegral length)
    case TextEncoding.decodeUtf8' bytes of
        Left _ -> expectationFailure "callback input was not UTF-8" >> pure ""
        Right value -> pure value

invocation :: Text -> Text -> BrowserCommand -> BrowserInvocation
invocation browserScopeId browserCallId browserCommand =
    BrowserInvocation{browserScopeId, browserCallId, browserCommand}

successResult :: Text -> Either Text ToolHandlerResult
successResult text = Right ToolHandlerResult
    { resultText = text
    , resultImages = []
    }

withBrowserHost
    :: BrowserCallback
    -> BrowserCancelCallback
    -> (BrowserHost -> IO a)
    -> IO a
withBrowserHost callback cancelCallback action =
    withCallbacks callback cancelCallback \callbackPtr cancelPtr -> do
        registration <- newMVar
            (Just BrowserRegistration
                { browserCallback = callbackPtr
                , browserCancelCallback = cancelPtr
                , browserContext = nullPtr
                })
        action (BrowserHost registration)

withCallbacks
    :: BrowserCallback
    -> BrowserCancelCallback
    -> (FunPtr BrowserCallback -> FunPtr BrowserCancelCallback -> IO a)
    -> IO a
withCallbacks callback cancelCallback action =
    bracket (wrapBrowserCallback callback) freeHaskellFunPtr \callbackPtr ->
        bracket
            (wrapBrowserCancelCallback cancelCallback)
            freeHaskellFunPtr
            (action callbackPtr)
#else
import Test.Hspec (Spec, describe, it, pendingWith)

spec :: Spec
spec = describe "native browser callback ABI" do
    it "links only on macOS" do
        pendingWith "the native browser bridge smoke test only links on macOS"
#endif
