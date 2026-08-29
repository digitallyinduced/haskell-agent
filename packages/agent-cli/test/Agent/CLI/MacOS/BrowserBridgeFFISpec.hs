{-# LANGUAGE ForeignFunctionInterface #-}

module Agent.CLI.MacOS.BrowserBridgeFFISpec (spec) where

import Agent.CLI.BrowserTools (BrowserCommand(..))
import Agent.CLI.MacOS.Bridge
    ( BrowserCallback
    , BrowserHost(..)
    , BrowserRegistration(..)
    , browserCommandABI
    , browserOutputCapacity
    , browserStatusMessage
    , browserToolsWhenEnabled
    , invokeBrowserCommand
    )
import Agent.Loop (defaultLoopDispatch)
import Agent.ToolDispatch
    ( ToolCallResult(..)
    , dispatchToolCall
    , functionToolCall
    )
import Agent.Tools.Types
    ( AppTool(..)
    , appToolHandlers
    )
import Control.Concurrent.MVar
    ( modifyMVar_
    , newEmptyMVar
    , newMVar
    , putMVar
    , takeMVar
    )
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
    ( poll
    , wait
    , withAsync
    )
import Control.Exception.Safe (bracket)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.IORef
    ( IORef
    , newIORef
    , readIORef
    , writeIORef
    )
import Data.Text (Text)
import qualified Data.Text.Encoding as TextEncoding
import Foreign
    ( FunPtr
    , Ptr
    , castPtr
    , copyBytes
    , freeHaskellFunPtr
    , nullPtr
    , poke
    )
import Foreign.C.Types
    ( CDouble(..)
    , CInt(..)
    , CSize(..)
    )
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

spec :: Spec
spec = describe "native browser callback ABI" do
    it "matches the C ABI and installs/disables a host callback" do
        browserCallbackAbiSmoke `shouldReturn` 0

    it "maps every typed command to stable ABI fields" do
        browserCommandABI (BrowserNavigate "https://example.com")
            `shouldBe` (1, "https://example.com", "", 0, 0, 0)
        browserCommandABI BrowserSnapshot
            `shouldBe` (2, "", "", 0, 0, 0)
        browserCommandABI (BrowserClick "#continue")
            `shouldBe` (3, "#continue", "", 0, 0, 0)
        browserCommandABI (BrowserType "#query" "λ" True)
            `shouldBe` (4, "#query", "λ", 0, 0, 1)
        browserCommandABI BrowserBack
            `shouldBe` (5, "", "", 0, 0, 0)
        browserCommandABI BrowserForward
            `shouldBe` (6, "", "", 0, 0, 0)
        browserCommandABI BrowserReload
            `shouldBe` (7, "", "", 0, 0, 0)
        browserCommandABI (BrowserKey "Enter")
            `shouldBe` (8, "Enter", "", 0, 0, 0)
        browserCommandABI (BrowserScroll 12.5 (-800))
            `shouldBe` (9, "", "", 12.5, -800, 0)
        browserOutputCapacity `shouldBe` 262144

    it "passes UTF-8 text and scroll fields to the registered callback" do
        observed <- newIORef Nothing
        withBrowserHost (recordingCallback observed) \host -> do
            invokeBrowserCommand host
                (BrowserType "#query" "Haskell λ" True)
                `shouldReturn` Right "native ✓"
            readIORef observed `shouldReturn`
                Just (4, "#query", "Haskell λ", 0, 0, 1)
            invokeBrowserCommand host (BrowserScroll 12.5 (-800))
                `shouldReturn` Right "native ✓"
            readIORef observed `shouldReturn`
                Just (9, "", "", 12.5, -800, 0)

    it "exposes tools only while a callback is registered" do
        registration <- newMVar Nothing
        let host = BrowserHost registration
        disabledTools <- browserToolsWhenEnabled host
        map (.appToolName) disabledTools `shouldBe` []
        withCallback (successCallback "ok") \callback -> do
            modifyMVar_ registration \_ ->
                pure (Just (BrowserRegistration callback nullContext))
            tools <- browserToolsWhenEnabled host
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
                ]
            modifyMVar_ registration (const (pure Nothing))
            retained <- dispatchToolCall defaultLoopDispatch
                (appToolHandlers tools)
                (functionToolCall
                    "browser-call"
                    "browser_snapshot"
                    "{}")
            retained.output `shouldBe`
                "Error: The browser view is not active."

    it "rejects oversized and invalid UTF-8 callback output" do
        withBrowserHost oversizedCallback \host ->
            invokeBrowserCommand host BrowserSnapshot `shouldReturn`
                Left "The browser returned more than the 256 KiB output limit."
        withBrowserHost invalidUtf8Callback \host ->
            invokeBrowserCommand host BrowserSnapshot `shouldReturn`
                Left "The browser returned invalid UTF-8."

    it "renders stable fallback errors for host statuses" do
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
        browserStatusMessage 99 `shouldSatisfy`
            (== "The browser command failed (status 99).")

    it "waits for an in-flight callback before disabling its context" do
        entered <- newEmptyMVar
        release <- newEmptyMVar
        let blockingCallback _ _ _ _ _ _ _ _ _
                output outputCapacity outputLength = do
                putMVar entered ()
                takeMVar release
                writeResult "done" output outputCapacity outputLength
        withCallback blockingCallback \callback -> do
            registration <- newMVar
                (Just (BrowserRegistration callback nullContext))
            let host = BrowserHost registration
            withAsync (invokeBrowserCommand host BrowserSnapshot) \running -> do
                takeMVar entered
                withAsync
                    (modifyMVar_ registration (const (pure Nothing)))
                    \disabling -> do
                        threadDelay 20000
                        poll disabling >>= \case
                            Nothing -> pure ()
                            Just _ -> expectationFailure
                                "disabling returned before the callback completed"
                        putMVar release ()
                        wait running `shouldReturn` Right "done"
                        wait disabling
            invokeBrowserCommand host BrowserSnapshot `shouldReturn`
                Left "The browser view is not active."

type ObservedCall = (CInt, Text, Text, CDouble, CDouble, CInt)

recordingCallback
    :: IORef (Maybe ObservedCall)
    -> BrowserCallback
recordingCallback observed _ command argument1 argument1Length
        argument2 argument2Length deltaX deltaY flags
        output outputCapacity outputLength = do
    first <- decodeBytes argument1 argument1Length
    second <- decodeBytes argument2 argument2Length
    writeIORef observed (Just
        (command, first, second, deltaX, deltaY, flags))
    writeResult ("native " <> BS.pack [0xe2, 0x9c, 0x93])
        output outputCapacity outputLength

successCallback :: ByteString -> BrowserCallback
successCallback result _ _ _ _ _ _ _ _ _ output capacity length =
    writeResult result output capacity length

oversizedCallback :: BrowserCallback
oversizedCallback _ _ _ _ _ _ _ _ _ _ _ outputLength = do
    poke outputLength (fromIntegral browserOutputCapacity + 1)
    pure 0

invalidUtf8Callback :: BrowserCallback
invalidUtf8Callback _ _ _ _ _ _ _ _ _
        output outputCapacity outputLength =
    writeResult (BS.pack [0xff]) output outputCapacity outputLength

writeResult :: ByteString -> Ptr a -> CSize -> Ptr CSize -> IO CInt
writeResult result output (CSize capacity) outputLength
    | BS.length result > fromIntegral capacity = pure 7
    | otherwise = do
        BS.useAsCStringLen result \(source, length) ->
            copyBytes output (castPtr source) length
        poke outputLength (fromIntegral (BS.length result))
        pure 0

decodeBytes :: Ptr a -> CSize -> IO Text
decodeBytes pointer (CSize length) = do
    bytes <- BS.packCStringLen
        (castPtr pointer, fromIntegral length)
    case TextEncoding.decodeUtf8' bytes of
        Left _ -> expectationFailure "callback input was not UTF-8" >> pure ""
        Right value -> pure value

withBrowserHost :: BrowserCallback -> (BrowserHost -> IO a) -> IO a
withBrowserHost callback action =
    withCallback callback \funPtr -> do
        registration <- newMVar
            (Just (BrowserRegistration funPtr nullContext))
        action (BrowserHost registration)

withCallback :: BrowserCallback -> (FunPtr BrowserCallback -> IO a) -> IO a
withCallback callback =
    bracket (wrapBrowserCallback callback) freeHaskellFunPtr

nullContext :: Ptr ()
nullContext = nullPtr
