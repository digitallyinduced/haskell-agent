{-# LANGUAGE ForeignFunctionInterface #-}

-- | Native browser callback ABI and its model-visible tool adapter.
module Agent.CLI.MacOS.BrowserBridge
    ( BrowserCancelCallback
    , BrowserCompletion
    , nextAvailableBrowserPendingId
    , BrowserCallback
    , BrowserHost(..)
    , BrowserRegistration(..)
    , browserCommandABI
    , browserOutputCapacity
    , browserStatusMessage
    , browserToolsWhenEnabled
    , invokeBrowserCommand
    ) where

import Agent.CLI.BrowserTools (BrowserCommand(..), BrowserInvocation(..), browserTools)
import Agent.CLI.MacOS.Marshalling (withText)
import Agent.Tools.Types (AppTool)
import Control.Concurrent.MVar
import Control.Exception.Safe (tryAny, bracket, onException, uninterruptibleMask_)
import Control.Monad (forM_, when, void)
import Agent.ToolDispatch (ToolHandlerResult(..), ToolResultImage(..))
import qualified Codec.Picture as Picture
import qualified Data.ByteString.Base64 as Base64
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import Data.Word (Word8, Word32, Word64)
import Foreign (FunPtr, Ptr, castPtr, nullPtr, peekByteOff, pokeByteOff)
import Foreign.Ptr (ptrToWordPtr, wordPtrToPtr)
import Foreign.C.Types (CDouble(..), CInt(..), CSize(..))
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Marshal.Utils (fillBytes)

type BrowserCallback =
    Ptr ()
    -> Ptr ()
    -> FunPtr BrowserCompletion
    -> Ptr ()
    -> IO CInt

type BrowserCancelCallback =
    Ptr ()
    -> Ptr Word8 -> CSize
    -> Ptr Word8 -> CSize
    -> IO ()

type BrowserCompletion =
    Ptr ()
    -> Ptr ()
    -> IO ()

foreign import ccall "dynamic"
    invokeBrowserCallback :: FunPtr BrowserCallback -> BrowserCallback
foreign import ccall "dynamic"
    invokeBrowserCancelCallback :: FunPtr BrowserCancelCallback -> BrowserCancelCallback
foreign import ccall "wrapper"
    makeBrowserCompletion :: BrowserCompletion -> IO (FunPtr BrowserCompletion)

data BrowserRegistration = BrowserRegistration
    { browserCallback :: !(FunPtr BrowserCallback)
    , browserCancelCallback :: !(FunPtr BrowserCancelCallback)
    , browserContext :: !(Ptr ())
    }
newtype BrowserHost = BrowserHost
    { browserRegistration :: MVar (Maybe BrowserRegistration) }

browserToolsWhenEnabled :: BrowserHost -> Text -> IO [AppTool]
browserToolsWhenEnabled host scopeId =
    withMVar host.browserRegistration \case
        Nothing -> pure []
        Just _ -> pure (browserTools scopeId (invokeBrowserCommand host))

invokeBrowserCommand
    :: BrowserHost
    -> BrowserInvocation
    -> IO (Either Text ToolHandlerResult)
invokeBrowserCommand host invocation =
    tryAny (withMVar host.browserRegistration invoke) >>= \case
        Left exception -> pure (Left (Text.pack (show exception)))
        Right result -> pure result
  where
    invoke
        :: Maybe BrowserRegistration
        -> IO (Either Text ToolHandlerResult)
    invoke Nothing =
        pure (Left "The browser view is not active.")
    invoke (Just registration) = do
        let
            ( commandCode
                , argument1
                , argument2
                , scrollDeltaX
                , scrollDeltaY
                , flags
                ) =
                browserCommandABI invocation.browserCommand
        if not (validBrowserIdentifier browserScopeIdMaxBytes
                    invocation.browserScopeId)
            || not (validBrowserIdentifier browserCallIdMaxBytes
                    invocation.browserCallId)
            then pure (Left
                "The browser scope or tool-call identifier is invalid.")
            else withTextBytes invocation.browserScopeId
                    \scopePtr scopeLength ->
                withTextBytes invocation.browserCallId
                    \callPtr callLength ->
                withBrowserTextBytes argument1
                    \argument1Ptr argument1Length ->
                withBrowserTextBytes argument2
                    \argument2Ptr argument2Length ->
                allocaBytes browserRequestStructSize \request -> do
                    fillBytes request 0 browserRequestStructSize
                    pokeByteOff request 0
                        (fromIntegral browserRequestStructSize :: Word32)
                    pokeByteOff request 4 commandCode
                    pokeByteOff request 8 flags
                    pokeByteOff request 16 scrollDeltaX
                    pokeByteOff request 24 scrollDeltaY
                    pokeByteOff request 32 scopePtr
                    pokeByteOff request 40 scopeLength
                    pokeByteOff request 48 callPtr
                    pokeByteOff request 56 callLength
                    pokeByteOff request 64 argument1Ptr
                    pokeByteOff request 72 argument1Length
                    pokeByteOff request 80 argument2Ptr
                    pokeByteOff request 88 argument2Length
                    let
                        start = do
                            (requestId, pending) <- registerBrowserPending
                            status <-
                                invokeBrowserCallback
                                    registration.browserCallback
                                    registration.browserContext
                                    (castPtr request)
                                    browserCompletionFunPtr
                                    (wordPtrToPtr (fromIntegral requestId))
                                `onException`
                                    unregisterBrowserPending requestId
                            if status /= 0
                                then do
                                    unregisterBrowserPending requestId
                                    pure (Left (browserStatusMessage status))
                                else pure (Right (requestId, pending))

                        cancelAndDrain = \case
                            Left _ -> pure ()
                            Right (requestId, pending) ->
                                uninterruptibleMask_ do
                                    stillPending <-
                                        browserPendingIsRegistered requestId
                                    when stillPending do
                                        invokeBrowserCancelCallback
                                            registration.browserCancelCallback
                                            registration.browserContext
                                            scopePtr
                                            scopeLength
                                            callPtr
                                            callLength
                                        void (takeMVar pending)

                        await = \case
                            Left err -> pure (Left err)
                            Right (_, pending) -> do
                                nativeResult <- takeMVar pending
                                pure (nativeResult >>=
                                    renderBrowserResult
                                        invocation.browserCommand)

                    bracket start cancelAndDrain await

browserCommandABI
    :: BrowserCommand
    -> (CInt, Text, Text, CDouble, CDouble, CInt)
browserCommandABI = \case
    BrowserNavigate url -> (1, url, "", 0, 0, 0)
    BrowserSnapshot -> (2, "", "", 0, 0, 0)
    BrowserClick ref -> (3, ref, "", 0, 0, 0)
    BrowserType ref text submit ->
        (4, ref, text, 0, 0, if submit then 1 else 0)
    BrowserBack -> (5, "", "", 0, 0, 0)
    BrowserForward -> (6, "", "", 0, 0, 0)
    BrowserReload -> (7, "", "", 0, 0, 0)
    BrowserKey key -> (8, key, "", 0, 0, 0)
    BrowserScroll deltaX deltaY ->
        (9, "", "", realToFrac deltaX, realToFrac deltaY, 0)
    BrowserScreenshot -> (10, "", "", 0, 0, 0)
    BrowserListTabs -> (11, "", "", 0, 0, 0)
    BrowserSwitchTab tabId -> (12, tabId, "", 0, 0, 0)
    BrowserListDownloads -> (13, "", "", 0, 0, 0)

browserOutputCapacity :: Int
browserOutputCapacity = 256 * 1024

browserRequestStructSize, browserResultStructSize :: Int
browserRequestStructSize = 96
browserResultStructSize = 56

browserImageCapacity :: Int
browserImageCapacity = 16 * 1024 * 1024

browserScopeIdMaxBytes, browserCallIdMaxBytes :: Int
browserScopeIdMaxBytes = 1024
browserCallIdMaxBytes = 1024

data BrowserNativeResult = BrowserNativeResult
    { browserNativeStatus :: !CInt
    , browserNativeText :: !Text
    , browserNativeImageFormat :: !CInt
    , browserNativeImageWidth :: !CInt
    , browserNativeImageHeight :: !CInt
    , browserNativeImageBytes :: !BS.ByteString
    }

type BrowserPending =
    MVar (Either Text BrowserNativeResult)

{-# NOINLINE browserPendingRegistry #-}
browserPendingRegistry :: MVar (Word64, Map Word64 BrowserPending)
browserPendingRegistry = unsafePerformIO (newMVar (1, Map.empty))

registerBrowserPending :: IO (Word64, BrowserPending)
registerBrowserPending = do
    pending <- newEmptyMVar
    modifyMVar browserPendingRegistry \(nextId, pendingById) -> do
        let requestId = nextAvailableBrowserPendingId nextId pendingById
            followingId = incrementBrowserPendingId requestId
        pure
            ( (followingId, Map.insert requestId pending pendingById)
            , (requestId, pending)
            )

nextAvailableBrowserPendingId :: Word64 -> Map Word64 value -> Word64
nextAvailableBrowserPendingId candidate pendingById
    | Map.notMember candidate pendingById = candidate
    | otherwise =
        nextAvailableBrowserPendingId
            (incrementBrowserPendingId candidate)
            pendingById

incrementBrowserPendingId :: Word64 -> Word64
incrementBrowserPendingId requestId
    | requestId == maxBound = 1
    | otherwise = requestId + 1

unregisterBrowserPending :: Word64 -> IO ()
unregisterBrowserPending requestId =
    modifyMVar_ browserPendingRegistry \(nextId, pendingById) ->
        pure (nextId, Map.delete requestId pendingById)

browserPendingIsRegistered :: Word64 -> IO Bool
browserPendingIsRegistered requestId =
    withMVar browserPendingRegistry \(_, pendingById) ->
        pure (Map.member requestId pendingById)

completeBrowserPending
    :: Word64
    -> Either Text BrowserNativeResult
    -> IO ()
completeBrowserPending requestId result = do
    pending <- modifyMVar browserPendingRegistry \(nextId, pendingById) ->
        pure
            ( (nextId, Map.delete requestId pendingById)
            , Map.lookup requestId pendingById
            )
    forM_ pending \slot ->
        void (tryPutMVar slot result)

{-# NOINLINE browserCompletionFunPtr #-}
browserCompletionFunPtr :: FunPtr BrowserCompletion
browserCompletionFunPtr = unsafePerformIO
    (makeBrowserCompletion browserCompletionCallback)

browserCompletionCallback :: BrowserCompletion
browserCompletionCallback context resultPointer = do
    decoded <- tryAny (decodeBrowserNativeResult resultPointer)
    let result = case decoded of
            Left exception ->
                Left ("The browser completion failed: "
                    <> Text.pack (show exception))
            Right value -> value
    completeBrowserPending
        (fromIntegral (ptrToWordPtr context))
        result

decodeBrowserNativeResult
    :: Ptr ()
    -> IO (Either Text BrowserNativeResult)
decodeBrowserNativeResult pointer
    | pointer == nullPtr =
        pure (Left "The browser returned a null result.")
    | otherwise = do
        structSize <- peekByteOff pointer 0 :: IO Word32
        status <- peekByteOff pointer 4
        imageFormat <- peekByteOff pointer 8
        imageWidth <- peekByteOff pointer 12
        imageHeight <- peekByteOff pointer 16
        reserved <- peekByteOff pointer 20 :: IO CInt
        textPointer <- peekByteOff pointer 24
        textLength <- peekByteOff pointer 32
        imagePointer <- peekByteOff pointer 40
        imageLength <- peekByteOff pointer 48
        if structSize /= fromIntegral browserResultStructSize
            then pure (Left "The browser returned an incompatible result structure.")
            else if reserved /= 0
                then pure (Left "The browser returned nonzero reserved fields.")
                else readBrowserBytes
                        "text"
                        browserOutputCapacity
                        textPointer
                        textLength >>= \case
                    Left err -> pure (Left err)
                    Right textBytes ->
                        case TextEncoding.decodeUtf8' textBytes of
                            Left _ ->
                                pure (Left "The browser returned invalid UTF-8.")
                            Right browserNativeText ->
                                readBrowserBytes
                                    "image"
                                    browserImageCapacity
                                    imagePointer
                                    imageLength >>= \case
                                        Left err -> pure (Left err)
                                        Right browserNativeImageBytes ->
                                            pure (Right BrowserNativeResult
                                                { browserNativeStatus = status
                                                , browserNativeText
                                                , browserNativeImageFormat =
                                                    imageFormat
                                                , browserNativeImageWidth =
                                                    imageWidth
                                                , browserNativeImageHeight =
                                                    imageHeight
                                                , browserNativeImageBytes
                                                })

readBrowserBytes
    :: Text
    -> Int
    -> Ptr Word8
    -> CSize
    -> IO (Either Text BS.ByteString)
readBrowserBytes label limit pointer length
    | length > fromIntegral limit =
        pure (Left ("The browser returned " <> label
            <> " data above its size limit."))
    | length == 0 =
        if pointer == nullPtr
            then pure (Right BS.empty)
            else pure (Left ("The browser returned a non-null " <> label
                <> " pointer for an empty slice."))
    | pointer == nullPtr =
        pure (Left ("The browser returned a null " <> label <> " pointer."))
    | otherwise =
        Right <$> BS.packCStringLen
            (castPtr pointer, fromIntegral length)

renderBrowserResult
    :: BrowserCommand
    -> BrowserNativeResult
    -> Either Text ToolHandlerResult
renderBrowserResult command result
    | result.browserNativeStatus /= 0 =
        if hasImage
            then Left "The browser returned an image with a failed result."
            else Left
                if Text.null result.browserNativeText
                    then browserStatusMessage result.browserNativeStatus
                    else result.browserNativeText
    | command == BrowserScreenshot =
        if result.browserNativeImageFormat /= 1
            || BS.null result.browserNativeImageBytes
            then Left "The browser screenshot did not contain a PNG image."
            else if result.browserNativeImageWidth <= 0
                || result.browserNativeImageHeight <= 0
                || result.browserNativeImageWidth >
                    browserLogicalImageMaxDimension
                || result.browserNativeImageHeight >
                    browserLogicalImageMaxDimension
                then Left "The browser screenshot dimensions were invalid."
                else case validateBrowserPng
                        result.browserNativeImageBytes of
                    Left validationError -> Left validationError
                    Right () -> Right ToolHandlerResult
                        { resultText =
                            if Text.null result.browserNativeText
                                then "Captured browser viewport ("
                                    <> Text.pack
                                        (show result.browserNativeImageWidth)
                                    <> "×"
                                    <> Text.pack
                                        (show result.browserNativeImageHeight)
                                    <> ")."
                                else result.browserNativeText
                        , resultImages =
                            [ ToolResultImage
                                { imageUrl =
                                    "data:image/png;base64,"
                                        <> TextEncoding.decodeUtf8
                                            (Base64.encode
                                                result.browserNativeImageBytes)
                                , imageDetail = Just "high"
                                }
                            ]
                        }
    | hasImage =
        Left "The browser returned an unexpected image for this command."
    | otherwise =
        Right ToolHandlerResult
            { resultText = result.browserNativeText
            , resultImages = []
            }
  where
    hasImage =
        result.browserNativeImageFormat /= 0
            || not (BS.null result.browserNativeImageBytes)
            || result.browserNativeImageWidth /= 0
            || result.browserNativeImageHeight /= 0

browserLogicalImageMaxDimension :: CInt
browserLogicalImageMaxDimension = 100000

browserPngMaxDimension :: Word64
browserPngMaxDimension = 16384

browserPngMaxPixels :: Word64
browserPngMaxPixels = 67108864

validateBrowserPng :: BS.ByteString -> Either Text ()
validateBrowserPng bytes =
    case browserPngHeaderDimensions bytes of
        Nothing ->
            Left "The browser screenshot was not a valid PNG byte sequence."
        Just (width, height)
            | width == 0
                || height == 0
                || width > browserPngMaxDimension
                || height > browserPngMaxDimension
                || width * height > browserPngMaxPixels ->
                Left "The browser screenshot PNG pixel dimensions were invalid."
            | otherwise ->
                case Picture.decodePng bytes of
                    Left _ ->
                        Left
                            "The browser screenshot was not a valid PNG byte sequence."
                    Right image ->
                        let decodedDimensions = Picture.dynamicMap
                                (\decoded ->
                                    ( fromIntegral
                                        (Picture.imageWidth decoded)
                                    , fromIntegral
                                        (Picture.imageHeight decoded)
                                    ))
                                image
                        in if decodedDimensions == (width, height)
                            then Right ()
                            else Left
                                "The browser screenshot was not a valid PNG byte sequence."

browserPngHeaderDimensions :: BS.ByteString -> Maybe (Word64, Word64)
browserPngHeaderDimensions bytes
    | BS.length bytes < 24 = Nothing
    | BS.take 8 bytes /= pngSignature = Nothing
    | browserPngWord32At 8 bytes /= 13 = Nothing
    | BS.take 4 (BS.drop 12 bytes) /= "IHDR" = Nothing
    | otherwise =
        Just
            ( browserPngWord32At 16 bytes
            , browserPngWord32At 20 bytes
            )
  where
    pngSignature = BS.pack [137, 80, 78, 71, 13, 10, 26, 10]

browserPngWord32At :: Int -> BS.ByteString -> Word64
browserPngWord32At offset =
    BS.foldl'
        (\value byte -> value * 256 + fromIntegral byte)
        0
        . BS.take 4
        . BS.drop offset

validBrowserIdentifier :: Int -> Text -> Bool
validBrowserIdentifier limit value =
    not (Text.null value)
        && BS.length (TextEncoding.encodeUtf8 value) <= limit

withBrowserTextBytes
    :: Text
    -> (Ptr Word8 -> CSize -> IO value)
    -> IO value
withBrowserTextBytes value action
    | Text.null value = action nullPtr 0
    | otherwise = withTextBytes value action

browserStatusMessage :: CInt -> Text
browserStatusMessage = \case
    1 -> "The browser host rejected an invalid argument."
    2 -> "The native browser bridge is unavailable."
    3 -> "The browser command timed out."
    4 -> "The browser host denied website or extension access."
    5 -> "The browser host does not support this operation."
    6 -> "The native browser bridge failed internally."
    7 -> "The browser host returned a result that was too large."
    8 -> "The browser command was cancelled."
    status ->
        "The browser command failed (status "
            <> Text.pack (show status)
            <> ")."

withTextBytes :: Text -> (Ptr Word8 -> CSize -> IO a) -> IO a
withTextBytes value action = withText value (\pointer size -> action (castPtr pointer) size)
