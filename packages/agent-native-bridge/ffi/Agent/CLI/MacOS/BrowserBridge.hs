{-# LANGUAGE ForeignFunctionInterface #-}

-- | Native browser callback ABI and its model-visible tool adapter.
module Agent.CLI.MacOS.BrowserBridge
    ( BrowserCallback
    , BrowserHost(..)
    , BrowserRegistration(..)
    , browserCommandABI
    , browserOutputCapacity
    , browserStatusMessage
    , browserToolsWhenEnabled
    , invokeBrowserCommand
    ) where

import Agent.CLI.BrowserTools (BrowserCommand(..), browserTools)
import Agent.CLI.MacOS.Marshalling (withText)
import Agent.Tools.Types (AppTool)
import Control.Concurrent.MVar (MVar, withMVar)
import Control.Exception.Safe (tryAny)
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import Data.Word (Word8)
import Foreign (FunPtr, Ptr, alloca, castPtr, peek, poke)
import Foreign.C.Types (CDouble(..), CInt(..), CSize(..))
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Marshal.Utils (fillBytes)

type BrowserCallback =
    Ptr () -> CInt
    -> Ptr Word8 -> CSize
    -> Ptr Word8 -> CSize
    -> CDouble -> CDouble
    -> CInt
    -> Ptr Word8 -> CSize -> Ptr CSize
    -> IO CInt

foreign import ccall "dynamic"
    invokeBrowserCallback :: FunPtr BrowserCallback -> BrowserCallback

data BrowserRegistration = BrowserRegistration
    { browserCallback :: !(FunPtr BrowserCallback)
    , browserContext :: !(Ptr ())
    }

newtype BrowserHost = BrowserHost
    { browserRegistration :: MVar (Maybe BrowserRegistration)
    }

browserToolsWhenEnabled :: BrowserHost -> IO [AppTool]
browserToolsWhenEnabled host =
    withMVar host.browserRegistration \case
        Nothing -> pure []
        Just _ -> pure (browserTools (invokeBrowserCommand host))

invokeBrowserCommand
    :: BrowserHost
    -> BrowserCommand
    -> IO (Either Text Text)
invokeBrowserCommand host command =
    tryAny (withMVar host.browserRegistration invoke) >>= \case
        Left exception -> pure (Left (Text.pack (show exception)))
        Right result -> pure result
  where
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
                browserCommandABI command
        withText argument1 \argument1Ptr argument1Length ->
            withText argument2 \argument2Ptr argument2Length ->
                allocaBytes browserOutputCapacity \output ->
                    alloca \outputLength -> do
                        -- The callback controls the reported length. Zero the
                        -- buffer so unwritten stack bytes are never exposed.
                        fillBytes output 0 browserOutputCapacity
                        poke outputLength 0
                        status <- invokeBrowserCallback
                            registration.browserCallback
                            registration.browserContext
                            commandCode
                            (castPtr argument1Ptr)
                            argument1Length
                            (castPtr argument2Ptr)
                            argument2Length
                            scrollDeltaX
                            scrollDeltaY
                            flags
                            output
                            (fromIntegral browserOutputCapacity)
                            outputLength
                        CSize length <- peek outputLength
                        if length > fromIntegral browserOutputCapacity
                            then pure (Left
                                "The browser returned more than the 256 KiB output limit.")
                            else do
                                bytes <- BS.packCStringLen
                                    (castPtr output, fromIntegral length)
                                pure $ case TextEncoding.decodeUtf8' bytes of
                                    Left _ ->
                                        Left "The browser returned invalid UTF-8."
                                    Right text
                                        | status == 0 -> Right text
                                        | Text.null text ->
                                            Left (browserStatusMessage status)
                                        | otherwise -> Left text

browserCommandABI
    :: BrowserCommand
    -> (CInt, Text, Text, CDouble, CDouble, CInt)
browserCommandABI = \case
    BrowserNavigate url -> (1, url, "", 0, 0, 0)
    BrowserSnapshot -> (2, "", "", 0, 0, 0)
    BrowserClick selector -> (3, selector, "", 0, 0, 0)
    BrowserType selector text submit ->
        (4, selector, text, 0, 0, if submit then 1 else 0)
    BrowserBack -> (5, "", "", 0, 0, 0)
    BrowserForward -> (6, "", "", 0, 0, 0)
    BrowserReload -> (7, "", "", 0, 0, 0)
    BrowserKey key -> (8, key, "", 0, 0, 0)
    BrowserScroll deltaX deltaY ->
        (9, "", "", realToFrac deltaX, realToFrac deltaY, 0)

browserOutputCapacity :: Int
browserOutputCapacity = 256 * 1024

browserStatusMessage :: CInt -> Text
browserStatusMessage = \case
    1 -> "The browser host rejected an invalid argument."
    2 -> "The native browser bridge is unavailable."
    3 -> "The browser command timed out."
    4 -> "The browser host denied website or extension access."
    5 -> "The browser host does not support this operation."
    6 -> "The native browser bridge failed internally."
    7 -> "The browser host returned a result that was too large."
    status ->
        "The browser command failed (status "
            <> Text.pack (show status)
            <> ")."
