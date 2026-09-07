{-# LANGUAGE ForeignFunctionInterface #-}

-- | Host registration replacement and delivery of interaction resolutions.
module Agent.CLI.MacOS.EngineHostRegistration () where

import Agent.CLI.MacOS.BrowserBridge (BrowserHost(..), BrowserRegistration(..), BrowserCallback, BrowserCancelCallback)
import Agent.CLI.MacOS.ComputerBridge
    ( ComputerCallback
    , ComputerRegistration(..)
    , replaceComputerRegistration
    )
import Agent.CLI.MacOS.EngineState (Engine(..))
import Agent.CLI.MacOS.InteractionState
    ( InteractionCallback
    , InteractionCallbackTarget(..)
    , InteractionRuntime(..)
    , NativeInteractionResolution(..)
    , cancelPendingInteractions
    , resolvePendingInteraction
    )
import Control.Concurrent.MVar (modifyMVar_, withMVar)
import Control.Concurrent.STM (atomically, writeTVar)
import Control.Exception.Safe (tryAny)
import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as TextEncoding
import Data.Word (Word8)
import Foreign (Ptr, FunPtr, StablePtr, castPtrToStablePtr, deRefStablePtr, castPtr, nullPtr, nullFunPtr)
import Foreign.C.Types (CInt(..), CSize(..))

foreign export ccall ha_engine_set_browser_callback
    :: Ptr () -> FunPtr BrowserCallback -> FunPtr BrowserCancelCallback -> Ptr () -> IO CInt

foreign export ccall ha_engine_set_computer_callback
    :: Ptr () -> FunPtr ComputerCallback -> Ptr () -> IO CInt

foreign export ccall ha_engine_set_interaction_callback
    :: Ptr () -> FunPtr InteractionCallback -> Ptr () -> IO CInt

foreign export ccall ha_engine_resolve_interaction
    :: Ptr () -> Ptr Word8 -> CSize -> Ptr Word8 -> CSize
    -> CInt -> Ptr Word8 -> CSize -> IO CInt

ha_engine_set_browser_callback
    :: Ptr () -> FunPtr BrowserCallback -> FunPtr BrowserCancelCallback -> Ptr () -> IO CInt
ha_engine_set_browser_callback pointer callback cancelCallback context
    | pointer == nullPtr = pure 1
    | (callback == nullFunPtr) /= (cancelCallback == nullFunPtr) = pure 2
    | otherwise = do
        updated <- tryAny do
            let stable = castPtrToStablePtr pointer :: StablePtr Engine
            engine <- deRefStablePtr stable
            modifyMVar_ engine.engineBrowser.browserRegistration $ \_ ->
                pure
                    if callback == nullFunPtr
                        then Nothing
                        else Just BrowserRegistration
                            { browserCallback = callback
                            , browserCancelCallback = cancelCallback
                            , browserContext = context
                            }
        pure $ case updated of
            Left _ -> 2
            Right () -> 0

ha_engine_set_computer_callback
    :: Ptr () -> FunPtr ComputerCallback -> Ptr () -> IO CInt
ha_engine_set_computer_callback pointer callback context
    | pointer == nullPtr = pure 1
    | otherwise = do
        updated <- tryAny do
            let stable = castPtrToStablePtr pointer :: StablePtr Engine
            engine <- deRefStablePtr stable
            replaceComputerRegistration engine.engineComputer $
                if callback == nullFunPtr
                    then Nothing
                    else Just ComputerRegistration
                        { computerCallback = callback
                        , computerContext = context
                        }
        pure $ case updated of
            Left _ -> 2
            Right () -> 0

ha_engine_set_interaction_callback
    :: Ptr () -> FunPtr InteractionCallback -> Ptr () -> IO CInt
ha_engine_set_interaction_callback pointer callback callbackContext
    | pointer == nullPtr = pure 1
    | otherwise = do
        result <- tryAny do
            let stable = castPtrToStablePtr pointer :: StablePtr Engine
            engine <- deRefStablePtr stable
            withMVar
                engine.engineInteractions.interactionCallbackLock
                \_ -> atomically do
                    writeTVar
                        engine.engineInteractions.interactionCallbackTarget
                        (if callback == nullFunPtr
                            then Nothing
                            else Just InteractionCallbackTarget
                                { interactionTargetCallback = callback
                                , interactionTargetContext = callbackContext
                                })
                    cancelPendingInteractions
                        engine.engineInteractions.interactionPending
        pure $ either (const 3) (const 0) result

ha_engine_resolve_interaction
    :: Ptr () -> Ptr Word8 -> CSize -> Ptr Word8 -> CSize
    -> CInt -> Ptr Word8 -> CSize -> IO CInt
ha_engine_resolve_interaction
        pointer
        turnID
        (CSize turnIDLength)
        interactionID
        (CSize interactionIDLength)
        (CInt selectedIndex)
        customText
        (CSize customTextLength)
    | pointer == nullPtr = pure 1
    | turnID == nullPtr || turnIDLength == 0 = pure 2
    | interactionID == nullPtr || interactionIDLength == 0 = pure 2
    | customText == nullPtr && customTextLength > 0 = pure 2
    | otherwise = do
        let selectedIndexValue = fromIntegral selectedIndex :: Int
        result <- tryAny do
            let stable = castPtrToStablePtr pointer :: StablePtr Engine
            engine <- deRefStablePtr stable
            let pendingRef =
                    engine.engineInteractions.interactionPending
            turnBytes <- BS.packCStringLen
                (castPtr turnID, fromIntegral turnIDLength)
            interactionBytes <- BS.packCStringLen
                (castPtr interactionID, fromIntegral interactionIDLength)
            customBytes <-
                if customTextLength == 0
                    then pure (Right Nothing)
                    else fmap (fmap Just . TextEncoding.decodeUtf8')
                        (BS.packCStringLen
                            (castPtr customText, fromIntegral customTextLength))
            case
                ( TextEncoding.decodeUtf8' turnBytes
                , TextEncoding.decodeUtf8' interactionBytes
                , customBytes
                )
              of
                (Right turnIDText, Right interactionIDText, Right custom) ->
                    fmap (\published -> if published then 0 else 4) $
                        atomically $
                            resolvePendingInteraction
                                pendingRef
                                (turnIDText, interactionIDText)
                                NativeInteractionResolution
                                    { interactionSelectedIndex =
                                        selectedIndexValue
                                    , interactionCustomText = custom
                                    }
                _ -> pure 2
        pure $ case result of
            Left _ -> 3
            Right status -> status
