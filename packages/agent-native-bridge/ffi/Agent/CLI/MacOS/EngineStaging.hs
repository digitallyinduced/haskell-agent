{-# LANGUAGE ForeignFunctionInterface #-}

-- | Copies host-owned image buffers and stages per-turn options atomically.
module Agent.CLI.MacOS.EngineStaging () where

import Agent.CLI.MacOS.EngineState (Engine(..))
import Agent.CLI.MacOS.TurnState
    (NativeTurnOptions(..), discardStagedTurnById)
import Agent.CLI.NativeRuntime (NativeInteractionMode(..), NativeShellMode(..))
import Agent.Loop (ImageAttachment(..))
import Control.Concurrent.STM (atomically, modifyTVar')
import Control.Exception.Safe (tryAny)
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import Data.Word (Word8)
import Foreign (Ptr, StablePtr, castPtrToStablePtr, deRefStablePtr, castPtr, nullPtr, sizeOf, plusPtr, peekByteOff)
import Foreign.C.Types (CInt(..), CSize(..))

foreign export ccall ha_engine_stage_turn_images
    :: Ptr () -> Ptr Word8 -> CSize -> Ptr () -> CSize -> IO CInt

foreign export ccall ha_engine_stage_turn_options
    :: Ptr () -> Ptr Word8 -> CSize -> CInt -> CInt -> IO CInt

foreign export ccall ha_engine_discard_turn_staging
    :: Ptr () -> Ptr Word8 -> CSize -> IO CInt

ha_engine_stage_turn_images
    :: Ptr () -> Ptr Word8 -> CSize -> Ptr () -> CSize -> IO CInt
ha_engine_stage_turn_images pointer turnID turnIDLength imagePointer imageCount
    | pointer == nullPtr = pure 1
    | turnID == nullPtr || not (validNativeTurnIDLength turnIDLength) = pure 2
    | imagePointer == nullPtr && imageCount > 0 = pure 4
    | toInteger imageCount > toInteger (maxBound :: Int) = pure 4
    | otherwise = do
        accepted <- tryAny do
            let stable = castPtrToStablePtr pointer :: StablePtr Engine
            engine <- deRefStablePtr stable
            turnIDBytes <- BS.packCStringLen
                (castPtr turnID, fromIntegral turnIDLength)
            let turnIDText = TextEncoding.decodeUtf8' turnIDBytes
            imageResults <- mapM peekImage
                [0 .. fromIntegral imageCount - 1]
            case (turnIDText, sequence imageResults) of
                (Right turnIDValue, Just images) -> do
                    atomically $ modifyTVar' engine.engineStagedImages $
                        if null images
                            then Map.delete turnIDValue
                            else Map.insert turnIDValue images
                    pure True
                _ -> pure False
        pure $ case accepted of
            Left _ -> 3
            Right False -> 4
            Right True -> 0
  where
    pointerSize = sizeOf (nullPtr :: Ptr ())
    sizeSize = sizeOf (undefined :: CSize)
    imageSize = pointerSize + sizeSize + pointerSize + sizeSize

    peekImage index = do
        let base = castPtr imagePointer `plusPtr` (index * imageSize)
            readPointer offset =
                peekByteOff base offset :: IO (Ptr Word8)
            readLength offset =
                peekByteOff base offset :: IO CSize
        mimePointer <- readPointer 0
        mimeLength <- readLength pointerSize
        bytesPointer <- readPointer (pointerSize + sizeSize)
        bytesLength <- readLength (pointerSize + sizeSize + pointerSize)
        if
            (mimePointer == nullPtr && mimeLength > 0)
                || (bytesPointer == nullPtr && bytesLength > 0)
                || mimeLength == 0
                || bytesLength == 0
        then pure Nothing
        else do
            mimeBytes <- BS.packCStringLen
                (castPtr mimePointer, fromIntegral mimeLength)
            let mime = TextEncoding.decodeUtf8' mimeBytes
            bytes <- BS.packCStringLen
                (castPtr bytesPointer, fromIntegral bytesLength)
            pure $ case mime of
                Left _ -> Nothing
                Right mimeValue -> Just ImageAttachment
                    { imageMime = mimeValue
                    , imageBytes = bytes
                    }

ha_engine_stage_turn_options
    :: Ptr () -> Ptr Word8 -> CSize -> CInt -> CInt -> IO CInt
ha_engine_stage_turn_options pointer turnID turnIDLength rawMode rawShell
    | pointer == nullPtr = pure 1
    | turnID == nullPtr || not (validNativeTurnIDLength turnIDLength) = pure 2
    | otherwise =
        case (interactionModeFromCode rawMode, shellModeFromCode rawShell) of
            (Just interactionMode, Just shellMode) -> do
                accepted <- tryAny do
                    let stable =
                            castPtrToStablePtr pointer :: StablePtr Engine
                    engine <- deRefStablePtr stable
                    bytes <- BS.packCStringLen
                        (castPtr turnID, fromIntegral turnIDLength)
                    case TextEncoding.decodeUtf8' bytes of
                        Left _ -> pure False
                        Right turnIDText
                            | Text.null turnIDText -> pure False
                            | otherwise -> do
                                atomically $ modifyTVar'
                                    engine.engineStagedTurnOptions
                                    (Map.insert
                                        turnIDText
                                        NativeTurnOptions
                                            { nativeTurnInteractionMode =
                                                interactionMode
                                            , nativeTurnShellMode = shellMode
                                            , nativeTurnVoice = Nothing
                                            })
                                pure True
                pure $ case accepted of
                    Left _ -> 3
                    Right False -> 2
                    Right True -> 0
            _ -> pure 4

ha_engine_discard_turn_staging
    :: Ptr () -> Ptr Word8 -> CSize -> IO CInt
ha_engine_discard_turn_staging pointer turnID turnIDLength
    | pointer == nullPtr = pure 1
    | turnID == nullPtr || not (validNativeTurnIDLength turnIDLength) = pure 2
    | otherwise = do
        result <- tryAny do
            let stable = castPtrToStablePtr pointer :: StablePtr Engine
            engine <- deRefStablePtr stable
            bytes <- BS.packCStringLen
                (castPtr turnID, fromIntegral turnIDLength)
            case TextEncoding.decodeUtf8' bytes of
                Left _ -> pure False
                Right turnIDText
                    | Text.null turnIDText -> pure False
                    | otherwise -> do
                        atomically $ discardStagedTurnById
                            turnIDText
                            engine.engineStagedImages
                            engine.engineStagedTurnOptions
                        pure True
        pure $ case result of
            Left _ -> 3
            Right False -> 2
            Right True -> 0

maxNativeTurnIDBytes :: Integer
maxNativeTurnIDBytes = 1_024

validNativeTurnIDLength :: CSize -> Bool
validNativeTurnIDLength length =
    let integerLength = toInteger length
    in integerLength > 0
        && integerLength <= toInteger (maxBound :: Int)
        && integerLength <= maxNativeTurnIDBytes

interactionModeFromCode :: CInt -> Maybe NativeInteractionMode
interactionModeFromCode = \case
    0 -> Just NativeAsk
    1 -> Just NativePlan
    2 -> Just NativeYolo
    _ -> Nothing

shellModeFromCode :: CInt -> Maybe NativeShellMode
shellModeFromCode = \case
    0 -> Just NativeShellNone
    1 -> Just NativeShellBash
    2 -> Just NativeShellGhci
    3 -> Just NativeShellBoth
    _ -> Nothing
