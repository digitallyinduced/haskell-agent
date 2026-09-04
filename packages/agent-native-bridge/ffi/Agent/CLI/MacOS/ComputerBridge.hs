{-# LANGUAGE ForeignFunctionInterface #-}

module Agent.CLI.MacOS.ComputerBridge
    ( CComputerAction(..)
    , CComputerPoint(..)
    , ComputerBatch(..)
    , ComputerCallback
    , ComputerHost(..)
    , ComputerRegistration(..)
    , ComputerSession
    , computerActionStructSize
    , computerErrorCapacity
    , computerOutputCapacity
    , computerStatusMessage
    , computerToolWhenEnabled
    , encodeComputerActions
    , invokeComputerTransaction
    , invokeComputerSessionTransaction
    , newComputerHost
    , newComputerSession
    ) where

import Agent.CLI.ComputerUse
    ( ComputerObservation(..)
    , ComputerUseBackend(..)
    , ScreenshotEncoding(..)
    , computerUseToolWith
    )
import Agent.Loop (ImageAttachment(..))
import Agent.Responses.Types
    ( ComputerAction(..)
    , ComputerPoint(..)
    )
import Agent.Tools.Types (AppTool)
import Control.Exception.Safe (bracket, tryAny)
import Control.Monad (foldM)
import Control.Concurrent.MVar (MVar, modifyMVar, newMVar, withMVar)
import qualified Data.ByteString as BS
import Data.Bits ((.|.))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import Data.Word (Word8, Word32, Word64)
import Foreign
    ( FunPtr
    , Ptr
    , Storable(..)
    , castPtr
    , nullPtr
    , peek
    , peekByteOff
    , poke
    , pokeByteOff
    )
import Foreign.C.Types (CInt(..), CSize(..))
import Foreign.Marshal.Alloc (allocaBytes, free, mallocBytes)
import Foreign.Marshal.Array (withArray)
import Foreign.Marshal.Utils (fillBytes)

data CComputerPoint = CComputerPoint
    { cComputerPointX :: !CInt
    , cComputerPointY :: !CInt
    } deriving (Eq, Show)

instance Storable CComputerPoint where
    sizeOf _ = 8
    alignment _ = alignment (undefined :: CInt)
    peek pointer =
        CComputerPoint
            <$> peekByteOff pointer 0
            <*> peekByteOff pointer 4
    poke pointer point = do
        pokeByteOff pointer 0 point.cComputerPointX
        pokeByteOff pointer 4 point.cComputerPointY

data CComputerAction = CComputerAction
    { cComputerStructSize :: !Word32
    , cComputerAction :: !CInt
    , cComputerX :: !CInt
    , cComputerY :: !CInt
    , cComputerDeltaX :: !CInt
    , cComputerDeltaY :: !CInt
    , cComputerButton :: !CInt
    , cComputerModifiers :: !Word32
    , cComputerTextOffset :: !Word64
    , cComputerTextLength :: !Word64
    , cComputerPointOffset :: !Word64
    , cComputerPointCount :: !Word64
    } deriving (Eq, Show)

instance Storable CComputerAction where
    sizeOf _ = computerActionStructSize
    alignment _ = alignment (undefined :: Word64)
    peek pointer =
        CComputerAction
            <$> peekByteOff pointer 0
            <*> peekByteOff pointer 4
            <*> peekByteOff pointer 8
            <*> peekByteOff pointer 12
            <*> peekByteOff pointer 16
            <*> peekByteOff pointer 20
            <*> peekByteOff pointer 24
            <*> peekByteOff pointer 28
            <*> peekByteOff pointer 32
            <*> peekByteOff pointer 40
            <*> peekByteOff pointer 48
            <*> peekByteOff pointer 56
    poke pointer action = do
        pokeByteOff pointer 0 action.cComputerStructSize
        pokeByteOff pointer 4 action.cComputerAction
        pokeByteOff pointer 8 action.cComputerX
        pokeByteOff pointer 12 action.cComputerY
        pokeByteOff pointer 16 action.cComputerDeltaX
        pokeByteOff pointer 20 action.cComputerDeltaY
        pokeByteOff pointer 24 action.cComputerButton
        pokeByteOff pointer 28 action.cComputerModifiers
        pokeByteOff pointer 32 action.cComputerTextOffset
        pokeByteOff pointer 40 action.cComputerTextLength
        pokeByteOff pointer 48 action.cComputerPointOffset
        pokeByteOff pointer 56 action.cComputerPointCount

data ComputerBatch = ComputerBatch
    { computerBatchActions :: ![CComputerAction]
    , computerBatchPoints :: ![CComputerPoint]
    , computerBatchText :: !BS.ByteString
    } deriving (Eq, Show)

type ComputerCallback =
    Ptr ()
    -> Word32
    -> CInt
    -> Word64
    -> CInt
    -> CInt
    -> Ptr CComputerAction
    -> CSize
    -> Ptr CComputerPoint
    -> CSize
    -> Ptr Word8
    -> CSize
    -> CInt
    -> Ptr Word8
    -> CSize
    -> Ptr CSize
    -> Ptr Word64
    -> Ptr CInt
    -> Ptr CInt
    -> Ptr CInt
    -> IO CInt

foreign import ccall safe "dynamic"
    invokeComputerCallback :: FunPtr ComputerCallback -> ComputerCallback

data ComputerRegistration = ComputerRegistration
    { computerCallback :: !(FunPtr ComputerCallback)
    , computerContext :: !(Ptr ())
    }

newtype ComputerHost = ComputerHost
    { computerRegistration :: MVar (Maybe ComputerRegistration)
    }

newtype ComputerSession = ComputerSession
    { computerSessionObservation :: MVar (Maybe NativeComputerDisplay)
    }

newComputerHost :: IO ComputerHost
newComputerHost = ComputerHost <$> newMVar Nothing

newComputerSession :: IO ComputerSession
newComputerSession = ComputerSession <$> newMVar Nothing

computerToolWhenEnabled :: ComputerHost -> IO (Maybe AppTool)
computerToolWhenEnabled host =
    withMVar host.computerRegistration \case
        Nothing -> pure Nothing
        Just _ -> do
            session <- newComputerSession
            pure . Just . computerUseToolWith $
                ComputerUseBackend
                    { computerRunTransaction =
                        invokeComputerSessionTransaction host session
                    }

encodeComputerActions :: [ComputerAction] -> Either Text ComputerBatch
encodeComputerActions actions = do
    batch <- foldM appendAction emptyBatch actions
    if length batch.computerBatchActions > 10
        then Left "Computer callback action count exceeds 10."
        else if length batch.computerBatchPoints > 10240
            then Left "Computer callback point count exceeds 10240."
            else if BS.length batch.computerBatchText > 327680
                then Left "Computer callback text exceeds 327680 UTF-8 bytes."
                else Right batch
  where
    emptyBatch = ComputerBatch [] [] BS.empty

appendAction :: ComputerBatch -> ComputerAction -> Either Text ComputerBatch
appendAction batch action = case action of
    ClickAction{clickX, clickY, clickButton, clickKeys} -> do
        button <- computerButtonCode clickButton
        modifiers <- computerModifierMask clickKeys
        pure (addAction batch ((plainAction 1)
            { cComputerX = int32 clickX
            , cComputerY = int32 clickY
            , cComputerButton = button
            , cComputerModifiers = modifiers
            }))
    DoubleClickAction{doubleClickX, doubleClickY, doubleClickKeys} -> do
        modifiers <- computerModifierMask doubleClickKeys
        pure (addAction batch ((plainAction 2)
            { cComputerX = int32 doubleClickX
            , cComputerY = int32 doubleClickY
            , cComputerModifiers = modifiers
            }))
    ScrollAction{scrollX, scrollY, scrollDx, scrollDy, scrollKeys} -> do
        modifiers <- computerModifierMask scrollKeys
        pure (addAction batch ((plainAction 3)
            { cComputerX = int32 scrollX
            , cComputerY = int32 scrollY
            , cComputerDeltaX = int32 scrollDx
            , cComputerDeltaY = int32 scrollDy
            , cComputerModifiers = modifiers
            }))
    MoveAction{moveX, moveY, moveKeys} -> do
        modifiers <- computerModifierMask moveKeys
        pure (addAction batch ((plainAction 4)
            { cComputerX = int32 moveX
            , cComputerY = int32 moveY
            , cComputerModifiers = modifiers
            }))
    DragAction{dragPath, dragKeys} -> do
        modifiers <- computerModifierMask dragKeys
        let offset = length batch.computerBatchPoints
            encodedPoints =
                [ CComputerPoint (int32 pointX) (int32 pointY)
                | ComputerPoint{pointX, pointY} <- dragPath
                ]
            encoded = (plainAction 5)
                { cComputerModifiers = modifiers
                , cComputerPointOffset = fromIntegral offset
                , cComputerPointCount = fromIntegral (length encodedPoints)
                }
        pure (addAction
            (batch
                { computerBatchPoints =
                    batch.computerBatchPoints <> encodedPoints
                })
            encoded)
    TypeAction value ->
        pure (addTextAction batch 6 value 0)
    KeypressAction keys -> case reverse keys of
        [] -> Left "Computer key combination is empty."
        key : reversedModifiers -> do
            modifiers <- computerModifierMask (reverse reversedModifiers)
            pure (addTextAction batch 7 (Text.strip key) modifiers)
    WaitAction ->
        pure (addAction batch (plainAction 8))
    ScreenshotAction ->
        Left "Computer screenshot must not cross the native action ABI."
    UnknownComputerAction _ ->
        Left "Unsupported computer action crossed the native action ABI."

addAction :: ComputerBatch -> CComputerAction -> ComputerBatch
addAction batch action =
    batch { computerBatchActions = batch.computerBatchActions <> [action] }

addTextAction
    :: ComputerBatch
    -> CInt
    -> Text
    -> Word32
    -> ComputerBatch
addTextAction batch actionCode value modifiers =
    let bytes = TextEncoding.encodeUtf8 value
        offset = BS.length batch.computerBatchText
        action = (plainAction actionCode)
            { cComputerModifiers = modifiers
            , cComputerTextOffset = fromIntegral offset
            , cComputerTextLength = fromIntegral (BS.length bytes)
            }
    in addAction
        (batch { computerBatchText = batch.computerBatchText <> bytes })
        action

plainAction :: CInt -> CComputerAction
plainAction action = CComputerAction
    { cComputerStructSize = fromIntegral computerActionStructSize
    , cComputerAction = action
    , cComputerX = 0
    , cComputerY = 0
    , cComputerDeltaX = 0
    , cComputerDeltaY = 0
    , cComputerButton = 0
    , cComputerModifiers = 0
    , cComputerTextOffset = 0
    , cComputerTextLength = 0
    , cComputerPointOffset = 0
    , cComputerPointCount = 0
    }

computerButtonCode :: Text -> Either Text CInt
computerButtonCode raw = case normalize raw of
    "left" -> Right 1
    "right" -> Right 2
    "wheel" -> Right 3
    "middle" -> Right 3
    "back" -> Right 4
    "forward" -> Right 5
    value -> Left ("Unsupported computer mouse button: " <> value)

computerModifierMask :: [Text] -> Either Text Word32
computerModifierMask =
    fmap (foldr (.|.) 0) . traverse modifier . map normalize
  where
    modifier = \case
        "shift" -> Right 1
        "ctrl" -> Right 2
        "control" -> Right 2
        "alt" -> Right 4
        "option" -> Right 4
        "cmd" -> Right 8
        "command" -> Right 8
        "meta" -> Right 8
        "fn" -> Right 16
        "function" -> Right 16
        value -> Left ("Unsupported computer modifier: " <> value)

normalize :: Text -> Text
normalize = Text.toLower . Text.strip

int32 :: Int -> CInt
int32 = fromIntegral

data NativeComputerDisplay = NativeComputerDisplay
    { nativeDisplayToken :: !Word64
    , nativeDisplayWidth :: !CInt
    , nativeDisplayHeight :: !CInt
    }

-- | Hold the callback registration for the complete query, display-specific
-- validation, and run sequence. This both serializes desktop transactions in
-- one engine and makes callback replacement quiescent across the whole
-- sequence rather than between its two native calls.
invokeComputerTransaction
    :: ComputerHost
    -> ScreenshotEncoding
    -> [ComputerAction]
    -> ((Int, Int) -> Either Text ())
    -> IO (Either Text ComputerObservation)
invokeComputerTransaction host encoding actions validateDisplay =
    fmap (fmap fst) $
        invokeComputerTransactionWithLease
            host
            Nothing
            encoding
            actions
            validateDisplay

-- | A tool instance retains the lease returned with its last screenshot.
-- Mutation batches therefore execute against the exact observation that the
-- model saw instead of minting a new lease after the model chose coordinates.
-- An explicit screenshot has no native actions and safely refreshes the lease.
invokeComputerSessionTransaction
    :: ComputerHost
    -> ComputerSession
    -> ScreenshotEncoding
    -> [ComputerAction]
    -> ((Int, Int) -> Either Text ())
    -> IO (Either Text ComputerObservation)
invokeComputerSessionTransaction host session encoding actions validateDisplay =
    modifyMVar session.computerSessionObservation \previous -> do
        result <-
            if null actions
                then invokeComputerTransactionWithLease
                    host Nothing encoding actions validateDisplay
                else case previous of
                    Nothing -> pure (Left
                        "Take a fresh computer screenshot before sending input actions.")
                    Just display ->
                        invokeComputerTransactionWithLease
                            host
                            (Just display)
                            encoding
                            actions
                            validateDisplay
        case result of
            Left err -> pure (previous, Left err)
            Right (observation, successor) ->
                pure (Just successor, Right observation)

invokeComputerTransactionWithLease
    :: ComputerHost
    -> Maybe NativeComputerDisplay
    -> ScreenshotEncoding
    -> [ComputerAction]
    -> ((Int, Int) -> Either Text ())
    -> IO
        (Either
            Text
            (ComputerObservation, NativeComputerDisplay))
invokeComputerTransactionWithLease
        host priorDisplay encoding actions validateDisplay =
    case encodeComputerActions actions of
        Left err -> pure (Left err)
        Right batch ->
            tryAny
                (withMVar host.computerRegistration \case
                    Nothing ->
                        pure (Left "Native computer control is not active.")
                    Just registration ->
                        maybe
                            (invokeComputerDisplay registration)
                            (pure . Right)
                            priorDisplay >>= \case
                            Left err -> pure (Left err)
                            Right display
                                | Left err <- validateDisplay
                                    ( fromIntegral display.nativeDisplayWidth
                                    , fromIntegral display.nativeDisplayHeight
                                    ) ->
                                    pure (Left err)
                                | otherwise ->
                                    invokeComputerRunAndObserve
                                        registration
                                        display
                                        encoding
                                        batch)
                >>= \case
                    Left exception ->
                        pure (Left (Text.pack (show exception)))
                    Right result -> pure result

invokeComputerDisplay
    :: ComputerRegistration
    -> IO (Either Text NativeComputerDisplay)
invokeComputerDisplay registration =
    invokeComputer computerErrorCapacity registration query >>= \case
        Left err -> pure (Left err)
        Right (bytes, token, width, height, imageFormat)
            | not (BS.null bytes)
                || token == 0
                || width <= 0
                || height <= 0
                || imageFormat /= 0 ->
                pure (Left
                    "The native computer host returned an invalid display lease.")
            | otherwise ->
                pure (Right NativeComputerDisplay
                    { nativeDisplayToken = token
                    , nativeDisplayWidth = width
                    , nativeDisplayHeight = height
                    })
  where
    query registration' output capacity outputLength outputToken
            width height imageFormat =
        invokeComputerCallback
            registration'.computerCallback
            registration'.computerContext
            1
            1
            0
            0
            0
            nullPtr
            0
            nullPtr
            0
            nullPtr
            0
            0
            output
            capacity
            outputLength
            outputToken
            width
            height
            imageFormat

invokeComputerRunAndObserve
    :: ComputerRegistration
    -> NativeComputerDisplay
    -> ScreenshotEncoding
    -> ComputerBatch
    -> IO
        (Either
            Text
            (ComputerObservation, NativeComputerDisplay))
invokeComputerRunAndObserve registration display encoding batch =
    withArray batch.computerBatchActions \actionPointer ->
        withArray batch.computerBatchPoints \pointPointer ->
            BS.useAsCStringLen batch.computerBatchText
                \(textPointer, textLength) ->
                    invokeComputer computerOutputCapacity registration
                        (run
                            actionPointer
                            pointPointer
                            (castPtr textPointer)
                            textLength)
                        >>= \case
                            Left err -> pure (Left err)
                            Right (bytes, token, width, height, imageFormat)
                                | token == 0
                                    || token == display.nativeDisplayToken
                                    || width /= display.nativeDisplayWidth
                                    || height /= display.nativeDisplayHeight ->
                                    pure (Left
                                        "The native computer host returned an invalid successor display lease.")
                                | imageFormat /= requestedFormat ->
                                    pure (Left
                                        "The native computer host returned a screenshot in the wrong format.")
                                | BS.null bytes ->
                                    pure (Left
                                        "The native computer host returned an empty screenshot.")
                                | otherwise ->
                                    case imageMime imageFormat bytes of
                                        Left err -> pure (Left err)
                                        Right mime ->
                                            pure (Right
                                                ( ComputerObservation
                                                    (ImageAttachment mime bytes)
                                                    Nothing
                                                , NativeComputerDisplay
                                                    { nativeDisplayToken =
                                                        token
                                                    , nativeDisplayWidth =
                                                        width
                                                    , nativeDisplayHeight =
                                                        height
                                                    }
                                                ))
  where
    requestedFormat = case encoding of
        ScreenshotPng -> 1
        ScreenshotJpeg -> 2
    run actionPointer pointPointer textPointer textLength
            registration' output capacity outputLength outputToken
            width height imageFormat =
        invokeComputerCallback
            registration'.computerCallback
            registration'.computerContext
            1
            2
            display.nativeDisplayToken
            display.nativeDisplayWidth
            display.nativeDisplayHeight
            actionPointer
            (fromIntegral (length batch.computerBatchActions))
            pointPointer
            (fromIntegral (length batch.computerBatchPoints))
            textPointer
            (fromIntegral textLength)
            requestedFormat
            output
            capacity
            outputLength
            outputToken
            width
            height
            imageFormat

type ComputerInvocation =
    ComputerRegistration
    -> Ptr Word8
    -> CSize
    -> Ptr CSize
    -> Ptr Word64
    -> Ptr CInt
    -> Ptr CInt
    -> Ptr CInt
    -> IO CInt

invokeComputer
    :: Int
    -> ComputerRegistration
    -> ComputerInvocation
    -> IO (Either Text (BS.ByteString, Word64, CInt, CInt, CInt))
invokeComputer capacity registration invocation =
    bracket (mallocBytes capacity) free \output -> do
        fillBytes output 0 capacity
        allocaResult \outputLength outputToken width height imageFormat -> do
            status <- invocation
                registration
                output
                (fromIntegral capacity)
                outputLength
                outputToken
                width
                height
                imageFormat
            CSize length <- peek outputLength
            observedToken <- peek outputToken
            observedWidth <- peek width
            observedHeight <- peek height
            observedFormat <- peek imageFormat
            if length > fromIntegral capacity
                then pure (Left
                    "The native computer host reported output beyond its buffer.")
                else do
                    bytes <- BS.packCStringLen
                        (castPtr output, fromIntegral length)
                    if status == 0
                        then pure (Right
                            ( bytes
                            , observedToken
                            , observedWidth
                            , observedHeight
                            , observedFormat
                            ))
                        else pure (Left
                            (computerFailureMessage status bytes))

allocaResult
    :: ( Ptr CSize
        -> Ptr Word64
        -> Ptr CInt
        -> Ptr CInt
        -> Ptr CInt
        -> IO value
       )
    -> IO value
allocaResult action =
    allocaBytes (sizeOf (undefined :: CSize)) \outputLength ->
        allocaBytes (sizeOf (undefined :: Word64)) \outputToken ->
            allocaBytes (sizeOf (undefined :: CInt)) \width ->
                allocaBytes (sizeOf (undefined :: CInt)) \height ->
                    allocaBytes (sizeOf (undefined :: CInt))
                        \imageFormat -> do
                            poke outputLength 0
                            poke outputToken 0
                            poke width 0
                            poke height 0
                            poke imageFormat 0
                            action
                                outputLength
                                outputToken
                                width
                                height
                                imageFormat

imageMime :: CInt -> BS.ByteString -> Either Text Text
imageMime imageFormat bytes = case imageFormat of
    1
        | BS.take 8 bytes == BS.pack [137, 80, 78, 71, 13, 10, 26, 10] ->
            Right "image/png"
        | otherwise ->
            Left "The native computer host returned malformed PNG data."
    2
        | BS.take 2 bytes == BS.pack [255, 216] ->
            Right "image/jpeg"
        | otherwise ->
            Left "The native computer host returned malformed JPEG data."
    _ -> Left "The native computer host returned an unsupported image format."

computerFailureMessage :: CInt -> BS.ByteString -> Text
computerFailureMessage status bytes =
    case TextEncoding.decodeUtf8' bytes of
        Right message | not (Text.null message) -> message
        _ -> computerStatusMessage status

computerStatusMessage :: CInt -> Text
computerStatusMessage = \case
    1 -> "The native computer host rejected an invalid argument."
    2 -> "Native computer control is unavailable."
    3 -> "The native computer operation timed out."
    4 -> "Accessibility or Screen Recording permission is required."
    5 -> "The native computer host does not support this operation."
    6 -> "The native computer host failed internally."
    7 -> "The native computer screenshot exceeded the 16 MiB output limit."
    8 -> "Computer use is unavailable while the macOS session is locked."
    9 ->
        "The main display changed during computer use; take a fresh screenshot before continuing."
    status ->
        "The native computer operation failed (status "
            <> Text.pack (show status)
            <> ")."

computerActionStructSize :: Int
computerActionStructSize = 64

computerErrorCapacity :: Int
computerErrorCapacity = 64 * 1024

computerOutputCapacity :: Int
computerOutputCapacity = 16 * 1024 * 1024
