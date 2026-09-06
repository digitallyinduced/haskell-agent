{-# LANGUAGE ForeignFunctionInterface #-}

-- | The native-only, accessibility-first macOS computer service.
--
-- The standalone CLI computer backend intentionally remains in
-- "Agent.CLI.ComputerUse". Native turns use this module's semantic JSON
-- protocol instead: models name stable AX elements and never provide pixels.
module Agent.CLI.MacOS.ComputerBridge
    ( ComputerCallback
    , ComputerHost
    , ComputerRegistration(..)
    , ComputerSession
    , NativeComputerResult(..)
    , closeComputerSession
    , computerAccessibilityCapacity
    , computerErrorCapacity
    , computerImageCapacity
    , computerResultCapacity
    , computerStatusMessage
    , computerToolSessionWhenEnabled
    , invokeComputerSessionRequest
    , newComputerHost
    , newComputerSession
    , replaceComputerRegistration
    , resetComputerSessionAccessibility
    ) where

import Agent.CLI.ComputerUse.Accessibility
    ( AccessibilityDeltaState
    , AccessibilityObservation
    , advanceAccessibilityObservation
    , decodeAccessibilitySnapshot
    , initialAccessibilityDeltaState
    , unavailableAccessibilityObservation
    )
import Agent.ToolDispatch
    ( ToolHandlerResult(..)
    , ToolResultImage(..)
    , streamingRichTextTool
    )
import Agent.Tools.Types
    ( AppTool(..)
    , ApprovalRule(..)
    , ToolAsyncCapability(..)
    , ToolExecutionPolicy(..)
    , ToolSchema(..)
    )
import Codec.Picture
    ( DynamicImage
    , dynamicMap
    , imageHeight
    , imageWidth
    , pixelAt
    )
import Codec.Picture.Jpg (decodeJpeg)
import Codec.Picture.Png (decodePng)
import qualified Control.Concurrent.MVar as MVar
import Control.Exception (evaluate)
import qualified Control.Exception.Safe as Exception
import Control.Exception.Safe (finally, mask_, onException, tryAny)
import Control.Monad (guard, unless, when)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Aeson.Types as AesonTypes
import Data.Bits ((.&.), complement, shiftR, xor)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import Data.Word (Word8, Word32, Word64)
import Foreign
    ( FunPtr
    , Ptr
    , castPtr
    , nullPtr
    , peek
    , poke
    )
import Foreign.C.Types (CInt(..), CSize(..))
import Foreign.Marshal.Alloc (alloca, free, mallocBytes)
import Foreign.Marshal.Utils (fillBytes)

-- | ABI v3 callback. Every buffer is callback-scoped. OPEN returns a nonzero
-- opaque token; all subsequent operations carry it. Result JSON, AX JSON,
-- image bytes, and errors have independent buffers so an oversized optional
-- channel cannot destroy another successful channel.
type ComputerCallback =
    Ptr ()
    -> Word32
    -> CInt
    -> Word64
    -> Ptr Word8
    -> CSize
    -> Ptr Word8
    -> CSize
    -> Ptr CSize
    -> Ptr Word8
    -> CSize
    -> Ptr CSize
    -> Ptr Word8
    -> CSize
    -> Ptr CSize
    -> Ptr Word8
    -> CSize
    -> Ptr CSize
    -> Ptr Word64
    -> Ptr CInt
    -> IO CInt

foreign import ccall safe "dynamic"
    invokeComputerCallback :: FunPtr ComputerCallback -> ComputerCallback

data ComputerRegistration = ComputerRegistration
    { computerCallback :: !(FunPtr ComputerCallback)
    , computerContext :: !(Ptr ())
    }

data RegistrationGeneration = RegistrationGeneration
    { generationRegistration :: !ComputerRegistration
    , generationSessions :: !Int
    }

data ComputerHostState = ComputerHostState
    { hostCurrentGeneration :: !(Maybe Word64)
    , hostGenerations :: !(Map.Map Word64 RegistrationGeneration)
    , hostNextGeneration :: !Word64
    }

newtype ComputerHost = ComputerHost
    { computerHostState :: IORef ComputerHostState
    }

data ComputerSession = ComputerSession
    { computerSessionHost :: !ComputerHost
    , computerSessionGeneration :: !Word64
    , computerSessionRegistration :: !ComputerRegistration
    , computerSessionState :: !(MVar.MVar ComputerSessionState)
    }

data ComputerSessionState
    = ComputerSessionOpen !Word64 !AccessibilityDeltaState
    | ComputerSessionClosed

data NativeComputerResult = NativeComputerResult
    { nativeComputerResultValue :: !Aeson.Value
    , nativeComputerAccessibility :: !(Maybe AccessibilityObservation)
    , nativeComputerImage :: !(Maybe ToolResultImage)
    } deriving (Eq, Show)

newComputerHost :: IO ComputerHost
newComputerHost =
    ComputerHost <$> newIORef ComputerHostState
        { hostCurrentGeneration = Nothing
        , hostGenerations = Map.empty
        , hostNextGeneration = 1
        }

-- | Publish a new callback immediately for new sessions. Existing sessions
-- retain their generation and keep using it until their CLOSE completes.
replaceComputerRegistration
    :: ComputerHost
    -> Maybe ComputerRegistration
    -> IO ()
replaceComputerRegistration host replacement =
    atomicModifyIORef' host.computerHostState \state ->
        let withoutUnusedOld = case state.hostCurrentGeneration of
                Nothing -> state.hostGenerations
                Just oldGeneration ->
                    Map.update
                        (\generation ->
                            if generation.generationSessions == 0
                                then Nothing
                                else Just generation)
                        oldGeneration
                        state.hostGenerations
        in case replacement of
            Nothing ->
                ( state
                    { hostCurrentGeneration = Nothing
                    , hostGenerations = withoutUnusedOld
                    }
                , ()
                )
            Just registration ->
                let generation = state.hostNextGeneration
                in
                    ( state
                        { hostCurrentGeneration = Just generation
                        , hostGenerations = Map.insert generation
                            (RegistrationGeneration registration 0)
                            withoutUnusedOld
                        , hostNextGeneration = generation + 1
                        }
                    , ()
                    )

newComputerSession :: ComputerHost -> IO (Either Text ComputerSession)
newComputerSession host =
    newComputerSessionWhenEnabled host >>= \case
        Left err -> pure (Left err)
        Right Nothing -> pure (Left "Native computer control is not active.")
        Right (Just session) -> pure (Right session)

newComputerSessionWhenEnabled
    :: ComputerHost
    -> IO (Either Text (Maybe ComputerSession))
newComputerSessionWhenEnabled host = mask_ do
    leased <- acquireCurrentGeneration host
    case leased of
        Nothing -> pure (Right Nothing)
        Just (generation, registration) -> do
            opened <-
                invokeComputerRaw registration operationOpen 0 False BS.empty
                    `onException` releaseGeneration host generation
            case opened of
                Left err -> do
                    releaseGeneration host generation
                    pure (Left err)
                Right response
                    | response.rawSessionToken == 0 -> do
                        releaseGeneration host generation
                        pure (Left
                            "The native computer host returned an invalid session token.")
                    | not (rawChannelsEmpty response) -> do
                        closeInvalidOpen registration response.rawSessionToken
                        releaseGeneration host generation
                        pure (Left
                            "The native computer host returned data while opening a session.")
                    | otherwise -> do
                        (do
                            state <- MVar.newMVar
                                (ComputerSessionOpen
                                    response.rawSessionToken
                                    initialAccessibilityDeltaState)
                            pure (Right (Just ComputerSession
                                { computerSessionHost = host
                                , computerSessionGeneration = generation
                                , computerSessionRegistration = registration
                                , computerSessionState = state
                                })))
                            `onException`
                                (closeInvalidOpen
                                    registration
                                    response.rawSessionToken
                                    `finally`
                                        releaseGeneration host generation)

closeComputerSession :: ComputerSession -> IO ()
closeComputerSession session = mask_ do
    token <- MVar.modifyMVar
        session.computerSessionState \case
            ComputerSessionClosed -> pure (ComputerSessionClosed, Nothing)
            ComputerSessionOpen sessionToken _ ->
                pure (ComputerSessionClosed, Just sessionToken)
    case token of
        Nothing -> pure ()
        Just sessionToken ->
            (do
                _ <- tryAny
                    (invokeComputerRaw
                        session.computerSessionRegistration
                        operationClose
                        sessionToken
                        False
                        BS.empty)
                pure ())
            `finally`
                releaseGeneration
                    session.computerSessionHost
                    session.computerSessionGeneration

resetComputerSessionAccessibility :: ComputerSession -> IO ()
resetComputerSessionAccessibility session =
    MVar.modifyMVar_
        session.computerSessionState \case
            ComputerSessionClosed -> pure ComputerSessionClosed
            ComputerSessionOpen token _ ->
                pure (ComputerSessionOpen token initialAccessibilityDeltaState)

computerToolSessionWhenEnabled
    :: ComputerHost
    -> IO (Either Text (Maybe (AppTool, IO (), IO ())))
computerToolSessionWhenEnabled host =
    newComputerSessionWhenEnabled host >>= \case
        Left err -> pure (Left err)
        Right Nothing -> pure (Right Nothing)
        Right (Just session) ->
            pure (Right (Just
                ( computerTool session
                , resetComputerSessionAccessibility session
                , closeComputerSession session
                )))

computerTool :: ComputerSession -> AppTool
computerTool session = AppTool
    { appToolName = "computer"
    , appToolDescription =
        "Inspect and control macOS through the accessibility tree. "
            <> "Use stable target_id and element_id values; screenshots are "
            <> "optional and returned only when include_screenshot is true."
    , appToolSchema = HostedComputerFunctionSchema computerToolParameters
    , appToolHandler = streamingRichTextTool "computer" \_emit arguments ->
        normalizeRequest arguments >>= \case
            Left err -> pure (Left err)
            Right request ->
                invokeComputerSessionRequest session request >>= \case
                    Left err -> pure (Left err)
                    Right result ->
                        pure (Right ToolHandlerResult
                            { resultText = encodeJson result.nativeComputerResultValue
                            , resultImages =
                                maybe [] pure result.nativeComputerImage
                            })
    , appToolApproval = AlwaysPrompt
    , appToolExecution = TurnSequential
    , appToolResourceClaims = Nothing
    , appToolAsyncCapability = BlockingOnly
    }

invokeComputerSessionRequest
    :: ComputerSession
    -> BS.ByteString
    -> IO (Either Text NativeComputerResult)
invokeComputerSessionRequest session request =
    MVar.modifyMVar session.computerSessionState \case
        ComputerSessionClosed ->
            pure (ComputerSessionClosed, Left
                "The native computer session is closed.")
        ComputerSessionOpen token accessibilityState -> do
            case decodeOperation request of
                Left err ->
                    pure
                        ( ComputerSessionOpen token accessibilityState
                        , Left err
                        )
                Right (operation, includeScreenshot) -> do
                    attempted <- tryAny
                        (invokeComputerRaw
                            session.computerSessionRegistration
                            operation
                            token
                            includeScreenshot
                            request)
                    let failed err =
                            pure
                                ( ComputerSessionOpen token accessibilityState
                                , Left err
                                )
                    case attempted of
                        Left exception -> failed (Text.pack (show exception))
                        Right (Left err) -> failed err
                        Right (Right response) -> do
                            validated <- validateResponse
                                operation
                                includeScreenshot
                                accessibilityState
                                response
                            case validated of
                                Left err -> failed err
                                Right (result, successorAccessibility) ->
                                    pure
                                        ( ComputerSessionOpen
                                            token
                                            successorAccessibility
                                        , Right result
                                        )

data RawComputerResponse = RawComputerResponse
    { rawResult :: !BS.ByteString
    , rawAccessibility :: !BS.ByteString
    , rawImage :: !BS.ByteString
    , rawSessionToken :: !Word64
    , rawImageFormat :: !CInt
    }

validateResponse
    :: CInt
    -> Bool
    -> AccessibilityDeltaState
    -> RawComputerResponse
    -> IO (Either Text (NativeComputerResult, AccessibilityDeltaState))
validateResponse operation includeScreenshot accessibilityState response =
    case validateMetadata of
        Left err -> pure (Left err)
        Right (object, accessibility, successorAccessibility) -> do
            decodedImage <- decodeImage
                includeScreenshot
                response.rawImageFormat
                response.rawImage
            pure do
                image <- decodedImage
                let resultObject = maybe object
                        (\observation ->
                            KeyMap.insert
                                "accessibility_state"
                                (Aeson.toJSON observation)
                                object)
                        accessibility
                pure
                    ( NativeComputerResult
                        { nativeComputerResultValue = Aeson.Object resultObject
                        , nativeComputerAccessibility = accessibility
                        , nativeComputerImage = image
                        }
                    , successorAccessibility
                    )
  where
    validateMetadata = do
        when (response.rawSessionToken /= 0) $
            Left "The native computer host changed a live session token."
        value <- case Aeson.eitherDecodeStrict' response.rawResult of
            Left err -> Left
                ("The native computer host returned invalid result JSON: "
                    <> Text.pack err)
            Right decoded -> Right decoded
        object <- case value of
            Aeson.Object fields -> Right fields
            _ -> Left "The native computer host result must be a JSON object."
        when (containsDataImage value) $
            Left "The native computer host embedded screenshot data in result JSON."
        when (operation == operationList
                && not (BS.null response.rawAccessibility)) $
            Left "The native computer host returned accessibility data for list_targets."
        let (observation, newAccessibilityState) =
                decodeAccessibility response.rawAccessibility accessibilityState
            (accessibility, successorAccessibility)
                | operation == operationList =
                    (Nothing, accessibilityState)
                | otherwise =
                    (Just observation, newAccessibilityState)
        pure (object, accessibility, successorAccessibility)

decodeAccessibility
    :: BS.ByteString
    -> AccessibilityDeltaState
    -> (AccessibilityObservation, AccessibilityDeltaState)
decodeAccessibility bytes state
    | BS.null bytes =
        unavailableAccessibilityObservation
            "Native accessibility snapshot unavailable."
            state
    | otherwise =
        case decodeAccessibilitySnapshot bytes of
            Left err ->
                unavailableAccessibilityObservation
                    ( "The native computer host returned invalid accessibility JSON: "
                        <> err
                    )
                    state
            Right snapshot ->
                advanceAccessibilityObservation state snapshot

decodeImage
    :: Bool
    -> CInt
    -> BS.ByteString
    -> IO (Either Text (Maybe ToolResultImage))
decodeImage includeScreenshot imageFormat bytes
    | BS.null bytes && imageFormat == imageNone = pure (Right Nothing)
    | not includeScreenshot =
        pure (Left "The native computer host returned an unrequested screenshot.")
    | BS.null bytes =
        pure (Left "The native computer host returned an empty screenshot.")
    | otherwise = do
        decodedMime <- imageMime imageFormat bytes
        pure do
            mime <- decodedMime
            pure (Just (ToolResultImage
                ( "data:"
                    <> mime
                    <> ";base64,"
                    <> encodeBase64 bytes
                )
                Nothing))

normalizeRequest :: Text -> IO (Either Text BS.ByteString)
normalizeRequest arguments =
    pure case Aeson.eitherDecodeStrict'
            (TextEncoding.encodeUtf8 arguments) of
        Left err -> Left ("Invalid computer arguments: " <> Text.pack err)
        Right value ->
            case AesonTypes.parseEither parseRequest value of
                Left err -> Left (Text.pack err)
                Right normalized -> Right
                    (LBS.toStrict (Aeson.encode normalized))

parseRequest :: Aeson.Value -> AesonTypes.Parser Aeson.Value
parseRequest = Aeson.withObject "computer arguments" \object -> do
    requireOnly object
        ["operation", "target_id", "actions", "include_screenshot"]
    operation <- object Aeson..: "operation"
    includeScreenshot <- object Aeson..: "include_screenshot"
    case (operation :: Text) of
        "list_targets" -> do
            requireNull object "target_id"
            requireNull object "actions"
            when includeScreenshot $
                fail "list_targets cannot include a screenshot"
            pure (Aeson.object
                [ "operation" Aeson..= operation
                , "include_screenshot" Aeson..= False
                ])
        "bind" -> do
            targetId <- object Aeson..: "target_id"
            requireNull object "actions"
            pure (Aeson.object
                [ "operation" Aeson..= operation
                , "target_id" Aeson..= (targetId :: Text)
                , "include_screenshot" Aeson..= includeScreenshot
                ])
        "observe" -> do
            requireNull object "target_id"
            requireNull object "actions"
            pure (Aeson.object
                [ "operation" Aeson..= operation
                , "include_screenshot" Aeson..= includeScreenshot
                ])
        "act" -> do
            requireNull object "target_id"
            rawActions <-
                object Aeson..: "actions"
                    :: AesonTypes.Parser [Aeson.Value]
            actions <- traverse parseSemanticAction rawActions
            when (null actions) $
                fail "act requires at least one semantic action"
            when (length actions > 64) $
                fail "act accepts at most 64 semantic actions"
            pure (Aeson.object
                [ "operation" Aeson..= operation
                , "actions" Aeson..= actions
                , "include_screenshot" Aeson..= includeScreenshot
                ])
        _ -> fail "unsupported computer operation"

parseSemanticAction :: Aeson.Value -> AesonTypes.Parser Aeson.Value
parseSemanticAction = Aeson.withObject "semantic computer action" \object -> do
    requireOnly object ["type", "element_id", "action", "value", "text"]
    actionType <- object Aeson..: "type"
    elementId <- object Aeson..: "element_id"
    case (actionType :: Text) of
        "perform" -> do
            action <- object Aeson..: "action"
            requireNull object "value"
            requireNull object "text"
            pure (Aeson.object
                [ "type" Aeson..= actionType
                , "element_id" Aeson..= (elementId :: Text)
                , "action" Aeson..= (action :: Text)
                ])
        "set_value" -> do
            requireNull object "action"
            requireNull object "text"
            value <- object Aeson..: "value"
            case (value :: Aeson.Value) of
                Aeson.String _ -> pure ()
                Aeson.Number _ -> pure ()
                Aeson.Bool _ -> pure ()
                _ -> fail "set_value.value must be a string, number, or boolean"
            pure (Aeson.object
                [ "type" Aeson..= actionType
                , "element_id" Aeson..= (elementId :: Text)
                , "value" Aeson..= value
                ])
        "replace_selected_text" -> do
            requireNull object "action"
            requireNull object "value"
            text <- object Aeson..: "text"
            pure (Aeson.object
                [ "type" Aeson..= actionType
                , "element_id" Aeson..= (elementId :: Text)
                , "text" Aeson..= (text :: Text)
                ])
        _ -> fail "unsupported semantic computer action"

requireOnly :: Aeson.Object -> [Text] -> AesonTypes.Parser ()
requireOnly object allowed =
    unless (null unexpected) $
        fail ("unexpected computer argument fields: "
            <> Text.unpack (Text.intercalate ", " unexpected))
  where
    unexpected =
        filter (`notElem` allowed) (map Key.toText (KeyMap.keys object))

requireNull :: Aeson.Object -> Text -> AesonTypes.Parser ()
requireNull object fieldName =
    case KeyMap.lookup (Key.fromText fieldName) object of
        Just Aeson.Null -> pure ()
        Just _ -> fail (Text.unpack fieldName <> " must be null for this operation")
        Nothing -> fail ("missing required field: " <> Text.unpack fieldName)

decodeOperation :: BS.ByteString -> Either Text (CInt, Bool)
decodeOperation bytes =
    case Aeson.eitherDecodeStrict' bytes of
        Left err -> Left ("Invalid normalized computer request: " <> Text.pack err)
        Right value ->
            case AesonTypes.parseEither parser value of
                Left err -> Left (Text.pack err)
                Right result -> Right result
  where
    parser = Aeson.withObject "computer request" \object -> do
        operation <- object Aeson..: "operation"
        includeScreenshot <- object Aeson..: "include_screenshot"
        command <- case (operation :: Text) of
            "list_targets" -> pure operationList
            "bind" -> pure operationBind
            "observe" -> pure operationObserveOrAct
            "act" -> pure operationObserveOrAct
            _ -> fail "unsupported computer operation"
        pure (command, includeScreenshot)

acquireCurrentGeneration
    :: ComputerHost
    -> IO (Maybe (Word64, ComputerRegistration))
acquireCurrentGeneration host =
    atomicModifyIORef' host.computerHostState \state ->
        case state.hostCurrentGeneration of
            Nothing -> (state, Nothing)
            Just generation ->
                case Map.lookup generation state.hostGenerations of
                    Nothing -> (state, Nothing)
                    Just entry ->
                        ( state
                            { hostGenerations = Map.insert generation
                                entry
                                    { generationSessions =
                                        entry.generationSessions + 1
                                    }
                                state.hostGenerations
                            }
                        , Just (generation, entry.generationRegistration)
                        )

releaseGeneration :: ComputerHost -> Word64 -> IO ()
releaseGeneration host generation =
    atomicModifyIORef' host.computerHostState \state ->
        let updated = Map.update
                (\entry ->
                    let sessions = max 0 (entry.generationSessions - 1)
                    in if sessions == 0
                            && state.hostCurrentGeneration /= Just generation
                        then Nothing
                        else Just entry { generationSessions = sessions })
                generation
                state.hostGenerations
        in (state { hostGenerations = updated }, ())

invokeComputerRaw
    :: ComputerRegistration
    -> CInt
    -> Word64
    -> Bool
    -> BS.ByteString
    -> IO (Either Text RawComputerResponse)
invokeComputerRaw registration operation sessionToken includeScreenshot request =
    if BS.length request > computerRequestCapacity
        then pure (Left "The native computer request exceeds its bounded buffer.")
        else withOutput computerResultCapacity \result resultCapacity resultLength ->
      withOutput computerAccessibilityCapacity
        \accessibility accessibilityCapacity accessibilityLength ->
          withOptionalOutput includeScreenshot computerImageCapacity
            \image imageCapacity imageLength ->
            withOutput computerErrorCapacity \err errorCapacity errorLength ->
              alloca \outputSessionToken ->
                alloca \outputImageFormat -> do
                    poke outputSessionToken 0
                    poke outputImageFormat imageNone
                    status <- BS.useAsCStringLen request
                        \(requestPointer, requestLength) ->
                            invokeComputerCallback registration.computerCallback
                                registration.computerContext
                                computerAbiVersion
                                operation
                                sessionToken
                                (if requestLength == 0
                                    then nullPtr
                                    else castPtr requestPointer)
                                (fromIntegral requestLength)
                                result resultCapacity resultLength
                                accessibility accessibilityCapacity
                                accessibilityLength
                                image imageCapacity imageLength
                                err errorCapacity errorLength
                                outputSessionToken
                                outputImageFormat
                    resultBytes <- readOutput
                        "result" computerResultCapacity result resultLength
                    accessibilityBytes <- readOutput
                        "accessibility"
                        computerAccessibilityCapacity
                        accessibility
                        accessibilityLength
                    imageBytes <- readOutput
                        "image" (fromIntegral imageCapacity) image imageLength
                    errorBytes <- readOutput
                        "error" computerErrorCapacity err errorLength
                    observedToken <- peek outputSessionToken
                    observedFormat <- peek outputImageFormat
                    case sequence
                            [resultBytes, accessibilityBytes, imageBytes, errorBytes] of
                        Left boundsError -> pure (Left boundsError)
                        Right [resultValue, accessibilityValue, imageValue, errorValue]
                            | status /= 0 ->
                                pure (Left
                                    (computerFailureMessage status errorValue))
                            | not (BS.null errorValue) ->
                                pure (Left
                                    "The native computer host returned an error on success.")
                            | otherwise ->
                                pure (Right RawComputerResponse
                                    { rawResult = resultValue
                                    , rawAccessibility = accessibilityValue
                                    , rawImage = imageValue
                                    , rawSessionToken = observedToken
                                    , rawImageFormat = observedFormat
                                    })
                        Right _ -> pure (Left "Internal computer buffer mismatch.")

withOutput
    :: Int
    -> (Ptr Word8 -> CSize -> Ptr CSize -> IO value)
    -> IO value
withOutput capacity action =
    Exception.bracket
        (mallocBytes capacity)
        free
        \buffer -> do
            fillBytes buffer 0 capacity
            alloca \lengthPointer -> do
                poke lengthPointer 0
                action
                    buffer
                    (fromIntegral capacity)
                    lengthPointer

withOptionalOutput
    :: Bool
    -> Int
    -> (Ptr Word8 -> CSize -> Ptr CSize -> IO value)
    -> IO value
withOptionalOutput enabled capacity action
    | enabled = withOutput capacity action
    | otherwise = alloca \lengthPointer -> do
        poke lengthPointer 0
        action nullPtr 0 lengthPointer

readOutput
    :: Text
    -> Int
    -> Ptr Word8
    -> Ptr CSize
    -> IO (Either Text BS.ByteString)
readOutput label capacity buffer lengthPointer = do
    CSize lengthValue <- peek lengthPointer
    if lengthValue > fromIntegral capacity
        then pure (Left
            ("The native computer host reported " <> label
                <> " output beyond its buffer."))
        else if lengthValue == 0
            then pure (Right BS.empty)
            else if buffer == nullPtr
                then pure (Left
                    ("The native computer host reported " <> label
                        <> " output without a buffer."))
                else Right <$> BS.packCStringLen
                    (castPtr buffer, fromIntegral lengthValue)

rawChannelsEmpty :: RawComputerResponse -> Bool
rawChannelsEmpty response =
    BS.null response.rawResult
        && BS.null response.rawAccessibility
        && BS.null response.rawImage
        && response.rawImageFormat == imageNone

closeInvalidOpen :: ComputerRegistration -> Word64 -> IO ()
closeInvalidOpen registration token =
    when (token /= 0) do
        _ <- tryAny
            (invokeComputerRaw registration operationClose token False BS.empty)
        pure ()

imageMime :: CInt -> BS.ByteString -> IO (Either Text Text)
imageMime imageFormat bytes = case imageFormat of
    1 -> validateDecodedImage
        "image/png"
        "The native computer host returned malformed PNG data."
        (validPng bytes)
    2 -> validateDecodedImage
        "image/jpeg"
        "The native computer host returned malformed JPEG data."
        (validJpeg bytes)
    _ -> pure
        (Left "The native computer host returned an unsupported image format.")

validateDecodedImage :: Text -> Text -> Bool -> IO (Either Text Text)
validateDecodedImage mime malformed valid = do
    attempted <- tryAny (evaluate valid)
    pure case attempted of
        Right True -> Right mime
        Right False -> Left malformed
        Left _ -> Left malformed

containsDataImage :: Aeson.Value -> Bool
containsDataImage = \case
    Aeson.Object object -> any containsDataImage object
    Aeson.Array values -> any containsDataImage values
    Aeson.String value ->
        "data:image/" `Text.isPrefixOf` Text.toLower value
    _ -> False

validPng :: BS.ByteString -> Bool
validPng bytes =
    case pngEnvelope bytes of
        Nothing -> False
        Just dimensions ->
            decodedImageMatches dimensions (decodePng bytes)

pngEnvelope :: BS.ByteString -> Maybe (Int, Int)
pngEnvelope bytes = do
    guard (BS.length bytes >= 45)
    guard (BS.take 8 bytes == pngSignature)
    validChunks True False Nothing (BS.drop 8 bytes)
  where
    pngSignature = BS.pack [137, 80, 78, 71, 13, 10, 26, 10]
    validChunks firstChunk sawImageData dimensions remaining = do
        guard (BS.length remaining >= 12)
        guard (chunkLength <= BS.length remaining - 12)
        guard (chunkChecksum == crc32 checksumInput)
        if firstChunk
            then do
                guard (chunkType == "IHDR")
                guard (chunkLength == 13)
                headerDimensions <- validHeader chunkData
                validChunks False False (Just headerDimensions) rest
            else case chunkType of
                "IHDR" -> Nothing
                "IDAT" ->
                    validChunks False True dimensions rest
                "IEND" -> do
                    guard (chunkLength == 0)
                    guard sawImageData
                    guard (BS.null rest)
                    dimensions
                _ ->
                    validChunks False sawImageData dimensions rest
      where
        chunkLength = word32At remaining 0
        chunkType = BS.take 4 (BS.drop 4 remaining)
        chunkData = BS.take chunkLength (BS.drop 8 remaining)
        checksumInput =
            BS.take (4 + chunkLength) (BS.drop 4 remaining)
        chunkChecksum =
            fromIntegral (word32At remaining (8 + chunkLength))
        rest = BS.drop (12 + chunkLength) remaining
    validHeader header = do
        guard (BS.length header == 13)
        let width = word32At header 0
            height = word32At header 4
        guard (dimensionsSafeToDecode (width, height))
        guard (BS.index header 10 == 0)
        guard (BS.index header 11 == 0)
        guard (BS.index header 12 <= 1)
        pure (width, height)

validJpeg :: BS.ByteString -> Bool
validJpeg bytes =
    case jpegDimensions bytes of
        Nothing -> False
        Just dimensions ->
            decodedImageMatches dimensions (decodeJpeg bytes)

jpegDimensions :: BS.ByteString -> Maybe (Int, Int)
jpegDimensions bytes = do
    guard (BS.length bytes >= 14)
    guard (BS.take 2 bytes == BS.pack [255, 216])
    guard (BS.drop (BS.length bytes - 2) bytes == BS.pack [255, 217])
    validSegments Nothing (BS.drop 2 bytes)
  where
    validSegments dimensions remaining =
        case nextMarker remaining of
            Nothing -> Nothing
            Just (marker, afterMarker)
                | marker == 0xd9 -> Nothing
                | standaloneMarker marker ->
                    validSegments dimensions afterMarker
                | BS.length afterMarker < 2 -> Nothing
                | segmentLength < 2
                    || segmentLength > BS.length afterMarker -> Nothing
                | marker == 0xda -> dimensions
                | startOfFrame marker -> do
                    guard (segmentLength >= 8)
                    let height = word16At afterMarker 3
                        width = word16At afterMarker 5
                    guard (dimensionsSafeToDecode (width, height))
                    validSegments
                        (Just (width, height))
                        (BS.drop segmentLength afterMarker)
                | otherwise ->
                    validSegments dimensions
                        (BS.drop segmentLength afterMarker)
              where
                segmentLength = word16At afterMarker 0
    nextMarker remaining =
        case BS.elemIndex 255 remaining of
            Nothing -> Nothing
            Just markerStart ->
                let markerBytes = BS.drop markerStart remaining
                    afterFill = BS.dropWhile (== 255) markerBytes
                in case BS.uncons afterFill of
                    Nothing -> Nothing
                    Just (0, rest) -> nextMarker rest
                    Just (marker, rest) -> Just (marker, rest)
    standaloneMarker marker =
        marker == 0x01
            || marker == 0xd8
            || (marker >= 0xd0 && marker <= 0xd7)
    startOfFrame marker =
        marker >= 0xc0
            && marker <= 0xcf
            && marker `notElem` [0xc4, 0xc8, 0xcc]

decodedImageMatches
    :: (Int, Int)
    -> Either String DynamicImage
    -> Bool
decodedImageMatches expectedDimensions = \case
    Left _ -> False
    Right image ->
        dynamicMap
            (\raster ->
                let width = imageWidth raster
                    height = imageHeight raster
                in
                    (width, height) == expectedDimensions
                        && (pixelAt raster (width - 1) (height - 1)
                            `seq` True))
            image

dimensionsSafeToDecode :: (Int, Int) -> Bool
dimensionsSafeToDecode (width, height) =
    width > 0
        && height > 0
        && width <= maximumDecodedImageSide
        && height <= maximumDecodedImageSide
        && toInteger width * toInteger height <= maximumDecodedImagePixels

maximumDecodedImageSide :: Int
maximumDecodedImageSide = 8192

maximumDecodedImagePixels :: Integer
maximumDecodedImagePixels = 25000000

crc32 :: BS.ByteString -> Word32
crc32 = complement . BS.foldl' update maxBound
  where
    update :: Word32 -> Word8 -> Word32
    update checksum byte =
        advance 8 (checksum `xor` fromIntegral byte)

    advance :: Int -> Word32 -> Word32
    advance 0 value = value
    advance count value =
        advance (count - 1)
            (if value .&. 1 == 1
                then (value `shiftR` 1) `xor` 0xedb88320
                else value `shiftR` 1)

word16At :: BS.ByteString -> Int -> Int
word16At bytes offset =
    fromIntegral (BS.index bytes offset) * 256
        + fromIntegral (BS.index bytes (offset + 1))

word32At :: BS.ByteString -> Int -> Int
word32At bytes offset =
    fromIntegral (BS.index bytes offset) * 16777216
        + fromIntegral (BS.index bytes (offset + 1)) * 65536
        + fromIntegral (BS.index bytes (offset + 2)) * 256
        + fromIntegral (BS.index bytes (offset + 3))

encodeBase64 :: BS.ByteString -> Text
encodeBase64 = Text.pack . go . BS.unpack
  where
    alphabet =
        "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
    at value = alphabet !! value
    go = \case
        [] -> []
        [a] ->
            [ at (fromIntegral a `div` 4)
            , at ((fromIntegral a `mod` 4) * 16)
            , '='
            , '='
            ]
        [a, b] ->
            [ at (fromIntegral a `div` 4)
            , at (((fromIntegral a `mod` 4) * 16) + (fromIntegral b `div` 16))
            , at ((fromIntegral b `mod` 16) * 4)
            , '='
            ]
        a : b : c : rest ->
            at (fromIntegral a `div` 4)
                : at (((fromIntegral a `mod` 4) * 16)
                    + (fromIntegral b `div` 16))
                : at (((fromIntegral b `mod` 16) * 4)
                    + (fromIntegral c `div` 64))
                : at (fromIntegral c `mod` 64)
                : go rest

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
    4 -> "Accessibility permission is required."
    5 -> "The native computer host does not support this operation."
    6 -> "The native computer host failed internally."
    7 -> "A native computer output exceeded its bounded buffer."
    8 -> "Computer use is unavailable while the macOS session is locked."
    9 -> "The selected application or window is stale."
    status ->
        "The native computer operation failed (status "
            <> Text.pack (show status)
            <> ")."

encodeJson :: Aeson.Value -> Text
encodeJson = TextEncoding.decodeUtf8 . LBS.toStrict . Aeson.encode

computerAbiVersion :: Word32
computerAbiVersion = 3

computerToolParameters :: Aeson.Value
computerToolParameters = strictObject
    [ ("operation", Aeson.object
        [ "type" Aeson..= ("string" :: Text)
        , "enum" Aeson..=
            (["list_targets", "bind", "observe", "act"] :: [Text])
        ])
    , ("target_id", nullableStringParameter)
    , ("actions", Aeson.object
        [ "type" Aeson..= (["array", "null"] :: [Text])
        , "minItems" Aeson..= (1 :: Int)
        , "maxItems" Aeson..= (64 :: Int)
        , "items" Aeson..= semanticActionParameters
        ])
    , ("include_screenshot", screenshotParameter)
    ]
    ["operation", "target_id", "actions", "include_screenshot"]

semanticActionParameters :: Aeson.Value
semanticActionParameters = strictObject
    [ ("type", Aeson.object
        [ "type" Aeson..= ("string" :: Text)
        , "enum" Aeson..=
            (["perform", "set_value", "replace_selected_text"] :: [Text])
        ])
    , ("element_id", stringParameter)
    , ("action", nullableStringParameter)
    , ("value", Aeson.object
        [ "type" Aeson..=
            (["string", "number", "boolean", "null"] :: [Text])
        ])
    , ("text", nullableStringParameter)
    ]
    ["type", "element_id", "action", "value", "text"]

strictObject :: [(Text, Aeson.Value)] -> [Text] -> Aeson.Value
strictObject properties requiredFields = Aeson.object
    [ "type" Aeson..= ("object" :: Text)
    , "additionalProperties" Aeson..= False
    , "properties" Aeson..= Aeson.object
        [ Key.fromText name Aeson..= schema | (name, schema) <- properties ]
    , "required" Aeson..= requiredFields
    ]

stringParameter :: Aeson.Value
stringParameter = scalarParameter "string"

nullableStringParameter :: Aeson.Value
nullableStringParameter = Aeson.object
    [ "type" Aeson..= (["string", "null"] :: [Text]) ]

scalarParameter :: Text -> Aeson.Value
scalarParameter parameterType = Aeson.object
    [ "type" Aeson..= parameterType ]

screenshotParameter :: Aeson.Value
screenshotParameter = Aeson.object
    [ "type" Aeson..= ("boolean" :: Text)
    , "description" Aeson..=
        ( "Set false unless visual evidence is necessary; screenshots are "
        <> "never returned implicitly."
        :: Text
        )
    ]

operationOpen, operationList, operationBind, operationObserveOrAct, operationClose
    :: CInt
operationOpen = 1
operationList = 2
operationBind = 3
operationObserveOrAct = 4
operationClose = 5

imageNone :: CInt
imageNone = 0

computerRequestCapacity, computerResultCapacity, computerAccessibilityCapacity, computerImageCapacity,
    computerErrorCapacity :: Int
computerRequestCapacity = 1024 * 1024
computerResultCapacity = 1024 * 1024
computerAccessibilityCapacity = 512 * 1024
computerImageCapacity = 16 * 1024 * 1024
computerErrorCapacity = 64 * 1024
