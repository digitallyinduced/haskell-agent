-- | OpenAI Codex image generation and editing.
--
-- This mirrors the image generation extension shipped by the upstream Codex
-- client: new images use @images/generations@, edits use @images/edits@, and
-- the result is returned as structured image content rather than a very large
-- textual data URL.
module Agent.OpenAI.ImageGeneration
    ( ImageGenerationHistory
    , newImageGenerationHistory
    , recordImageGenerationImages
    , recordImageGenerationResponseItems
    , clearImageGenerationHistory
    , imageGenerationTool
    , imageGenerationToolAt
    , imageGenerationToolName
    , imageGenerationNamespace
    , imageGenerationNamespaceDescription
    , imageGenerationDescription
    ) where

import Agent.Error (ApiError(..), ErrorType(..))
import Agent.Json (RawJson, rawJsonBytes, rawJsonDecoder)
import Agent.Loop (ImageAttachment(..))
import Agent.OpenAI.Client (defaultCodexBaseUrl)
import Agent.OpenAI.Error (classifyHttpFailure)
import Agent.OpenAI.Http (postCodexJson)
import Agent.OsPath (fromText, unsafeToFilePath)
import Agent.Provider
    ( Credential(..)
    , Provider(..)
    , TokenProvider
    , runWithTokenProvider
    )
import Agent.Responses.Types
    ( ImageGenerationCall(..)
    , ResponseItem(..)
    )
import Agent.ToolDispatch
    ( ToolCall(..)
    , ToolHandlerResult(..)
    , ToolResultImage(..)
    , typedRichToolWithCall
    )
import Agent.Tools.FileSystem (resolveForRead, resolveUnderCwd)
import Agent.Tools.ShowImage
    ( ImageDisplayHooks(..)
    , ImageDisplayRequest(..)
    , maxShowImageBytes
    , sniffImageMime
    )
import Agent.Tools.Types
    ( AppTool
    , ApprovalRule(AlwaysAllowed)
    , ToolEnv
    , ToolExecutionPolicy(TurnSequential)
    , rawJsonAppToolWithExecution
    )
import Control.Applicative ((<|>))
import Control.Exception.Safe
    ( bracketOnError
    , finally
    , tryAny
    )
import Control.Monad (unless)
import Control.Monad.Trans.Except
    ( ExceptT(..)
    , except
    , runExceptT
    , throwE
    , withExceptT
    )
import Data.Bifunctor (first)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as AesonKey
import qualified Data.Aeson.KeyMap as AesonKeyMap
import qualified Data.Aeson.Types as AesonTypes
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as Base64
import Data.Char (isAlphaNum)
import Data.IORef
    ( IORef
    , atomicModifyIORef'
    , newIORef
    , readIORef
    , writeIORef
    )
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Encoding.Error as Text
import Network.Http.Client
    ( RequestBuilder
    , Response
    , getStatusCode
    , setHeader
    )
import qualified System.IO.Streams as Streams
import System.Directory.OsPath
    ( createDirectory
    , doesDirectoryExist
    , doesFileExist
    , doesPathExist
    , getFileSize
    , pathIsSymbolicLink
    )
import System.IO (hClose)
import System.OsPath
    ( OsPath
    , isAbsolute
    , takeDirectory
    , (</>)
    )
import System.Posix.IO
    ( OpenFileFlags(..)
    , OpenMode(WriteOnly)
    , closeFd
    , defaultFileFlags
    , fdToHandle
    , openFd
    )

imageGenerationToolName :: Text
imageGenerationToolName = "imagegen"

imageGenerationNamespace :: Text
imageGenerationNamespace = "image_gen"

imageGenerationNamespaceDescription :: Text
imageGenerationNamespaceDescription = "Tools in the image_gen namespace."

imageGenerationDescription :: Text
imageGenerationDescription =
    "The `image_gen.imagegen` tool enables image generation from descriptions and editing of existing images based on specific instructions. Use it when:\n\
    \\n\
    \- The user requests an image based on a scene description, such as a diagram, portrait, comic, meme, or any other visual.\n\
    \- The user wants to modify an attached or previously generated image with specific changes, including adding or removing elements, altering colors, improving quality/resolution, or transforming the style (e.g., cartoon, oil painting).\n\
    \\n\
    \Guidelines:\n\
    \- imagegen needs a few minutes to finish. In code-mode, use the first-line @exec directive to give the initial call 120 seconds and the same yield for any waits that follow. Once it finishes, return the image with generatedImage(result).\n\
    \- Omit both `referenced_image_paths` and `num_last_images_to_include` when generating a brand new image.\n\
    \- For edits, use `referenced_image_paths` when every target image has a local file path.\n\
    \- If you have not seen a local image yet, use `view_image` to inspect it before editing.\n\
    \- Use `num_last_images_to_include` only when at least one target image has no local file path.\n\
    \- Set `num_last_images_to_include` to the smallest number of recent conversation images that includes every target image, up to 5.\n\
    \- Never provide both `referenced_image_paths` and `num_last_images_to_include`.\n\
    \- If neither mechanism can include every target image, ask the user to attach the missing images again.\n\
    \- Directly generate the image without reconfirmation or clarification unless required images must be attached again.\n\
    \- Always use this tool for image editing unless the user explicitly requests otherwise. Do not use the `python` tool for image editing unless specifically instructed.\n"

-- | Session-local chronological image history. Only the five images that can
-- be selected by the public tool contract need to be retained.
newtype ImageGenerationHistory = ImageGenerationHistory
    { recentImages :: IORef [ImageAttachment]
    }

newImageGenerationHistory :: IO ImageGenerationHistory
newImageGenerationHistory =
    ImageGenerationHistory <$> newIORef []

recordImageGenerationImages
    :: ImageGenerationHistory
    -> [ImageAttachment]
    -> IO ()
recordImageGenerationImages _ [] = pure ()
recordImageGenerationImages history images =
    atomicModifyIORef' history.recentImages \previous ->
        let combined = previous <> images
            retained = drop (max 0 (length combined - maxSelectableImages)) combined
        in (retained, ())

-- | Rehydrate recent images from a persisted provider transcript. Rich tool
-- outputs and user messages both encode images as @input_image@ content; the
-- provider-native image-generation item stores bare PNG base64 instead.
recordImageGenerationResponseItems
    :: ImageGenerationHistory
    -> [ResponseItem]
    -> IO ()
recordImageGenerationResponseItems history items =
    recordImageGenerationImages history $
        reverse
            (take maxSelectableImages
                (mapMaybe decodeImageDataUrl newestFirstUrls))
  where
    newestFirstUrls =
        concatMap (reverse . responseItemImageUrls) (reverse items)

clearImageGenerationHistory :: ImageGenerationHistory -> IO ()
clearImageGenerationHistory history =
    writeIORef history.recentImages []

imageGenerationTool
    :: TokenProvider
    -> ToolEnv
    -> ImageGenerationHistory
    -> Maybe ImageDisplayHooks
    -> AppTool
imageGenerationTool =
    imageGenerationToolAt defaultCodexBaseUrl

imageGenerationToolAt
    :: Text
    -> TokenProvider
    -> ToolEnv
    -> ImageGenerationHistory
    -> Maybe ImageDisplayHooks
    -> AppTool
imageGenerationToolAt baseUrl tokenProvider env history displayHooks =
    rawJsonAppToolWithExecution
        imageGenerationToolName
        imageGenerationDescription
        imageGenerationSchema
        AlwaysAllowed
        TurnSequential
        (typedRichToolWithCall
            imageGenerationToolName
            rawJsonDecoder
            (runImageGeneration
                baseUrl
                tokenProvider
                env
                history
                displayHooks))

data ImageGenerationArgs = ImageGenerationArgs
    { prompt :: !Text
    , referencedImagePaths :: !(Maybe [Text])
    , numLastImagesToInclude :: !(Maybe Int)
    }
    deriving (Eq, Show)

instance Aeson.FromJSON ImageGenerationArgs where
    parseJSON = Aeson.withObject "imagegen arguments" \object -> do
        let allowed =
                [ "prompt"
                , "referenced_image_paths"
                , "num_last_images_to_include"
                ]
            unknown =
                filter (`notElem` allowed)
                    (map AesonKey.toText (AesonKeyMap.keys object))
        unless (null unknown) $
            fail ("unknown field(s): " <> Text.unpack (Text.intercalate ", " unknown))
        ImageGenerationArgs
            <$> object Aeson..: "prompt"
            <*> object Aeson..:? "referenced_image_paths"
            <*> object Aeson..:? "num_last_images_to_include"

imageGenerationSchema :: Aeson.Value
imageGenerationSchema = Aeson.object
    [ "type" Aeson..= ("object" :: Text)
    , "additionalProperties" Aeson..= False
    , "required" Aeson..= ["prompt" :: Text]
    , "properties" Aeson..= Aeson.object
        [ "prompt" Aeson..= Aeson.object
            [ "type" Aeson..= ("string" :: Text)
            ]
        , "referenced_image_paths" Aeson..= Aeson.object
            [ "type" Aeson..= ["array" :: Text, "null"]
            , "items" Aeson..= Aeson.object
                [ "type" Aeson..= ("string" :: Text)
                , "description" Aeson..=
                    ("A path that is guaranteed to be absolute and normalized (though it is not guaranteed to be canonicalized or exist on the filesystem).\n\nIMPORTANT: When deserializing an `AbsolutePathBuf`, a base path must be set using [AbsolutePathBufGuard::new]. If no base path is set, the deserialization will fail unless the path being deserialized is already absolute." :: Text)
                ]
            , "maxItems" Aeson..= maxSelectableImages
            ]
        , "num_last_images_to_include" Aeson..= Aeson.object
            [ "type" Aeson..= ["integer" :: Text, "null"]
            , "format" Aeson..= ("uint" :: Text)
            , "minimum" Aeson..= (1 :: Int)
            , "maximum" Aeson..= maxSelectableImages
            ]
        ]
    ]

runImageGeneration
    :: Text
    -> TokenProvider
    -> ToolEnv
    -> ImageGenerationHistory
    -> Maybe ImageDisplayHooks
    -> ToolCall
    -> RawJson
    -> IO (Either Text ToolHandlerResult)
runImageGeneration baseUrl tokenProvider env history displayHooks call raw =
    runExceptT do
        args <- except $
            first
                (("invalid imagegen arguments: " <>) . Text.pack)
                (Aeson.eitherDecodeStrict' (rawJsonBytes raw))
        except (validateArgs args)
        references <- liftTextError (selectReferenceImages env history args)
        let (endpoint, requestBody) = imageRequest args.prompt references
        encoded <- liftApiError $
            runWithTokenProvider tokenProvider \credential ->
                if credential.provider /= OpenAIProvider
                    then pure $ Left $ ProviderError
                        InvalidRequestError
                        "Image generation requires an OpenAI credential."
                        Nothing
                    else postCodexJson
                        baseUrl
                        endpoint
                        credential.accessToken
                        credential.accountId
                        (imageRequestHeaders call.callId)
                        requestBody
                        imageResponseHandler
        liftTextError $
            finishGeneratedImage
                env
                history
                displayHooks
                call
                encoded

liftTextError :: IO (Either Text value) -> ExceptT Text IO value
liftTextError = ExceptT

liftApiError :: IO (Either ApiError value) -> ExceptT Text IO value
liftApiError = withExceptT renderApiError . ExceptT

validateArgs :: ImageGenerationArgs -> Either Text ()
validateArgs args
    | maybe False ((> maxSelectableImages) . length) args.referencedImagePaths =
        Left "`referenced_image_paths` must contain at most 5 paths"
    | maybe False (\count -> count < 1 || count > maxSelectableImages)
            args.numLastImagesToInclude =
        Left "`num_last_images_to_include` must be between 1 and 5"
    | maybe False (not . null) args.referencedImagePaths
        && maybe False (const True) args.numLastImagesToInclude =
            Left
                "provide only one of `referenced_image_paths` or `num_last_images_to_include`"
    | otherwise = Right ()

selectReferenceImages
    :: ToolEnv
    -> ImageGenerationHistory
    -> ImageGenerationArgs
    -> IO (Either Text [ImageAttachment])
selectReferenceImages env history args =
    case args.referencedImagePaths of
        Just paths | not (null paths) ->
            loadReferencedImages env paths
        _ -> case args.numLastImagesToInclude of
            Nothing -> pure (Right [])
            Just count -> do
                available <- readIORef history.recentImages
                let selected = drop (length available - count) available
                if length selected == count
                    then pure (Right selected)
                    else pure $ Left $
                        "requested the last "
                            <> Text.pack (show count)
                            <> " conversation images, but only "
                            <> Text.pack (show (length available))
                            <> " were available"

loadReferencedImages
    :: ToolEnv
    -> [Text]
    -> IO (Either Text [ImageAttachment])
loadReferencedImages env paths =
    runExceptT (traverse loadReferencedImage paths)
  where
    loadReferencedImage pathText = do
        let requested = fromText pathText
        unless (isAbsolute requested) $
            throwE $
                "`referenced_image_paths` entries must be absolute paths: "
                    <> pathText
        path <- liftTextError (resolveForRead env requested)
        liftTextError (readReferencedImage pathText path)

readReferencedImage
    :: Text
    -> OsPath
    -> IO (Either Text ImageAttachment)
readReferencedImage display path =
    tryAny readImage >>= \case
        Left err ->
            pure $ Left $
                "failed to read referenced image "
                    <> display
                    <> ": "
                    <> Text.pack (show err)
        Right result -> pure result
  where
    readImage = do
        exists <- doesFileExist path
        if not exists
            then pure (Left ("referenced image not found: " <> display))
            else do
                size <- getFileSize path
                if size > toInteger maxDecodedImageBytes
                    then pure $ Left $
                        "referenced image exceeds the 32 MiB limit: " <> display
                    else do
                        bytes <- BS.readFile (unsafeToFilePath path)
                        pure $
                            if BS.length bytes > maxDecodedImageBytes
                                then Left $
                                    "referenced image exceeds the 32 MiB limit: "
                                        <> display
                                else case sniffEditableImageMime bytes of
                                    Nothing -> Left $
                                        "unsupported referenced image format: "
                                            <> display
                                    Just mime -> Right ImageAttachment
                                        { imageMime = mime
                                        , imageBytes = bytes
                                        }

imageRequest
    :: Text
    -> [ImageAttachment]
    -> (Text, Aeson.Value)
imageRequest prompt references
    | null references =
        ( "/images/generations"
        , Aeson.object
            [ "prompt" Aeson..= prompt
            , "background" Aeson..= ("auto" :: Text)
            , "model" Aeson..= ("gpt-image-2" :: Text)
            , "quality" Aeson..= ("auto" :: Text)
            , "size" Aeson..= ("auto" :: Text)
            ]
        )
    | otherwise =
        ( "/images/edits"
        , Aeson.object
            [ "images" Aeson..= map imageUrlObject references
            , "prompt" Aeson..= prompt
            , "background" Aeson..= ("auto" :: Text)
            , "model" Aeson..= ("gpt-image-2" :: Text)
            , "quality" Aeson..= ("auto" :: Text)
            , "size" Aeson..= ("auto" :: Text)
            ]
        )
  where
    imageUrlObject image = Aeson.object
        [ "image_url" Aeson..= attachmentDataUrl image
        ]

imageRequestHeaders :: Text -> RequestBuilder () -> RequestBuilder ()
imageRequestHeaders callId request = do
    request
    setHeader "Accept" "application/json"
    setHeader "x-codex-image-turn-id" (Text.encodeUtf8 callId)
    setHeader "originator" "haskell-agent"

newtype ImageResponse = ImageResponse
    { responseImages :: [ImageResponseData]
    }

newtype ImageResponseData = ImageResponseData
    { responseBase64 :: Text
    }

instance Aeson.FromJSON ImageResponse where
    parseJSON = Aeson.withObject "image response" \object -> do
        created <- object Aeson..: "created" :: AesonTypes.Parser Integer
        images <- object Aeson..: "data"
        created `seq` pure (ImageResponse images)

instance Aeson.FromJSON ImageResponseData where
    parseJSON = Aeson.withObject "image response data" \object ->
        ImageResponseData <$> object Aeson..: "b64_json"

imageResponseHandler
    :: Response
    -> Streams.InputStream BS.ByteString
    -> IO (Either ApiError Text)
imageResponseHandler response stream =
    readBoundedResponseBody stream >>= \case
        Left err -> pure (Left err)
        Right body -> do
            let status = getStatusCode response
                bodyText = Text.decodeUtf8With Text.lenientDecode body
            if status < 200 || status >= 300
                then pure (Left (classifyHttpFailure status bodyText))
                else pure $ case Aeson.eitherDecodeStrict' body of
                    Left err -> Left $ JsonDecodeError
                        (Text.pack err)
                        (Text.take 2000 bodyText)
                    Right ImageResponse{responseImages = []} ->
                        Left $ JsonDecodeError
                            "image response contained no images"
                            (Text.take 2000 bodyText)
                    Right ImageResponse{responseImages = image : _} ->
                        Right image.responseBase64

readBoundedResponseBody
    :: Streams.InputStream BS.ByteString
    -> IO (Either ApiError BS.ByteString)
readBoundedResponseBody = go 0 []
  where
    go total reversed stream =
        Streams.read stream >>= \case
            Nothing -> pure (Right (BS.concat (reverse reversed)))
            Just chunk ->
                let nextTotal = total + BS.length chunk
                in if nextTotal > maxImageResponseBytes
                    then pure $ Left $ ProviderError
                        PayloadTooLargeError
                        "Image generation response exceeded the 48 MiB limit."
                        Nothing
                    else go nextTotal (chunk : reversed) stream

finishGeneratedImage
    :: ToolEnv
    -> ImageGenerationHistory
    -> Maybe ImageDisplayHooks
    -> ToolCall
    -> Text
    -> IO (Either Text ToolHandlerResult)
finishGeneratedImage env history displayHooks call encodedText = do
    let stripped = Text.strip encodedText
        encoded = Text.encodeUtf8 stripped
    if BS.length encoded > maxEncodedImageBytes
        then pure $ Left "generated image exceeded the encoded size limit"
        else case Base64.decode encoded of
            Left err ->
                pure $ Left $
                    "image service returned invalid base64 data: " <> Text.pack err
            Right bytes
                | BS.length bytes > maxDecodedImageBytes ->
                    pure $ Left "generated image exceeded the 32 MiB limit"
                | otherwise -> do
                    let image = ImageAttachment
                            { imageMime = "image/png"
                            , imageBytes = bytes
                            }
                        fileName = generatedImageFileName call.callId
                        displayPath = "generated_images/" <> fileName
                        dataUrl = "data:image/png;base64," <> stripped
                    saveResult <- saveGeneratedImage env fileName bytes
                    recordImageGenerationImages history [image]
                    displayGeneratedImage
                        displayHooks
                        call.callId
                        displayPath
                        image
                    let hint = case saveResult of
                            Right () ->
                                imageOutputHint "generated_images" displayPath
                            Left _ -> ""
                    pure $ Right ToolHandlerResult
                        { resultText = hint
                        , resultImages =
                            [ ToolResultImage
                                { imageUrl = dataUrl
                                , imageDetail = Just "high"
                                }
                            ]
                        }

saveGeneratedImage
    :: ToolEnv
    -> Text
    -> BS.ByteString
    -> IO (Either Text ())
saveGeneratedImage env fileName bytes = do
    let relativeDirectory = fromText "generated_images"
        relativePath = relativeDirectory </> fromText fileName
    resolveUnderCwd env relativePath >>= \case
        Left err -> pure (Left err)
        Right destination ->
            tryAny (writeDestination destination) >>= \case
                Left err ->
                    pure $ Left $
                        "failed to save generated image: "
                            <> Text.pack (show err)
                Right result -> pure result
  where
    writeDestination destination = do
        let directory = takeDirectory destination
        directoryExists <- doesPathExist directory
        unless directoryExists (createDirectory directory)
        directoryIsLink <- pathIsSymbolicLink directory
        directoryIsDirectory <- doesDirectoryExist directory
        if directoryIsLink || not directoryIsDirectory
            then pure $ Left
                "generated_images exists but is not a regular directory"
            else do
                destinationExists <- doesPathExist destination
                if destinationExists
                    then pure $ Left
                        "generated image destination already exists"
                    else do
                        handle <- bracketOnError
                            (openFd
                                (unsafeToFilePath destination)
                                WriteOnly
                                defaultFileFlags
                                    { creat = Just 0o600
                                    , exclusive = True
                                    , nofollow = True
                                    , cloexec = True
                                    })
                            closeFd
                            fdToHandle
                        BS.hPut handle bytes `finally` hClose handle
                        pure (Right ())

displayGeneratedImage
    :: Maybe ImageDisplayHooks
    -> Text
    -> Text
    -> ImageAttachment
    -> IO ()
displayGeneratedImage Nothing _ _ _ = pure ()
displayGeneratedImage (Just hooks) callId displayPath image
    | BS.length image.imageBytes > maxShowImageBytes = pure ()
    | otherwise = do
        _ <- tryAny $ hooks.showImage ImageDisplayRequest
            { displayCallId = callId
            , displayPath
            , displayCaption = Nothing
            , displayImage = image
            }
        pure ()

imageOutputHint :: Text -> Text -> Text
imageOutputHint outputDirectory outputPath =
    "Generated images are saved to "
        <> outputDirectory
        <> " as "
        <> outputPath
        <> " by default.\n\
        \If you need to use a generated image at another path, copy it and leave the original in place unless the user explicitly asks you to delete it.\n\
        \The generated image is already displayed to the user. There is no need to render it in the final response as a Markdown image or file link."

attachmentDataUrl :: ImageAttachment -> Text
attachmentDataUrl image =
    "data:"
        <> image.imageMime
        <> ";base64,"
        <> Text.decodeUtf8 (Base64.encode image.imageBytes)

responseItemImageUrls :: ResponseItem -> [Text]
responseItemImageUrls item =
    imageUrlsInValue (Aeson.toJSON item)
        <> case item of
            ImageGenerationCallItem ImageGenerationCall{result = Just encoded} ->
                ["data:image/png;base64," <> encoded]
            _ -> []

imageUrlsInValue :: Aeson.Value -> [Text]
imageUrlsInValue = \case
    Aeson.Array values ->
        concatMap imageUrlsInValue values
    Aeson.Object object
        | Just (Aeson.String contentType) <- AesonKeyMap.lookup "type" object
        , contentType `elem` ["input_image", "computer_screenshot"]
        , Just (Aeson.String imageUrl) <- AesonKeyMap.lookup "image_url" object ->
            [imageUrl]
        | otherwise ->
            concatMap imageUrlsInValue (AesonKeyMap.elems object)
    _ -> []

decodeImageDataUrl :: Text -> Maybe ImageAttachment
decodeImageDataUrl dataUrl = do
    encodedWithMime <- Text.stripPrefix "data:" dataUrl
    let (declaredMime, encodedWithSeparator) =
            Text.breakOn ";base64," encodedWithMime
    encodedText <- Text.stripPrefix ";base64," encodedWithSeparator
    let encoded = Text.encodeUtf8 encodedText
    if Text.isPrefixOf "image/" declaredMime
            && BS.length encoded <= maxEncodedImageBytes
        then case Base64.decode encoded of
            Right bytes
                | BS.length bytes <= maxDecodedImageBytes
                , Just mime <- sniffEditableImageMime bytes ->
                    Just ImageAttachment
                        { imageMime = mime
                        , imageBytes = bytes
                        }
            _ -> Nothing
        else Nothing

sniffEditableImageMime :: BS.ByteString -> Maybe Text
sniffEditableImageMime bytes =
    sniffImageMime bytes
        <|> if isWebP bytes then Just "image/webp" else Nothing
  where
    isWebP value =
        BS.length value >= 12
            && BS.take 4 value == "RIFF"
            && BS.take 4 (BS.drop 8 value) == "WEBP"

generatedImageFileName :: Text -> Text
generatedImageFileName callId =
    let sanitized = Text.map sanitizeCharacter callId
        stem
            | Text.null sanitized = "image"
            | otherwise = sanitized
    in stem <> ".png"
  where
    sanitizeCharacter character
        | isAlphaNum character || character == '-' || character == '_' =
            character
        | otherwise = '_'

renderApiError :: ApiError -> Text
renderApiError = \case
    HttpError status body ->
        "Image generation request failed (HTTP "
            <> Text.pack (show status)
            <> "): "
            <> Text.take 2000 body
    JsonDecodeError err _ ->
        "Image generation response was invalid: " <> err
    ProviderError _ message _ -> message
    CredentialError message -> message
    ConnectionError message ->
        "Image generation connection failed: " <> message
    CredentialsExhausted{} ->
        "No eligible OpenAI subscription credential is currently available."

maxSelectableImages :: Int
maxSelectableImages = 5

maxDecodedImageBytes :: Int
maxDecodedImageBytes = 32 * 1024 * 1024

-- Upstream checks this before decoding so a maliciously padded string cannot
-- allocate far past the 32 MiB decoded limit.
maxEncodedImageBytes :: Int
maxEncodedImageBytes = 44_739_244

maxImageResponseBytes :: Int
maxImageResponseBytes = 48 * 1024 * 1024
