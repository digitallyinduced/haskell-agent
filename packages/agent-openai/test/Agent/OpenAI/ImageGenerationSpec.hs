module Agent.OpenAI.ImageGenerationSpec (spec) where

import Agent.Loop
    ( ImageAttachment(..)
    , defaultLoopDispatch
    )
import Agent.OpenAI.ImageGeneration
    ( imageGenerationToolAt
    , newImageGenerationHistory
    , recordImageGenerationResponseItems
    )
import Agent.OsPath (fromText)
import Agent.Provider
    ( BillingMode(SubscriptionBilled)
    , Credential(..)
    , Provider(OpenAIProvider)
    , tokenProvider
    )
import Agent.ToolDispatch
    ( ToolCallResult(..)
    , ToolResultImage(..)
    , dispatchToolCall
    , functionToolCall
    , toolCallResultImages
    )
import Agent.Responses.Types
    ( MessageContent(..)
    , ResponseContentPart(..)
    , ResponseItem(..)
    , ResponseMessage(..)
    , ResponseRole(RoleUser)
    )
import Agent.Tools.ShowImage
    ( ImageDisplayHooks(..)
    , ImageDisplayRequest(..)
    )
import Agent.Tools.Types
    ( appToolHandlers
    , defaultToolEnv
    )
import qualified Data.Aeson as Aeson
import Data.Aeson ((.=))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as Base64
import Data.IORef
    ( IORef
    , atomicModifyIORef'
    , newIORef
    , readIORef
    )
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

data RecordedRequest = RecordedRequest
    { requestMethod :: !HTTP.Method
    , requestPath :: !Text
    , requestHeaders :: !HTTP.RequestHeaders
    , requestBody :: !Aeson.Value
    }

spec :: Spec
spec = describe "image generation tool" do
    it "generates an image, returns rich model content, saves it, and displays it" do
        withSystemTempDirectory "agent-imagegen" \cwd -> do
            recorded <- newIORef []
            displayed <- newIORef []
            withImageServer recorded generatedPng \baseUrl -> do
                env <- defaultToolEnv (fromText (Text.pack cwd))
                history <- newImageGenerationHistory
                let hooks = ImageDisplayHooks \request -> do
                        atomicModifyIORef' displayed \requests ->
                            (requests <> [request], ())
                        pure (Right ())
                    tool = imageGenerationToolAt
                        baseUrl
                        openAiProvider
                        env
                        history
                        (Just hooks)
                result <- dispatchToolCall
                    defaultLoopDispatch
                    (appToolHandlers [tool])
                    (functionToolCall
                        "call-1"
                        "image_gen.imagegen"
                        "{\"prompt\":\"paint a moonlit lake\"}")

                result.output `shouldSatisfy`
                    Text.isInfixOf "generated_images/call-1.png"
                toolCallResultImages result `shouldBe`
                    [ ToolResultImage
                        { imageUrl = pngDataUrl generatedPng
                        , imageDetail = Just "high"
                        }
                    ]
                BS.readFile (cwd </> "generated_images" </> "call-1.png")
                    `shouldReturn` generatedPng
                readIORef displayed `shouldReturn`
                    [ ImageDisplayRequest
                        { displayCallId = "call-1"
                        , displayPath = "generated_images/call-1.png"
                        , displayCaption = Nothing
                        , displayImage = ImageAttachment "image/png" generatedPng
                        }
                    ]

            [request] <- readIORef recorded
            request.requestMethod `shouldBe` HTTP.methodPost
            request.requestPath `shouldBe` "/images/generations"
            lookup "authorization" request.requestHeaders
                `shouldBe` Just "Bearer test-token"
            lookup "chatgpt-account-id" request.requestHeaders
                `shouldBe` Just "account-1"
            lookup "x-codex-image-turn-id" request.requestHeaders
                `shouldBe` Just "call-1"
            lookup "originator" request.requestHeaders
                `shouldBe` Just "haskell-agent"
            request.requestBody `shouldBe` Aeson.object
                [ "prompt" .= ("paint a moonlit lake" :: Text)
                , "background" .= ("auto" :: Text)
                , "model" .= ("gpt-image-2" :: Text)
                , "quality" .= ("auto" :: Text)
                , "size" .= ("auto" :: Text)
                ]

    it "edits the requested number of recent images in chronological order" do
        withSystemTempDirectory "agent-imagegen-edit" \cwd -> do
            recorded <- newIORef []
            withImageServer recorded generatedPng \baseUrl -> do
                env <- defaultToolEnv (fromText (Text.pack cwd))
                history <- newImageGenerationHistory
                let first = ImageAttachment "image/png" generatedPng
                    second = ImageAttachment "image/jpeg" jpegBytes
                    persistedMessage = MessageItem ResponseMessage
                        { messageId = Nothing
                        , content = MessageContentParts
                            [ inputImagePart first
                            , inputImagePart second
                            ]
                        , role = RoleUser
                        , status = Nothing
                        , phase = Nothing
                        , passthrough = Nothing
                        }
                recordImageGenerationResponseItems history [persistedMessage]
                let tool = imageGenerationToolAt
                        baseUrl
                        openAiProvider
                        env
                        history
                        Nothing
                _ <- dispatchToolCall
                    defaultLoopDispatch
                    (appToolHandlers [tool])
                    (functionToolCall
                        "edit-1"
                        "imagegen"
                        "{\"prompt\":\"change the lighting\",\
                        \\"num_last_images_to_include\":2}")
                pure ()

            [request] <- readIORef recorded
            request.requestPath `shouldBe` "/images/edits"
            request.requestBody `shouldBe` Aeson.object
                [ "images" .=
                    [ Aeson.object ["image_url" .= attachmentDataUrl
                        (ImageAttachment "image/png" generatedPng)]
                    , Aeson.object ["image_url" .= attachmentDataUrl
                        (ImageAttachment "image/jpeg" jpegBytes)]
                    ]
                , "prompt" .= ("change the lighting" :: Text)
                , "background" .= ("auto" :: Text)
                , "model" .= ("gpt-image-2" :: Text)
                , "quality" .= ("auto" :: Text)
                , "size" .= ("auto" :: Text)
                ]

    it "rejects conflicting image selectors before making a request" do
        withSystemTempDirectory "agent-imagegen-invalid" \cwd -> do
            env <- defaultToolEnv (fromText (Text.pack cwd))
            history <- newImageGenerationHistory
            let tool = imageGenerationToolAt
                    "http://127.0.0.1:1"
                    openAiProvider
                    env
                    history
                    Nothing
            result <- dispatchToolCall
                defaultLoopDispatch
                (appToolHandlers [tool])
                (functionToolCall
                    "invalid-1"
                    "imagegen"
                    "{\"prompt\":\"edit\",\
                    \\"referenced_image_paths\":[\"/tmp/a.png\"],\
                    \\"num_last_images_to_include\":1}")
            result.output `shouldSatisfy`
                Text.isInfixOf
                    "provide only one of `referenced_image_paths` or `num_last_images_to_include`"
            toolCallResultImages result `shouldBe` []

openAiProvider =
    tokenProvider SubscriptionBilled \_ ->
        pure $ Right Credential
            { accessToken = "test-token"
            , accountId = "account-1"
            , leaseId = Nothing
            , provider = OpenAIProvider
            }

withImageServer
    :: IORef [RecordedRequest]
    -> BS.ByteString
    -> (Text -> IO a)
    -> IO a
withImageServer recorded imageBytes action =
    Warp.testWithApplication (pure app) \port ->
        action ("http://127.0.0.1:" <> Text.pack (show port))
  where
    app request respond = do
        body <- Wai.strictRequestBody request
        let decoded = case Aeson.eitherDecode body of
                Right value -> value
                Left _ -> Aeson.Null
            captured = RecordedRequest
                { requestMethod = Wai.requestMethod request
                , requestPath = Text.decodeUtf8 (Wai.rawPathInfo request)
                , requestHeaders = Wai.requestHeaders request
                , requestBody = decoded
                }
        atomicModifyIORef' recorded \requests ->
            (requests <> [captured], ())
        respond $ Wai.responseLBS
            HTTP.status200
            [("Content-Type", "application/json")]
            (Aeson.encode (Aeson.object
                [ "created" .= (1 :: Int)
                , "data" .=
                    [ Aeson.object
                        [ "b64_json" .=
                            Text.decodeUtf8 (Base64.encode imageBytes)
                        ]
                    ]
                ]))

attachmentDataUrl :: ImageAttachment -> Text
attachmentDataUrl image =
    "data:"
        <> image.imageMime
        <> ";base64,"
        <> Text.decodeUtf8 (Base64.encode image.imageBytes)

inputImagePart :: ImageAttachment -> ResponseContentPart
inputImagePart image =
    InputImagePart
        { detail = Nothing
        , fileId = Nothing
        , imageUrl = Just (attachmentDataUrl image)
        , promptCacheBreakpoint = Nothing
        }

pngDataUrl :: BS.ByteString -> Text
pngDataUrl bytes =
    "data:image/png;base64," <> Text.decodeUtf8 (Base64.encode bytes)

generatedPng :: BS.ByteString
generatedPng =
    BS.pack
        [ 0x89, 0x50, 0x4e, 0x47
        , 0x0d, 0x0a, 0x1a, 0x0a
        , 0x00, 0x00, 0x00, 0x00
        ]

jpegBytes :: BS.ByteString
jpegBytes = BS.pack [0xff, 0xd8, 0xff, 0x00]
