-- | Native streaming GenerateContent client for the Gemini Developer and
-- Gemini Code Assist APIs.
module Agent.Gemini.Client
    ( StreamEventCallback
    , createResponse
    , createResponseWith
    , createResponseWithEvents
    , createResponseWithEventsPolicy
    , retryTransientGeminiResultWithPolicy
    ) where

import Agent.Error
    ( ApiError(..)
    , ErrorType(..)
    , isInlineRetryableProviderError
    )
import Agent.Gemini.Error (classifyFailure)
import Agent.Gemini.Options
import Agent.Gemini.Request
import Agent.Gemini.Response
import Agent.Gemini.Stream
import Agent.Http.Header (parseRetryAfterSeconds)
import Agent.Http.Url (trimTrailingSlash)
import Agent.Provider (Credential(..), Provider(..))
import Agent.Responses.Client (retryStreamingResultWithPolicy)
import Agent.Responses.Types
import Control.Exception.Safe
    ( Exception
    , SomeException
    , fromException
    , throwIO
    , tryAny
    )
import Control.Monad (foldM)
import Control.Retry
    ( RetryPolicyM
    , exponentialBackoff
    , limitRetries
    )
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import qualified Data.Text.Encoding.Error as TextEncoding (lenientDecode)
import Data.Time.Clock.POSIX (getPOSIXTime)
import qualified Network.HTTP.Client as HttpClient
import qualified Network.HTTP.Client.TLS as HttpTls
import Network.HTTP.Simple hiding (Response)
import qualified Network.HTTP.Types.URI as Uri
import qualified System.Timeout as Timeout

type StreamEventCallback = GeminiStreamEvent -> IO ()

data GeminiTransport
    = DeveloperApi
    | CodeAssist !Text

newtype StreamStalled = StreamStalled Int

instance Show StreamStalled where
    show (StreamStalled seconds) =
        "Gemini streaming response stalled: no data received for "
            <> show seconds
            <> "s"

instance Exception StreamStalled

-- | Send one request using environment-derived client options.
createResponse
    :: Credential
    -> ResponseCreateParams
    -> IO (Either ApiError Response)
createResponse credential request = do
    options <- clientOptionsFromEnv
    createResponseWith options credential request

-- | Send one request using explicit client options.
createResponseWith
    :: ClientOptions
    -> Credential
    -> ResponseCreateParams
    -> IO (Either ApiError Response)
createResponseWith options credential request =
    createResponseWithEvents options credential request (const (pure ()))

-- | Send one request and deliver text, reasoning, and complete function calls
-- incrementally in wire order.
createResponseWithEvents
    :: ClientOptions
    -> Credential
    -> ResponseCreateParams
    -> StreamEventCallback
    -> IO (Either ApiError Response)
createResponseWithEvents =
    createResponseWithEventsPolicy transientResultPolicy

-- | Variant with an injectable retry policy. A transient failure is retried
-- only before the first callback, so consumers never observe replayed output.
createResponseWithEventsPolicy
    :: RetryPolicyM IO
    -> ClientOptions
    -> Credential
    -> ResponseCreateParams
    -> StreamEventCallback
    -> IO (Either ApiError Response)
createResponseWithEventsPolicy policy options credential request onEvent
    | credential.provider /= GeminiProvider =
        pure $ Left $ ProviderError ApiErrorType
            "agent-gemini requires a Gemini credential"
            Nothing
    | Text.null (Text.strip credential.accessToken) =
        pure $ Left $ CredentialError $ case credentialTransport credential of
            DeveloperApi -> "Gemini API key is empty"
            CodeAssist _ -> "Gemini access token is empty"
    | otherwise =
        case buildRequest options.defaultModel request of
            Left err -> pure (Left err)
            Right nativeRequest ->
                retryTransientGeminiResultWithPolicy
                    policy
                    (performOnce nativeRequest)
                    onEvent
  where
    performOnce nativeRequest =
        performGeminiRequest
            options
            credential
            request
            nativeRequest

retryTransientGeminiResultWithPolicy
    :: RetryPolicyM IO
    -> ((event -> IO ()) -> IO (Either ApiError value))
    -> (event -> IO ())
    -> IO (Either ApiError value)
retryTransientGeminiResultWithPolicy policy request onEvent =
    retryStreamingResultWithPolicy
        policy
        isInlineRetryableProviderError
        request
        (Just onEvent)

transientResultPolicy :: RetryPolicyM IO
transientResultPolicy =
    exponentialBackoff 1_000_000 <> limitRetries 3

maxErrorBodyBytes :: Int
maxErrorBodyBytes = 1024 * 1024

performGeminiRequest
    :: ClientOptions
    -> Credential
    -> ResponseCreateParams
    -> GeminiRequest
    -> StreamEventCallback
    -> IO (Either ApiError Response)
performGeminiRequest options credential canonical nativeRequest emit =
    tryAny performRequest >>= \case
        Left exception -> pure $ Left $ ConnectionError
            ( "Gemini request failed: "
                <> safeExceptionText credential.accessToken exception
            )
        Right result -> pure result
  where
    performRequest = do
        fallbackId <- freshResponseId nativeRequest.requestModel
        baseRequest <- parseRequest
            ("POST " <> generateContentUrl
                options
                transport
                nativeRequest.requestModel)
        manager <- HttpTls.getGlobalManager
        HttpClient.withResponse
            ( setRequestBodyLBS
                (Aeson.encode
                    (transportRequestBody
                        transport
                        fallbackId
                        nativeRequest))
            $ applyCredential transport credential.accessToken
            $ setRequestHeader "Content-Type" ["application/json"]
            $ setRequestHeader "Accept" ["text/event-stream"]
            $ setRequestHeader "User-Agent" ["haskell-agent"]
            $ withTimeout options.requestTimeoutSeconds
            $ withoutRedirects baseRequest
            )
            manager
            (handleResponse fallbackId)

    transport = credentialTransport credential

    handleResponse fallbackId response = do
        let status = getResponseStatusCode response
            body = HttpClient.responseBody response
        if status >= 200 && status < 300
            then consumeSse fallbackId body
            else do
                failureBody <- consumeBody body
                let bodyText = TextEncoding.decodeUtf8With
                        TextEncoding.lenientDecode
                        (LBS.toStrict failureBody)
                pure $ Left $
                    classifyFailure
                        status
                        (parseRetryAfterSeconds
                            (getResponseHeader "Retry-After" response))
                        (redactSecret credential.accessToken bodyText)

    consumeSse fallbackId body =
        go
            newSseDecoder
            (initialStreamStateWithCustomTools
                nativeRequest.requestModel
                fallbackId
                nativeRequest.requestCustomToolNames)
      where
        go decoder state = do
            chunk <- readChunkWithin options.requestTimeoutSeconds body
            if BS.null chunk
                then case finishSseDecoder decoder of
                    Left err -> pure (Left err)
                    Right trailing -> do
                        finalState <- foldM deliver state trailing
                        if streamReachedTerminal finalState
                            then pure $ Right
                                (buildResponse canonical finalState)
                            else pure $ Left $ ConnectionError
                                "Gemini stream ended before a terminal finish reason"
                else case feedSseDecoder decoder chunk of
                    Left err -> pure (Left err)
                    Right (nextDecoder, responses) -> do
                        nextState <- foldM deliver state responses
                        go nextDecoder nextState

        deliver state response = do
            let (nextState, events) = applyChunk state response
            mapM_ emit events
            pure nextState

    consumeBody body =
        LBS.fromChunks <$> readChunks maxErrorBodyBytes []
      where
        readChunks remaining reversedChunks
            | remaining <= 0 = pure (reverse reversedChunks)
        readChunks remaining reversedChunks = do
            chunk <- readChunkWithin options.requestTimeoutSeconds body
            if BS.null chunk
                then pure (reverse reversedChunks)
                else
                    let kept = BS.take remaining chunk
                    in readChunks
                        (remaining - BS.length kept)
                        (kept : reversedChunks)

withTimeout :: Int -> Request -> Request
withTimeout timeoutSeconds request =
    request
        { HttpClient.responseTimeout =
            HttpClient.responseTimeoutMicro
                (timeoutSeconds * 1_000_000)
        }

withoutRedirects :: Request -> Request
withoutRedirects request = request { HttpClient.redirectCount = 0 }

readChunkWithin
    :: Int
    -> HttpClient.BodyReader
    -> IO BS.ByteString
readChunkWithin timeoutSeconds body
    | timeoutSeconds <= 0 = HttpClient.brRead body
    | otherwise =
        Timeout.timeout
            (timeoutSeconds * 1_000_000)
            (HttpClient.brRead body) >>= \case
                Just chunk -> pure chunk
                Nothing -> throwIO (StreamStalled timeoutSeconds)

credentialTransport :: Credential -> GeminiTransport
credentialTransport credential =
    case credential.leaseId >>= Text.stripPrefix "code-assist:" of
        Just project -> CodeAssist project
        Nothing -> DeveloperApi

applyCredential :: GeminiTransport -> Text -> Request -> Request
applyCredential transport token =
    setRequestHeader headerName [headerValue]
  where
    encoded = TextEncoding.encodeUtf8 token
    (headerName, headerValue) = case transport of
        DeveloperApi -> ("x-goog-api-key", encoded)
        CodeAssist _ -> ("Authorization", "Bearer " <> encoded)

transportRequestBody :: GeminiTransport -> Text -> GeminiRequest -> Aeson.Value
transportRequestBody DeveloperApi _ nativeRequest =
    nativeRequest.requestBody
transportRequestBody (CodeAssist project) promptId nativeRequest =
    Aeson.object
        [ "model" Aeson..= nativeRequest.requestModel
        , "project" Aeson..= project
        , "user_prompt_id" Aeson..= promptId
        , "request" Aeson..= nativeRequest.requestBody
        ]

generateContentUrl :: ClientOptions -> GeminiTransport -> Text -> String
generateContentUrl options transport model = case transport of
    DeveloperApi ->
        trimTrailingSlash options.baseUrl
            <> "/models/"
            <> BS8.unpack
                (Uri.urlEncode True (TextEncoding.encodeUtf8 model))
            <> ":streamGenerateContent?alt=sse"
    CodeAssist _ ->
        trimTrailingSlash options.codeAssistBaseUrl
            <> ":streamGenerateContent?alt=sse"

freshResponseId :: Text -> IO Text
freshResponseId model = do
    now <- getPOSIXTime
    let micros = round (now * 1_000_000) :: Integer
        modelTag = Text.map sanitize model
    pure ("gemini-" <> modelTag <> "-" <> Text.pack (show micros))
  where
    sanitize character
        | character >= 'a' && character <= 'z' = character
        | character >= 'A' && character <= 'Z' = character
        | character >= '0' && character <= '9' = character
        | character == '-' = character
        | otherwise = '-'

-- Rendering a complete 'HttpExceptionRequest' would include custom headers;
-- unlike Authorization, @x-goog-api-key@ is not guaranteed to be redacted by
-- http-client. Keep the useful failure content while deliberately dropping
-- the request and its credential-bearing headers.
safeExceptionText :: Text -> SomeException -> Text
safeExceptionText secret exception =
    redactSecret secret $ case
        fromException exception :: Maybe HttpClient.HttpException of
        Just (HttpClient.HttpExceptionRequest _ content) ->
            Text.pack (show content)
        Just (HttpClient.InvalidUrlException url reason) ->
            "invalid URL " <> Text.pack url <> ": " <> Text.pack reason
        Nothing -> Text.pack (show exception)

redactSecret :: Text -> Text -> Text
redactSecret secret text
    | Text.null secret = text
    | otherwise = Text.replace secret "<redacted>" text
