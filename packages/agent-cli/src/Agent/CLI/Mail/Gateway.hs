-- | Trusted first-party email backend over the organization gateway's MCP
-- endpoint.
--
-- The gateway MCP catalog is consumed internally instead of being registered
-- with the generic MCP fleet.  This preserves the canonical @email_*@ names
-- and the host's non-bypassable 'AlwaysConfirm' policy for draft writes.
module Agent.CLI.Mail.Gateway
    ( gatewayMailTools
    , gatewayMailToolsWith
    , GatewayMailRequest(..)
    ) where

import Agent.CLI.GatewayClient
    ( GatewayCredential(..)
    , validateGatewayCredential
    )
import Agent.CLI.Mail.Tools
    ( MailToolsEnv(..)
    , mailToolsForConnectedAccounts
    )
import Agent.Mail.Contract
import Agent.Mail.Types
import Agent.Tools.Types (AppTool, ToolEnv)
import Control.Exception.Safe (tryAny)
import Data.Aeson
    ( FromJSON
    , Value(..)
    , object
    , (.:)
    , (.=)
    )
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Aeson.Types as AesonTypes
import qualified Data.ByteString as BS
import Data.Foldable (toList, traverse_)
import Data.List (find)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import Network.HTTP.Client
    ( BodyReader
    , Manager
    , Request
    , RequestBody(..)
    , brRead
    , checkResponse
    , method
    , parseRequest
    , redirectCount
    , requestBody
    , requestHeaders
    , responseBody
    , responseStatus
    , responseTimeout
    , responseTimeoutMicro
    , withResponse
    )
import Network.HTTP.Client.TLS (newTlsManager)
import Network.HTTP.Types
    ( hAccept
    , hAuthorization
    , hContentType
    , statusCode
    , statusIsSuccessful
    )
import Network.HTTP.Types.URI (urlEncode)

data GatewayMailRequest = GatewayMailRequest
    { gatewayMailRequestTool :: !Text
    , gatewayMailRequestArguments :: !Value
    }
    deriving (Eq, Show)

-- | Build gateway-backed tools after verifying the exact first-party email
-- contract and listing accounts for the authenticated gateway user.
gatewayMailTools
    :: ToolEnv
    -> GatewayCredential
    -> IO (Either Text [AppTool])
gatewayMailTools toolEnv credential =
    case validateGatewayCredential credential of
        Left _ -> pure (Left "The organization gateway credential is invalid.")
        Right () ->
            tryAny newTlsManager >>= \case
                Left _ -> pure (Left gatewayUnavailable)
                Right manager ->
                    gatewayMailToolsWith
                        toolEnv
                        (performGatewayMailRequest manager credential)
                        (downloadGatewayAttachment manager credential)

-- | Injectable factory used by tests.  The request callback speaks the
-- structured email MCP contract, while the download callback implements the
-- authenticated same-origin binary data plane.
gatewayMailToolsWith
    :: ToolEnv
    -> (GatewayMailRequest -> IO (Either Text Value))
    -> (MailAttachmentDownload -> Int
        -> IO (Either Text MailAttachmentContent))
    -> IO (Either Text [AppTool])
gatewayMailToolsWith toolEnv call download = do
    verifyGatewayMailContract call >>= \case
        Left err -> pure (Left err)
        Right () -> do
            let invoke
                    :: (FromJSON value)
                    => Text
                    -> Value
                    -> IO (Either Text value)
                invoke name arguments =
                    call GatewayMailRequest
                        { gatewayMailRequestTool = name
                        , gatewayMailRequestArguments = arguments
                        }
                        >>= pure . (>>= decodeMailMcpResult)
                listAccounts =
                    invoke mailListAccountsToolName (object [])
                env = MailToolsEnv
                    { mailToolsToolEnv = toolEnv
                    , mailToolsLimits = defaultMailToolLimits
                    , mailToolsListAccounts = listAccounts
                    , mailToolsListMailboxes = \accountId _ ->
                        invoke mailListMailboxesToolName
                            (object ["account_id" .= accountId])
                    , mailToolsSearch = \request ->
                        invoke mailSearchToolName (Aeson.toJSON request)
                    , mailToolsGetMessage = \request _ ->
                        invoke mailGetToolName (Aeson.toJSON request)
                    , mailToolsDownloadAttachment = \request maximum -> do
                        prepared <-
                            invoke
                                mailDownloadAttachmentToolName
                                (Aeson.toJSON request)
                        case prepared of
                            Left err -> pure (Left err)
                            Right descriptor -> download descriptor maximum
                    , mailToolsCreateDraft = \request ->
                        invoke mailCreateDraftToolName
                            (Aeson.toJSON request)
                    , mailToolsUpdateDraft = \request ->
                        invoke mailUpdateDraftToolName
                            (Aeson.toJSON request)
                    , mailToolsReplyDraft = \request ->
                        invoke mailReplyDraftToolName
                            (Aeson.toJSON request)
                    }
            listAccounts >>= \case
                Left err -> pure (Left err)
                Right accounts ->
                    pure
                        (Right
                            (mailToolsForConnectedAccounts env accounts))

verifyGatewayMailContract
    :: (GatewayMailRequest -> IO (Either Text Value))
    -> IO (Either Text ())
verifyGatewayMailContract call =
    call GatewayMailRequest
        { gatewayMailRequestTool = contractDiscoveryOperation
        , gatewayMailRequestArguments = object []
        }
        >>= pure . (>>= verify)
  where
    verify (Object response) = do
        listed <- maybe
            (Left incompatibleMessage)
            Right
            (KeyMap.lookup "tools" response)
        tools <- case listed of
            Array values -> Right (toList values)
            _ -> Left incompatibleMessage
        traverse_ (verifyTool tools) mailMcpToolDefinitions
    verify _ = Left incompatibleMessage

    verifyTool actual expected@(Object expectedObject) = do
        expectedName <- textField "name" expectedObject
        actualTool <- maybe
            (Left incompatibleMessage)
            Right
            (find (hasName expectedName) actual)
        if contractShape actualTool == contractShape expected
            then Right ()
            else Left incompatibleMessage
    verifyTool _ _ = Left incompatibleMessage

    hasName expectedName (Object value) =
        KeyMap.lookup "name" value == Just (String expectedName)
    hasName _ _ = False

    contractShape (Object value) = object
        [ "name" .= KeyMap.lookup "name" value
        , "description" .= KeyMap.lookup "description" value
        , "inputSchema" .= KeyMap.lookup "inputSchema" value
        , "annotations" .= KeyMap.lookup "annotations" value
        , "_meta" .= KeyMap.lookup "_meta" value
        ]
    contractShape _ = Null

    textField key value =
        case KeyMap.lookup key value of
            Just (String text) -> Right text
            _ -> Left incompatibleMessage

contractDiscoveryOperation :: Text
contractDiscoveryOperation = "$email/tools/list"

incompatibleMessage :: Text
incompatibleMessage =
    "The organization gateway does not provide a compatible email service."

performGatewayMailRequest
    :: Manager
    -> GatewayCredential
    -> GatewayMailRequest
    -> IO (Either Text Value)
performGatewayMailRequest manager credential request
    | request.gatewayMailRequestTool == contractDiscoveryOperation =
        rpc "tools/list" (object [])
            >>= pure . (>>= resultObject)
    | otherwise =
        rpc
            "tools/call"
            ( object
                [ "name" .= request.gatewayMailRequestTool
                , "arguments" .= request.gatewayMailRequestArguments
                ]
            )
            >>= pure . (>>= resultValue)
  where
    rpc methodName parameters = do
        parsed <- tryAny (parseRequest (Text.unpack mcpUrl))
        case parsed of
            Left _ -> pure (Left gatewayUnavailable)
            Right initial -> do
                attempted <- tryAny $
                    withResponse
                        (mcpRequest credential initial methodName parameters)
                        manager
                        \response -> do
                            body <-
                                readBoundedBody
                                    maximumMcpResponseBytes
                                    response.responseBody
                            pure (response.responseStatus, body)
                pure case attempted of
                    Left _ -> Left gatewayUnavailable
                    Right (status, body)
                        | not (statusIsSuccessful status) ->
                            Left
                                ( "The organization gateway email service "
                                    <> "returned HTTP "
                                    <> Text.pack (show (statusCode status))
                                    <> "."
                                )
                        | otherwise ->
                            body >>= decodeRpcResponse

    mcpUrl =
        Text.dropWhileEnd (== '/')
            (Text.strip credential.gatewayBaseUrl)
            <> "/mcp"

mcpRequest
    :: GatewayCredential
    -> Request
    -> Text
    -> Value
    -> Request
mcpRequest credential initial methodName parameters =
    initial
        { method = "POST"
        , requestHeaders =
            [ (hContentType, "application/json")
            , (hAccept, "application/json")
            , ("MCP-Protocol-Version", "2025-11-25")
            , ( hAuthorization
              , "Bearer "
                    <> TextEncoding.encodeUtf8
                        credential.gatewayAccessToken
              )
            ]
        , requestBody =
            RequestBodyLBS
                ( Aeson.encode
                    ( object
                        [ "jsonrpc" .= ("2.0" :: Text)
                        , "id" .= (1 :: Int)
                        , "method" .= methodName
                        , "params" .= parameters
                        ]
                    )
                )
        , redirectCount = 0
        , checkResponse = \_ _ -> pure ()
        , responseTimeout =
            responseTimeoutMicro gatewayRequestTimeoutMicros
        }

decodeRpcResponse :: BS.ByteString -> Either Text Value
decodeRpcResponse bytes = do
    value <- either
        (const (Left "The organization gateway returned invalid email data."))
        Right
        (Aeson.eitherDecodeStrict' bytes)
    AesonTypes.parseEither parser value
        & either (const (Left "The organization gateway returned invalid email data."))
            Right
  where
    parser = Aeson.withObject "MCP response" \response ->
        case KeyMap.lookup "error" response of
            Just _ -> fail "MCP operation failed"
            Nothing -> response .: "result"

    (&) = flip ($)

resultObject :: Value -> Either Text Value
resultObject (Object value) =
    maybe (Left incompatibleMessage) Right (KeyMap.lookup "tools" value)
        >>= \tools -> Right (object ["tools" .= tools])
resultObject _ = Left incompatibleMessage

resultValue :: Value -> Either Text Value
resultValue = Right

downloadGatewayAttachment
    :: Manager
    -> GatewayCredential
    -> MailAttachmentDownload
    -> Int
    -> IO (Either Text MailAttachmentContent)
downloadGatewayAttachment manager credential descriptor requestedMaximum
    | Left _ <-
        validateOpaqueMailReference
            "download_ref"
            descriptor.mailAttachmentDownloadRef =
        pure (Left attachmentUnavailable)
    | descriptor.mailAttachmentDownloadSizeBytes < 0
        || descriptor.mailAttachmentDownloadSizeBytes > maximum =
            pure (Left "The attachment exceeded the configured download limit.")
    | otherwise = do
        parsed <- tryAny (parseRequest (Text.unpack downloadUrl))
        case parsed of
            Left _ -> pure (Left attachmentUnavailable)
            Right initial -> do
                attempted <- tryAny $
                    withResponse
                        initial
                            { method = "GET"
                            , requestHeaders =
                                [ ( hAuthorization
                                  , "Bearer "
                                        <> TextEncoding.encodeUtf8
                                            credential.gatewayAccessToken
                                  )
                                , (hAccept, "application/octet-stream")
                                ]
                            , redirectCount = 0
                            , checkResponse = \_ _ -> pure ()
                            , responseTimeout =
                                responseTimeoutMicro
                                    gatewayAttachmentTimeoutMicros
                            }
                        manager
                        \response -> do
                            body <-
                                readBoundedBody maximum response.responseBody
                            pure (response.responseStatus, body)
                pure case attempted of
                    Left _ -> Left attachmentUnavailable
                    Right (status, body)
                        | not (statusIsSuccessful status) ->
                            Left attachmentUnavailable
                        | otherwise -> do
                            bytes <- body
                            if BS.length bytes
                                /= descriptor.mailAttachmentDownloadSizeBytes
                                then Left attachmentUnavailable
                                else Right MailAttachmentContent
                                    { mailDownloadedAttachmentFilename =
                                        descriptor.mailAttachmentDownloadFilename
                                    , mailDownloadedAttachmentContentType =
                                        descriptor.mailAttachmentDownloadContentType
                                    , mailDownloadedAttachmentBytes = bytes
                                    }
  where
    maximum = max 1 requestedMaximum
    encodedRef =
        TextEncoding.decodeUtf8
            (urlEncode True
                (TextEncoding.encodeUtf8
                    descriptor.mailAttachmentDownloadRef))
    downloadUrl =
        Text.dropWhileEnd (== '/')
            (Text.strip credential.gatewayBaseUrl)
            <> "/v1/email/attachments/"
            <> encodedRef

readBoundedBody
    :: Int
    -> BodyReader
    -> IO (Either Text BS.ByteString)
readBoundedBody maximum = go [] 0
  where
    go chunks size reader = brRead reader >>= \chunk ->
        if BS.null chunk
            then pure (Right (BS.concat (reverse chunks)))
            else
                let size' = size + BS.length chunk
                in if size' > maximum
                    then pure
                        (Left "The organization gateway response was too large.")
                    else go (chunk : chunks) size' reader

gatewayUnavailable :: Text
gatewayUnavailable =
    "Could not reach the organization gateway email service."

attachmentUnavailable :: Text
attachmentUnavailable =
    "The gateway attachment is unavailable or has expired."

maximumMcpResponseBytes, gatewayRequestTimeoutMicros
    , gatewayAttachmentTimeoutMicros :: Int
maximumMcpResponseBytes = 1024 * 1024
gatewayRequestTimeoutMicros = 15 * 1_000_000
gatewayAttachmentTimeoutMicros = 30 * 1_000_000
