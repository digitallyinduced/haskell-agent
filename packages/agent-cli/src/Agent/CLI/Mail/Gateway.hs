-- | Trusted first-party email backend over the organization gateway's MCP
-- endpoint.
--
-- The gateway MCP catalog is consumed internally instead of being registered
-- with the generic MCP fleet.  This preserves the canonical @email_*@ names
-- and the host's non-bypassable 'AlwaysConfirm' policy for draft writes.
module Agent.CLI.Mail.Gateway
    ( GatewayMailRuntime(..)
    , gatewayMailTools
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
import Agent.Json (rawJsonBytes)
import Agent.Mail.Contract
import Agent.Mail.Types
import Agent.MCP.Client
    ( closeMcpClient
    , ensureMcpClientReady
    , requestMcpOnce
    , startMcpClient
    )
import Agent.MCP.Types
    ( McpClient
    , McpProtocolPreference(..)
    , McpServerConfig(..)
    )
import Agent.Tools.Types (AppTool, ToolEnv)
import Control.Exception.Safe (finally, mask, mask_, onException, tryAny)
import Control.Monad (void, when)
import Data.Aeson
    ( FromJSON
    , Series
    , Value(..)
    , object
    , (.=)
    )
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Char (isAlphaNum, isAscii)
import Data.Foldable (toList, traverse_)
import Data.List (find, sort)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import Network.HTTP.Client
    ( BodyReader
    , Manager
    , brRead
    , checkResponse
    , closeManager
    , method
    , parseRequest
    , redirectCount
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
    , statusIsSuccessful
    )

data GatewayMailRequest = GatewayMailRequest
    { gatewayMailRequestTool :: !Text
    , gatewayMailRequestArguments :: !Value
    }
    deriving (Eq)

instance Show GatewayMailRequest where
    show request =
        "GatewayMailRequest { gatewayMailRequestTool = "
            <> show request.gatewayMailRequestTool
            <> ", gatewayMailRequestArguments = <redacted> }"

data GatewayMailRuntime = GatewayMailRuntime
    { gatewayMailRuntimeTools :: ![AppTool]
    , gatewayMailRuntimeClose :: !(IO ())
    }

-- | Build gateway-backed tools after verifying the exact first-party email
-- contract and listing accounts for the authenticated gateway user.
gatewayMailTools
    :: ToolEnv
    -> GatewayCredential
    -> IO (Either Text GatewayMailRuntime)
gatewayMailTools toolEnv credential =
    case validateGatewayCredential credential of
        Left _ -> pure (Left "The organization gateway credential is invalid.")
        Right () -> mask \restore ->
            tryAny newTlsManager >>= \case
                Left _ -> pure (Left gatewayUnavailable)
                Right manager -> do
                    started <-
                        tryAny
                            (restore
                                (startMcpClient
                                    (gatewayMailMcpConfig credential)))
                            `onException` closeManagerOnly manager
                    case started of
                        Left _ -> do
                            closeManagerOnly manager
                            pure (Left gatewayUnavailable)
                        Right client ->
                            (do
                                ready <-
                                    tryAny
                                        (restore
                                            (ensureMcpClientReady client))
                                case ready of
                                    Left _ ->
                                        closeFailed
                                            manager client gatewayUnavailable
                                    Right (Left _) ->
                                        closeFailed
                                            manager client gatewayUnavailable
                                    Right (Right _) -> do
                                        built <- tryAny . restore $
                                            gatewayMailToolsWith
                                                toolEnv
                                                (performGatewayMailRequest client)
                                                (downloadGatewayAttachment
                                                    manager credential)
                                        case built of
                                            Left _ ->
                                                closeFailed
                                                    manager
                                                    client
                                                    gatewayUnavailable
                                            Right (Left err) ->
                                                closeFailed manager client err
                                            Right (Right registered) ->
                                                pure . Right $
                                                    GatewayMailRuntime
                                                        { gatewayMailRuntimeTools =
                                                            registered
                                                        , gatewayMailRuntimeClose =
                                                            closeResources
                                                                manager client
                                                        })
                                `onException` closeResources manager client
  where
    closeManagerOnly manager = do
        _ <- tryAny (closeManager manager)
        pure ()

    closeFailed manager client err = do
        closeResources manager client
        pure (Left err)

    closeResources manager client = mask_ do
        void (tryAny (closeMcpClient client))
            `finally` void (tryAny (closeManager manager))

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
        Right () -> gatewayMailToolsVerified toolEnv call download

gatewayMailToolsVerified
    :: ToolEnv
    -> (GatewayMailRequest -> IO (Either Text Value))
    -> (MailAttachmentDownload -> Int
        -> IO (Either Text MailAttachmentContent))
    -> IO (Either Text [AppTool])
gatewayMailToolsVerified toolEnv call download = do
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
                >>= pure
                    . (>>= validateGatewayList
                        100
                        validateGatewayAccount)
        env = MailToolsEnv
            { mailToolsToolEnv = toolEnv
            , mailToolsLimits = defaultMailToolLimits
            , mailToolsListAccounts = listAccounts
            , mailToolsListMailboxes = \accountId _ ->
                invoke mailListMailboxesToolName
                    (object ["account_id" .= accountId])
                    >>= pure
                        . (>>= validateGatewayList
                            200
                            validateGatewayMailbox)
            , mailToolsSearch = \request ->
                invoke mailSearchToolName (Aeson.toJSON request)
                    >>= pure
                        . (>>= validateGatewayList
                            (min 50 request.mailSearchLimit)
                            validateGatewayMessageSummary)
            , mailToolsGetMessage = \request _ ->
                invoke mailGetToolName (Aeson.toJSON request)
                    >>= pure . (>>= validateGatewayMessage)
            , mailToolsDownloadAttachment = \request maximum -> do
                prepared <-
                    invoke
                        mailDownloadAttachmentToolName
                        (Aeson.toJSON request)
                case prepared of
                    Left err -> pure (Left err)
                    Right descriptor ->
                        case validateGatewayDownload descriptor of
                            Left err -> pure (Left err)
                            Right checked -> download checked maximum
            , mailToolsCreateDraft = \request ->
                invoke mailCreateDraftToolName
                    (Aeson.toJSON request)
                    >>= pure . (>>= validateGatewayDraft)
            , mailToolsUpdateDraft = \request ->
                invoke mailUpdateDraftToolName
                    (Aeson.toJSON request)
                    >>= pure . (>>= validateGatewayDraft)
            , mailToolsReplyDraft = \request ->
                invoke mailReplyDraftToolName
                    (Aeson.toJSON request)
                    >>= pure . (>>= validateGatewayDraft)
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
verifyGatewayMailContract call = go 0 0 [] [] Nothing
  where
    go pageCount byteCount seenCursors collected cursor
        | pageCount >= maximumDiscoveryPages =
            pure (Left incompatibleMessage)
        | otherwise =
            call GatewayMailRequest
                { gatewayMailRequestTool = contractDiscoveryOperation
                , gatewayMailRequestArguments =
                    maybe (object []) (\value -> object ["cursor" .= value]) cursor
                }
                >>= \case
                    Left err -> pure (Left err)
                    Right response ->
                        let byteCount' =
                                byteCount
                                    + fromIntegral
                                        (LBS.length (Aeson.encode response))
                        in if byteCount' > maximumDiscoveryBytes
                            then pure (Left incompatibleMessage)
                            else case decodePage response of
                                Left err -> pure (Left err)
                                Right (pageTools, nextCursor) -> do
                                    let tools = collected <> pageTools
                                    if length tools > maximumDiscoveredTools
                                        then
                                                pure (Left incompatibleMessage)
                                        else case nextCursor of
                                            Nothing -> pure (verify tools)
                                            Just next
                                                | next `elem` seenCursors ->
                                                    pure
                                                        (Left
                                                            incompatibleMessage)
                                                | otherwise ->
                                                    go
                                                        (pageCount + 1)
                                                        byteCount'
                                                        (next : seenCursors)
                                                        tools
                                                        (Just next)

    decodePage (Object response) = do
        when
            (KeyMap.size response
                > if KeyMap.member "nextCursor" response then 2 else 1)
            (Left incompatibleMessage)
        listed <- maybe
            (Left incompatibleMessage)
            Right
            (KeyMap.lookup "tools" response)
        tools <- case listed of
            Array values -> Right (toList values)
            _ -> Left incompatibleMessage
        nextCursor <- case KeyMap.lookup "nextCursor" response of
            Nothing -> Right Nothing
            Just Null -> Right Nothing
            Just (String value)
                | not (Text.null value)
                , Text.length value <= maximumDiscoveryCursorLength ->
                    Right (Just value)
            _ -> Left incompatibleMessage
        Right (tools, nextCursor)
    decodePage _ = Left incompatibleMessage

    verify tools = do
        let actualNames =
                [ name
                | Object tool <- tools
                , Just (String name) <- [KeyMap.lookup "name" tool]
                ]
            expectedNames =
                [ name
                | Object tool <- mailMcpToolDefinitions
                , Just (String name) <- [KeyMap.lookup "name" tool]
                ]
        if sort actualNames /= sort expectedNames
                || length actualNames /= length tools
            then Left incompatibleMessage
            else traverse_ (verifyTool tools) mailMcpToolDefinitions

    verifyTool actual expected@(Object expectedObject) = do
        expectedName <- textField "name" expectedObject
        actualTool <- maybe
            (Left incompatibleMessage)
            Right
            (find (hasName expectedName) actual)
        if actualTool == expected
            then Right ()
            else Left incompatibleMessage
    verifyTool _ _ = Left incompatibleMessage

    hasName expectedName (Object value) =
        KeyMap.lookup "name" value == Just (String expectedName)
    hasName _ _ = False

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
    :: McpClient
    -> GatewayMailRequest
    -> IO (Either Text Value)
performGatewayMailRequest client request
    | request.gatewayMailRequestTool == contractDiscoveryOperation =
        case discoveryParameters request.gatewayMailRequestArguments of
            Left err -> pure (Left err)
            Right parameters ->
                rpc "tools/list" parameters
                    >>= pure . (>>= resultObject)
    | otherwise =
        rpc
            "tools/call"
            ( ("name" .= request.gatewayMailRequestTool)
                <> ("arguments" .= request.gatewayMailRequestArguments)
            )
            >>= pure . (>>= resultValue)
  where
    rpc methodName parameters = do
        attempted <- tryAny
            (requestMcpOnce
                client
                gatewayRequestTimeoutMicros
                methodName
                parameters)
        pure case attempted of
            Left _ -> Left gatewayUnavailable
            Right (Left _) -> Left gatewayUnavailable
            Right (Right raw)
                | BS.length (rawJsonBytes raw) > maximumMcpResponseBytes ->
                    Left gatewayUnavailable
                | otherwise ->
                    either
                        (const (Left
                            "The organization gateway returned invalid email data."))
                        Right
                        (Aeson.eitherDecodeStrict' (rawJsonBytes raw))

resultObject :: Value -> Either Text Value
resultObject value@(Object fields) =
    case KeyMap.lookup "tools" fields of
        Just (Array _) -> Right value
        _ -> Left incompatibleMessage
resultObject _ = Left incompatibleMessage

discoveryParameters :: Value -> Either Text Series
discoveryParameters (Object arguments)
    | KeyMap.size arguments > 1 = Left incompatibleMessage
    | otherwise =
        case KeyMap.lookup "cursor" arguments of
            Nothing -> Right mempty
            Just (String cursor)
                | not (Text.null cursor)
                , Text.length cursor <= maximumDiscoveryCursorLength ->
                    Right ("cursor" .= cursor)
            _ -> Left incompatibleMessage
discoveryParameters _ = Left incompatibleMessage

resultValue :: Value -> Either Text Value
resultValue = Right

gatewayMailMcpConfig :: GatewayCredential -> McpServerConfig
gatewayMailMcpConfig credential = McpServerConfig
    { mcpServerName = "gateway-email"
    , mcpServerUrl =
        Just
            ( Text.dropWhileEnd (== '/')
                (Text.strip credential.gatewayBaseUrl)
                <> "/mcp/email"
            )
    , mcpServerCommand = ""
    , mcpServerArgs = []
    , mcpServerCwd = Nothing
    , mcpServerEnv =
        [ ("MCP_ACCESS_TOKEN", Text.unpack credential.gatewayAccessToken) ]
    , mcpServerStartupTimeoutSeconds = 15
    , mcpServerRequestTimeoutSeconds = 15
    , mcpServerProtocol = McpProtocolLegacy
    }

validateGatewayAccount
    :: MailAccountSummary
    -> Either Text MailAccountSummary
validateGatewayAccount account = do
    reference <- validateGatewayReference account.mailAccountId
    when
        (account.mailAccountProvider
            `notElem` ["gmail", "microsoft", "imap"])
        (Left invalidGatewayData)
    case normalizeMailEmail account.mailAccountEmail of
        Right normalized
            | normalized == account.mailAccountEmail -> pure ()
        _ -> Left invalidGatewayData
    validateTextMaximum 32 account.mailAccountProvider
    validateTextMaximum 320 account.mailAccountEmail
    traverse_ (validateTextMaximum 160) account.mailAccountLabel
    pure MailAccountSummary
        { mailAccountId = reference
        , mailAccountProvider = account.mailAccountProvider
        , mailAccountEmail = account.mailAccountEmail
        , mailAccountLabel = account.mailAccountLabel
        , mailAccountEnabled = account.mailAccountEnabled
        , mailAccountVerified = account.mailAccountVerified
        }

validateGatewayMailbox
    :: MailboxSummary
    -> Either Text MailboxSummary
validateGatewayMailbox mailbox = do
    reference <- validateGatewayReference mailbox.mailMailboxId
    validateTextMaximum 512 mailbox.mailMailboxName
    traverse_ (validateTextMaximum 64) mailbox.mailMailboxRole
    traverse_ (validateNonnegative "unread_count")
        mailbox.mailMailboxUnreadCount
    pure mailbox { mailMailboxId = reference }

validateGatewayMessageSummary
    :: MailMessageSummary
    -> Either Text MailMessageSummary
validateGatewayMessageSummary message = do
    messageId <- validateGatewayReference message.mailMessageSummaryId
    threadId <-
        traverse validateGatewayReference message.mailMessageSummaryThreadId
    traverse_ (validateTextMaximum 2048) message.mailMessageSummarySubject
    traverse_ (validateTextMaximum 2048) message.mailMessageSummaryFrom
    traverse_ (validateTextMaximum 2048) message.mailMessageSummaryReplyTo
    traverse_ (validateTextMaximum 4096) message.mailMessageSummaryTo
    traverse_ (validateTextMaximum 128) message.mailMessageSummaryReceivedAt
    traverse_ (validateTextMaximum 4096) message.mailMessageSummarySnippet
    traverse_ (validateNonnegative "attachment_count")
        message.mailMessageSummaryAttachmentCount
    pure message
        { mailMessageSummaryId = messageId
        , mailMessageSummaryThreadId = threadId
        }

validateGatewayMessage :: MailMessage -> Either Text MailMessage
validateGatewayMessage message = do
    messageId <- validateGatewayReference message.mailMessageId
    threadId <- traverse validateGatewayReference message.mailMessageThreadId
    traverse_ (validateTextMaximum 2048) message.mailMessageSubject
    traverse_ (validateTextMaximum 2048) message.mailMessageFrom
    traverse_ (validateTextMaximum 2048) message.mailMessageReplyTo
    traverse_ (validateTextMaximum 4096) message.mailMessageTo
    traverse_ (validateTextMaximum 4096) message.mailMessageCc
    traverse_ (validateTextMaximum 128) message.mailMessageReceivedAt
    traverse_ (validateTextMaximum 128) message.mailMessageSentAt
    traverse_ (validateUtf8Maximum 49152) message.mailMessageBody
    attachments <-
        validateGatewayList
            200
            validateAttachment
            message.mailMessageAttachments
    pure message
        { mailMessageId = messageId
        , mailMessageThreadId = threadId
        , mailMessageAttachments = attachments
        }
  where
    validateAttachment attachment = do
        reference <- validateGatewayReference attachment.mailAttachmentId
        traverse_ (validateTextMaximum 1024) attachment.mailAttachmentFilename
        traverse_ (validateTextMaximum 255)
            attachment.mailAttachmentContentType
        traverse_ (validateNonnegative "size_bytes")
            attachment.mailAttachmentSizeBytes
        pure attachment { mailAttachmentId = reference }

validateGatewayDraft :: MailDraft -> Either Text MailDraft
validateGatewayDraft draft = do
    draftId <- validateGatewayReference draft.mailDraftId
    messageId <- traverse validateGatewayReference draft.mailDraftMessageId
    threadId <- traverse validateGatewayReference draft.mailDraftThreadId
    traverse_ (validateTextMaximum 1024) draft.mailDraftWarning
    pure draft
        { mailDraftId = draftId
        , mailDraftMessageId = messageId
        , mailDraftThreadId = threadId
        }

validateGatewayDownload
    :: MailAttachmentDownload
    -> Either Text MailAttachmentDownload
validateGatewayDownload descriptor = do
    reference <-
        validateDownloadReference descriptor.mailAttachmentDownloadRef
    traverse_ (validateTextMaximum 1024)
        descriptor.mailAttachmentDownloadFilename
    traverse_ (validateTextMaximum 255)
        descriptor.mailAttachmentDownloadContentType
    validateNonnegative
        "size_bytes"
        descriptor.mailAttachmentDownloadSizeBytes
    pure descriptor { mailAttachmentDownloadRef = reference }

validateGatewayList
    :: Int
    -> (value -> Either Text checked)
    -> [value]
    -> Either Text [checked]
validateGatewayList maximum validate values
    | length values > maximum =
        Left "The organization gateway returned too much email data."
    | otherwise = traverse validate values

validateGatewayReference :: Text -> Either Text Text
validateGatewayReference value
    | Text.null value
        || Text.length value > maximumGatewayReferenceLength
        || not (Text.all isBase64UrlCharacter value) =
            Left "The organization gateway returned an invalid email reference."
    | otherwise = Right value

validateTextMaximum :: Int -> Text -> Either Text ()
validateTextMaximum maximum value =
    when (Text.length value > maximum) (Left invalidGatewayData)

validateUtf8Maximum :: Int -> Text -> Either Text ()
validateUtf8Maximum maximum value =
    when
        (BS.length (TextEncoding.encodeUtf8 value) > maximum)
        (Left invalidGatewayData)

validateNonnegative :: Text -> Int -> Either Text ()
validateNonnegative _ value =
    when (value < 0) (Left invalidGatewayData)

downloadGatewayAttachment
    :: Manager
    -> GatewayCredential
    -> MailAttachmentDownload
    -> Int
    -> IO (Either Text MailAttachmentContent)
downloadGatewayAttachment manager credential descriptor requestedMaximum
    | descriptor.mailAttachmentDownloadSizeBytes < 0
        || descriptor.mailAttachmentDownloadSizeBytes > maximum =
            pure (Left "The attachment exceeded the configured download limit.")
    | otherwise =
        case validateDownloadReference
                descriptor.mailAttachmentDownloadRef of
            Right checkedRef
                | checkedRef == descriptor.mailAttachmentDownloadRef ->
                    download checkedRef
            _ -> pure (Left attachmentUnavailable)
  where
    maximum = max 1 requestedMaximum

    download checkedRef = do
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
                                , ( "X-Haskell-Agent-Email-Attachment"
                                  , TextEncoding.encodeUtf8 checkedRef
                                  )
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
                                readBoundedBody
                                    (min maximum
                                        (descriptor.mailAttachmentDownloadSizeBytes
                                            + 1))
                                    response.responseBody
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
        downloadUrl =
            Text.dropWhileEnd (== '/')
                (Text.strip credential.gatewayBaseUrl)
                <> "/v1/email/attachments"

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

invalidGatewayData :: Text
invalidGatewayData =
    "The organization gateway returned invalid email data."

validateDownloadReference :: Text -> Either Text Text
validateDownloadReference value
    | Text.null value
        || Text.length value > maximumDownloadReferenceLength
        || not (Text.all isBase64UrlCharacter value) =
            Left attachmentUnavailable
    | otherwise = Right value

isBase64UrlCharacter :: Char -> Bool
isBase64UrlCharacter character =
    isAscii character
        && (isAlphaNum character || character == '-' || character == '_')

maximumGatewayReferenceLength, maximumDownloadReferenceLength :: Int
maximumGatewayReferenceLength = 8192
maximumDownloadReferenceLength = 1024

maximumDiscoveryPages, maximumDiscoveredTools
    , maximumDiscoveryCursorLength, maximumDiscoveryBytes :: Int
maximumDiscoveryPages = 8
maximumDiscoveredTools = 8
maximumDiscoveryCursorLength = 1024
maximumDiscoveryBytes = 16 * 1024 * 1024

maximumMcpResponseBytes, gatewayRequestTimeoutMicros
    , gatewayAttachmentTimeoutMicros :: Int
-- Keep this aligned with agent-mcp's transport cap.  A valid bounded email
-- result can exceed one MiB because MCP mirrors structuredContent in content.
maximumMcpResponseBytes = 16 * 1024 * 1024
gatewayRequestTimeoutMicros = 15 * 1_000_000
gatewayAttachmentTimeoutMicros = gatewayRequestTimeoutMicros
