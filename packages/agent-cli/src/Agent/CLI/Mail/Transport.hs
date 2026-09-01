{-# LANGUAGE ScopedTypeVariables #-}

-- | Production transports for connected email accounts.
--
-- Provider responses are read incrementally into fixed-size buffers.  IMAP
-- literals are inspected for their advertised size before any literal bytes
-- are read.  Errors returned from this module never contain response bodies,
-- credentials, mailbox content, or exception text.
module Agent.CLI.Mail.Transport
    ( productionMailTransport
    , decodeImapMessageId
    , decodeGmailAttachmentRef
    , mailProviderStatusError
    , parseGmailMessageValue
    , encodeImapDraftId
    , decodeImapDraftId
    , parseGmailDraftValue
    , parseGraphDraftValue
    , parseImapAppendUid
    , parseImapMailboxListLine
    , parseMailReplyRecipient
    , imapUidHasFlag
    , validateMailReplyRecipient
    ) where

import Agent.CLI.Mail.Imap (withMailImapConnection)
import Agent.CLI.Mail.Mime
    ( ParsedMailAttachment(..)
    , mailMimeAttachmentContent
    , mailMimeAttachments
    , mailMimeTextBody
    , mailMimeTextBodyTruncated
    , parseMailMime
    , renderMailDraftMime
    )
import Agent.CLI.Mail.OAuth (refreshMailOAuthCredential)
import Agent.CLI.Mail.Store
import Agent.CLI.Mail.Tools
import Control.Applicative ((<|>))
import Control.Concurrent.Async (mapConcurrently)
import Control.Exception.Safe (SomeException, tryAny)
import Control.Monad (unless, when)
import qualified Data.Aeson as Aeson
import Data.Aeson ((.:), (.:?))
import Data.Aeson.Types (Parser, parseEither)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Base64.URL as Base64URL
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import Data.Char (isControl, isDigit, isSpace)
import Data.List (find, nub)
import Data.Maybe (catMaybes, fromMaybe, isJust, listToMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import qualified Data.Text.Encoding.Error as TextEncodingError
import Data.Time (Day, addDays, defaultTimeLocale, formatTime, parseTimeM)
import Network.Connection (Connection)
import qualified Network.Connection as Connection
import Network.HTTP.Client
    ( BodyReader
    , Manager
    , RequestBody(..)
    , brRead
    , checkResponse
    , method
    , parseRequest
    , requestBody
    , requestHeaders
    , responseBody
    , responseStatus
    , responseTimeout
    , responseTimeoutMicro
    , setQueryString
    , withResponse
    )
import Network.HTTP.Client.TLS (newTlsManager)
import Network.HTTP.Types
    ( Header
    , Status
    , hAuthorization
    , statusCode
    , statusIsSuccessful
    )
import Network.HTTP.Types.URI (urlEncode)
import System.IO.Unsafe (unsafePerformIO)
import Text.HTML.TagSoup (innerText, parseTags)

productionMailTransport :: MailTransport
productionMailTransport = MailTransport
    { mailTransportListMailboxes = listMailboxes
    , mailTransportSearch = searchMessages
    , mailTransportGetMessage = getMessage
    , mailTransportDownloadAttachment = downloadAttachment
    , mailTransportCreateDraft = createDraft
    , mailTransportUpdateDraft = updateDraft
    , mailTransportReplyDraft = replyDraft
    }

listMailboxes
    :: MailCredential -> Int -> IO (Either Text [MailboxSummary])
listMailboxes credential requestedMaximum =
    dispatchOAuth credential
        (\token -> gmailListMailboxes token maximum)
        (\token -> graphListMailboxes token maximum)
        (\settings password -> imapListMailboxes settings password maximum)
  where
    maximum = boundedCount 200 requestedMaximum

searchMessages
    :: MailCredential -> MailSearchRequest
    -> IO (Either Text [MailMessageSummary])
searchMessages credential request =
    dispatchOAuth credential
        (\token -> gmailSearch token request)
        (\token -> graphSearch token request)
        (\settings password -> imapSearch settings password request)

getMessage
    :: MailCredential -> MailGetRequest -> Int
    -> IO (Either Text MailMessage)
getMessage credential request requestedBodyMaximum =
    dispatchOAuth credential
        (\token -> gmailGet token request requestedBodyMaximum)
        (\token -> graphGet token request requestedBodyMaximum)
        (\settings password ->
            imapGet settings password request requestedBodyMaximum)

downloadAttachment
    :: MailCredential -> MailAttachmentRequest -> Int
    -> IO (Either Text MailAttachmentContent)
downloadAttachment credential request requestedMaximum =
    dispatchOAuth credential
        (\token -> gmailDownload token request maximum)
        (\token -> graphDownload token request maximum)
        (\settings password ->
            imapDownload settings password request maximum)
  where
    maximum = max 1 (min maximumAttachmentBytes requestedMaximum)

createDraft
    :: MailCredential -> MailCreateDraftRequest -> IO (Either Text MailDraft)
createDraft credential request =
    case validateMailDraftContent defaultMailToolLimits
            request.mailCreateDraftContent of
        Left err -> pure (Left err)
        Right content ->
            dispatchOAuth credential
                (\token -> gmailCreateDraft token sender content Nothing Nothing)
                (\token -> graphCreateDraft token content)
                (\settings password ->
                    imapCreateDraft settings password sender content)
  where
    sender = credential.mailCredentialAccount.mailAccountEmail

updateDraft
    :: MailCredential -> MailUpdateDraftRequest -> IO (Either Text MailDraft)
updateDraft credential request =
    case validateMailDraftContent defaultMailToolLimits
            request.mailUpdateDraftContent of
        Left err -> pure (Left err)
        Right content ->
            let checked = request { mailUpdateDraftContent = content }
            in dispatchOAuth credential
                (\token -> gmailUpdateDraft token sender
                    checked.mailUpdateDraftId checked.mailUpdateDraftContent)
                (\token -> graphUpdateDraft token checked.mailUpdateDraftId
                    checked.mailUpdateDraftContent)
                (\settings password ->
                    imapUpdateDraft settings password sender checked)
  where
    sender = credential.mailCredentialAccount.mailAccountEmail

replyDraft
    :: MailCredential -> MailReplyDraftRequest -> IO (Either Text MailDraft)
replyDraft credential request =
    case validateMailDraftContent defaultMailToolLimits MailDraftContent
            { mailDraftTo = request.mailReplyDraftTo
            , mailDraftCc = []
            , mailDraftBcc = []
            , mailDraftSubject = ""
            , mailDraftBody = request.mailReplyDraftBody
            } of
        Left err -> pure (Left err)
        Right checked
            | length checked.mailDraftTo /= 1 ->
                pure (Left "A reply draft requires exactly one recipient.")
            | otherwise ->
            let checkedRequest =
                    request
                        { mailReplyDraftTo = checked.mailDraftTo
                        , mailReplyDraftBody = checked.mailDraftBody
                        }
            in dispatchOAuth credential
                (\token -> gmailReplyDraft token sender checkedRequest)
                (\token -> graphReplyDraft token checkedRequest)
                (\settings password ->
                    imapReplyDraft settings password sender checkedRequest)
  where
    sender = credential.mailCredentialAccount.mailAccountEmail

dispatchOAuth
    :: MailCredential
    -> (Text -> IO (Either Text value))
    -> (Text -> IO (Either Text value))
    -> (MailImapSettings -> Text -> IO (Either Text value))
    -> IO (Either Text value)
dispatchOAuth credential gmail microsoft imap =
    case (credential.mailCredentialAccount.mailAccountProvider,
          credential.mailCredentialSecret) of
        (GmailProvider, MailOAuthSecret {}) ->
            withOAuthAccessToken credential gmail
        (MicrosoftProvider, MailOAuthSecret {}) ->
            withOAuthAccessToken credential microsoft
        (ImapProvider, MailImapSecret { mailImapPassword }) ->
            case credential.mailCredentialAccount.mailAccountImapSettings of
                Nothing -> pure (Left "The custom IMAP account is incomplete.")
                Just settings ->
                    trackOperationalState credential.mailCredentialAccount
                        (imap settings mailImapPassword)
        _ -> pure (Left "The email account credential is invalid.")

trackOperationalState
    :: MailAccount
    -> IO (Either Text value)
    -> IO (Either Text value)
trackOperationalState account action =
    action >>= \result -> do
        case result of
            Left err
                | reconnectRequired err ->
                    voidResult $ setMailAccountStateIfUnchanged account
                        MailNeedsReauthorization
                        (Just "provider_auth_failed")
                | imapAuthenticationFailed err ->
                    voidResult $ setMailAccountStateIfUnchanged account
                        MailConnectionError
                        (Just "imap_auth_failed")
                | imapCertificateFailed err ->
                    voidResult $ setMailAccountStateIfUnchanged account
                        MailConnectionError
                        (Just "imap_tls_failed")
            _ -> pure ()
        pure result
  where
    folded = Text.toCaseFold
    reconnectRequired =
        Text.isInfixOf "must be reconnected" . folded
    imapAuthenticationFailed =
        Text.isPrefixOf "imap authentication failed" . folded
    imapCertificateFailed =
        Text.isInfixOf "tls certificate could not be verified" . folded

voidResult :: IO (Either Text ()) -> IO ()
voidResult action = do
    _ <- action
    pure ()

withOAuthAccessToken
    :: MailCredential
    -> (Text -> IO (Either Text value))
    -> IO (Either Text value)
withOAuthAccessToken credential action =
    refreshMailOAuthCredential credential >>= \case
        Left err -> pure (Left err)
        Right refreshed ->
            case refreshed.mailCredentialSecret of
                MailOAuthSecret { mailOAuthAccessToken }
                    | not (Text.null mailOAuthAccessToken) ->
                        trackOperationalState
                            refreshed.mailCredentialAccount
                            (action mailOAuthAccessToken)
                _ -> pure (Left "The email account must be reconnected.")

-- HTTP ----------------------------------------------------------------------

{-# NOINLINE mailHttpManager #-}
mailHttpManager :: Manager
mailHttpManager = unsafePerformIO newTlsManager

providerGet
    :: Text
    -> Text
    -> [(BS.ByteString, Maybe BS.ByteString)]
    -> [Header]
    -> Int
    -> IO (Either Text BS.ByteString)
providerGet token rawUrl query extraHeaders maximum = do
    attempted <- tryAny do
        initial <- parseRequest (Text.unpack rawUrl)
        let request = setQueryString query initial
                { method = "GET"
                , requestHeaders =
                    (hAuthorization, "Bearer " <> TextEncoding.encodeUtf8 token)
                        : extraHeaders
                , responseTimeout = responseTimeoutMicro httpTimeoutMicros
                , checkResponse = \_ _ -> pure ()
                }
        withResponse request mailHttpManager \response -> do
            if statusIsSuccessful response.responseStatus
                then readBodyBounded maximum response.responseBody
                else pure
                    (Left (mailProviderStatusError response.responseStatus))
    pure case attempted of
        Left (_ :: SomeException) ->
            Left "The email provider request could not be completed."
        Right result -> result

providerJson
    :: Text
    -> Text
    -> [(BS.ByteString, Maybe BS.ByteString)]
    -> [Header]
    -> Int
    -> IO (Either Text Aeson.Value)
providerJson token rawUrl query headers maximum =
    providerGet token rawUrl query headers maximum >>= \case
        Left err -> pure (Left err)
        Right bytes ->
            pure case Aeson.eitherDecodeStrict' bytes of
                Left _ -> Left "The email provider returned invalid JSON."
                Right value -> Right value

-- | Bounded JSON mutation helper.  The mail transport deliberately has no
-- generic send endpoint: callers name only the draft-only provider paths
-- below, and OAuth scopes never include Graph Mail.Send.
providerJsonWrite
    :: BS.ByteString
    -> Text
    -> Text
    -> [Header]
    -> Aeson.Value
    -> Int
    -> IO (Either Text Aeson.Value)
providerJsonWrite verb token rawUrl extraHeaders payload maximum = do
    attempted <- tryAny do
        initial <- parseRequest (Text.unpack rawUrl)
        let request = initial
                { method = verb
                , requestBody = RequestBodyBS (LBS.toStrict (Aeson.encode payload))
                , requestHeaders =
                    (hAuthorization, "Bearer " <> TextEncoding.encodeUtf8 token)
                        : ("Content-Type", "application/json") : extraHeaders
                , responseTimeout = responseTimeoutMicro httpTimeoutMicros
                , checkResponse = \_ _ -> pure ()
                }
        withResponse request mailHttpManager \response -> do
            if statusIsSuccessful response.responseStatus
                then readBodyBounded maximum response.responseBody >>= \case
                    Left err -> pure (Left err)
                    Right bytes
                        | BS.null bytes -> pure (Right (Aeson.object []))
                        | otherwise -> pure case Aeson.eitherDecodeStrict' bytes of
                            Left _ -> Left "The email provider returned invalid JSON."
                            Right value -> Right value
                else pure (Left (mailProviderStatusError response.responseStatus))
    pure case attempted of
        Left (_ :: SomeException) ->
            Left "The email provider request could not be completed."
        Right result -> result

readBodyBounded :: Int -> BodyReader -> IO (Either Text BS.ByteString)
readBodyBounded requestedMaximum reader =
    go [] 0
  where
    maximum = max 1 requestedMaximum
    go chunks size = brRead reader >>= \chunk ->
        if BS.null chunk
            then pure (Right (BS.concat (reverse chunks)))
            else
                let size' = size + BS.length chunk
                in if size' > maximum
                    then pure (Left "The email provider response was too large.")
                    else go (chunk : chunks) size'

mailProviderStatusError :: Status -> Text
mailProviderStatusError status
    | statusCode status == 401 =
        "The email account must be reconnected."
    | statusCode status == 403 =
        "The email provider denied this request. Check the account's mail permissions or organization policy."
    | statusCode status == 404 =
        "The requested email item was not found."
    | statusCode status == 429 =
        "The email provider is temporarily rate limiting requests."
    | otherwise = "The email provider request failed."

parseProvider :: Text -> (Aeson.Value -> Parser value) -> Aeson.Value
    -> Either Text value
parseProvider message parser =
    either (const (Left message)) Right . parseEither parser

component :: Text -> Text
component =
    TextEncoding.decodeUtf8 . urlEncode True . TextEncoding.encodeUtf8

encodeProviderDraftId :: Text -> Text -> Text
encodeProviderDraftId prefix =
    (prefix <>)
        . TextEncoding.decodeUtf8
        . Base64URL.encodeUnpadded
        . TextEncoding.encodeUtf8

decodeProviderDraftId :: Text -> Text -> Either Text Text
decodeProviderDraftId prefix value = do
    encoded <- maybe
        (Left "The email draft reference is invalid.")
        Right
        (Text.stripPrefix prefix value)
    bytes <- either
        (const (Left "The email draft reference is invalid."))
        Right
        (Base64URL.decodeUnpadded (TextEncoding.encodeUtf8 encoded))
    decoded <- either
        (const (Left "The email draft reference is invalid."))
        Right
        (TextEncoding.decodeUtf8' bytes)
    if validProviderIdentifier decoded
        then Right decoded
        else Left "The email draft reference is invalid."

validProviderIdentifier :: Text -> Bool
validProviderIdentifier value =
    not (Text.null value)
        && utf8Length value <= maximumProviderIdentifierBytes
        && not (Text.any isControl value)

-- Gmail ---------------------------------------------------------------------

gmailBase :: Text
gmailBase = "https://gmail.googleapis.com/gmail/v1/users/me"

gmailListMailboxes :: Text -> Int -> IO (Either Text [MailboxSummary])
gmailListMailboxes token maximum =
    providerJson token (gmailBase <> "/labels") [] [] jsonResponseMaximum
        >>= pure . (>>= parseProvider
            "Gmail returned invalid mailbox data."
            (Aeson.withObject "Gmail labels" \object -> do
                labels <- object .:? "labels" Aeson..!= []
                take maximum <$> traverse parseLabel labels))
  where
    parseLabel = Aeson.withObject "Gmail label" \label ->
        MailboxSummary
            <$> label .: "id"
            <*> label .: "name"
            <*> (fmap gmailRole <$> label .:? "type")
            <*> label .:? "messagesUnread"
    gmailRole kind
        | (kind :: Text) == "system" = "system"
        | otherwise = "label"

gmailSearch
    :: Text -> MailSearchRequest -> IO (Either Text [MailMessageSummary])
gmailSearch token request =
    providerJson token (gmailBase <> "/messages")
        ([("maxResults", Just (BS8.pack (show maximum))),
          ("q", nonEmptyBytes (gmailQuery request))]
            <> maybe [] (\mailbox ->
                [("labelIds", Just (TextEncoding.encodeUtf8 mailbox))])
                request.mailSearchMailboxId)
        [] jsonResponseMaximum >>= \case
            Left err -> pure (Left err)
            Right value ->
                case parseProvider "Gmail returned invalid search data."
                    parseIds value of
                    Left err -> pure (Left err)
                    Right ids -> do
                        batches <- traverse
                            (mapConcurrently (gmailSummary token))
                            (chunksOf gmailSummaryConcurrency ids)
                        pure (sequence (concat batches))
  where
    maximum = boundedCount 50 request.mailSearchLimit
    parseIds = Aeson.withObject "Gmail search" \object -> do
        messages <- object .:? "messages" Aeson..!= []
        take maximum <$> traverse
            (Aeson.withObject "Gmail message id" (.: "id")) messages

gmailSummary :: Text -> Text -> IO (Either Text MailMessageSummary)
gmailSummary token messageId =
    providerJson token (gmailBase <> "/messages/" <> component messageId)
        [ ("format", Just "full")
        , ("fields", Just gmailSummaryFields)
        ]
        [] gmailMetadataMaximum
        >>= pure . (>>= parseProvider
            "Gmail returned invalid message metadata." parseGmailSummary)

gmailGet
    :: Text -> MailGetRequest -> Int -> IO (Either Text MailMessage)
gmailGet token request requestedMaximum =
    providerJson token
        (gmailBase <> "/messages/" <> component request.mailGetMessageId)
        [("format", Just "full")] []
        (messageResponseMaximum requestedMaximum)
        >>= pure . (>>= parseGmailMessageValue requestedMaximum)

gmailDownload
    :: Text -> MailAttachmentRequest -> Int
    -> IO (Either Text MailAttachmentContent)
gmailDownload token request maximum =
    let (providerAttachmentId, filename, contentType) =
            decodeGmailAttachmentRef request.mailAttachmentRequestId
    in
    providerJson token
        (gmailBase <> "/messages/" <> component request.mailAttachmentMessageId
            <> "/attachments/" <> component providerAttachmentId)
        [] [] (base64ResponseMaximum maximum) >>= \case
            Left err -> pure (Left err)
            Right value ->
                pure $ parseProvider "Gmail returned invalid attachment data."
                    (Aeson.withObject "Gmail attachment" \object -> do
                        encoded <- object .: "data"
                        bytes <- either
                            (const (fail "invalid attachment encoding"))
                            pure
                            (Base64URL.decode
                                (TextEncoding.encodeUtf8 encoded))
                        when (BS.length bytes > maximum)
                            (fail "attachment too large")
                        pure MailAttachmentContent
                            { mailDownloadedAttachmentFilename = filename
                            , mailDownloadedAttachmentContentType = contentType
                            , mailDownloadedAttachmentBytes = bytes
                            })
                    value

graphCreateDraft :: Text -> MailDraftContent -> IO (Either Text MailDraft)
graphCreateDraft token content =
    providerJsonWrite "POST" token (graphBase <> "/messages") []
        (graphDraftPayload content) jsonResponseMaximum
        >>= pure . (>>= parseGraphDraft)

graphUpdateDraft :: Text -> Text -> MailDraftContent -> IO (Either Text MailDraft)
graphUpdateDraft token encodedDraftId content =
    case decodeProviderDraftId "graph-draft:" encodedDraftId of
        Left err -> pure (Left err)
        Right draftId ->
            providerJson token (graphBase <> "/messages/" <> component draftId)
                [("$select", Just "id,isDraft,conversationId")] []
                jsonResponseMaximum >>= \case
                    Left err -> pure (Left err)
                    Right existing ->
                        case parseProvider "Microsoft returned invalid draft data."
                                parseIsDraft existing of
                            Left err -> pure (Left err)
                            Right False -> pure (Left
                                "That message is not a draft and cannot be changed.")
                            Right True ->
                                providerJsonWrite "PATCH" token
                                    (graphBase <> "/messages/" <> component draftId) []
                                    (graphDraftPayload content) jsonResponseMaximum
                                    >>= pure . (>>= parseGraphDraft)

graphReplyDraft :: Text -> MailReplyDraftRequest -> IO (Either Text MailDraft)
graphReplyDraft token request =
    graphReplyRecipient token request.mailReplyDraftMessageId >>= \case
        Left err -> pure (Left err)
        Right recipient ->
            case ensureExpectedReplyRecipient request.mailReplyDraftTo recipient of
                Left err -> pure (Left err)
                Right () ->
                    providerJsonWrite "POST" token
                        (graphBase <> "/messages/"
                            <> component request.mailReplyDraftMessageId
                            <> "/createReply")
                        [] (Aeson.object
                            ["comment" Aeson..= request.mailReplyDraftBody])
                        jsonResponseMaximum
                        >>= pure . (>>= parseGraphDraft)

graphReplyRecipient :: Text -> Text -> IO (Either Text Text)
graphReplyRecipient token messageId =
    providerJson token (graphBase <> "/messages/" <> component messageId)
        [("$select", Just "replyTo,from")] [] jsonResponseMaximum
        >>= pure . (>>= parseProvider
            "Microsoft returned invalid reply metadata."
            (Aeson.withObject "Graph reply metadata" \object -> do
                replyTo <- object .:? "replyTo" Aeson..!= []
                from <- object .:? "from"
                addresses <- traverse parseGraphRecipient replyTo
                raw <- case addresses of
                    [address] -> pure address
                    [] -> maybe
                        (fail "missing reply recipient")
                        parseGraphRecipient
                        from
                    _ -> fail "ambiguous reply recipient"
                either (const (fail "invalid reply recipient")) pure
                    (normalizeMailEmail raw)))
  where
    parseGraphRecipient = Aeson.withObject "Graph recipient" \recipient -> do
        emailAddress <- recipient .: "emailAddress"
        Aeson.withObject "Graph email address" (.: "address") emailAddress

graphDraftPayload :: MailDraftContent -> Aeson.Value
graphDraftPayload content = Aeson.object
    [ "toRecipients" Aeson..= map graphRecipientValue content.mailDraftTo
    , "ccRecipients" Aeson..= map graphRecipientValue content.mailDraftCc
    , "bccRecipients" Aeson..= map graphRecipientValue content.mailDraftBcc
    , "subject" Aeson..= content.mailDraftSubject
    , "body" Aeson..= Aeson.object
        [ "contentType" Aeson..= ("text" :: Text)
        , "content" Aeson..= content.mailDraftBody
        ]
    ]

graphRecipientValue :: Text -> Aeson.Value
graphRecipientValue address = Aeson.object
    ["emailAddress" Aeson..= Aeson.object ["address" Aeson..= address]]

parseIsDraft :: Aeson.Value -> Parser Bool
parseIsDraft = Aeson.withObject "Graph message" \object ->
    object .:? "isDraft" Aeson..!= False

parseGraphDraft :: Aeson.Value -> Either Text MailDraft
parseGraphDraft = parseProvider "Microsoft returned invalid draft data."
    (Aeson.withObject "Graph draft" \object -> do
        draftId <- object .: "id"
        unless (validProviderIdentifier draftId) (fail "invalid draft id")
        isDraft <- object .:? "isDraft" Aeson..!= False
        unless isDraft (fail "message is not a draft")
        MailDraft
            <$> pure (encodeProviderDraftId "graph-draft:" draftId)
            <*> pure (Just draftId)
            <*> object .:? "conversationId"
            <*> pure Nothing)

parseGraphDraftValue :: Aeson.Value -> Either Text MailDraft
parseGraphDraftValue = parseGraphDraft

gmailCreateDraft
    :: Text -> Text -> MailDraftContent -> Maybe Text
    -> Maybe (Text, Maybe Text)
    -> IO (Either Text MailDraft)
gmailCreateDraft token sender content threadId replyHeaders =
    providerJsonWrite "POST" token (gmailBase <> "/drafts") [] payload
        jsonResponseMaximum >>= pure . (>>= parseGmailDraft)
  where
    payload = Aeson.object
        [ "message" Aeson..= Aeson.object
            ([ "raw" Aeson..= TextEncoding.decodeUtf8
                (Base64URL.encodeUnpadded
                    (renderMailDraftMime sender content replyHeaders))
             ] <> maybe [] (\value -> ["threadId" Aeson..= value]) threadId)
        ]

gmailUpdateDraft
    :: Text -> Text -> Text -> MailDraftContent -> IO (Either Text MailDraft)
gmailUpdateDraft token sender encodedDraftId content =
    case decodeProviderDraftId "gmail-draft:" encodedDraftId of
        Left err -> pure (Left err)
        Right draftId -> gmailUpdateRawDraft token sender draftId content

gmailUpdateRawDraft
    :: Text -> Text -> Text -> MailDraftContent -> IO (Either Text MailDraft)
gmailUpdateRawDraft token sender draftId content =
    -- Preserve Gmail's thread association when replacing a reply draft. A
    -- user-supplied draft id is never accepted as a send-capable message id.
    providerJson token (gmailBase <> "/drafts/" <> component draftId)
        [("format", Just "metadata"),
         ("metadataHeaders", Just "In-Reply-To"),
         ("metadataHeaders", Just "References")]
        [] gmailMetadataMaximum >>= \case
            Left err -> pure (Left err)
            Right existing ->
                case parseProvider "Gmail returned invalid draft data."
                        parseGmailDraftThread existing of
                    Left err -> pure (Left err)
                    Right (threadId, replyHeaders) ->
                        providerJsonWrite "PUT" token
                            (gmailBase <> "/drafts/" <> component draftId)
                            [] (Aeson.object
                                ["message" Aeson..= Aeson.object
                                    ([ "raw" Aeson..= TextEncoding.decodeUtf8
                                        (Base64URL.encodeUnpadded
                                            (renderMailDraftMime
                                                sender content replyHeaders))
                                     ] <> maybe [] (\value ->
                                        ["threadId" Aeson..= value]) threadId)])
                            jsonResponseMaximum >>= pure . (>>= parseGmailDraft)

gmailReplyDraft
    :: Text -> Text -> MailReplyDraftRequest -> IO (Either Text MailDraft)
gmailReplyDraft token sender request =
    providerJson token
        (gmailBase <> "/messages/" <> component request.mailReplyDraftMessageId)
        [("format", Just "metadata"),
         ("metadataHeaders", Just "Subject"),
         ("metadataHeaders", Just "Message-ID"),
         ("metadataHeaders", Just "References"),
         ("metadataHeaders", Just "Reply-To"),
         ("metadataHeaders", Just "From")]
        [] gmailMetadataMaximum >>= \case
            Left err -> pure (Left err)
            Right value ->
                pure (parseProvider "Gmail returned invalid reply metadata."
                    parseReplyMetadata value) >>= \case
                    Left err -> pure (Left err)
                    Right (threadId, subject, messageId, references, recipient) ->
                        case ensureExpectedReplyRecipient
                                request.mailReplyDraftTo recipient of
                            Left err -> pure (Left err)
                            Right () ->
                                gmailCreateDraft token sender MailDraftContent
                                    { mailDraftTo = [recipient]
                                    , mailDraftCc = []
                                    , mailDraftBcc = []
                                    , mailDraftSubject = safeReplySubject subject
                                    , mailDraftBody = request.mailReplyDraftBody
                                    }
                                    (Just threadId)
                                    (Just
                                        ( messageId
                                        , appendReference references messageId
                                        ))
  where
    parseReplyMetadata = Aeson.withObject "Gmail reply message" \object -> do
        threadId <- object .: "threadId"
        payload <- object .: "payload"
        headers <- Aeson.withObject "Gmail headers" (.:? "headers") payload
        let values = fromMaybe [] headers
            lookupValue wanted =
                listToMaybe [value | header <- values
                    , Right (name, value) <- [parseEither
                        (Aeson.withObject "Gmail header" \h ->
                            (,) <$> h .: "name" <*> h .: "value") header]
                    , Text.toCaseFold name == Text.toCaseFold wanted]
        messageId <- maybe (fail "missing Message-ID") pure (lookupValue "Message-ID")
        checkedMessageId <- either (const (fail "invalid Message-ID")) pure
            (validateMessageIdHeader messageId)
        recipient <- either (const (fail "invalid reply recipient")) pure
            (replyRecipientFromHeaders
                (lookupValue "Reply-To")
                (lookupValue "From"))
        pure
            ( threadId
            , fromMaybe "" (lookupValue "Subject")
            , checkedMessageId
            , validateReferencesHeader =<< lookupValue "References"
            , recipient
            )

parseGmailDraft :: Aeson.Value -> Either Text MailDraft
parseGmailDraft = parseProvider "Gmail returned invalid draft data."
    (Aeson.withObject "Gmail draft" \object -> do
        draftId <- object .: "id"
        unless (validProviderIdentifier draftId) (fail "invalid draft id")
        message <- object .: "message"
        messageId <- Aeson.withObject "Gmail draft message" (.:? "id") message
        threadId <- Aeson.withObject "Gmail draft message" (.:? "threadId") message
        pure MailDraft
            { mailDraftId = encodeProviderDraftId "gmail-draft:" draftId
            , mailDraftMessageId = messageId
            , mailDraftThreadId = threadId
            , mailDraftWarning = Nothing
            })

parseGmailDraftValue :: Aeson.Value -> Either Text MailDraft
parseGmailDraftValue = parseGmailDraft

imapAwaitContinuation :: Connection -> Text -> IO ()
imapAwaitContinuation connection tag =
    go maximumImapResponseLines 0
  where
    go remaining totalBytes
        | remaining <= 0 =
            failText "The IMAP response exceeded the safe limit."
        | otherwise = do
            line <- imapReadLine connection
            let totalBytes' = totalBytes + utf8Length line
            when (totalBytes' > maximumImapResponseBytes) $
                failText "The IMAP response exceeded the safe size limit."
            if "+" `Text.isPrefixOf` line
                then pure ()
                else if (Text.toCaseFold tag <> " ") `Text.isPrefixOf`
                        Text.toCaseFold line
                    then do
                        ensureTaggedOk tag line
                        failText "The IMAP server omitted the APPEND continuation."
                    else case imapLiteralLength line of
                        Just _ -> failText
                            "The IMAP server returned an unexpected literal."
                        Nothing -> go (remaining - 1) totalBytes'

imapReadTaggedCompletion :: Connection -> Text -> IO Text
imapReadTaggedCompletion connection tag =
    go maximumImapResponseLines 0
  where
    go remaining totalBytes
        | remaining <= 0 =
            failText "The IMAP response exceeded the safe limit."
        | otherwise = do
            line <- imapReadLine connection
            let totalBytes' = totalBytes + utf8Length line
            when (totalBytes' > maximumImapResponseBytes) $
                failText "The IMAP response exceeded the safe size limit."
            if (Text.toCaseFold tag <> " ") `Text.isPrefixOf`
                    Text.toCaseFold line
                then ensureTaggedOk tag line >> pure line
                else case imapLiteralLength line of
                    Just _ -> failText
                        "The IMAP server returned an unexpected literal."
                    Nothing -> go (remaining - 1) totalBytes'

parseGmailDraftThread
    :: Aeson.Value -> Parser (Maybe Text, Maybe (Text, Maybe Text))
parseGmailDraftThread = Aeson.withObject "Gmail draft" \object -> do
    message <- object .: "message"
    threadId <- Aeson.withObject "Gmail draft message" (.:? "threadId") message
    payload <- Aeson.withObject "Gmail draft message" (.:? "payload") message
    headers <- maybe (pure []) (Aeson.withObject "Gmail payload"
        (\payloadObject -> payloadObject .:? "headers" Aeson..!= [])) payload
    let values = headers
        lookupValue wanted =
            listToMaybe [value | header <- values
                , Right (name, value) <- [parseEither
                    (Aeson.withObject "Gmail header" \h ->
                        (,) <$> h .: "name" <*> h .: "value") header]
                , Text.toCaseFold name == Text.toCaseFold wanted]
    replyHeaders <- case
        (lookupValue "In-Reply-To", lookupValue "References") of
        (Nothing, Nothing) -> pure Nothing
        (Nothing, Just _) -> fail "References without In-Reply-To"
        (Just rawInReplyTo, rawReferences) -> do
            inReplyTo <- either (const (fail "invalid In-Reply-To")) pure
                (validateMessageIdHeader rawInReplyTo)
            references <- case rawReferences of
                Nothing -> pure Nothing
                Just rawReferences -> maybe
                    (fail "invalid References")
                    (pure . Just)
                    (validateReferencesHeader rawReferences)
            pure (Just (inReplyTo, references))
    pure (threadId, replyHeaders)

replySubject :: Text -> Text
replySubject subject
    | "re:" `Text.isPrefixOf` Text.toCaseFold (Text.strip subject) = subject
    | otherwise = "Re: " <> subject

-- Provider message headers are untrusted mailbox data. Bound and remove
-- controls before inserting a derived subject into a new RFC 5322 header.
safeReplySubject :: Text -> Text
safeReplySubject =
    truncateTextBytes maximumReplySubjectBytes
        . replySubject
        . Text.filter (not . isControl)

replyRecipientFromHeaders :: Maybe Text -> Maybe Text -> Either Text Text
replyRecipientFromHeaders replyTo from =
    case replyTo of
        Just value -> parseSingleMailbox value
        Nothing -> maybe
            (Left "The source email has no reply recipient.")
            parseSingleMailbox
            from

parseMailReplyRecipient :: Maybe Text -> Maybe Text -> Either Text Text
parseMailReplyRecipient = replyRecipientFromHeaders

ensureExpectedReplyRecipient :: [Text] -> Text -> Either Text ()
ensureExpectedReplyRecipient expected actual = do
    checkedExpected <- case expected of
        [recipient] -> normalizeMailEmail recipient
        _ -> Left "A reply draft requires exactly one approved recipient."
    checkedActual <- normalizeMailEmail actual
    unlessEither
        (checkedExpected == checkedActual)
        "The approved recipient does not match the source message's reply address."

validateMailReplyRecipient :: [Text] -> Text -> Either Text ()
validateMailReplyRecipient = ensureExpectedReplyRecipient

parseSingleMailbox :: Text -> Either Text Text
parseSingleMailbox raw
    | Text.null stripped
        || utf8Length stripped > maximumMailboxHeaderBytes
        || Text.any isControl stripped =
            Left "The source email has an invalid reply recipient."
    | Text.null angleSuffix =
        normalizeMailEmail stripped
    | otherwise = do
        let afterOpen = Text.drop 1 angleSuffix
            (candidate, afterClose) = Text.breakOn ">" afterOpen
        unlessEither
            ( not (Text.null afterClose)
                && Text.null (Text.strip (Text.drop 1 afterClose))
                && not (Text.any (`elem` ['<', '>']) display)
                && not (Text.any (`elem` ['<', '>']) candidate)
            )
            "The source email has an ambiguous reply recipient."
        normalizeMailEmail candidate
  where
    stripped = Text.strip raw
    (display, angleSuffix) = Text.breakOn "<" stripped

validateMessageIdHeader :: Text -> Either Text Text
validateMessageIdHeader raw =
    case Text.stripPrefix "<" stripped >>= Text.stripSuffix ">" of
        Just inner
            | not (Text.null inner)
            , utf8Length stripped <= maximumReplyHeaderBytes
            , Text.all valid inner ->
                Right stripped
        _ -> Left "The source email has invalid reply metadata."
  where
    stripped = Text.strip raw
    valid character =
        not (isControl character || isSpace character
            || character == '<' || character == '>')

validateReferencesHeader :: Text -> Maybe Text
validateReferencesHeader value = do
    checked <- traverse (eitherToMaybe . validateMessageIdHeader)
        (Text.words value)
    let joined = Text.unwords checked
    if not (null checked) && utf8Length joined <= maximumReplyHeaderBytes
        then Just joined
        else Nothing

appendReference :: Maybe Text -> Text -> Maybe Text
appendReference references messageId =
    let joined = maybe messageId (<> " " <> messageId) references
    in validateReferencesHeader joined <|> Just messageId

eitherToMaybe :: Either left value -> Maybe value
eitherToMaybe = either (const Nothing) Just

unlessEither :: Bool -> Text -> Either Text ()
unlessEither condition message
    | condition = Right ()
    | otherwise = Left message

gmailQuery :: MailSearchRequest -> Text
gmailQuery request = Text.unwords . catMaybes $
    [ quoteTerm <$> request.mailSearchQuery
    , fieldTerm "from" <$> request.mailSearchFrom
    , fieldTerm "to" <$> request.mailSearchTo
    , fieldTerm "subject" <$> request.mailSearchSubject
    , ("after:" <>) . Text.filter (/= '-') <$> request.mailSearchAfter
    , ("before:" <>) . Text.filter (/= '-') . nextIsoDay
        <$> request.mailSearchBefore
    , fmap (\present -> if present then "has:attachment"
                        else "-has:attachment")
        request.mailSearchHasAttachments
    ]
  where
    fieldTerm field value = field <> ":" <> quoteTerm value
    quoteTerm value =
        "\"" <> Text.replace "\"" "\\\"" (Text.replace "\\" "\\\\" value) <> "\""

parseGmailSummary :: Aeson.Value -> Parser MailMessageSummary
parseGmailSummary = Aeson.withObject "Gmail message" \object -> do
    messageId <- object .: "id"
    threadId <- object .:? "threadId"
    snippet <- object .:? "snippet"
    payload <- object .:? "payload"
    let headers = maybe [] gmailHeaders payload
        attachments = maybe [] gmailAttachments payload
    pure MailMessageSummary
        { mailMessageSummaryId = messageId
        , mailMessageSummaryThreadId = threadId
        , mailMessageSummarySubject = headerValue "subject" headers
        , mailMessageSummaryFrom = headerValue "from" headers
        , mailMessageSummaryReplyTo = effectiveReplyRecipient headers
        , mailMessageSummaryTo = headerValue "to" headers
        , mailMessageSummaryReceivedAt = headerValue "date" headers
        , mailMessageSummarySnippet = snippet
        , mailMessageSummaryHasAttachments = not (null attachments)
        , mailMessageSummaryAttachmentCount =
            if null attachments then Just 0 else Just (length attachments)
        }

parseGmailMessage :: Int -> Aeson.Value -> Parser MailMessage
parseGmailMessage bodyMaximum value =
    Aeson.withObject "Gmail message" (\object -> do
        messageId <- object .: "id"
        threadId <- object .:? "threadId"
        payload <- object .:? "payload"
        let headers = maybe [] gmailHeaders payload
            attachments = maybe [] gmailAttachments payload
            bodyResult = payload >>= gmailTextBody bodyMaximum
        pure MailMessage
            { mailMessageId = messageId
            , mailMessageThreadId = threadId
            , mailMessageSubject = headerValue "subject" headers
            , mailMessageFrom = headerValue "from" headers
            , mailMessageReplyTo = effectiveReplyRecipient headers
            , mailMessageTo = headerValue "to" headers
            , mailMessageCc = headerValue "cc" headers
            , mailMessageReceivedAt = headerValue "date" headers
            , mailMessageSentAt = headerValue "date" headers
            , mailMessageBody = fst <$> bodyResult
            , mailMessageBodyTruncated = maybe False snd bodyResult
            , mailMessageAttachments = attachments
            }) value

parseGmailMessageValue :: Int -> Aeson.Value -> Either Text MailMessage
parseGmailMessageValue requestedMaximum =
    parseProvider
        "Gmail returned an invalid message."
        (parseGmailMessage (max 1 requestedMaximum))

gmailHeaders :: Aeson.Value -> [(Text, Text)]
gmailHeaders value =
    fromMaybe [] $ either (const Nothing) Just $ parseEither
        (Aeson.withObject "Gmail payload" \payload -> do
            headers <- payload .:? "headers" Aeson..!= []
            traverse
                (Aeson.withObject "Gmail header" \header ->
                    (,) <$> header .: "name" <*> header .: "value")
                headers)
        value

headerValue :: Text -> [(Text, Text)] -> Maybe Text
headerValue name =
    fmap snd . find ((== Text.toCaseFold name) . Text.toCaseFold . fst)

effectiveReplyRecipient :: [(Text, Text)] -> Maybe Text
effectiveReplyRecipient headers =
    eitherToMaybe (replyRecipientFromHeaders
        (headerValue "reply-to" headers)
        (headerValue "from" headers))

gmailAttachments :: Aeson.Value -> [MailAttachment]
gmailAttachments value =
    either (const []) id $ parseEither parseParts value
  where
    parseParts = Aeson.withObject "Gmail payload" \part -> do
        filename <- part .:? "filename" Aeson..!= ""
        contentType <- part .:? "mimeType"
        body <- part .:? "body"
        nested <- part .:? "parts" Aeson..!= []
        direct <- case body of
            Nothing -> pure []
            Just bodyValue -> Aeson.withObject "Gmail body" (\bodyObject -> do
                attachmentId <- bodyObject .:? "attachmentId"
                size <- bodyObject .:? "size"
                pure case attachmentId of
                    Nothing -> []
                    Just identifier ->
                        let safeFilename = safeGmailMetadata filename
                            safeContentType =
                                contentType >>= safeGmailMetadata
                        in [MailAttachment
                            (encodeGmailAttachmentRef
                                identifier
                                safeFilename
                                safeContentType)
                            safeFilename safeContentType size]) bodyValue
        children <- concat <$> traverse parseParts nested
        pure (direct <> children)

gmailTextBody :: Int -> Aeson.Value -> Maybe (Text, Bool)
gmailTextBody maximum value =
    firstBody "text/plain" id
        <|> firstBody "text/html" stripHtml
  where
    firstBody :: Text -> (Text -> Text) -> Maybe (Text, Bool)
    firstBody contentType transform =
        listToMaybe
            (mapMaybe (decodePart contentType transform) (flatten value))
    flatten :: Aeson.Value -> [Aeson.Value]
    flatten part = part : either (const []) id
        (parseEither
            (Aeson.withObject "Gmail payload" \object -> do
                nested <- object .:? "parts" Aeson..!= []
                pure (concatMap flatten nested))
            part)
    decodePart
        :: Text -> (Text -> Text) -> Aeson.Value -> Maybe (Text, Bool)
    decodePart contentType transform =
        either (const Nothing) id . parseEither
        (Aeson.withObject "Gmail text part" \part -> do
            mime <- part .:? "mimeType" Aeson..!= ""
            filename <- part .:? "filename" Aeson..!= ""
            if mime /= contentType
                || isJust (nonEmptyText filename)
                then pure Nothing
                else do
                    body <- part .:? "body"
                    case body of
                        Nothing -> pure Nothing
                        Just bodyValue ->
                            Aeson.withObject "Gmail body" (\bodyObject -> do
                                attachmentId <- bodyObject .:? "attachmentId"
                                encoded <- bodyObject .:? "data"
                                pure case attachmentId of
                                    Just (_ :: Text) -> Nothing
                                    Nothing ->
                                        encoded >>= decodeText transform)
                                bodyValue)
    decodeText :: (Text -> Text) -> Text -> Maybe (Text, Bool)
    decodeText transform encoded = do
        bytes <- either (const Nothing) Just
            (Base64URL.decode
                (TextEncoding.encodeUtf8 encoded))
        let decoded = transform
                (TextEncoding.decodeUtf8With
                    TextEncodingError.lenientDecode
                    bytes)
        pure
            ( truncateTextBytes maximum decoded
            , utf8Length decoded > maximum
            )
    stripHtml = Text.unwords . Text.words . innerText . parseTags

encodeGmailAttachmentRef
    :: Text -> Maybe Text -> Maybe Text -> Text
encodeGmailAttachmentRef providerId filename contentType =
    let encoded =
            TextEncoding.decodeUtf8
                . Base64URL.encodeUnpadded
                . TextEncoding.encodeUtf8
                . Text.intercalate "\NUL" $
                    [ "gmail-attachment-v1"
                    , providerId
                    , fromMaybe "" filename
                    , fromMaybe "" contentType
                    ]
    in if utf8Length encoded <= maximumGmailAttachmentReferenceBytes
        then encoded
        else providerId

decodeGmailAttachmentRef :: Text -> (Text, Maybe Text, Maybe Text)
decodeGmailAttachmentRef encoded =
    fromMaybe (encoded, Nothing, Nothing) do
        bytes <- either (const Nothing) Just
            (Base64URL.decodeUnpadded (TextEncoding.encodeUtf8 encoded))
        decoded <- either (const Nothing) Just
            (TextEncoding.decodeUtf8' bytes)
        case Text.splitOn "\NUL" decoded of
            ["gmail-attachment-v1", providerId, filename, contentType]
                | not (Text.null providerId)
                , not (Text.any (== '\NUL') providerId) ->
                    Just
                        ( providerId
                        , nonEmptyText filename
                        , nonEmptyText contentType
                        )
            _ -> Nothing

safeGmailMetadata :: Text -> Maybe Text
safeGmailMetadata =
    nonEmptyText
        . Text.take maximumGmailMetadataCharacters
        . Text.filter (not . isControl)

-- Microsoft Graph -----------------------------------------------------------

graphBase :: Text
graphBase = "https://graph.microsoft.com/v1.0/me"

graphListMailboxes :: Text -> Int -> IO (Either Text [MailboxSummary])
graphListMailboxes token maximum =
    providerJson token (graphBase <> "/mailFolders")
        [("$top", Just (BS8.pack (show maximum))),
         ("$select", Just "id,displayName,unreadItemCount")]
        [] jsonResponseMaximum
        >>= pure . (>>= parseProvider
            "Microsoft returned invalid mailbox data."
            (Aeson.withObject "Graph folders" \object -> do
                folders <- object .:? "value" Aeson..!= []
                take maximum <$> traverse parseFolder folders))
  where
    parseFolder = Aeson.withObject "Graph folder" \folder ->
        MailboxSummary
            <$> folder .: "id"
            <*> folder .: "displayName"
            <*> pure Nothing
            <*> folder .:? "unreadItemCount"

graphSearch
    :: Text -> MailSearchRequest -> IO (Either Text [MailMessageSummary])
graphSearch token request =
    providerJson token endpoint
        ([("$top", Just (BS8.pack (show maximum))),
          ("$select", Just graphSummaryFields)]
            <> maybe [] (\value ->
                [("$search", Just (TextEncoding.encodeUtf8 value))])
                (nonEmptyText (graphSearchText request)))
        graphTextHeaders jsonResponseMaximum
        >>= pure . (>>= parseProvider "Microsoft returned invalid search data."
            (Aeson.withObject "Graph messages" \object -> do
                messages <- object .:? "value" Aeson..!= []
                take maximum <$> traverse parseGraphSummary messages))
  where
    maximum = boundedCount 50 request.mailSearchLimit
    endpoint = maybe (graphBase <> "/messages")
        (\mailbox -> graphBase <> "/mailFolders/" <> component mailbox
            <> "/messages")
        request.mailSearchMailboxId

graphGet
    :: Text -> MailGetRequest -> Int -> IO (Either Text MailMessage)
graphGet token request requestedMaximum = do
    message <- providerJson token
        (graphBase <> "/messages/" <> component request.mailGetMessageId)
        [("$select", Just graphMessageFields)]
        graphTextHeaders (messageResponseMaximum requestedMaximum)
    attachments <- providerJson token
        (graphBase <> "/messages/" <> component request.mailGetMessageId
            <> "/attachments")
        [("$top", Just "100"),
         ("$select", Just "id,name,contentType,size,isInline")]
        [] graphAttachmentMetadataMaximum
    pure do
        value <- message
        attachmentValue <- attachments
        parsedAttachments <- parseProvider
            "Microsoft returned invalid attachment metadata."
            parseGraphAttachments attachmentValue
        parseProvider "Microsoft returned an invalid message."
            (parseGraphMessage (max 1 requestedMaximum) parsedAttachments)
            value

graphDownload
    :: Text -> MailAttachmentRequest -> Int
    -> IO (Either Text MailAttachmentContent)
graphDownload token request maximum =
    providerJson token
        (graphBase <> "/messages/" <> component request.mailAttachmentMessageId
            <> "/attachments/" <> component request.mailAttachmentRequestId)
        [] [] (base64ResponseMaximum maximum) >>= \case
            Left err -> pure (Left err)
            Right value ->
                pure $ parseProvider
                    "Microsoft returned invalid attachment data."
                    (Aeson.withObject "Graph attachment" \object -> do
                        filename <- object .:? "name"
                        contentType <- object .:? "contentType"
                        encoded <- object .: "contentBytes"
                        bytes <- either
                            (const (fail "invalid attachment encoding"))
                            pure
                            (Base64.decode
                                (TextEncoding.encodeUtf8 encoded))
                        when (BS.length bytes > maximum)
                            (fail "attachment too large")
                        pure MailAttachmentContent
                            { mailDownloadedAttachmentFilename = filename
                            , mailDownloadedAttachmentContentType = contentType
                            , mailDownloadedAttachmentBytes = bytes
                            })
                    value

parseGraphSummary :: Aeson.Value -> Parser MailMessageSummary
parseGraphSummary = Aeson.withObject "Graph message" \object -> do
    messageId <- object .: "id"
    conversationId <- object .:? "conversationId"
    subject <- object .:? "subject"
    sender <- object .:? "from" >>= traverse parseGraphRecipient
    replyTo <- object .:? "replyTo" Aeson..!= []
    recipients <- object .:? "toRecipients" Aeson..!= []
    received <- object .:? "receivedDateTime"
    preview <- object .:? "bodyPreview"
    hasAttachments <- object .:? "hasAttachments" Aeson..!= False
    pure MailMessageSummary
        { mailMessageSummaryId = messageId
        , mailMessageSummaryThreadId = conversationId
        , mailMessageSummarySubject = subject
        , mailMessageSummaryFrom = sender
        , mailMessageSummaryReplyTo =
            graphEffectiveReplyRecipient replyTo sender
        , mailMessageSummaryTo =
            nonEmptyText (Text.intercalate ", " (mapMaybe graphRecipient recipients))
        , mailMessageSummaryReceivedAt = received
        , mailMessageSummarySnippet = preview
        , mailMessageSummaryHasAttachments = hasAttachments
        , mailMessageSummaryAttachmentCount = Nothing
        }

parseGraphMessage :: Int -> [MailAttachment] -> Aeson.Value
    -> Parser MailMessage
parseGraphMessage maximum attachments =
    Aeson.withObject "Graph message" \object -> do
        messageId <- object .: "id"
        conversationId <- object .:? "conversationId"
        subject <- object .:? "subject"
        sender <- object .:? "from" >>= traverse parseGraphRecipient
        replyTo <- object .:? "replyTo" Aeson..!= []
        toRecipients <- object .:? "toRecipients" Aeson..!= []
        ccRecipients <- object .:? "ccRecipients" Aeson..!= []
        received <- object .:? "receivedDateTime"
        sent <- object .:? "sentDateTime"
        body <- object .:? "body"
        bodyText <- traverse
            (Aeson.withObject "Graph body" (.: "content")) body
        let bodyWasTruncated =
                maybe False ((> maximum) . utf8Length) bodyText
        pure MailMessage
            { mailMessageId = messageId
            , mailMessageThreadId = conversationId
            , mailMessageSubject = subject
            , mailMessageFrom = sender
            , mailMessageReplyTo =
                graphEffectiveReplyRecipient replyTo sender
            , mailMessageTo = recipientsText toRecipients
            , mailMessageCc = recipientsText ccRecipients
            , mailMessageReceivedAt = received
            , mailMessageSentAt = sent
            , mailMessageBody = fmap (truncateTextBytes maximum) bodyText
            , mailMessageBodyTruncated = bodyWasTruncated
            , mailMessageAttachments = attachments
            }

parseGraphAttachments :: Aeson.Value -> Parser [MailAttachment]
parseGraphAttachments = Aeson.withObject "Graph attachments" \object -> do
    attachments <- object .:? "value" Aeson..!= []
    catMaybes <$> traverse
        (Aeson.withObject "Graph attachment" \attachment -> do
            kind <- attachment .:? "@odata.type"
            if kind /= Just ("#microsoft.graph.fileAttachment" :: Text)
                then pure Nothing
                else fmap Just $ MailAttachment
                    <$> attachment .: "id"
                    <*> attachment .:? "name"
                    <*> attachment .:? "contentType"
                    <*> attachment .:? "size")
        attachments

parseGraphRecipient :: Aeson.Value -> Parser Text
parseGraphRecipient value =
    maybe (fail "missing address") pure (graphRecipient value)

graphRecipient :: Aeson.Value -> Maybe Text
graphRecipient =
    either (const Nothing) id . parseEither
        (Aeson.withObject "Graph recipient" \recipient -> do
            address <- recipient .:? "emailAddress"
            case address of
                Nothing -> pure Nothing
                Just value ->
                    Aeson.withObject "Graph address" (.:? "address") value)

graphEffectiveReplyRecipient :: [Aeson.Value] -> Maybe Text -> Maybe Text
graphEffectiveReplyRecipient replyTo sender =
    case replyTo of
        [] -> sender >>= normalized
        [recipient] -> graphRecipient recipient >>= normalized
        _ -> Nothing
  where
    normalized = eitherToMaybe . normalizeMailEmail

recipientsText :: [Aeson.Value] -> Maybe Text
recipientsText =
    nonEmptyText . Text.intercalate ", " . mapMaybe graphRecipient

graphSearchText :: MailSearchRequest -> Text
graphSearchText request = Text.unwords . catMaybes $
    [ graphQuote <$> request.mailSearchQuery
    , ("from:" <>) . graphQuote <$> request.mailSearchFrom
    , ("to:" <>) . graphQuote <$> request.mailSearchTo
    , ("subject:" <>) . graphQuote <$> request.mailSearchSubject
    , ("received>=" <>) <$> request.mailSearchAfter
    , ("received<" <>) . nextIsoDay <$> request.mailSearchBefore
    , fmap (\present -> "hasattachments:" <>
        if present then "true" else "false") request.mailSearchHasAttachments
    ]
  where
    graphQuote value =
        "\"" <> Text.replace "\"" " " value <> "\""

nextIsoDay :: Text -> Text
nextIsoDay value =
    case (parseTimeM True defaultTimeLocale "%F" (Text.unpack value)
            :: Maybe Day) of
        Nothing -> value
        Just day -> Text.pack (formatTime defaultTimeLocale "%F" (addDays 1 day))

graphSummaryFields, graphMessageFields :: BS.ByteString
graphSummaryFields =
    "id,conversationId,subject,from,replyTo,toRecipients,receivedDateTime,bodyPreview,hasAttachments"
graphMessageFields =
    "id,conversationId,subject,from,replyTo,toRecipients,ccRecipients,receivedDateTime,sentDateTime,body"

graphTextHeaders :: [Header]
graphTextHeaders =
    [("Prefer", "outlook.body-content-type=\"text\""),
     ("ConsistencyLevel", "eventual")]

-- IMAP ----------------------------------------------------------------------

imapListMailboxes
    :: MailImapSettings -> Text -> Int
    -> IO (Either Text [MailboxSummary])
imapListMailboxes settings password maximum =
    withMailImapConnection settings password \connection -> do
        lines' <- imapSimpleCommand connection "m101" "LIST \"\" \"*\""
        pure . take maximum . mapMaybe parseListLine $ lines'

imapSearch
    :: MailImapSettings -> Text -> MailSearchRequest
    -> IO (Either Text [MailMessageSummary])
imapSearch settings password request =
    case traverse decodeImapMailboxId request.mailSearchMailboxId of
        Left err -> pure (Left err)
        Right selected ->
            withMailImapConnection settings password \connection -> do
                let mailbox = fromMaybe "INBOX" selected
                selectedLines <- imapSimpleCommand connection "m102"
                    ("EXAMINE " <> imapQuote mailbox)
                uidValidity <- maybe
                    (failText "The IMAP server omitted mailbox identity data.")
                    pure
                    (parseUidValidity selectedLines)
                searched <- imapSearchCommand connection "m103"
                    ("UID SEARCH " <> imapSearchCriteria request)
                let fetchLimit
                        | isJust request.mailSearchHasAttachments = 50
                        | otherwise = boundedCount 50 request.mailSearchLimit
                    uids = take fetchLimit
                        (reverse (concatMap searchUids searched))
                summaries <- traverse
                    (imapFetchSummary connection mailbox uidValidity) uids
                pure $ take request.mailSearchLimit
                    (filter matchesAttachmentFilter summaries)
  where
    matchesAttachmentFilter summary =
        maybe True
            (== summary.mailMessageSummaryHasAttachments)
            request.mailSearchHasAttachments

imapGet
    :: MailImapSettings -> Text -> MailGetRequest -> Int
    -> IO (Either Text MailMessage)
imapGet settings password request requestedMaximum =
    case decodeImapMessageId request.mailGetMessageId of
        Left err -> pure (Left err)
        Right (mailbox, expectedUidValidity, uid) -> do
            connected <- withMailImapConnection settings password \connection -> do
                selectedLines <- imapSimpleCommand connection "m201"
                    ("EXAMINE " <> imapQuote mailbox)
                currentUidValidity <- maybe
                    (failText "The IMAP server omitted mailbox identity data.")
                    pure
                    (parseUidValidity selectedLines)
                unless (currentUidValidity == expectedUidValidity) $
                    failText
                        "The IMAP message reference has expired. Search again."
                summary <- imapFetchSummary
                    connection mailbox currentUidValidity uid
                raw <- imapFetchRawMessage connection "m204" "m205" uid
                    maximumImapRawMessageBytes
                pure case parseMailMime raw of
                    Left err -> Left err
                    Right parsed ->
                        let body = mailMimeTextBody maximum parsed
                        in Right MailMessage
                            { mailMessageId = request.mailGetMessageId
                            , mailMessageThreadId = Nothing
                            , mailMessageSubject =
                                summary.mailMessageSummarySubject
                            , mailMessageFrom = summary.mailMessageSummaryFrom
                            , mailMessageReplyTo =
                                summary.mailMessageSummaryReplyTo
                            , mailMessageTo = summary.mailMessageSummaryTo
                            , mailMessageCc = Nothing
                            , mailMessageReceivedAt =
                                summary.mailMessageSummaryReceivedAt
                            , mailMessageSentAt =
                                summary.mailMessageSummaryReceivedAt
                            , mailMessageBody = body
                            , mailMessageBodyTruncated =
                                mailMimeTextBodyTruncated maximum parsed
                            , mailMessageAttachments =
                                map toMailAttachment
                                    (mailMimeAttachments parsed)
                            }
            pure (connected >>= id)
  where
    maximum = max 1 (min maximumImapBodyBytes requestedMaximum)

-- Custom IMAP draft support is intentionally capability-conservative.  A
-- server must advertise a RFC 6154 \Drafts mailbox and UIDPLUS APPENDUID;
-- without a stable UID reference we cannot safely expose update_draft.
imapCreateDraft
    :: MailImapSettings -> Text -> Text -> MailDraftContent
    -> IO (Either Text MailDraft)
imapCreateDraft settings password sender content =
    fmap (>>= id) $ withMailImapConnection settings password \connection -> do
        draftsMailbox <- imapDraftsMailbox connection
        imapRequireUidPlus connection "m401"
        imapAppendDraft connection "m402" draftsMailbox
            (renderMailDraftMime sender content Nothing)

imapUpdateDraft
    :: MailImapSettings -> Text -> Text -> MailUpdateDraftRequest
    -> IO (Either Text MailDraft)
imapUpdateDraft settings password sender request =
    case decodeImapDraftId request.mailUpdateDraftId of
        Left err -> pure (Left err)
        Right (mailbox, expectedUidValidity, uid) ->
            fmap (>>= id) $ withMailImapConnection settings password \connection -> do
                draftsMailbox <- imapDraftsMailbox connection
                unless (mailbox == draftsMailbox) $
                    failText "That IMAP item is not in the server's Drafts mailbox."
                imapRequireUidPlus connection "m410"
                selected <- imapSimpleCommand connection "m411"
                    ("SELECT " <> imapQuote mailbox)
                currentUidValidity <- maybe
                    (failText "The IMAP server omitted mailbox identity data.")
                    pure (parseUidValidity selected)
                unless (currentUidValidity == expectedUidValidity) $
                    failText "The IMAP draft reference has expired. Create a new draft."
                flags <- imapSimpleCommand connection "m412"
                    ("UID FETCH " <> uid <> " (FLAGS)")
                unless (imapUidHasFlag uid "\\Draft" flags) $
                    failText "That mailbox item is not a draft and cannot be changed."
                (headerBytes, _) <- imapFetchLiteralWithLines connection "m416"
                    ("UID FETCH " <> uid
                        <> " (BODY.PEEK[HEADER.FIELDS (IN-REPLY-TO REFERENCES)])")
                    maximumImapHeaderBytes
                replyHeaders <- either failText pure
                    (validatedReplyHeaders
                        (parseHeaders (decodeUtf8Lenient headerBytes)))
                saved <- imapAppendDraft connection "m413" mailbox
                    (renderMailDraftMime
                        sender request.mailUpdateDraftContent replyHeaders)
                -- Never issue a bare EXPUNGE/CLOSE: UID EXPUNGE limits removal
                -- to exactly the replacement target. If the server does not
                -- support it, retain both drafts rather than deleting data.
                case saved of
                    Left err -> pure (Left err)
                    Right draft
                        | "imap-untracked:" `Text.isPrefixOf`
                                draft.mailDraftId ->
                            pure (Right draft
                                { mailDraftWarning = Just
                                    "The replacement draft was saved without a stable identifier; the previous draft was retained."
                                })
                    Right draft -> do
                        -- The replacement has already been durably appended.
                        -- Do not turn a later best-effort cleanup failure into
                        -- a retryable error that could duplicate drafts.
                        cleanup <- tryAny do
                            currentFlags <- imapSimpleCommand connection "m414"
                                ("UID FETCH " <> uid <> " (FLAGS)")
                            unless (imapUidHasFlag uid "\\Draft" currentFlags) $
                                failText "The previous IMAP item changed."
                            _ <- imapSimpleCommand connection "m415"
                                ("UID STORE " <> uid
                                    <> " +FLAGS.SILENT (\\Deleted)")
                            _ <- imapSimpleCommand connection "m417"
                                ("UID EXPUNGE " <> uid)
                            pure ()
                        pure . Right $ case cleanup of
                            Left (_ :: SomeException) -> draft
                                { mailDraftWarning = Just
                                    "The replacement draft was saved, but the previous draft could not be removed automatically."
                                }
                            Right () -> draft

imapReplyDraft
    :: MailImapSettings -> Text -> Text -> MailReplyDraftRequest
    -> IO (Either Text MailDraft)
imapReplyDraft settings password sender request =
    case decodeImapMessageId request.mailReplyDraftMessageId of
        Left err -> pure (Left err)
        Right (mailbox, expectedUidValidity, uid) ->
            fmap (>>= id) $ withMailImapConnection settings password \connection -> do
                selected <- imapSimpleCommand connection "m421"
                    ("EXAMINE " <> imapQuote mailbox)
                currentUidValidity <- maybe
                    (failText "The IMAP server omitted mailbox identity data.")
                    pure (parseUidValidity selected)
                unless (currentUidValidity == expectedUidValidity) $
                    failText "The IMAP message reference has expired. Search again."
                (headerBytes, _) <- imapFetchLiteralWithLines connection "m422"
                    ("UID FETCH " <> uid
                        <> " (BODY.PEEK[HEADER.FIELDS (SUBJECT MESSAGE-ID REFERENCES REPLY-TO FROM)])")
                    maximumImapHeaderBytes
                let headers = parseHeaders (decodeUtf8Lenient headerBytes)
                rawMessageId <- maybe
                    (failText "The source email cannot be used for a reply draft.")
                    pure (lookupHeader "message-id" headers)
                inReplyTo <- either failText pure
                    (validateMessageIdHeader rawMessageId)
                recipient <- either failText pure
                    (replyRecipientFromHeaders
                        (lookupHeader "reply-to" headers)
                        (lookupHeader "from" headers))
                either failText pure
                    (ensureExpectedReplyRecipient
                        request.mailReplyDraftTo recipient)
                draftsMailbox <- imapDraftsMailbox connection
                imapRequireUidPlus connection "m420"
                imapAppendDraft connection "m423" draftsMailbox
                    (renderMailDraftMime sender MailDraftContent
                        { mailDraftTo = [recipient]
                        , mailDraftCc = []
                        , mailDraftBcc = []
                        , mailDraftSubject = safeReplySubject
                            (fromMaybe "" (lookupHeader "subject" headers))
                        , mailDraftBody = request.mailReplyDraftBody
                        }
                        (Just
                            ( inReplyTo
                            , appendReference
                                (validateReferencesHeader
                                    =<< lookupHeader "references" headers)
                                inReplyTo
                            )))

validatedReplyHeaders
    :: [(Text, Text)]
    -> Either Text (Maybe (Text, Maybe Text))
validatedReplyHeaders headers =
    case
        ( lookupHeader "in-reply-to" headers
        , lookupHeader "references" headers
        )
    of
        (Nothing, Nothing) -> Right Nothing
        (Nothing, Just _) ->
            Left "The existing draft contains invalid reply metadata."
        (Just rawInReplyTo, rawReferences) -> do
            inReplyTo <- validateMessageIdHeader rawInReplyTo
            references <- case rawReferences of
                Nothing -> Right Nothing
                Just value -> maybe
                    (Left "The existing draft contains invalid reply metadata.")
                    (Right . Just)
                    (validateReferencesHeader value)
            Right (Just (inReplyTo, references))

imapUidHasFlag :: Text -> Text -> [Text] -> Bool
imapUidHasFlag expectedUid expectedFlag =
    any matches
  where
    tokens =
        Text.words
            . Text.toCaseFold
            . Text.map (\character ->
                if character `elem` ['(', ')'] then ' ' else character)
    matches line = findUid (tokens line)
    findUid = \case
        "uid" : uid : rest
            | uid == Text.toCaseFold expectedUid ->
                findFlag rest
            | otherwise -> False
        _ : rest -> findUid rest
        [] -> False
    findFlag = \case
        "flags" : rest -> Text.toCaseFold expectedFlag `elem` rest
        _ : rest -> findFlag rest
        [] -> False

imapDraftsMailbox :: Connection -> IO Text
imapDraftsMailbox connection = do
    lines' <- imapSimpleCommand connection "m400" "LIST \"\" \"*\""
    case nub
        [ mailbox.mailMailboxName
        | mailbox <- mapMaybe parseListLine lines'
        , mailbox.mailMailboxRole == Just "drafts"
        ] of
        [mailbox] -> pure mailbox
        [] -> failText
            "This IMAP server does not advertise a Drafts mailbox, so drafts are unavailable."
        _ -> failText
            "This IMAP server advertises multiple Drafts mailboxes, so drafts are unavailable."

imapRequireUidPlus :: Connection -> Text -> IO ()
imapRequireUidPlus connection tag = do
    capabilities <- imapSimpleCommand connection tag "CAPABILITY"
    unless (any (\line ->
        " uidplus " `Text.isInfixOf` (" " <> Text.toCaseFold line <> " "))
        capabilities) $
        failText "This IMAP server does not support UIDPLUS, so stable drafts are unavailable."

imapAppendDraft :: Connection -> Text -> Text -> BS.ByteString
    -> IO (Either Text MailDraft)
imapAppendDraft connection tag mailbox bytes = do
    Connection.connectionPut connection
        (TextEncoding.encodeUtf8
            (tag <> " APPEND " <> imapQuote mailbox <> " (\\Draft) {"
                <> Text.pack (show (BS.length bytes)) <> "}\r\n"))
    imapAwaitContinuation connection tag
    Connection.connectionPut connection bytes
    Connection.connectionPut connection "\r\n"
    completion <- imapReadTaggedCompletion connection tag
    case parseAppendUid completion of
        Nothing -> pure (Right MailDraft
            { mailDraftId = "imap-untracked:" <> encodeImapMailboxId mailbox
            , mailDraftMessageId = Nothing
            , mailDraftThreadId = Nothing
            , mailDraftWarning = Just
                "The draft was saved, but this IMAP server did not return a stable identifier, so the agent cannot update it."
            })
        Just (uidValidity, uid) -> pure (Right MailDraft
            { mailDraftId = encodeImapDraftId mailbox uidValidity uid
            , mailDraftMessageId = Just (encodeImapMessageId mailbox uidValidity uid)
            , mailDraftThreadId = Nothing
            , mailDraftWarning = Nothing
            })

imapDownload
    :: MailImapSettings -> Text -> MailAttachmentRequest -> Int
    -> IO (Either Text MailAttachmentContent)
imapDownload settings password request maximum =
    case decodeImapMessageId request.mailAttachmentMessageId of
        Left err -> pure (Left err)
        Right (mailbox, expectedUidValidity, uid) -> do
            connected <- withMailImapConnection settings password
                \connection -> do
                    selectedLines <- imapSimpleCommand connection "m301"
                        ("EXAMINE " <> imapQuote mailbox)
                    currentUidValidity <- maybe
                        (failText
                            "The IMAP server omitted mailbox identity data.")
                        pure
                        (parseUidValidity selectedLines)
                    unless (currentUidValidity == expectedUidValidity) $
                        failText
                            "The IMAP message reference has expired. Search again."
                    raw <- imapFetchRawMessage connection "m302" "m303" uid
                        maximumImapRawMessageBytes
                    pure do
                        parsed <- parseMailMime raw
                        attachment <- mailMimeAttachmentContent
                            maximum
                            request.mailAttachmentRequestId
                            parsed
                        pure MailAttachmentContent
                            { mailDownloadedAttachmentFilename =
                                Just attachment.parsedMailAttachmentFilename
                            , mailDownloadedAttachmentContentType =
                                Just attachment.parsedMailAttachmentContentType
                            , mailDownloadedAttachmentBytes =
                                attachment.parsedMailAttachmentBytes
                            }
            pure (connected >>= id)

toMailAttachment :: ParsedMailAttachment -> MailAttachment
toMailAttachment attachment = MailAttachment
    { mailAttachmentId = attachment.parsedMailAttachmentId
    , mailAttachmentFilename =
        Just attachment.parsedMailAttachmentFilename
    , mailAttachmentContentType =
        Just attachment.parsedMailAttachmentContentType
    , mailAttachmentSizeBytes =
        Just (BS.length attachment.parsedMailAttachmentBytes)
    }

imapFetchRawMessage
    :: Connection
    -> Text
    -> Text
    -> Text
    -> Int
    -> IO BS.ByteString
imapFetchRawMessage connection sizeTag fetchTag uid maximum =
    imapFetchSize connection sizeTag uid >>= \case
        Nothing ->
            failText "The IMAP server did not return the message size."
        Just size
            | size > maximum ->
                failText "The IMAP message exceeded the safe download limit."
            | otherwise ->
                imapFetchLiteral connection fetchTag
                    ("UID FETCH " <> uid <> " (BODY.PEEK[])")
                    maximum

imapFetchSize :: Connection -> Text -> Text -> IO (Maybe Int)
imapFetchSize connection tag uid = do
    response <- imapSimpleCommand connection tag
        ("UID FETCH " <> uid <> " (RFC822.SIZE)")
    pure (listToMaybe (mapMaybe parseSize response))
  where
    parseSize line = findSize (Text.words (Text.map punctuationToSpace line))
    findSize = \case
        key : value : rest
            | Text.toCaseFold key == "rfc822.size" ->
                case reads (Text.unpack value) of
                    [(size, "")] | size >= 0 -> Just size
                    _ -> Nothing
            | otherwise -> findSize (value : rest)
        _ -> Nothing
    punctuationToSpace character
        | character == '(' || character == ')' = ' '
        | otherwise = character

imapFetchSummary
    :: Connection -> Text -> Text -> Text -> IO MailMessageSummary
imapFetchSummary connection mailbox uidValidity uid = do
    (headerBytes, responseLines) <- imapFetchLiteralWithLines connection "m202"
        ("UID FETCH " <> uid
            <> " (BODY.PEEK[HEADER.FIELDS (SUBJECT FROM REPLY-TO TO DATE)] BODYSTRUCTURE)")
        maximumImapHeaderBytes
    let headers = parseHeaders (decodeUtf8Lenient headerBytes)
        joined = Text.toCaseFold (Text.unwords responseLines)
        hasAttachment = "\"attachment\"" `Text.isInfixOf` joined
            || " attachment " `Text.isInfixOf` joined
    pure MailMessageSummary
        { mailMessageSummaryId =
            encodeImapMessageId mailbox uidValidity uid
        , mailMessageSummaryThreadId = Nothing
        , mailMessageSummarySubject = lookupHeader "subject" headers
        , mailMessageSummaryFrom = lookupHeader "from" headers
        , mailMessageSummaryReplyTo =
            eitherToMaybe (replyRecipientFromHeaders
                (lookupHeader "reply-to" headers)
                (lookupHeader "from" headers))
        , mailMessageSummaryTo = lookupHeader "to" headers
        , mailMessageSummaryReceivedAt = lookupHeader "date" headers
        , mailMessageSummarySnippet = Nothing
        , mailMessageSummaryHasAttachments = hasAttachment
        , mailMessageSummaryAttachmentCount =
            if hasAttachment then Nothing else Just 0
        }

imapSimpleCommand :: Connection -> Text -> Text -> IO [Text]
imapSimpleCommand =
    imapSimpleCommandBounded
        maximumImapLineBytes
        maximumImapResponseBytes

-- UID SEARCH commonly returns every matching UID on one line. Keep that
-- response bounded, but allow a larger dedicated limit than ordinary IMAP
-- control lines so normal large mailboxes can still return the newest items.
imapSearchCommand :: Connection -> Text -> Text -> IO [Text]
imapSearchCommand =
    imapSimpleCommandBounded
        maximumImapSearchLineBytes
        maximumImapSearchResponseBytes

imapSimpleCommandBounded
    :: Int -> Int -> Connection -> Text -> Text -> IO [Text]
imapSimpleCommandBounded maximumLineBytes maximumResponseBytes
        connection tag command = do
    imapSend connection (tag <> " " <> command)
    go maximumImapResponseLines 0 []
  where
    go remaining totalBytes accumulated
        | remaining <= 0 = failText "The IMAP response exceeded the safe limit."
        | otherwise = do
            line <- imapReadLineWithLimit maximumLineBytes connection
            let totalBytes' = totalBytes + utf8Length line
            when (totalBytes' > maximumResponseBytes) $
                failText "The IMAP response exceeded the safe size limit."
            if (Text.toCaseFold tag <> " ") `Text.isPrefixOf`
                    Text.toCaseFold line
                then do
                    ensureTaggedOk tag line
                    pure (reverse accumulated)
                else
                    case imapLiteralLength line of
                        Just _ -> failText
                            "The IMAP server returned an unexpected literal."
                        Nothing ->
                            go (remaining - 1) totalBytes' (line : accumulated)

imapFetchLiteralWithLines
    :: Connection -> Text -> Text -> Int -> IO (BS.ByteString, [Text])
imapFetchLiteralWithLines connection tag command maximum = do
    imapSend connection (tag <> " " <> command)
    seek maximumImapResponseLines []
  where
    seek remaining linesSeen
        | remaining <= 0 = failText "The IMAP response exceeded the safe limit."
        | otherwise = do
            line <- imapReadLine connection
            case imapLiteralLength line of
                Just literalLength -> do
                    when (literalLength > maximum) $
                        failText "The IMAP item exceeded the configured size limit."
                    bytes <- connectionReadExactly connection literalLength
                    finish maximumImapResponseLines (line : linesSeen) bytes
                Nothing
                    | (Text.toCaseFold tag <> " ") `Text.isPrefixOf`
                        Text.toCaseFold line ->
                            failText "The IMAP server returned no message data."
                    | otherwise -> seek (remaining - 1) (line : linesSeen)
    finish remaining linesSeen bytes
        | remaining <= 0 = failText "The IMAP response exceeded the safe limit."
        | otherwise = do
            line <- imapReadLine connection
            if (Text.toCaseFold tag <> " ") `Text.isPrefixOf`
                    Text.toCaseFold line
                then do
                    ensureTaggedOk tag line
                    pure (bytes, reverse (line : linesSeen))
                else
                    case imapLiteralLength line of
                        Just _ -> failText
                            "The IMAP server returned multiple unexpected literals."
                        Nothing -> finish (remaining - 1) (line : linesSeen) bytes

imapFetchLiteral :: Connection -> Text -> Text -> Int -> IO BS.ByteString
imapFetchLiteral connection tag command maximum =
    fst <$> imapFetchLiteralWithLines connection tag command maximum

imapSend :: Connection -> Text -> IO ()
imapSend connection line =
    Connection.connectionPut connection
        (TextEncoding.encodeUtf8 line <> "\r\n")

imapReadLine :: Connection -> IO Text
imapReadLine = imapReadLineWithLimit maximumImapLineBytes

imapReadLineWithLimit :: Int -> Connection -> IO Text
imapReadLineWithLimit maximum connection =
    decodeUtf8Lenient
        <$> Connection.connectionGetLine maximum connection

connectionReadExactly :: Connection -> Int -> IO BS.ByteString
connectionReadExactly connection requested =
    go requested []
  where
    go remaining chunks
        | remaining <= 0 = pure (BS.concat (reverse chunks))
        | otherwise = do
            chunk <- Connection.connectionGet connection remaining
            when (BS.null chunk) $
                failText "The IMAP server closed an incomplete response."
            go (remaining - BS.length chunk) (chunk : chunks)

ensureTaggedOk :: Text -> Text -> IO ()
ensureTaggedOk tag line =
    unless ((Text.toCaseFold tag <> " ok") `Text.isPrefixOf`
            Text.toCaseFold line) $
        failText "The IMAP server rejected the mailbox operation."

parseListLine :: Text -> Maybe MailboxSummary
parseListLine raw = do
    let stripped = Text.strip raw
    guardPrefix "* list " stripped
    mailbox <- lastImapArgument stripped
    whenMaybe (not (Text.any (`elem` ['\\', '"', '\r', '\n', '\NUL']) mailbox))
    whenMaybe (Text.length (encodeImapMailboxId mailbox) <= 900)
    pure MailboxSummary
        { mailMailboxId = encodeImapMailboxId mailbox
        , mailMailboxName = mailbox
        , mailMailboxRole =
            if imapListHasFlag "\\Drafts" stripped
                then Just "drafts"
                else Nothing
        , mailMailboxUnreadCount = Nothing
        }

parseImapMailboxListLine :: Text -> Maybe MailboxSummary
parseImapMailboxListLine = parseListLine

imapListHasFlag :: Text -> Text -> Bool
imapListHasFlag expected line =
    case Text.breakOn "(" line of
        (_, suffix)
            | not (Text.null suffix)
            , let (rawFlags, close) = Text.breakOn ")" (Text.drop 1 suffix)
            , not (Text.null close) ->
                Text.toCaseFold expected
                    `elem` map Text.toCaseFold (Text.words rawFlags)
        _ -> False

lastImapArgument :: Text -> Maybe Text
lastImapArgument line =
    case Text.unsnoc (Text.strip line) of
        Just (before, '"') ->
            let reversed = Text.reverse before
                (value, rest) = Text.breakOn "\"" reversed
            in if Text.null rest then Nothing
                else Just (Text.reverse value)
        _ -> nonEmptyText (lastWord line)
  where
    lastWord value = case Text.words value of
        [] -> ""
        words' -> last words'

guardPrefix :: Text -> Text -> Maybe ()
guardPrefix prefix value
    | prefix `Text.isPrefixOf` Text.toCaseFold value = Just ()
    | otherwise = Nothing

imapSearchCriteria :: MailSearchRequest -> Text
imapSearchCriteria request =
    case catMaybes
        [ ("TEXT " <>) . imapQuote <$> request.mailSearchQuery
        , ("FROM " <>) . imapQuote <$> request.mailSearchFrom
        , ("TO " <>) . imapQuote <$> request.mailSearchTo
        , ("SUBJECT " <>) . imapQuote <$> request.mailSearchSubject
        , ("SINCE " <>) . imapDate <$> request.mailSearchAfter
        , ("BEFORE " <>) . imapDate . nextIsoDay <$> request.mailSearchBefore
        ] of
        [] -> "ALL"
        criteria -> Text.unwords criteria

imapDate :: Text -> Text
imapDate value =
    case (parseTimeM True defaultTimeLocale "%F" (Text.unpack value)
            :: Maybe Day) of
        Nothing -> value
        Just day -> Text.pack (formatTime defaultTimeLocale "%d-%b-%Y" day)

imapQuote :: Text -> Text
imapQuote value =
    "\"" <> Text.concatMap escape value <> "\""
  where
    escape '"' = "\\\""
    escape '\\' = "\\\\"
    escape '\r' = " "
    escape '\n' = " "
    escape '\NUL' = " "
    escape character = Text.singleton character

searchUids :: Text -> [Text]
searchUids line =
    case Text.words line of
        first : second : rest
            | Text.toCaseFold first == "*"
            , Text.toCaseFold second == "search" ->
                filter (Text.all isDigit) rest
        _ -> []

imapLiteralLength :: Text -> Maybe Int
imapLiteralLength line = do
    let stripped = Text.strip line
    guardSuffix "}" stripped
    let (before, suffix) = Text.breakOnEnd "{" stripped
        digits = Text.dropEnd 1 suffix
        normalized = Text.dropWhileEnd (== '+') digits
    whenMaybe (not (Text.null before) && not (Text.null normalized)
        && Text.all isDigit normalized)
    case reads (Text.unpack normalized) of
        [(value, "")] -> Just value
        _ -> Nothing

guardSuffix :: Text -> Text -> Maybe ()
guardSuffix suffix value
    | suffix `Text.isSuffixOf` value = Just ()
    | otherwise = Nothing

whenMaybe :: Bool -> Maybe ()
whenMaybe True = Just ()
whenMaybe False = Nothing

parseHeaders :: Text -> [(Text, Text)]
parseHeaders =
    reverse . fst . foldl addHeader ([], Nothing) . Text.lines
  where
    addHeader (headers, current) line
        | Text.null line = (headers, current)
        | Text.head line == ' ' || Text.head line == '\t' =
            case headers of
                [] -> (headers, current)
                (name, value) : rest ->
                    ((name, value <> " " <> Text.strip line) : rest, current)
        | otherwise =
            let (name, rawValue) = Text.breakOn ":" line
            in if Text.null rawValue then (headers, current)
                else ((Text.toCaseFold name, Text.strip (Text.drop 1 rawValue))
                    : headers, Just name)

lookupHeader :: Text -> [(Text, Text)] -> Maybe Text
lookupHeader name = lookup (Text.toCaseFold name)

parseUidValidity :: [Text] -> Maybe Text
parseUidValidity = listToMaybe . mapMaybe parseLine
  where
    marker = "[uidvalidity "
    parseLine raw =
        let (_, suffix) = Text.breakOn marker (Text.toCaseFold raw)
            digits =
                Text.takeWhile isDigit
                    (Text.drop (Text.length marker) suffix)
        in if Text.null suffix || Text.null digits
            then Nothing
            else Just digits

parseAppendUid :: Text -> Maybe (Text, Text)
parseAppendUid raw =
    case Text.words (Text.map punctuationToSpace raw) of
        words' -> go words'
  where
    go = \case
        marker : uidValidity : uid : _
            | Text.toCaseFold marker == "appenduid"
            , Text.all isDigit uidValidity
            , Text.all isDigit uid ->
                Just (uidValidity, uid)
        _ : rest -> go rest
        [] -> Nothing
    punctuationToSpace character
        | character == '[' || character == ']' = ' '
        | otherwise = character

parseImapAppendUid :: Text -> Maybe (Text, Text)
parseImapAppendUid = parseAppendUid

encodeImapMessageId :: Text -> Text -> Text -> Text
encodeImapMessageId mailbox uidValidity uid =
    TextEncoding.decodeUtf8 . Base64URL.encodeUnpadded . TextEncoding.encodeUtf8 $
        mailbox <> "\NUL" <> uidValidity <> "\NUL" <> uid

decodeImapMessageId :: Text -> Either Text (Text, Text, Text)
decodeImapMessageId encoded = do
    bytes <- either
        (const (Left "The IMAP message reference is invalid."))
        Right
        (Base64URL.decodeUnpadded (TextEncoding.encodeUtf8 encoded))
    decoded <- either
        (const (Left "The IMAP message reference is invalid."))
        Right
        (TextEncoding.decodeUtf8' bytes)
    case Text.splitOn "\NUL" decoded of
        [mailbox, uidValidity, uid]
            | validDecodedImapMailbox mailbox
            , not (Text.null uidValidity)
            , Text.all isDigit uidValidity
            , not (Text.null uid)
            , Text.all isDigit uid ->
                Right (mailbox, uidValidity, uid)
        _ -> Left "The IMAP message reference is invalid."

encodeImapDraftId :: Text -> Text -> Text -> Text
encodeImapDraftId mailbox uidValidity uid =
    "imap-draft:" <> encodeImapMessageId mailbox uidValidity uid

decodeImapDraftId :: Text -> Either Text (Text, Text, Text)
decodeImapDraftId value =
    case Text.stripPrefix "imap-draft:" value of
        Nothing -> Left "The IMAP draft reference is invalid."
        Just encoded ->
            case decodeImapMessageId encoded of
                Left _ -> Left "The IMAP draft reference is invalid."
                Right decoded -> Right decoded

encodeImapMailboxId :: Text -> Text
encodeImapMailboxId =
    TextEncoding.decodeUtf8
        . Base64URL.encodeUnpadded
        . TextEncoding.encodeUtf8

decodeImapMailboxId :: Text -> Either Text Text
decodeImapMailboxId encoded = do
    bytes <- either
        (const (Left "The IMAP mailbox reference is invalid."))
        Right
        (Base64URL.decodeUnpadded (TextEncoding.encodeUtf8 encoded))
    mailbox <- either
        (const (Left "The IMAP mailbox reference is invalid."))
        Right
        (TextEncoding.decodeUtf8' bytes)
    if not (validDecodedImapMailbox mailbox)
        then Left "The IMAP mailbox reference is invalid."
        else Right mailbox

validDecodedImapMailbox :: Text -> Bool
validDecodedImapMailbox mailbox =
    not (Text.null mailbox)
        && not (Text.any isControl mailbox)

-- Shared bounds -------------------------------------------------------------

boundedCount :: Int -> Int -> Int
boundedCount hardMaximum requested =
    max 1 (min hardMaximum requested)

chunksOf :: Int -> [value] -> [[value]]
chunksOf requested values
    | null values = []
    | otherwise =
        let (chunk, rest) = splitAt (max 1 requested) values
        in chunk : chunksOf requested rest

nonEmptyBytes :: Text -> Maybe BS.ByteString
nonEmptyBytes value
    | Text.null value = Nothing
    | otherwise = Just (TextEncoding.encodeUtf8 value)

nonEmptyText :: Text -> Maybe Text
nonEmptyText value
    | Text.null (Text.strip value) = Nothing
    | otherwise = Just value

truncateTextBytes :: Int -> Text -> Text
truncateTextBytes maximum =
    decodeUtf8Lenient . BS.take (max 0 maximum) . TextEncoding.encodeUtf8

utf8Length :: Text -> Int
utf8Length = BS.length . TextEncoding.encodeUtf8

decodeUtf8Lenient :: BS.ByteString -> Text
decodeUtf8Lenient =
    TextEncoding.decodeUtf8With TextEncodingError.lenientDecode

base64ResponseMaximum :: Int -> Int
base64ResponseMaximum maximum =
    min maximumProviderResponseBytes
        (max 4096 (maximum + maximum `div` 2 + 64 * 1024))

failText :: Text -> IO value
failText = ioError . userError . Text.unpack

httpTimeoutMicros :: Int
httpTimeoutMicros = 15 * 1000 * 1000

jsonResponseMaximum, gmailMetadataMaximum :: Int
jsonResponseMaximum = 2 * 1024 * 1024
gmailMetadataMaximum = 512 * 1024

gmailSummaryConcurrency :: Int
gmailSummaryConcurrency = 5

gmailSummaryFields :: BS.ByteString
gmailSummaryFields =
    "id,threadId,snippet,payload(headers,filename,mimeType,"
        <> "body(attachmentId,size),parts(filename,mimeType,"
        <> "body(attachmentId,size),parts(filename,mimeType,"
        <> "body(attachmentId,size))))"

messageResponseMaximum :: Int -> Int
messageResponseMaximum bodyMaximum =
    min (2 * 1024 * 1024)
        (max (128 * 1024) (max 1 bodyMaximum * 2 + 128 * 1024))

graphAttachmentMetadataMaximum, maximumProviderResponseBytes :: Int
graphAttachmentMetadataMaximum = 1024 * 1024
maximumProviderResponseBytes = 31 * 1024 * 1024

maximumAttachmentBytes, maximumImapBodyBytes, maximumImapHeaderBytes :: Int
maximumAttachmentBytes = 20 * 1024 * 1024
maximumImapBodyBytes = 256 * 1024
maximumImapHeaderBytes = 32 * 1024

maximumGmailAttachmentReferenceBytes, maximumGmailMetadataCharacters :: Int
maximumGmailAttachmentReferenceBytes = 900
maximumGmailMetadataCharacters = 255

maximumReplySubjectBytes :: Int
maximumReplySubjectBytes = 700

maximumProviderIdentifierBytes, maximumMailboxHeaderBytes, maximumReplyHeaderBytes :: Int
maximumProviderIdentifierBytes = 700
maximumMailboxHeaderBytes = 2048
maximumReplyHeaderBytes = 700

maximumImapRawMessageBytes :: Int
maximumImapRawMessageBytes = 8 * 1024 * 1024

maximumImapLineBytes, maximumImapSearchLineBytes, maximumImapResponseLines :: Int
maximumImapLineBytes = 16 * 1024
maximumImapSearchLineBytes = 1024 * 1024
maximumImapResponseLines = 256

maximumImapResponseBytes, maximumImapSearchResponseBytes :: Int
maximumImapResponseBytes = 512 * 1024
maximumImapSearchResponseBytes = 1024 * 1024
