-- | Byte-oriented, bounded MIME decoding for custom IMAP messages.
--
-- The IMAP transport caps the raw message before calling this module. Parsing
-- keeps bodies as strict ByteStrings, limits part count and header sizes, and
-- decodes only the selected text body or requested attachment.
module Agent.CLI.Mail.Mime
    ( ParsedMailMime
    , ParsedMailAttachment(..)
    , parseMailMime
    , mailMimeTextBody
    , mailMimeTextBodyTruncated
    , mailMimeAttachments
    , mailMimeAttachmentContent
    , renderMailDraftMime
    ) where

import Control.Applicative ((<|>))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Base64.URL as Base64URL
import qualified Data.ByteString.Builder as ByteStringBuilder
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import Data.Char (chr, isControl, isHexDigit, ord, toLower)
import Data.List (find, foldl')
import Data.Maybe (fromMaybe, isJust, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import qualified Data.Text.Encoding.Error as TextEncodingError
import Text.HTML.TagSoup (innerText, parseTags)
import Text.Read (readMaybe)
import Agent.CLI.Mail.Tools (MailDraftContent(..))

data ParsedMailMime = ParsedMailMime
    { parsedMailParts :: ![MimePart]
    }

data MimePart = MimePart
    { mimePartIndex :: !Int
    , mimePartContentType :: !Text
    , mimePartCharset :: !(Maybe Text)
    , mimePartTransferEncoding :: !Text
    , mimePartDisposition :: !(Maybe Text)
    , mimePartFilename :: !(Maybe Text)
    , mimePartBody :: !BS.ByteString
    }

data ParsedMailAttachment = ParsedMailAttachment
    { parsedMailAttachmentId :: !Text
    , parsedMailAttachmentFilename :: !Text
    , parsedMailAttachmentContentType :: !Text
    , parsedMailAttachmentBytes :: !BS.ByteString
    }
    deriving (Eq, Show)

-- | Render a deliberately small, text/plain RFC 5322 message for draft-only
-- mailbox APIs. Tool-layer validation has already restricted recipients to
-- bare addresses and excluded header controls. Body bytes are base64 encoded
-- so arbitrary Unicode and newlines cannot become headers.
renderMailDraftMime
    :: MailDraftContent
    -> Maybe (Text, Maybe Text) -- ^ In-Reply-To and References
    -> BS.ByteString
renderMailDraftMime content replyHeaders =
    BS.intercalate "\r\n" (headers <> ["", encodedBody, ""])
  where
    headers =
        concat
            [ addressHeader "To" content.mailDraftTo
            , addressHeader "Cc" content.mailDraftCc
            , addressHeader "Bcc" content.mailDraftBcc
            , ["Subject: " <> encodedHeader content.mailDraftSubject]
            , maybe [] renderReplyHeaders
                (replyHeaders >>= safeReplyHeaders)
            , [ "MIME-Version: 1.0"
              , "Content-Type: text/plain; charset=UTF-8"
              , "Content-Transfer-Encoding: base64"
              ]
            ]
    addressHeader name values
        | null values = []
        | otherwise =
            [TextEncoding.encodeUtf8 name <> ": "
                <> TextEncoding.encodeUtf8 (Text.intercalate ", " values)]
    encodedHeader value
        | Text.all (\character -> character >= ' ' && character <= '~') value =
            TextEncoding.encodeUtf8 value
        | otherwise =
            "=?UTF-8?B?"
                <> Base64.encode (TextEncoding.encodeUtf8 value)
                <> "?="
    encodedBody =
        BS.intercalate "\r\n" (chunks 76
            (Base64.encode (TextEncoding.encodeUtf8 (normalizeLines content.mailDraftBody))))
    renderReplyHeaders (inReplyTo, references) =
        ["In-Reply-To: " <> TextEncoding.encodeUtf8 inReplyTo]
            <> maybe [] (\value ->
                ["References: " <> TextEncoding.encodeUtf8 value]) references
    safeReplyHeaders (inReplyTo, references) = do
        checkedInReplyTo <- safeHeaderValue inReplyTo
        pure (checkedInReplyTo, references >>= safeHeaderValue)
    safeHeaderValue value
        | Text.null stripped = Nothing
        | BS.length (TextEncoding.encodeUtf8 stripped)
            > maximumReplyHeaderBytes = Nothing
        | not (Text.all isSafeAscii stripped) = Nothing
        | otherwise = Just stripped
      where
        stripped = Text.strip value
        isSafeAscii character = character >= ' ' && character <= '~'
    chunks width bytes
        | BS.null bytes = [""]
        | otherwise = let (before, after) = BS.splitAt width bytes
            in before : chunks width after
    normalizeLines =
        Text.replace "\r\n" "\n" . Text.replace "\r" "\n"

    maximumReplyHeaderBytes = 4096

parseMailMime :: BS.ByteString -> Either Text ParsedMailMime
parseMailMime raw
    | BS.null raw = Left "The IMAP server returned an empty MIME message."
    | BS.length raw > maximumRawMimeBytes =
        Left "The IMAP message exceeded the safe MIME parsing limit."
    | otherwise = do
        (_, parts) <- parseEntity 0 0 raw
        if length parts > maximumMimeParts
            then Left "The IMAP message contained too many MIME parts."
            else Right ParsedMailMime { parsedMailParts = parts }

mailMimeTextBody :: Int -> ParsedMailMime -> Maybe Text
mailMimeTextBody requestedMaximum parsed =
    fst <$> decodedTextBody (max 1 requestedMaximum) parsed

mailMimeTextBodyTruncated :: Int -> ParsedMailMime -> Bool
mailMimeTextBodyTruncated requestedMaximum parsed =
    maybe False snd (decodedTextBody (max 1 requestedMaximum) parsed)

mailMimeAttachments :: ParsedMailMime -> [ParsedMailAttachment]
mailMimeAttachments parsed =
    mapMaybe (attachmentFromPart maximumAttachmentDecodeBytes)
        parsed.parsedMailParts

mailMimeAttachmentContent
    :: Int -> Text -> ParsedMailMime -> Either Text ParsedMailAttachment
mailMimeAttachmentContent requestedMaximum rawAttachmentId parsed = do
    partIndex <- decodeAttachmentId rawAttachmentId
    part <- maybe
        (Left "The custom IMAP attachment reference is invalid.")
        Right
        (find ((== partIndex) . (.mimePartIndex)) parsed.parsedMailParts)
    if not (isAttachmentPart part)
        then Left "The custom IMAP attachment reference is invalid."
        else maybe
            (Left "The IMAP attachment exceeded the download limit or was invalid.")
            Right
            (attachmentFromPart maximum part)
  where
    maximum =
        max 1 (min maximumAttachmentDecodeBytes requestedMaximum)

decodedTextBody :: Int -> ParsedMailMime -> Maybe (Text, Bool)
decodedTextBody maximum parsed =
    decoded "text/plain"
        maximumTextDecodeBytes
        id
        <|> decoded "text/html" maximumTextDecodeBytes stripHtml
  where
    decoded contentType decodeMaximum transform = do
        part <- find
            (\candidate ->
                candidate.mimePartContentType == contentType
                    && not (isAttachmentPart candidate))
            parsed.parsedMailParts
        (bytes, sourceTruncated) <- decodeTransferPrefix
            decodeMaximum
            part
        let decodedBody = transform (decodeText part.mimePartCharset bytes)
            truncated =
                sourceTruncated || utf8Length decodedBody > maximum
        pure
            ( truncateUtf8 maximum decodedBody
            , truncated
            )

attachmentFromPart :: Int -> MimePart -> Maybe ParsedMailAttachment
attachmentFromPart maximum part
    | not (isAttachmentPart part) = Nothing
    | otherwise = do
        bytes <- decodeTransferBounded maximum part
        let filename = fromMaybe
                ("attachment-" <> Text.pack (show part.mimePartIndex)
                    <> extensionForContentType part.mimePartContentType)
                (part.mimePartFilename >>= nonEmptyText)
        pure ParsedMailAttachment
            { parsedMailAttachmentId = encodeAttachmentId part.mimePartIndex
            , parsedMailAttachmentFilename =
                Text.take maximumFilenameCharacters filename
            , parsedMailAttachmentContentType = part.mimePartContentType
            , parsedMailAttachmentBytes = bytes
            }

isAttachmentPart :: MimePart -> Bool
isAttachmentPart part =
    maybe False ((== "attachment") . Text.toCaseFold)
        part.mimePartDisposition
        || isJust (part.mimePartFilename >>= nonEmptyText)

parseEntity :: Int -> Int -> BS.ByteString -> Either Text (Int, [MimePart])
parseEntity depth nextIndex raw
    | depth >= maximumMimeDepth =
        Left "The IMAP message nested MIME parts too deeply."
    | nextIndex >= maximumMimeParts =
        Left "The IMAP message contained too many MIME parts."
    | otherwise = do
        (headers, body) <- splitHeadersBody raw
        parsedHeaders <- parseHeaders headers
        let rawContentType =
                fromMaybe "text/plain" (lookupHeader "content-type" parsedHeaders)
            (contentType, contentParameters) =
                parseHeaderValueWithParameters rawContentType
            transferEncoding = Text.toCaseFold . Text.strip $
                fromMaybe "7bit"
                    (lookupHeader "content-transfer-encoding" parsedHeaders)
            rawDisposition = lookupHeader "content-disposition" parsedHeaders
            (disposition, dispositionParameters) =
                maybe (Nothing, []) (\value ->
                    let (kind, parameters) =
                            parseHeaderValueWithParameters value
                    in (Just kind, parameters)) rawDisposition
            filename =
                lookup "filename" dispositionParameters
                    <|> lookup "name" contentParameters
            charset = lookup "charset" contentParameters
        if "multipart/" `Text.isPrefixOf` contentType
            then do
                boundary <- maybe
                    (Left "The IMAP multipart message omitted its boundary.")
                    Right
                    (lookup "boundary" contentParameters >>= nonEmptyText)
                children <- splitMultipart boundary body
                foldl'
                    (\acc child -> do
                        (index, accumulated) <- acc
                        (next, parsed) <- parseEntity (depth + 1) index child
                        pure (next, accumulated <> parsed))
                    (Right (nextIndex, []))
                    children
            else
                Right
                    ( nextIndex + 1
                    , [MimePart
                        { mimePartIndex = nextIndex
                        , mimePartContentType = contentType
                        , mimePartCharset = charset
                        , mimePartTransferEncoding = transferEncoding
                        , mimePartDisposition = disposition
                        , mimePartFilename = filename
                        , mimePartBody = body
                        }]
                    )

splitHeadersBody :: BS.ByteString -> Either Text (BS.ByteString, BS.ByteString)
splitHeadersBody raw =
    case firstSeparator raw of
        Nothing
            | BS.length raw <= maximumMimeHeaderBytes -> Right (raw, BS.empty)
            | otherwise -> Left "The IMAP MIME headers exceeded the safe limit."
        Just (offset, separatorLength)
            | offset > maximumMimeHeaderBytes ->
                Left "The IMAP MIME headers exceeded the safe limit."
            | otherwise ->
                Right
                    ( BS.take offset raw
                    , BS.drop (offset + separatorLength) raw
                    )
  where
    firstSeparator bytes =
        minimumMaybe
            [ (offset, BS.length separator)
            | separator <- ["\r\n\r\n", "\n\n"]
            , let (prefix, suffix) = BS.breakSubstring separator bytes
            , not (BS.null suffix)
            , let offset = BS.length prefix
            ]

parseHeaders :: BS.ByteString -> Either Text [(Text, Text)]
parseHeaders bytes
    | length physicalLines > maximumMimeHeaderLines =
        Left "The IMAP MIME headers contained too many lines."
    | otherwise =
        traverse parseHeader (unfoldHeaderLines physicalLines)
  where
    physicalLines = map stripTrailingCR (BS8.lines bytes)
    parseHeader line =
        let (rawName, rawValue) = BS8.break (== ':') line
            name = Text.toCaseFold . Text.strip $
                TextEncoding.decodeLatin1 rawName
            value = Text.strip . TextEncoding.decodeLatin1 $
                BS.drop 1 rawValue
        in if BS.null rawValue
                || Text.null name
                || Text.any (`elem` ['\r', '\n', '\NUL']) name
            then Left "The IMAP message contained invalid MIME headers."
            else Right (name, value)

unfoldHeaderLines :: [BS.ByteString] -> [BS.ByteString]
unfoldHeaderLines =
    reverse . foldl' add []
  where
    add [] line = [line]
    add (current : rest) line
        | maybe False isHorizontalSpaceByte (fst <$> BS.uncons line) =
            (current <> " " <> BS8.dropWhile isHorizontalSpaceChar line) : rest
        | otherwise = line : current : rest
    isHorizontalSpaceByte byte = byte == 32 || byte == 9
    isHorizontalSpaceChar character =
        character == ' ' || character == '\t'

lookupHeader :: Text -> [(Text, Text)] -> Maybe Text
lookupHeader requested =
    fmap snd . find ((== Text.toCaseFold requested) . fst)

parseHeaderValueWithParameters :: Text -> (Text, [(Text, Text)])
parseHeaderValueWithParameters raw =
    case splitSemicolonAware raw of
        [] -> ("", [])
        value : parameters ->
            ( Text.toCaseFold (Text.strip value)
            , mapMaybe parseParameter parameters
            )
  where
    parseParameter parameter =
        let (rawName, rawValue) = Text.breakOn "=" parameter
            name = Text.toCaseFold (Text.strip rawName)
            value = unquote (Text.strip (Text.drop 1 rawValue))
        in if Text.null rawValue || Text.null name
            then Nothing
            else Just (name, value)

splitSemicolonAware :: Text -> [Text]
splitSemicolonAware =
    reverse . finish . Text.foldl' step (False, [], [])
  where
    step (quoted, current, completed) character
        | character == '"' = (not quoted, character : current, completed)
        | character == ';' && not quoted =
            (quoted, [], Text.pack (reverse current) : completed)
        | otherwise = (quoted, character : current, completed)
    finish (_, current, completed) =
        Text.pack (reverse current) : completed

unquote :: Text -> Text
unquote value
    | Text.length value >= 2
        && Text.head value == '"'
        && Text.last value == '"' =
            unescapeQuoted (Text.init (Text.tail value))
    | otherwise = value

unescapeQuoted :: Text -> Text
unescapeQuoted text =
    case Text.uncons text of
        Nothing -> ""
        Just ('\\', rest) ->
            case Text.uncons rest of
                Nothing -> "\\"
                Just (character, suffix) ->
                    Text.cons character (unescapeQuoted suffix)
        Just (character, rest) ->
            Text.cons character (unescapeQuoted rest)

splitMultipart :: Text -> BS.ByteString -> Either Text [BS.ByteString]
splitMultipart boundary body
    | Text.any (`elem` ['\r', '\n', '\NUL']) boundary
        || Text.length boundary > maximumBoundaryCharacters =
            Left "The IMAP multipart boundary was invalid."
    | BS8.count '\n' body > maximumMimeBodyLines =
        Left "The IMAP multipart body contained too many lines."
    | otherwise =
        let marker = "--" <> TextEncoding.encodeUtf8 boundary
            closing = marker <> "--"
            lines' = splitLinesKeepingContent body
            (_, current, parts, closed) =
                foldl' (step marker closing) (False, [], [], False) lines'
            completed =
                if closed || null current
                    then parts
                    else BS.intercalate "\r\n" (reverse current) : parts
            ordered = reverse (filter (not . BS.null) completed)
        in if length ordered > maximumMimeParts
            then Left "The IMAP message contained too many MIME parts."
            else Right ordered
  where
    step marker closing state@(inside, current, parts, closed) line
        | closed = state
        | stripBoundaryWhitespace (stripTrailingCR line) == closing =
            (False, [], finishCurrent current parts, True)
        | stripBoundaryWhitespace (stripTrailingCR line) == marker =
            (True, [], finishCurrent current parts, False)
        | inside = (True, stripTrailingCR line : current, parts, False)
        | otherwise = state
    finishCurrent [] parts = parts
    finishCurrent current parts =
        BS.intercalate "\r\n" (reverse current) : parts

splitLinesKeepingContent :: BS.ByteString -> [BS.ByteString]
splitLinesKeepingContent = BS8.split '\n'

decodeTransferBounded :: Int -> MimePart -> Maybe BS.ByteString
decodeTransferBounded requestedMaximum part = do
    (bytes, truncated) <- decodeTransferPrefix requestedMaximum part
    if truncated then Nothing else Just bytes

decodeTransferPrefix :: Int -> MimePart -> Maybe (BS.ByteString, Bool)
decodeTransferPrefix requestedMaximum part
    | requestedMaximum < 1 = Nothing
    | transfer == "base64" = do
        let compact = BS8.filter (not . isBase64Whitespace) part.mimePartBody
            encodedMaximum = base64EncodedMaximum requestedMaximum
            encodedPrefix = BS.take encodedMaximum compact
        decoded <- either (const Nothing) Just (Base64.decode encodedPrefix)
        pure
            ( BS.take requestedMaximum decoded
            , BS.length compact > encodedMaximum
                || BS.length decoded > requestedMaximum
            )
    | transfer == "quoted-printable" =
        quotedPrintableDecodePrefix requestedMaximum part.mimePartBody
    | transfer `elem` ["7bit", "8bit", "binary", ""] =
        Just
            ( BS.take requestedMaximum part.mimePartBody
            , BS.length part.mimePartBody > requestedMaximum
            )
    | otherwise = Nothing
  where
    transfer = Text.toCaseFold (Text.strip part.mimePartTransferEncoding)
    isBase64Whitespace character =
        character == ' ' || character == '\t'
            || character == '\r' || character == '\n'

quotedPrintableDecodePrefix
    :: Int -> BS.ByteString -> Maybe (BS.ByteString, Bool)
quotedPrintableDecodePrefix maximum input =
    let decoded = LBS.toStrict . ByteStringBuilder.toLazyByteString $
            decodeAtMost (maximum + 1) input mempty
    in Just (BS.take maximum decoded, BS.length decoded > maximum)
  where
    decodeAtMost remaining bytes builder
        | remaining <= 0 || BS.null bytes = builder
        | otherwise =
            case decodeNext bytes of
                Nothing -> builder
                Just (Nothing, suffix) ->
                    decodeAtMost remaining suffix builder
                Just (Just byte, suffix) ->
                    decodeAtMost (remaining - 1) suffix
                        (builder <> ByteStringBuilder.word8 byte)
    decodeNext bytes =
        case BS.uncons bytes of
            Nothing -> Nothing
            Just (61, rest)
                | Just (13, afterCR) <- BS.uncons rest
                , Just (10, afterLF) <- BS.uncons afterCR ->
                    Just (Nothing, afterLF)
                | Just (10, afterLF) <- BS.uncons rest ->
                    Just (Nothing, afterLF)
                | Just (first, afterFirst) <- BS.uncons rest
                , Just (second, afterSecond) <- BS.uncons afterFirst
                , Just decoded <- decodeHex first second ->
                    Just (Just decoded, afterSecond)
                | otherwise -> Just (Just 61, rest)
            Just (byte, rest) -> Just (Just byte, rest)
    decodeHex first second
        | isHexDigit (chr (fromIntegral first))
            && isHexDigit (chr (fromIntegral second)) =
                Just (hexValue first * 16 + hexValue second)
        | otherwise = Nothing
    hexValue byte
        | byte >= 48 && byte <= 57 = byte - 48
        | otherwise =
            fromIntegral
                (ord (toLower (chr (fromIntegral byte))) - ord 'a' + 10)

decodeText :: Maybe Text -> BS.ByteString -> Text
decodeText rawCharset bytes =
    case Text.toCaseFold . Text.strip <$> rawCharset of
        Just "iso-8859-1" -> TextEncoding.decodeLatin1 bytes
        Just "latin1" -> TextEncoding.decodeLatin1 bytes
        Just "us-ascii" -> TextEncoding.decodeLatin1 bytes
        _ -> TextEncoding.decodeUtf8With TextEncodingError.lenientDecode bytes

encodeAttachmentId :: Int -> Text
encodeAttachmentId index =
    TextEncoding.decodeUtf8 . Base64URL.encodeUnpadded
        . TextEncoding.encodeUtf8 $
            "part:" <> Text.pack (show index)

decodeAttachmentId :: Text -> Either Text Int
decodeAttachmentId encoded = do
    bytes <- either
        (const (Left "The custom IMAP attachment reference is invalid."))
        Right
        (Base64URL.decodeUnpadded (TextEncoding.encodeUtf8 encoded))
    decoded <- either
        (const (Left "The custom IMAP attachment reference is invalid."))
        Right
        (TextEncoding.decodeUtf8' bytes)
    case Text.stripPrefix "part:" decoded >>= readMaybe . Text.unpack of
        Just index | index >= 0 && index < maximumMimeParts -> Right index
        _ -> Left "The custom IMAP attachment reference is invalid."

extensionForContentType :: Text -> Text
extensionForContentType contentType =
    case Text.toCaseFold contentType of
        "application/pdf" -> ".pdf"
        "image/jpeg" -> ".jpg"
        "image/png" -> ".png"
        "text/plain" -> ".txt"
        "text/csv" -> ".csv"
        _ -> ""

stripHtml :: Text -> Text
stripHtml = Text.unwords . Text.words . innerText . parseTags

nonEmptyText :: Text -> Maybe Text
nonEmptyText text
    | Text.null (Text.strip text) = Nothing
    | otherwise = Just text

minimumMaybe :: Ord value => [value] -> Maybe value
minimumMaybe = \case
    [] -> Nothing
    value : rest -> Just (foldl' min value rest)

stripTrailingCR :: BS.ByteString -> BS.ByteString
stripTrailingCR bytes =
    fromMaybe bytes (BS.stripSuffix "\r" bytes)

stripBoundaryWhitespace :: BS.ByteString -> BS.ByteString
stripBoundaryWhitespace =
    BS8.dropWhileEnd (\character -> character == ' ' || character == '\t')

base64EncodedMaximum :: Int -> Int
base64EncodedMaximum maximum =
    4 * ((maximum + 2) `div` 3)

maximumRawMimeBytes, maximumAttachmentDecodeBytes :: Int
maximumRawMimeBytes = 8 * 1024 * 1024
maximumAttachmentDecodeBytes = maximumRawMimeBytes

maximumMimeParts, maximumMimeDepth, maximumMimeHeaderBytes
    , maximumMimeHeaderLines, maximumMimeBodyLines :: Int
maximumMimeParts = 1000
maximumMimeDepth = 16
maximumMimeHeaderBytes = 256 * 1024
maximumMimeHeaderLines = 2000
maximumMimeBodyLines = 150 * 1024

maximumBoundaryCharacters, maximumFilenameCharacters :: Int
maximumBoundaryCharacters = 200
maximumFilenameCharacters = 255

maximumTextDecodeBytes :: Int
maximumTextDecodeBytes = 512 * 1024

truncateUtf8 :: Int -> Text -> Text
truncateUtf8 maximum =
    decodePrefix . BS.take maximum . TextEncoding.encodeUtf8
  where
    decodePrefix bytes =
        case TextEncoding.decodeUtf8' bytes of
            Right value -> value
            Left _
                | BS.null bytes -> ""
                | otherwise -> decodePrefix (BS.init bytes)

utf8Length :: Text -> Int
utf8Length = BS.length . TextEncoding.encodeUtf8
