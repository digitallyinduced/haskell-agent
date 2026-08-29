module Agent.CLI.Lsp.Formatting
    ( formatLspResult
    ) where

import Agent.CLI.FileUri (fileUriPath)
import Agent.GrokBuild.Dialect.Lsp (LspOperation(..))
import Agent.Json
    ( RawJson
    , rawJsonBytes
    , rawJsonDecoder
    )
import Agent.Json.Decode
    ( decodeEither
    , optionalKey
    )
import Agent.Json.Decode qualified as Hermes
import Control.Applicative ((<|>))
import Data.Maybe (fromMaybe, maybeToList)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Text.Encoding.Error (lenientDecode)

formatLspResult :: LspOperation -> RawJson -> Text
formatLspResult operation value =
    case operation of
        GoToDefinition -> formatLocations "definition" value
        FindReferences -> formatLocations "references" value
        GoToImplementation -> formatLocations "implementations" value
        Hover -> formatHover value
        DocumentSymbol -> formatSymbols value
        WorkspaceSymbol -> formatSymbols value

formatLocations :: Text -> RawJson -> Text
formatLocations label value =
    case decodeRawJson locationsDecoder value of
        Left _ -> compactJson value
        Right [] -> "No " <> label <> " found."
        Right locations -> Text.intercalate "\n" locations

locationsDecoder :: Hermes.Decoder [Text]
locationsDecoder =
    Hermes.getType >>= \case
        Hermes.VArray ->
            concat <$> Hermes.list locationsDecoder
        Hermes.VObject ->
            maybeToList <$> locationDecoder
        _ -> [] <$ rawJsonDecoder

locationDecoder :: Hermes.Decoder (Maybe Text)
locationDecoder =
    Hermes.object do
        uriValue <- optionalKey "uri" Hermes.text
        targetUri <- optionalKey "targetUri" Hermes.text
        rangeValue <- optionalKey "range" startPositionDecoder
        selectionRange <-
            optionalKey "targetSelectionRange" startPositionDecoder
        targetRange <- optionalKey "targetRange" startPositionDecoder
        let uri = uriValue <|> targetUri
            range = rangeValue <|> selectionRange <|> targetRange
        pure do
            locationUri <- uri
            let (line, character) = fromMaybe (0, 0) range
                path = maybe locationUri Text.pack (fileUriPath locationUri)
            pure $
                path
                    <> ":"
                    <> Text.pack (show (line + 1))
                    <> ":"
                    <> Text.pack (show (character + 1))

startPositionDecoder :: Hermes.Decoder (Int, Int)
startPositionDecoder =
    Hermes.object $
        Hermes.atKey "start" $
            Hermes.object $
                (,)
                    <$> Hermes.atKey "line" Hermes.int
                    <*> Hermes.atKey "character" Hermes.int

formatHover :: RawJson -> Text
formatHover value =
    either (const (compactJson value)) id $
        decodeRawJson hoverDecoder value

hoverDecoder :: Hermes.Decoder Text
hoverDecoder =
    Hermes.getType >>= \case
        Hermes.VNull ->
            "No hover information found." <$ Hermes.isNull
        Hermes.VString -> Hermes.text
        Hermes.VArray ->
            Text.intercalate "\n\n" <$> Hermes.list hoverDecoder
        Hermes.VObject ->
            Hermes.withOwnedRawJson \bytes ->
                pure $
                    either
                        (const (Text.decodeUtf8With lenientDecode bytes))
                        (fromMaybe (Text.decodeUtf8With lenientDecode bytes))
                        (decodeEither hoverObjectDecoder bytes)
        _ ->
            rawTextDecoder

hoverObjectDecoder :: Hermes.Decoder (Maybe Text)
hoverObjectDecoder =
    Hermes.object do
        contents <- optionalKey "contents" hoverDecoder
        value <- optionalKey "value" Hermes.text
        language <- optionalKey "language" Hermes.text
        pure (contents <|> value <|> language)

formatSymbols :: RawJson -> Text
formatSymbols value =
    case decodeRawJson (symbolLinesDecoder 0) value of
        Left _ -> compactJson value
        Right [] -> "No symbols found."
        Right lines' -> Text.intercalate "\n" lines'

symbolLinesDecoder :: Int -> Hermes.Decoder [Text]
symbolLinesDecoder depth =
    Hermes.getType >>= \case
        Hermes.VArray ->
            concat <$> Hermes.list (symbolLinesDecoder depth)
        Hermes.VObject ->
            Hermes.object do
                name <- optionalKey "name" Hermes.text
                location <-
                    optionalKey "location" locationDecoder
                directLocation <- Hermes.liftObjectDecoder locationDecoder
                children <-
                    fromMaybe []
                        <$> optionalKey "children"
                            (symbolLinesDecoder (depth + 1))
                pure case name of
                    Nothing -> []
                    Just symbolName ->
                        let suffix =
                                maybe ""
                                    (" — " <>)
                                    ((location >>= id) <|> directLocation)
                            current =
                                Text.replicate depth "  "
                                    <> "- "
                                    <> symbolName
                                    <> suffix
                        in current : children
        _ -> [] <$ rawJsonDecoder

rawTextDecoder :: Hermes.Decoder Text
rawTextDecoder =
    Hermes.withOwnedRawJson $
        pure . Text.decodeUtf8With lenientDecode

decodeRawJson :: Hermes.Decoder a -> RawJson -> Either Text a
decodeRawJson decoder =
    either (Left . Hermes.jsonErrorMessage) Right
        . decodeEither decoder
        . rawJsonBytes

compactJson :: RawJson -> Text
compactJson =
    Text.decodeUtf8With lenientDecode . rawJsonBytes
