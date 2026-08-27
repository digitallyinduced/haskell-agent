module Agent.CLI.Lsp.Formatting
    ( formatLspResult
    ) where

import Agent.CLI.FileUri (fileUriPath)
import Agent.GrokBuild.Dialect.Lsp (LspOperation(..))
import Control.Applicative ((<|>))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe (fromMaybe)
import Data.Scientific (toBoundedInteger)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Text.Encoding.Error (lenientDecode)
import qualified Data.Vector as Vector

formatLspResult :: LspOperation -> Aeson.Value -> Text
formatLspResult operation value =
    case operation of
        GoToDefinition -> formatLocations "definition" value
        FindReferences -> formatLocations "references" value
        GoToImplementation -> formatLocations "implementations" value
        Hover -> formatHover value
        DocumentSymbol -> formatSymbols value
        WorkspaceSymbol -> formatSymbols value

formatLocations :: Text -> Aeson.Value -> Text
formatLocations label value =
    case collectLocations value of
        [] -> "No " <> label <> " found."
        locations -> Text.intercalate "\n" locations

collectLocations :: Aeson.Value -> [Text]
collectLocations = \case
    Aeson.Array values ->
        concatMap collectLocations (Vector.toList values)
    Aeson.Object object ->
        maybe [] pure (locationFromObject object)
    _ -> []

locationFromObject :: KeyMap.KeyMap Aeson.Value -> Maybe Text
locationFromObject object = do
    uri <- stringField "uri" object <|> stringField "targetUri" object
    let range =
            KeyMap.lookup "range" object
                <|> KeyMap.lookup "targetSelectionRange" object
                <|> KeyMap.lookup "targetRange" object
        (line, character) = fromMaybe (0, 0) (range >>= startPosition)
        path = maybe uri Text.pack (fileUriPath uri)
    pure $
        path <> ":" <> Text.pack (show (line + 1))
            <> ":" <> Text.pack (show (character + 1))

startPosition :: Aeson.Value -> Maybe (Int, Int)
startPosition (Aeson.Object range) = do
    Aeson.Object start <- KeyMap.lookup "start" range
    line <- integerField "line" start
    character <- integerField "character" start
    pure (line, character)
startPosition _ = Nothing

formatHover :: Aeson.Value -> Text
formatHover Aeson.Null = "No hover information found."
formatHover (Aeson.Object object) =
    maybe
        (compactJson (Aeson.Object object))
        formatHoverContents
        (KeyMap.lookup "contents" object)
formatHover value = formatHoverContents value

formatHoverContents :: Aeson.Value -> Text
formatHoverContents = \case
    Aeson.String value -> value
    Aeson.Array values ->
        Text.intercalate "\n\n"
            (map formatHoverContents (Vector.toList values))
    Aeson.Object object ->
        fromMaybe
            (compactJson (Aeson.Object object))
            (stringField "value" object <|> stringField "language" object)
    Aeson.Null -> "No hover information found."
    value -> compactJson value

formatSymbols :: Aeson.Value -> Text
formatSymbols value =
    case symbolLines 0 value of
        [] -> "No symbols found."
        lines' -> Text.intercalate "\n" lines'

symbolLines :: Int -> Aeson.Value -> [Text]
symbolLines depth = \case
    Aeson.Array values ->
        concatMap (symbolLines depth) (Vector.toList values)
    Aeson.Object object ->
        case stringField "name" object of
            Nothing -> []
            Just name ->
                let location =
                        KeyMap.lookup "location" object >>= \case
                            Aeson.Object locationObject ->
                                locationFromObject locationObject
                            _ -> Nothing
                    directLocation = locationFromObject object
                    suffix = maybe "" (" — " <>) (location <|> directLocation)
                    current =
                        Text.replicate depth "  " <> "- " <> name <> suffix
                    children =
                        maybe []
                            (symbolLines (depth + 1))
                            (KeyMap.lookup "children" object)
                in current : children
    _ -> []

stringField :: Text -> KeyMap.KeyMap Aeson.Value -> Maybe Text
stringField name object =
    case KeyMap.lookup (Key.fromText name) object of
        Just (Aeson.String value) -> Just value
        _ -> Nothing

integerField :: Text -> KeyMap.KeyMap Aeson.Value -> Maybe Int
integerField name object =
    case KeyMap.lookup (Key.fromText name) object of
        Just (Aeson.Number value) -> toBoundedInteger value
        _ -> Nothing

compactJson :: Aeson.Value -> Text
compactJson =
    Text.decodeUtf8With lenientDecode . LBS.toStrict . Aeson.encode
