-- | Trusted frontend interactions. Secret values never enter this protocol:
-- the server lends a private file for the frontend to fill and then removes it.
module Agent.Runtime.Session.Interaction
    ( LocalInput(..), encodeLocalInput, decodeLocalInput
    , encodeElicitationReply, decodeElicitationReply
    ) where

import Agent.Json (rawJsonFromEncoding)
import Agent.MCP (McpElicitRequest(..), McpElicitMode(..), McpElicitResult(..))
import Agent.Tools.Secret (SecretPrompt(..))
import Data.Aeson
import Data.Aeson.Types (Parser, parseEither)
import Data.ByteString.Lazy qualified as ByteString
import Data.Text (Text)
import Data.Text.Encoding qualified as Text

data LocalInput = LocalElicitation !McpElicitRequest | LocalSecret !SecretPrompt !FilePath

encodeLocalInput :: LocalInput -> Text
encodeLocalInput = Text.decodeUtf8 . ByteString.toStrict . encode . \case
    LocalElicitation request -> object
        [ "kind" .= ("elicitation" :: Text), "server" .= request.elicitServerName
        , "message" .= request.elicitMessage
        , "mode" .= case request.elicitMode of
            McpElicitForm schema -> object ["schema" .= schema]
            McpElicitUrl url -> object ["url" .= url] ]
    LocalSecret prompt path -> object
        [ "kind" .= ("secret" :: Text), "message" .= prompt.secretPromptMessage
        , "purpose" .= prompt.secretPromptPurpose, "path" .= path ]

decodeLocalInput :: Text -> Either String LocalInput
decodeLocalInput text = eitherDecodeStrict' (Text.encodeUtf8 text) >>= parseEither (withObject "local input" \value ->
    value .: "kind" >>= \case
        "elicitation" -> do
            name <- value .: "server"
            message <- value .: "message"
            mode <- value .: "mode" >>= withObject "elicitation mode" \fields -> fields .:? "url" >>= \case
                Just url -> pure (McpElicitUrl url)
                Nothing -> McpElicitForm . rawJsonFromEncoding . toEncoding <$> (fields .: "schema" :: Parser Value)
            pure (LocalElicitation (McpElicitRequest name message mode))
        "secret" -> LocalSecret <$> (SecretPrompt <$> value .: "message" <*> value .:? "purpose") <*> value .: "path"
        (_ :: Text) -> fail "unknown local input")

encodeElicitationReply :: McpElicitResult -> Text
encodeElicitationReply = Text.decodeUtf8 . ByteString.toStrict . encode . \case
    McpElicitAccept content -> object ["action" .= ("accept" :: Text), "content" .= content]
    McpElicitDecline -> object ["action" .= ("decline" :: Text)]
    McpElicitCancel -> object ["action" .= ("cancel" :: Text)]

decodeElicitationReply :: Text -> Either String McpElicitResult
decodeElicitationReply text = eitherDecodeStrict' (Text.encodeUtf8 text) >>= parseEither (withObject "elicitation result" \value ->
    value .: "action" >>= \case
        "accept" -> McpElicitAccept . fmap (rawJsonFromEncoding . toEncoding) <$> (value .:? "content" :: Parser (Maybe Value))
        "decline" -> pure McpElicitDecline
        "cancel" -> pure McpElicitCancel
        (_ :: Text) -> fail "invalid elicitation result")
