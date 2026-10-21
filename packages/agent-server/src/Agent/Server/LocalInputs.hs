-- | Trusted local prompts use the durable request lifecycle. Secret replies
-- contain only an acknowledgement; values use a short-lived private file.
module Agent.Server.LocalInputs (localSessionCallbacks) where

import Agent.MCP (McpElicitResult(..))
import Agent.Runtime.Session.Engine (SessionCallbacks(..), defaultSessionCallbacks)
import Agent.Runtime.Session.Interaction
import Agent.Runtime.Session.SecretExchange
import Agent.Server.Supervisor (TurnControl(..))
import Agent.Server.Types
import Agent.Tools.Secret (SecretPromptHooks(..))
import Agent.Tools.ShowImage (ImageDisplayHooks(..), ImageDisplayRequest(..))
import Agent.Loop (ImageAttachment(..))
import Agent.Server.LocalEvents (localPresentationEvent, writeLocalTurnResult)
import Data.ByteString.Base64 qualified as Base64
import Data.Text.Encoding qualified as Text
import Data.Text (Text)
import Data.Aeson (object, (.=))
import System.Directory (getHomeDirectory)
import System.Environment (lookupEnv)
import System.FilePath ((</>))

localSessionCallbacks :: TurnControl -> SessionCallbacks
localSessionCallbacks control = defaultSessionCallbacks
    { sessionNotice = \message -> control.turnControlEmit "notice" (object ["message" .= message])
    , sessionCatalog = \catalog -> do
        (kind, payload) <- localPresentationEvent control.turnControlId.unTurnId "local_catalog" catalog
        control.turnControlEmit kind payload
    , sessionPreferences = \preferences -> do
        (kind, payload) <- localPresentationEvent control.turnControlId.unTurnId "local_preferences" preferences
        control.turnControlEmit kind payload
    , sessionAnswer = \answer -> do
        let value = object ["kind" .= ("answer" :: Text), "text" .= answer]
        writeLocalTurnResult control.turnControlId.unTurnId value
        (kind, payload) <- localPresentationEvent control.turnControlId.unTurnId "local_answer" value
        control.turnControlEmit kind payload
    , sessionImage = Just $ ImageDisplayHooks \request -> do
        (kind, payload) <- localPresentationEvent control.turnControlId.unTurnId "local_image" (object
            [ "callId" .= request.displayCallId, "path" .= request.displayPath
            , "caption" .= request.displayCaption, "mime" .= request.displayImage.imageMime
            , "bytes" .= Text.decodeUtf8 (Base64.encode request.displayImage.imageBytes) ])
        control.turnControlEmit kind payload
        pure (Right ())
    , sessionElicit = Just \request ->
        ask (LocalElicitation request) >>= \case
            Right response | response.humanResponseDecision == "answer"
                , Just encoded <- response.humanResponseValue ->
                    pure (either (const McpElicitCancel) id (decodeElicitationReply encoded))
            _ -> pure McpElicitCancel
    , sessionSecret = Just $ SecretPromptHooks \prompt -> do
        home <- getHomeDirectory
        root <- maybe (home </> ".haskell-agent/runtime") id <$> lookupEnv "HASKELL_AGENT_RUNTIME_DIR"
        withSecretExchange (root </> "local-inputs") \path ->
            ask (LocalSecret prompt path) >>= \case
                Right response | response.humanResponseDecision == "answer" -> Right . Just <$> readSecretExchange path
                Left err -> pure (Left err)
                _ -> pure (Right Nothing)
    }
  where
    ask input = control.turnControlRequestInput HumanRequestSpec
        { humanRequestSpecKind = LocalInputRequest
        , humanRequestSpecPrompt = encodeLocalInput input
        , humanRequestSpecOptions = ["answer", "cancel"] }
