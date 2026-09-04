module Agent.MCP.Client.Internal.Operations where

import Agent.Json
    ( rawJsonBytes, rawJsonDecoder, rawJsonEncoding, RawJson )
import Agent.MCP.Client.Internal.Runtime
    ( McpRequest(requestName, requestHeaderParams, requestOnProgress),
      decodeMcpPayload,
      requestAndDecode,
      renderTextMcpResult,
      compactRawJson,
      encodeHeaderValue,
      clientRequest,
      requestMcpFull,
      invokeWithInputRounds,
      invokeWithInputRoundsT )
import Agent.MCP.Types
    ( McpTool(discoveredReadOnly, discoveredHeaderParams,
              discoveredName, discoveredTitle, discoveredDescription,
              discoveredInputSchema),
      McpHeaderParam(..),
      McpCompletion,
      McpPromptResult(promptResultMessages, promptResultDescription),
      McpPromptMessage(promptMessageRole, promptMessageContent),
      McpPrompt,
      McpResourceTemplate,
      McpResource,
      McpResourceContent(mcpResourceMimeType, mcpResourceText,
                         mcpResourceBlob, mcpResourceUri),
      McpSkillEntry,
      McpSkillsCapability,
      McpClient(clientConfig, clientDiscoveredSkills, clientTransport,
                clientServerInfo),
      McpClientTransport(McpClientStdio, McpClientHttp),
      McpProgress(progressMessage, progressValue, progressTotal),
      McpError,
      McpServerCapabilities(capabilitySkills),
      McpServerInfo(serverInfoCapabilities),
      McpServerConfig(mcpServerName),
      renderMcpError,
      mcpSkillEntryDecoder,
      mcpResourceContentDecoder,
      mcpResourceDecoder,
      mcpResourceTemplateDecoder,
      mcpPromptDecoder,
      mcpPromptResultDecoder,
      mcpCompletionDecoder,
      mcpToolDecoder,
      projectRawOr )
import Agent.ToolDispatch ( typedStreamingTool )
import Agent.Tools.IO ()
import Agent.Tools.Types
    ( AppTool(..),
      ApprovalRule(..),
      ToolExecutionPolicy(..),
      ToolSchema(..) )
import Control.Concurrent ()
import Control.Concurrent.Async ()
import Control.Concurrent.MVar ()
import Control.Concurrent.STM
    ( atomically, readTVarIO, modifyTVar' )
import Control.Exception.Safe ()
import Control.Monad ( forM )
import Control.Monad.Trans.Class ()
import Control.Monad.Trans.Except ( runExceptT )
import Data.Aeson
    ( Value(Array, Number, Object, String, Bool),
      object,
      KeyValue((.=)),
      ToJSON(toJSON) )
import Data.Char ( isAlphaNum, isAscii )
import Data.IORef ( atomicModifyIORef', newIORef )
import Data.List ()
import Data.Maybe ( catMaybes, fromMaybe, isJust, mapMaybe )
import Data.Scientific ( floatingOrInteger )
import Data.String ()
import Data.Text ( Text )
import Data.Text.Encoding.Error ()
import Data.Time.Clock.POSIX ()
import Data.Word ()
import GHC.Clock ()
import Network.HTTP.Client ()
import Network.HTTP.Client.TLS ()
import Network.HTTP.Types ()
import System.Environment ()
import System.IO ()
import System.IO.Unsafe ()
import System.Process ()
import System.Timeout ()
import qualified Data.Aeson as Aeson ( decodeStrict )
import qualified Data.Aeson.Encoding as AesonEncoding ( pair )
import qualified Data.Aeson.Encoding.Internal as AesonEncodingInternal
    ()
import qualified Data.ByteString as BS ()
import qualified Data.ByteString.Char8 as BS8 ()
import qualified Data.ByteString.Base64 as Base64 ()
import qualified Network.HTTP.Client as HC ()
import qualified Data.IntMap.Strict as IntMap ()
import qualified Agent.Json.Decode as Json
    ( Decoder,
      decodeEither,
      defaultKey,
      optionalKey,
      atKey,
      bool,
      getType,
      list,
      object,
      text,
      JsonError(jsonErrorMessage),
      ValueType(VArray, VObject) )
import qualified Data.Aeson.Key as Key ( fromText, toText )
import qualified Data.Aeson.KeyMap as KeyMap
    ( delete, elems, lookup, member, toList )
import qualified Data.ByteString.Lazy as LBS ()
import qualified Data.Map.Strict as Map ()
import qualified Agent.MCP.OAuth as OAuth ()
import qualified Data.Text as Text
    ( pack,
      unpack,
      empty,
      all,
      intercalate,
      length,
      null,
      replace,
      strip,
      toLower )
import qualified Data.Text.Encoding as TextEncoding ()

discoverMcpTools :: McpClient -> IO ([McpTool], [Text])
discoverMcpTools client = do
    tools <- paginate client "tools/list" "tools" mcpToolDecoder
        >>= either (ioError . userError . Text.unpack . renderMcpError) pure
    let isHttp = case client.clientTransport of
            McpClientHttp _ -> True
            McpClientStdio _ -> False
        annotated =
            [ (tool.discoveredName, annotateHeaderParams isHttp tool)
            | tool <- tools
            ]
        accepted = [tool | (_, Right tool) <- annotated]
        warnings =
            [ "MCP server " <> client.clientConfig.mcpServerName
                <> " tool " <> name <> " was rejected: " <> reason
            | (name, Left reason) <- annotated
            ]
    pure (accepted, warnings)

-- | Fetch every page of a list request.
paginate
    :: McpClient
    -> Text
    -> Text
    -> Json.Decoder a
    -> IO (Either McpError [a])
paginate client method key itemDecoder = runExceptT (go Nothing [])
  where
    go cursor collected = do
        let parameters = maybe mempty
                (\value -> AesonEncoding.pair "cursor" (rawJsonEncoding value))
                cursor
        (items, nextCursor) <-
            requestAndDecode client
                (clientRequest client method parameters)
                (method <> " response")
                pageDecoder
        case nextCursor of
            Just next -> go (Just next) (collected <> items)
            Nothing -> pure (collected <> items)

    pageDecoder = Json.object $
        (,)
            <$> Json.defaultKey [] key (Json.list itemDecoder)
            <*> Json.optionalKey "nextCursor" rawJsonDecoder

-- | Enumerate skill metadata only.  Skill resources are intentionally not
-- fetched here; hosts retrieve and verify SKILL.md when the user activates a
-- skill.
discoverMcpSkills :: McpClient -> IO [Text]
discoverMcpSkills client = do
    capability <- clientSkillsCapability client
    case capability of
        Nothing -> pure []
        Just _ -> go Nothing []
  where
    -- Skill discovery is deliberately not expressed as one McpCall: catalog
    -- failures are warnings and already decoded pages remain usable.
    go cursor warnings = do
        let parameters = maybe mempty
                (\value -> AesonEncoding.pair "cursor" (rawJsonEncoding value))
                cursor
        requestMcpFull client (clientRequest client "skills/list" parameters) >>= \case
            Left err -> pure ["MCP server " <> client.clientConfig.mcpServerName
                <> " skills/list failed: " <> renderMcpError err]
            Right result ->
                case Json.decodeEither pageDecoder (rawJsonBytes result) of
                    Left err -> pure ["MCP server "
                        <> client.clientConfig.mcpServerName
                        <> " returned invalid skills/list response: " <> err.jsonErrorMessage]
                    Right (rawSkills, nextCursor) -> do
                        let skills = mapMaybe decodeSkill rawSkills
                            invalid = length skills /= length rawSkills
                        atomically $ modifyTVar' client.clientDiscoveredSkills
                            (<> skills)
                        let pageWarnings =
                                [ "MCP server " <> client.clientConfig.mcpServerName
                                    <> " returned invalid skills/list entry"
                                | invalid
                                ]
                        case nextCursor of
                            Nothing -> pure (warnings <> pageWarnings)
                            Just next -> go (Just next) (warnings <> pageWarnings)

    pageDecoder = Json.object $
        (,) <$> Json.defaultKey [] "skills" (Json.list rawJsonDecoder)
            <*> Json.optionalKey "nextCursor" rawJsonDecoder
    decodeSkill value =
        case Json.decodeEither mcpSkillEntryDecoder (rawJsonBytes value) of
            Left _ -> Nothing
            Right skill -> Just skill

clientSkillsCapability :: McpClient -> IO (Maybe McpSkillsCapability)
clientSkillsCapability client =
    (>>= (.serverInfoCapabilities.capabilitySkills))
        <$> readTVarIO client.clientServerInfo

-- | Retrieve one skill manifest by URI.  Unlike 'skills/list', this also
-- supports servers whose catalog is not enumerable.
getMcpSkill :: McpClient -> Text -> IO (Either Text McpSkillEntry)
getMcpSkill client uri = do
    capability <- clientSkillsCapability client
    case capability of
        Nothing -> pure (Left ("MCP server "
            <> client.clientConfig.mcpServerName
            <> " does not support io.modelcontextprotocol/skills"))
        Just _ -> do
            result <- runExceptT $
                requestAndDecode client
                    (clientRequest client "skills/get" ("uri" .= uri))
                    "skills/get response"
                    (Json.object (Json.atKey "skill" mcpSkillEntryDecoder))
            pure (renderTextMcpResult "skills/get response" result)

-- | Read one or more resource contents using the standard MCP resources/read
-- method.  This does not activate a skill; callers must perform their own
-- approval, frontmatter, and manifest verification.
readMcpResource :: McpClient -> Text -> IO (Either Text [McpResourceContent])
readMcpResource client uri = do
    result <- runExceptT do
        raw <- invokeWithInputRoundsT client
            (clientRequest client "resources/read" ("uri" .= uri))
                { requestName = Just uri }
        decodeMcpPayload "resources/read response"
            (Json.object
                (Json.defaultKey [] "contents"
                    (Json.list mcpResourceContentDecoder)))
            raw
    pure (renderTextMcpResult "resources/read response" result)

listMcpResources :: McpClient -> IO (Either McpError [McpResource])
listMcpResources client =
    paginate client "resources/list" "resources" mcpResourceDecoder

listMcpResourceTemplates :: McpClient -> IO (Either McpError [McpResourceTemplate])
listMcpResourceTemplates client =
    paginate client "resources/templates/list" "resourceTemplates"
        mcpResourceTemplateDecoder

listMcpPrompts :: McpClient -> IO (Either McpError [McpPrompt])
listMcpPrompts client =
    paginate client "prompts/list" "prompts" mcpPromptDecoder

-- | Resolve a prompt template. Servers may ask for additional input first.
getMcpPrompt
    :: McpClient
    -> Text
    -> [(Text, Text)]
    -> IO (Either McpError McpPromptResult)
getMcpPrompt client name arguments =
    runExceptT do
        raw <- invokeWithInputRoundsT client
            (clientRequest client "prompts/get"
                ("name" .= name
                    <> "arguments" .= object
                        [Key.fromText key .= value | (key, value) <- arguments]))
                { requestName = Just name }
        decodeMcpPayload "prompts/get response" mcpPromptResultDecoder raw

data McpCompletionRef
    = McpCompletePrompt !Text
    | McpCompleteResource !Text
    deriving (Eq, Show)

-- | Request argument completions for a prompt or resource template.
completeMcpArgument
    :: McpClient
    -> McpCompletionRef
    -> Text
    -> Text
    -> [(Text, Text)]
    -> IO (Either McpError McpCompletion)
completeMcpArgument client ref argumentName partial context = do
    let refValue = case ref of
            McpCompletePrompt name ->
                object ["type" .= ("ref/prompt" :: Text), "name" .= name]
            McpCompleteResource uri ->
                object ["type" .= ("ref/resource" :: Text), "uri" .= uri]
        parameters =
            "ref" .= refValue
                <> "argument" .= object ["name" .= argumentName, "value" .= partial]
                <> (if null context
                        then mempty
                        else "context" .= object
                            [ "arguments" .= object
                                [Key.fromText key .= value | (key, value) <- context]
                            ])
    runExceptT $
        requestAndDecode client
            (clientRequest client "completion/complete" parameters)
            "completion/complete response"
            mcpCompletionDecoder

-- * Tools

appToolFor :: McpClient -> McpTool -> AppTool
appToolFor client tool = AppTool
    { appToolName = qualifiedName
    , appToolDescription = describeTool tool
    -- Tool schemas enter the legacy Aeson-valued tool API here. Their wire
    -- decode and storage remain RawJson.
    , appToolSchema =
        RawJsonFunctionSchema (toJSON tool.discoveredInputSchema)
    , appToolHandler =
        typedStreamingTool qualifiedName rawObjectDecoder \publish arguments -> do
            -- Snapshots accumulate: each progress line is appended to the
            -- text already shown for this call.
            shown <- newIORef Text.empty
            callDiscoveredToolWith client tool arguments $ Just \progress -> do
                snapshot <- atomicModifyIORef' shown \current ->
                    let next = current <> formatProgress progress
                    in (next, next)
                publish snapshot
    , appToolApproval =
        if tool.discoveredReadOnly then AlwaysReadOnly else AlwaysPrompt
    , appToolExecution =
        if tool.discoveredReadOnly then ParallelSafe else TurnSequential
    , appToolResourceClaims = Nothing
    }
  where
    qualifiedName = qualifiedMcpToolName
        client.clientConfig.mcpServerName
        tool.discoveredName

-- | Description shown to the model: the server's description, with the
-- human-readable title as a prefix when the server provides one.
describeTool :: McpTool -> Text
describeTool tool =
    case tool.discoveredTitle of
        Just title
            | not (Text.null (Text.strip title))
            , Text.strip title /= tool.discoveredName ->
                Text.strip title
                    <> (if Text.null tool.discoveredDescription
                        then ""
                        else ": " <> tool.discoveredDescription)
        _ -> tool.discoveredDescription

formatProgress :: McpProgress -> Text
formatProgress progress =
    Text.intercalate " "
        (catMaybes
            [ Just ("progress " <> showNumber progress.progressValue
                <> maybe "" (\total -> "/" <> showNumber total) progress.progressTotal)
            , progress.progressMessage
            ])
        <> "\n"
  where
    showNumber value
        | value == fromIntegral (round value :: Integer) =
            Text.pack (show (round value :: Integer))
        | otherwise = Text.pack (show value)

rawObjectDecoder :: Json.Decoder RawJson
rawObjectDecoder =
    Json.getType >>= \case
        Json.VObject -> rawJsonDecoder
        _ -> fail "expected object"

qualifiedMcpToolName :: Text -> Text -> Text
qualifiedMcpToolName serverName toolName =
    escapeComponent serverName <> "__" <> escapeComponent toolName
  where
    escapeComponent =
        Text.replace "__" "%5F%5F"
            . Text.replace "%" "%25"

callDiscoveredTool :: McpClient -> McpTool -> RawJson -> IO (Either Text Text)
callDiscoveredTool client tool arguments =
    callDiscoveredToolWith client tool arguments Nothing

callDiscoveredToolWith
    :: McpClient
    -> McpTool
    -> RawJson
    -> Maybe (McpProgress -> IO ())
    -> IO (Either Text Text)
callDiscoveredToolWith client tool arguments onProgress = do
    let parameters =
            "name" .= tool.discoveredName
                <> AesonEncoding.pair "arguments" (rawJsonEncoding arguments)
    invokeWithInputRounds client
        (clientRequest client "tools/call" parameters)
            { requestName = Just tool.discoveredName
            , requestHeaderParams = headerParamValues tool arguments
            , requestOnProgress = onProgress
            }
        >>= \case
        Left err -> pure (Left (renderMcpError err))
        Right result -> pure (normalizeMcpToolResult result)

-- | Render a @CallToolResult@ for the model.
normalizeMcpToolResult :: RawJson -> Either Text Text
normalizeMcpToolResult result =
    case Json.decodeEither mcpToolResultDecoder (rawJsonBytes result) of
        Left _ -> Right (compactRawJson result)
        Right (isError, structured, blocks) ->
            let rendered = mapMaybe renderContentBlock blocks
                output
                    | isJust structured && not (null rendered) =
                        compactRawJson result
                    | Just value <- structured = compactRawJson value
                    | not (null rendered) = Text.intercalate "\n" rendered
                    | otherwise = compactRawJson result
            in if isError then Left output else Right output

-- | Flatten a resolved prompt into one user turn. Assistant-authored
-- messages are labelled so the model can tell the two roles apart.
renderMcpPromptResult :: McpPromptResult -> Text
renderMcpPromptResult result =
    Text.intercalate "\n\n" $
        filter (not . Text.null) $
            maybe [] (\description -> ["# " <> description]) result.promptResultDescription
                <> map renderMessage result.promptResultMessages
  where
    renderMessage :: McpPromptMessage -> Text
    renderMessage message =
        let blocks = projectRawOr [] blocksDecoder message.promptMessageContent
            body = Text.intercalate "\n" (mapMaybe renderContentBlock blocks)
        in if message.promptMessageRole == "assistant"
            then "[assistant]\n" <> body
            else body
    blocksDecoder =
        Json.getType >>= \case
            Json.VArray -> Json.list contentBlockDecoder
            _ -> pure <$> contentBlockDecoder

data McpContentBlock
    = McpTextBlock !Text
    | McpImageBlock !(Maybe Text) !Int
    | McpAudioBlock !(Maybe Text) !Int
    | McpResourceLinkBlock !Text !(Maybe Text) !(Maybe Text) !(Maybe Text)
    | McpEmbeddedResourceBlock !McpResourceContent
    | McpUnknownBlock !Text
    deriving (Eq, Show)

renderContentBlock :: McpContentBlock -> Maybe Text
renderContentBlock = \case
    McpTextBlock text -> Just text
    McpImageBlock mimeType size ->
        Just ("[image " <> fromMaybe "image" mimeType <> ", "
            <> Text.pack (show size) <> " base64 bytes; binary content is not shown]")
    McpAudioBlock mimeType size ->
        Just ("[audio " <> fromMaybe "audio" mimeType <> ", "
            <> Text.pack (show size) <> " base64 bytes; binary content is not shown]")
    McpResourceLinkBlock uri name description mimeType ->
        Just ("[resource_link] " <> uri
            <> maybe "" (\value -> " (" <> value <> ")") name
            <> maybe "" (\value -> " [" <> value <> "]") mimeType
            <> maybe "" (": " <>) description)
    McpEmbeddedResourceBlock content ->
        Just $ case (content.mcpResourceText, content.mcpResourceBlob) of
            (Just text, _) ->
                "[resource " <> content.mcpResourceUri
                    <> maybe "" (\value -> " (" <> value <> ")") content.mcpResourceMimeType
                    <> "]\n" <> text
            (Nothing, Just blob) ->
                "[resource " <> content.mcpResourceUri
                    <> maybe "" (\value -> " (" <> value <> ")") content.mcpResourceMimeType
                    <> ": " <> Text.pack (show (Text.length blob))
                    <> " base64 bytes; binary content is not shown]"
            (Nothing, Nothing) -> "[resource " <> content.mcpResourceUri <> "]"
    McpUnknownBlock _ -> Nothing

mcpToolResultDecoder
    :: Json.Decoder (Bool, Maybe RawJson, [McpContentBlock])
mcpToolResultDecoder = Json.object do
    rawError <- Json.optionalKey "isError" rawJsonDecoder
    structured <- Json.optionalKey "structuredContent" rawJsonDecoder
    rawContent <- Json.optionalKey "content" rawJsonDecoder
    let isError = maybe False (projectRawOr False Json.bool) rawError
        blocks = maybe [] (projectRawOr [] (Json.list contentBlockDecoder)) rawContent
    pure (isError, structured, blocks)

contentBlockDecoder :: Json.Decoder McpContentBlock
contentBlockDecoder = Json.object do
    contentType <- Json.defaultKey "" "type" Json.text
    case contentType of
        "text" -> McpTextBlock <$> Json.defaultKey "" "text" Json.text
        "image" ->
            McpImageBlock
                <$> Json.optionalKey "mimeType" Json.text
                <*> (maybe 0 Text.length <$> Json.optionalKey "data" Json.text)
        "audio" ->
            McpAudioBlock
                <$> Json.optionalKey "mimeType" Json.text
                <*> (maybe 0 Text.length <$> Json.optionalKey "data" Json.text)
        "resource_link" ->
            McpResourceLinkBlock
                <$> Json.defaultKey "" "uri" Json.text
                <*> Json.optionalKey "name" Json.text
                <*> Json.optionalKey "description" Json.text
                <*> Json.optionalKey "mimeType" Json.text
        "resource" ->
            McpEmbeddedResourceBlock <$> Json.atKey "resource" mcpResourceContentDecoder
        other -> pure (McpUnknownBlock other)

-- * Header mirroring (@x-mcp-header@)

-- | Resolve the @x-mcp-header@ annotations of a tool's input schema. Stdio
-- transports ignore the annotations entirely; HTTP transports reject tools
-- whose annotations violate the specification's constraints.
annotateHeaderParams :: Bool -> McpTool -> Either Text McpTool
annotateHeaderParams isHttp tool
    | not isHttp = Right tool { discoveredHeaderParams = [] }
    | otherwise =
        case Aeson.decodeStrict (rawJsonBytes tool.discoveredInputSchema) of
            Nothing -> Right tool { discoveredHeaderParams = [] }
            Just schema -> do
                params <- collectHeaderParams schema
                validateHeaderNames params
                pure tool { discoveredHeaderParams = params }

collectHeaderParams :: Value -> Either Text [McpHeaderParam]
collectHeaderParams root = case root of
    Object fields
        | KeyMap.member "x-mcp-header" fields ->
            Left "x-mcp-header must annotate a property, not the schema root"
        | otherwise -> walkObject [] fields
    _ -> Right []
  where
    walkObject path fields =
        concat <$> forM (KeyMap.toList fields) (walkField path)

    walkField path (key, value)
        | Key.toText key == "properties" = walkProperties path value
        | containsHeaderAnnotation value =
            Left ("x-mcp-header under \"" <> Key.toText key
                <> "\" is not statically reachable from the schema root")
        | otherwise = Right []

    walkProperties path = \case
        Object properties ->
            concat <$> forM (KeyMap.toList properties) \(name, property) ->
                walkProperty (path <> [Key.toText name]) property
        _ -> Right []

    walkProperty path property = case property of
        Object fields -> do
            here <- case KeyMap.lookup "x-mcp-header" fields of
                Nothing -> Right []
                Just (String name) -> do
                    case KeyMap.lookup "type" fields of
                        Just (String kind)
                            | kind `elem` ["string", "integer", "boolean"] -> Right ()
                        _ -> Left ("x-mcp-header on \"" <> Text.intercalate "." path
                            <> "\" requires a string, integer, or boolean type")
                    Right [McpHeaderParam path name]
                Just _ -> Left ("x-mcp-header on \"" <> Text.intercalate "." path
                    <> "\" must be a string")
            nested <- walkObject path (KeyMap.delete "x-mcp-header" fields)
            Right (here <> nested)
        _ -> Right []

containsHeaderAnnotation :: Value -> Bool
containsHeaderAnnotation = \case
    Object fields ->
        KeyMap.member "x-mcp-header" fields
            || any containsHeaderAnnotation (KeyMap.elems fields)
    Array values -> any containsHeaderAnnotation values
    _ -> False

validateHeaderNames :: [McpHeaderParam] -> Either Text ()
validateHeaderNames params = go [] params
  where
    go :: [Text] -> [McpHeaderParam] -> Either Text ()
    go _ [] = Right ()
    go seen (param : rest)
        | Text.null param.headerParamName = Left "x-mcp-header must not be empty"
        | not (Text.all isToken param.headerParamName) =
            Left ("x-mcp-header \"" <> param.headerParamName
                <> "\" is not a valid HTTP field name")
        | Text.toLower param.headerParamName `elem` seen =
            Left ("x-mcp-header \"" <> param.headerParamName
                <> "\" is not unique within the tool")
        | otherwise = go (Text.toLower param.headerParamName : seen) rest
    isToken character =
        isAscii character
            && (isAlphaNum character || character `elem` ("!#$%&'*+-.^_`|~" :: String))

-- | Compute the @Mcp-Param-{name}@ headers for one call.
headerParamValues :: McpTool -> RawJson -> [(Text, Text)]
headerParamValues tool arguments
    | null tool.discoveredHeaderParams = []
    | otherwise =
        case Aeson.decodeStrict (rawJsonBytes arguments) of
            Nothing -> []
            Just value -> mapMaybe (paramHeader value) tool.discoveredHeaderParams
  where
    paramHeader :: Value -> McpHeaderParam -> Maybe (Text, Text)
    paramHeader value param = do
        leaf <- lookupPath param.headerParamPath value
        rendered <- renderHeaderValue leaf
        pure ("Mcp-Param-" <> param.headerParamName, encodeHeaderValue rendered)
    lookupPath [] value = Just value
    lookupPath (key : rest) value = case value of
        Object fields -> KeyMap.lookup (Key.fromText key) fields >>= lookupPath rest
        _ -> Nothing
    renderHeaderValue = \case
        String text -> Just text
        Bool flag -> Just (if flag then "true" else "false")
        Number number -> case (floatingOrInteger number :: Either Double Integer) of
            Right integer -> Just (Text.pack (show (integer :: Integer)))
            Left _ -> Nothing
        _ -> Nothing
