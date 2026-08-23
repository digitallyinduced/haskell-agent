-- | Approval-gated Haskell orchestration over a dedicated GHCi runtime.
--
-- Nested calls re-enter the active agent loop, so they retain the normal
-- approval, plan-mode, cancellation, event, and registered-tool behavior.
module Agent.Tools.HaskellProgram
    ( haskellProgramTool
    , haskellProgramToolName
    ) where

import Agent.ToolArgs
    ( objectArgs
    , optIntOrString
    , reqText
    )
import Agent.Responses.Types
    ( Response
    , ResponseCreateParams
    )
import Agent.ToolDSL
    ( PropertySchema(..)
    , PropertyType(..)
    )
import Agent.ToolDispatch
    ( ToolCall(..)
    , ToolCallResult(..)
    , ToolRuntime(..)
    , canonicalToolName
    , functionToolCall
    , typedToolWithRuntimeAndCall
    )
import Agent.Tools.Ghci.Runtime
    ( GhciOutcome(..)
    , GhciProgramRequest(..)
    , GhciProgramResponse(..)
    , GhciResult(..)
    , GhciSession
    , evalGhciProgram
    )
import Agent.Tools.PlanMode
    ( PlanModeEnv
    , isPlanModeActive
    )
import Agent.Tools.Types
    ( AppTool
    , ApprovalRule(..)
    , ToolExecutionPolicy(..)
    , jsonAppToolWithExecution
    )
import Control.Concurrent.Async (concurrently)
import Data.Aeson (FromJSON(..), Value)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import Data.IORef
    ( IORef
    , atomicModifyIORef'
    , newIORef
    )
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import Data.Unique (hashUnique, newUnique)

haskellProgramToolName :: Text
haskellProgramToolName = "run_haskell_program"

data HaskellProgramArgs = HaskellProgramArgs
    { source :: !Text
    , timeout :: !(Maybe Int)
    , maxLlmCalls :: !(Maybe Int)
    , description :: !Text
    }

instance FromJSON HaskellProgramArgs where
    parseJSON = objectArgs \object -> HaskellProgramArgs
        <$> reqText object "source"
        <*> optIntOrString object "timeout"
        <*> optIntOrString object "max_llm_calls"
        <*> reqText object "description"

haskellProgramTool :: GhciSession -> PlanModeEnv -> AppTool
haskellProgramTool session planMode =
    jsonAppToolWithExecution
        haskellProgramToolName
        haskellProgramDescription
        [ PropertySchema "source" PropertyString True $ Just
            "Complete Haskell expression or statement, usually a do block."
        , PropertySchema "timeout" PropertyInteger False $ Just
            "Optional overall timeout in milliseconds (max 300000). Default: 30000."
        , PropertySchema "max_llm_calls" PropertyInteger False $ Just
            "Maximum callLLM invocations in this program (0-64). Default: 16."
        , PropertySchema "description" PropertyString True $ Just
            "One sentence explaining what the program will orchestrate."
        ]
        -- The current GHCi process is not an OS sandbox. Even though callTool
        -- preserves nested approvals, arbitrary Haskell IO can bypass it.
        PromptEveryCall
        TurnSequential
        (typedToolWithRuntimeAndCall haskellProgramToolName
            (runHaskellProgram session planMode))

haskellProgramDescription :: Text
haskellProgramDescription =
    "Run a Haskell program in a fresh dedicated GHCi process.\n\
    \Use this to orchestrate multiple tool calls, filter large results, and return only a compact answer.\n\
    \Inside the program, call `callTool \"tool_name\" (object [\"key\" .= value]) :: IO Text`.\n\
    \Use `callLLM request :: IO Response` for an isolated raw Responses API call. `ResponseCreateParams`, `Response`, their related canonical types, and `defaultResponseCreateParams` are preimported from `Agent.Responses.Types`. Provider transports may enforce required wire settings; the OpenAI Codex transport sends streaming, non-stored responses.\n\
    \When updating a raw request, give the binding an explicit signature to disambiguate canonical record fields. For OpenAI Codex, supply message items rather than bare text: `let request :: ResponseCreateParams; request = defaultResponseCreateParams { input = Just (ResponseInputItems [MessageItem ResponseMessage { messageId = Nothing, content = MessageContentParts [InputTextPart prompt Nothing mempty], role = RoleUser, status = Nothing, phase = Nothing, extraFields = mempty }]), model = Just \"model-name\" }`.\n\
    \callLLM returns the complete lossless response, including output items, usage, response ids, unknown fields, and raw tool calls; it does not run an automatic nested agent loop.\n\
    \For independent calls, `Concurrently(..)` and `runConcurrently` are preimported; combine `Concurrently (callTool ...)` or `Concurrently (callLLM ...)` actions applicatively to fan out.\n\
    \For a dynamic list, use `runConcurrently (traverse (Concurrently . action) values)`; tuple field syntax such as `.1` is not valid Haskell.\n\
    \Only tools registered as parallel-safe overlap. Stateful tools remain serialized, and concurrently submitted stateful calls have no defined order.\n\
    \`Text`, qualified `Text`, qualified `T`, `Value`, `object`, `(.=)`, `sort`, `sortOn`, `nub`, `group`, `mapMaybe`, `catMaybes`, and `fromMaybe` are also imported. Do not place `import` declarations inside the program expression.\n\
    \Use only tools and argument schemas advertised for the current provider. callTool returns the same formatted Text a direct tool call would return, including status metadata such as shell exit lines.\n\
    \Nested calls use the normal harness approvals and tool dispatch. Their results stay inside GHCi unless you pass selected text to `emitText`.\n\
    \Each outer invocation has isolated Haskell bindings and must be a complete program.\n\
    \End IO programs with `pure ()`; ordinary stdout is also returned as outer tool output.\n\
    \Do not call `run_haskell_program` or `run_ghci` from inside the program.\n\
    \This tool is unavailable while Plan Mode is active.\n\
    \The GHCi runtime is not OS-sandboxed, so every outer program requires approval."

runHaskellProgram
    :: GhciSession
    -> PlanModeEnv
    -> ToolRuntime
    -> ToolCall
    -> HaskellProgramArgs
    -> IO (Either Text Text)
runHaskellProgram session planMode runtime outerCall args
    | Text.null (Text.strip args.description) =
        pure (Left "Missing parameter: description")
    | Text.null (Text.strip args.source) =
        pure (Left "Missing parameter: source")
    | otherwise = do
        planActive <- isPlanModeActive planMode
        if planActive
            then pure $ Left
                "run_haskell_program is unavailable in Plan Mode because \
                \unrestricted Haskell IO could modify files outside plan.md."
            else do
                llmCallCount <- newIORef (0 :: Int)
                result <- evalGhciProgram
                    session
                    (discardProgramResult args.source)
                    (normalizeTimeout (fromMaybe 30000 args.timeout))
                    (invokeProgramRequests
                        runtime
                        outerCall
                        llmCallCount
                        (normalizeMaxLlmCalls
                            (fromMaybe 16 args.maxLlmCalls)))
                pure (Right (formatProgramResult result))

-- GHCi prints the value returned by a top-level @IO a@ action. Discard that
-- value so a program ending directly in @callTool@ cannot accidentally copy
-- the full nested result into model-visible stdout.
discardProgramResult :: Text -> Text
discardProgramResult source =
    "(\n"
        <> Text.unlines
            [ "    " <> line
            | line <- Text.lines source
            ]
        <> ") >> pure ()"

data PreparedProgramRequest
    = PreparedTool !(Either Text ToolCall)
    | PreparedLlm !Bool !ResponseCreateParams

invokeProgramRequests
    :: ToolRuntime
    -> ToolCall
    -> IORef Int
    -> Int
    -> [GhciProgramRequest]
    -> IO [GhciProgramResponse]
invokeProgramRequests runtime outerCall llmCallCount maxCalls requests = do
    allowedLlmCalls <- reserveLlmCalls
        llmCallCount
        maxCalls
        (length [() | GhciLlmRequest{} <- requests])
    prepared <- prepareRequests allowedLlmCalls requests
    let toolCalls =
            [ call
            | PreparedTool (Right call) <- prepared
            ]
        llmRequests =
            [ request
            | PreparedLlm True request <- prepared
            ]
        invokeTools
            | null toolCalls = pure []
            | otherwise = runtime.invokeNestedTools toolCalls
        invokeLlms
            | null llmRequests = pure []
            | otherwise = runtime.invokeNestedResponses llmRequests
    (toolResults, llmResults) <- concurrently invokeTools invokeLlms
    pure (restore prepared toolResults llmResults)
  where
    blockedTools =
        [ haskellProgramToolName
        , "run_ghci"
        , "enter_plan_mode"
        , "exit_plan_mode"
        ]
    prepareTool requestedName arguments
        | Text.null toolName =
            pure (Left "Error: nested tool name must not be empty")
        | toolName `elem` blockedTools =
            pure (Left
                ("Error: " <> toolName
                    <> " cannot be called from Haskell code mode"))
        | otherwise = do
            unique <- newUnique
            let callId =
                    outerCall.callId
                        <> "/haskell/"
                        <> Text.pack (show (hashUnique unique))
            pure (Right
                (functionToolCall callId toolName
                    (encodeArguments arguments)))
      where
        toolName = canonicalToolName requestedName

    prepareRequests _ [] = pure []
    prepareRequests allowed (request : rest) = case request of
        GhciToolRequest requestedName arguments -> do
            prepared <- PreparedTool <$> prepareTool requestedName arguments
            (prepared :) <$> prepareRequests allowed rest
        GhciLlmRequest llmRequest ->
            let permitted = allowed > 0
            in (PreparedLlm permitted llmRequest :)
                <$> prepareRequests
                    (if permitted then allowed - 1 else 0)
                    rest

    restore
        :: [PreparedProgramRequest]
        -> [ToolCallResult]
        -> [Either Text Response]
        -> [GhciProgramResponse]
    restore [] _ _ = []
    restore (PreparedTool (Left output) : rest) toolResults llmResults =
        GhciToolResponse output : restore rest toolResults llmResults
    restore (PreparedTool (Right _) : rest)
            (result : toolResults) llmResults =
        GhciToolResponse result.output
            : restore rest toolResults llmResults
    restore (PreparedTool (Right _) : rest) [] llmResults =
        GhciToolResponse "Nested tool bridge failed: missing result"
            : restore rest [] llmResults
    restore (PreparedLlm False _ : rest) toolResults llmResults =
        GhciLlmResponse
            (Left
                ("callLLM limit exceeded: this run_haskell_program call allows "
                    <> Text.pack (show maxCalls)
                    <> " LLM calls"))
            : restore rest toolResults llmResults
    restore (PreparedLlm True _ : rest) toolResults
            (result : llmResults) =
        GhciLlmResponse result
            : restore rest toolResults llmResults
    restore (PreparedLlm True _ : rest) toolResults [] =
        GhciLlmResponse
            (Left "Nested LLM bridge failed: missing result")
            : restore rest toolResults []

encodeArguments :: Value -> Text
encodeArguments =
    TextEncoding.decodeUtf8 . LBS.toStrict . Aeson.encode

formatProgramResult :: GhciResult -> Text
formatProgramResult result =
    let status = case result.ghciOutcome of
            GhciCompleted
                | result.ghciOk -> "ok"
                | otherwise -> "error"
            GhciTimedOut
                | result.ghciRestarted ->
                    "timeout (session restarted; bindings lost)"
                | otherwise -> "timeout"
            GhciCancelled
                | result.ghciRestarted ->
                    "cancelled (session restarted; bindings lost)"
                | otherwise -> "cancelled"
            GhciProcessFailed
                | result.ghciRestarted ->
                    "process failed (session restarted; bindings lost)"
                | otherwise -> "process failed"
        truncated =
            if result.ghciTruncated then "\noutput: truncated" else ""
        body
            | Text.null result.ghciOutput =
                "\n(no output; call emitText to return selected text)"
            | otherwise = "\n" <> result.ghciOutput
    in "status: " <> status <> truncated <> body

normalizeTimeout :: Int -> Int
normalizeTimeout = min 300000 . max 1

normalizeMaxLlmCalls :: Int -> Int
normalizeMaxLlmCalls = min 64 . max 0

reserveLlmCalls :: IORef Int -> Int -> Int -> IO Int
reserveLlmCalls countRef maximum requested =
    atomicModifyIORef' countRef \used ->
        let available = max 0 (maximum - used)
            accepted = min available requested
        in (used + accepted, accepted)
