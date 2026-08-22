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
import Data.Aeson (FromJSON(..), Value)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
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
    , description :: !Text
    }

instance FromJSON HaskellProgramArgs where
    parseJSON = objectArgs \object -> HaskellProgramArgs
        <$> reqText object "source"
        <*> optIntOrString object "timeout"
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
    \`Text`, qualified `Text`, qualified `T`, `Value`, `object`, and `(.=)` are already imported.\n\
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
                result <- evalGhciProgram
                    session
                    (discardProgramResult args.source)
                    (normalizeTimeout (fromMaybe 30000 args.timeout))
                    (invokeProgramTool runtime outerCall)
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

invokeProgramTool
    :: ToolRuntime
    -> ToolCall
    -> Text
    -> Value
    -> IO Text
invokeProgramTool runtime outerCall requestedName arguments
    | Text.null toolName = pure "Error: nested tool name must not be empty"
    | toolName `elem` blockedTools =
        pure ("Error: " <> toolName <> " cannot be called from Haskell code mode")
    | otherwise = do
        unique <- newUnique
        let callId =
                outerCall.callId
                    <> "/haskell/"
                    <> Text.pack (show (hashUnique unique))
            nestedCall =
                functionToolCall callId toolName (encodeArguments arguments)
        (.output) <$> runtime.invokeNestedTool nestedCall
  where
    toolName = canonicalToolName requestedName
    blockedTools =
        [ haskellProgramToolName
        , "run_ghci"
        , "enter_plan_mode"
        , "exit_plan_mode"
        ]

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
