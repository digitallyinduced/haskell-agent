{-# LANGUAGE CPP #-}

module Main (main) where

import Control.Concurrent.Async (mapConcurrently)
import Control.Monad (unless)
import Data.Char (ord)
import Data.List (foldl')
import System.Environment
    ( getArgs
    , getEnvironment
    , getExecutablePath
    , lookupEnv
    )
import System.Exit (ExitCode(..), die, exitFailure)
import System.Process
    ( CreateProcess(..)
    , proc
    , waitForProcess
    , withCreateProcess
    )
import Test.Hspec (Spec, hspec)
import Test.Hspec.Runner (Config(..), Path, defaultConfig, hspecWith)
import Text.Read (readMaybe)

import qualified Agent.CLI.AccountSelectionSpec as AccountSelectionSpec
import qualified Agent.CLI.AgentSessionsSpec as AgentSessionsSpec
import qualified Agent.CLI.AgentViewportSpec as AgentViewportSpec
import qualified Agent.CLI.AgentViewportRuntimeSpec as AgentViewportRuntimeSpec
import qualified Agent.CLI.ApprovalSpec as ApprovalSpec
import qualified Agent.CLI.ArtifactSpec as ArtifactSpec
import qualified Agent.CLI.AuthSpec as AuthSpec
import qualified Agent.CLI.BtwSpec as BtwSpec
import qualified Agent.CLI.BrowserToolsSpec as BrowserToolsSpec
import qualified Agent.CLI.CancelWatchSpec as CancelWatchSpec
import qualified Agent.CLI.ClipboardSpec as ClipboardSpec
import qualified Agent.CLI.ClaudeGatewayProxySpec as ClaudeGatewayProxySpec
import qualified Agent.CLI.ClaudeSpec as ClaudeSpec
import qualified Agent.CLI.CommandSpec as CommandSpec
import qualified Agent.CLI.ComputerUseSpec as ComputerUseSpec
import qualified Agent.CLI.ConfigSpec as ConfigSpec
import qualified Agent.CLI.CompactionSpec as CompactionSpec
import qualified Agent.CLI.ContextSpec as ContextSpec
import qualified Agent.CLI.ConversationStoreSpec as ConversationStoreSpec
import qualified Agent.CLI.ConnectivitySpec as ConnectivitySpec
import qualified Agent.CLI.DialectsSpec as DialectsSpec
import qualified Agent.CLI.DatabaseSpec as DatabaseSpec
import qualified Agent.CLI.DesktopSpec as DesktopSpec
import qualified Agent.CLI.ExternalProgramSpec as ExternalProgramSpec
import qualified Agent.CLI.ExternalSessionSpec as ExternalSessionSpec
import qualified Agent.CLI.FileUriSpec as FileUriSpec
import qualified Agent.CLI.GatewayModelsSpec as GatewayModelsSpec
import qualified Agent.CLI.GitDiffSpec as GitDiffSpec
import qualified Agent.CLI.ImagePreviewSpec as ImagePreviewSpec
import qualified Agent.CLI.InputSpec as InputSpec
import qualified Agent.CLI.InterruptSpec as InterruptSpec
import qualified Agent.CLI.LoginSpec as LoginSpec
import qualified Agent.CLI.LearnedSkillsSpec as LearnedSkillsSpec
import qualified Agent.CLI.MarkdownSpec as MarkdownSpec
import qualified Agent.CLI.McpManagerSpec as McpManagerSpec
import qualified Agent.CLI.MetaConsoleSpec as MetaConsoleSpec
import qualified Agent.CLI.MetaConsoleRuntimeSpec as MetaConsoleRuntimeSpec
import qualified Agent.CLI.ModelPickerSpec as ModelPickerSpec
import qualified Agent.CLI.McpAdminSpec as McpAdminSpec
import qualified Agent.CLI.NativeAgentsSpec as NativeAgentsSpec
import qualified Agent.CLI.NativeRuntimeSpec as NativeRuntimeSpec
import qualified Agent.CLI.NotificationSpec as NotificationSpec
import qualified Agent.CLI.OptionsSpec as OptionsSpec
import qualified Agent.CLI.PendingInputsSpec as PendingInputsSpec
import qualified Agent.CLI.PermissionSpec as PermissionSpec
import qualified Agent.CLI.PickerSpec as PickerSpec
import qualified Agent.CLI.PlanSpec as PlanSpec
import qualified Agent.CLI.ProgressSpec as ProgressSpec
import qualified Agent.CLI.ProjectSpec as ProjectSpec
import qualified Agent.CLI.PromptSpec as PromptSpec
import qualified Agent.CLI.RecapSpec as RecapSpec
import qualified Agent.CLI.ProviderFallbackSpec as ProviderFallbackSpec
import qualified Agent.CLI.ProviderAvailabilitySpec as ProviderAvailabilitySpec
import qualified Agent.CLI.ProviderTransitionSpec as ProviderTransitionSpec
import qualified Agent.CLI.RequestSpec as RequestSpec
import qualified Agent.CLI.RepositoryDeliverySpec as RepositoryDeliverySpec
import qualified Agent.CLI.RepositoryReviewSpec as RepositoryReviewSpec
import qualified Agent.CLI.ResourceAdminSpec as ResourceAdminSpec
import qualified Agent.CLI.RenderSpec as RenderSpec
import qualified Agent.CLI.ReplStatusSpec as ReplStatusSpec
import qualified Agent.CLI.ResumeSpec as ResumeSpec
import qualified Agent.CLI.ReviewSpec as ReviewSpec
import qualified Agent.CLI.SecretSpec as SecretSpec
import qualified Agent.CLI.SessionHistorySpec as SessionHistorySpec
import qualified Agent.CLI.SessionStateSpec as SessionStateSpec
import qualified Agent.CLI.SessionTitleSpec as SessionTitleSpec
import qualified Agent.CLI.SkillsSpec as SkillsSpec
import qualified Agent.CLI.SubagentStoreSpec as SubagentStoreSpec
import qualified Agent.CLI.StyleSpec as StyleSpec
import qualified Agent.CLI.TimestampSpec as TimestampSpec
import qualified Agent.CLI.TranscriptSpec as TranscriptSpec
import qualified Agent.CLI.TranscriptExportSpec as TranscriptExportSpec
import qualified Agent.CLI.TurnSpec as TurnSpec
import qualified Agent.CLI.TerminalSpec as TerminalSpec
import qualified Agent.CLI.TextLayoutSpec as TextLayoutSpec
import qualified Agent.CLI.ToolsSpec as ToolsSpec
import qualified Agent.CLI.TUIAppSpec as TUIAppSpec
import qualified Agent.CLI.TUIBridgeSpec as TUIBridgeSpec
import qualified Agent.CLI.TUIComposerSpec as TUIComposerSpec
import qualified Agent.CLI.TUIImagePreviewSpec as TUIImagePreviewSpec
import qualified Agent.CLI.TUIHistorySpec as TUIHistorySpec
import qualified Agent.CLI.TUIPropertySpec as TUIPropertySpec
import qualified Agent.CLI.TUIScrollSpec as TUIScrollSpec
import qualified Agent.CLI.TUITranscriptSpec as TUITranscriptSpec
import qualified Agent.CLI.UsageSpec as UsageSpec
import qualified Agent.CLI.WebLspSpec as WebLspSpec
import qualified Agent.CLI.WorktreeSpec as WorktreeSpec
import qualified Agent.CLI.MacOS.EngineMailboxSpec as EngineMailboxSpec
import qualified Agent.CLI.MacOS.NativeLoopEventSpec as NativeLoopEventSpec
import qualified Agent.CLI.MacOS.TaskSchedulerSpec as TaskSchedulerSpec
#ifdef darwin_HOST_OS
import qualified Agent.CLI.MacOS.BridgeFFISpec as BridgeFFISpec
import qualified Agent.CLI.MacOS.BridgeHeaderSpec as BridgeHeaderSpec
import qualified Agent.CLI.MacOS.BridgeSpec as BridgeSpec
import qualified Agent.CLI.MacOS.BrowserBridgeFFISpec as BrowserBridgeFFISpec
#endif

main :: IO ()
main = do
    shard <- lookupEnv "AGENT_CLI_TEST_SHARD"
    case shard of
        Just value ->
            case parseShard value of
                Just (index, count) ->
                    hspecWith
                        defaultConfig
                            { configFilterPredicate =
                                Just (belongsToShard index count)
                            }
                        specs
                Nothing ->
                    die $
                        "invalid AGENT_CLI_TEST_SHARD (expected INDEX/COUNT): "
                            <> value
        Nothing -> do
            shardCount <- lookupEnv "AGENT_CLI_TEST_SHARDS"
            case shardCount of
                Nothing -> hspec specs
                Just value ->
                    case readMaybe value of
                        Just 1 -> hspec specs
                        Just count
                            | validShardCount count -> runShards count
                        _ ->
                            die $
                                "invalid AGENT_CLI_TEST_SHARDS "
                                    <> "(expected an integer from 1 to 32): "
                                    <> value

specs :: Spec
specs = do
    AccountSelectionSpec.spec
    AgentViewportSpec.spec
    AgentViewportRuntimeSpec.spec
    AgentSessionsSpec.spec
    ApprovalSpec.spec
    ArtifactSpec.spec
    AuthSpec.spec
    BtwSpec.spec
    BrowserToolsSpec.spec
#ifdef darwin_HOST_OS
    BrowserBridgeFFISpec.spec
    BridgeFFISpec.spec
    BridgeHeaderSpec.spec
    BridgeSpec.spec
#endif
    CancelWatchSpec.spec
    ClipboardSpec.spec
    ClaudeGatewayProxySpec.spec
    ClaudeSpec.spec
    CommandSpec.spec
    ComputerUseSpec.spec
    ConfigSpec.spec
    CompactionSpec.spec
    ContextSpec.spec
    ConversationStoreSpec.spec
    ConnectivitySpec.spec
    DialectsSpec.spec
    DatabaseSpec.spec
    DesktopSpec.spec
    ExternalProgramSpec.spec
    ExternalSessionSpec.spec
    FileUriSpec.spec
    GatewayModelsSpec.spec
    GitDiffSpec.spec
    ImagePreviewSpec.spec
    InputSpec.spec
    InterruptSpec.spec
    LoginSpec.spec
    LearnedSkillsSpec.spec
    MarkdownSpec.spec
    McpAdminSpec.spec
    McpManagerSpec.spec
    MetaConsoleSpec.spec
    MetaConsoleRuntimeSpec.spec
    ModelPickerSpec.spec
    NativeAgentsSpec.spec
    NativeRuntimeSpec.spec
    NotificationSpec.spec
    OptionsSpec.spec
    PendingInputsSpec.spec
    PermissionSpec.spec
    PickerSpec.spec
    PlanSpec.spec
    ProgressSpec.spec
    ProjectSpec.spec
    PromptSpec.spec
    RecapSpec.spec
    ProviderFallbackSpec.spec
    ProviderAvailabilitySpec.spec
    ProviderTransitionSpec.spec
    RequestSpec.spec
    RepositoryDeliverySpec.spec
    RepositoryReviewSpec.spec
    ResourceAdminSpec.spec
    RenderSpec.spec
    ReplStatusSpec.spec
    ResumeSpec.spec
    ReviewSpec.spec
    SecretSpec.spec
    StyleSpec.spec
    SessionHistorySpec.spec
    TimestampSpec.spec
    TranscriptSpec.spec
    TranscriptExportSpec.spec
    TurnSpec.spec
    TerminalSpec.spec
    TextLayoutSpec.spec
    SessionStateSpec.spec
    SessionTitleSpec.spec
    SkillsSpec.spec
    SubagentStoreSpec.spec
    ToolsSpec.spec
    TUIAppSpec.spec
    TUIBridgeSpec.spec
    TUIComposerSpec.spec
    TUIImagePreviewSpec.spec
    TUIHistorySpec.spec
    TUIPropertySpec.spec
    TUIScrollSpec.spec
    TUITranscriptSpec.spec
    UsageSpec.spec
    WebLspSpec.spec
    WorktreeSpec.spec
    EngineMailboxSpec.spec
    NativeLoopEventSpec.spec
    TaskSchedulerSpec.spec

runShards :: Int -> IO ()
runShards count = do
    executable <- getExecutablePath
    arguments <- getArgs
    environment <- getEnvironment
    exits <-
        mapConcurrently
            (runShard executable arguments environment count)
            [0 .. count - 1]
    unless (all (== ExitSuccess) exits) exitFailure

runShard
    :: FilePath
    -> [String]
    -> [(String, String)]
    -> Int
    -> Int
    -> IO ExitCode
runShard executable arguments environment count index =
    withCreateProcess
        (proc executable arguments)
            { env =
                Just $
                    ("AGENT_CLI_TEST_SHARD", show index <> "/" <> show count)
                        : filter
                            ( \entry ->
                                fst entry
                                    `notElem`
                                        [ "AGENT_CLI_TEST_SHARD"
                                        , "AGENT_CLI_TEST_SHARDS"
                                        ]
                            )
                            environment
            }
        \_ _ _ processHandle -> waitForProcess processHandle

parseShard :: String -> Maybe (Int, Int)
parseShard value =
    case break (== '/') value of
        (indexText, '/' : countText) -> do
            index <- readMaybe indexText
            count <- readMaybe countText
            if validShardCount count && index >= 0 && index < count
                then Just (index, count)
                else Nothing
        _ -> Nothing

validShardCount :: Int -> Bool
validShardCount count = count >= 1 && count <= 32

belongsToShard :: Int -> Int -> Path -> Bool
belongsToShard index count (groups, requirement) =
    stablePathHash (groups <> [requirement]) `mod` count == index

stablePathHash :: [String] -> Int
stablePathHash =
    foldl'
        (\hash character -> (hash * 33 + ord character) `mod` 2_147_483_647)
        5_381
        . unlines
