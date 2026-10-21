-- | CLI presentation and lifecycle helpers for Agent Skills.
module Agent.CLI.Skills
    ( formatSkillsListing
    , installSkillCatalog
    , installSkillCatalogWithOmissions
    , installSkillToolRoots
    , loadMcpSkillsCatalog
    , loadOperatorSkillsCatalog
    , loadSkillsCatalog
    , loadSkillsCatalogQuiet
    , mergeSkillCatalogs
    , queueSkillCatalogContext
    , queueSkillCatalogContextWithOmissions
    , reservedSlashNames
    , resolvePromptSkillMentions
    , resolvePromptSkillMentionsWithWarnings
    , resolveSkillContent
    , skillCatalogContextMissing
    , skillInvocationCommand
    , verifyMcpSkillContent
    ) where

import Agent.CLI.Command
    ( SkillCommand(..)
    , SlashCommand(..)
    , slashCommands
    )
import Agent.CLI.Options (CliOptions(..))
import Agent.CLI.Render (putTextLn)
import Agent.CLI.Style
    ( glyphSession
    , glyphWarn
    , roleMuted
    , rolePrompt
    , roleWarn
    )
import Agent.CLI.Terminal (resolveColor)
import Agent.Json (rawJsonBytes)
import Agent.MCP.Fleet qualified as MCP
import Agent.MCP.Types
    ( McpFleet
    , McpResourceContent(..)
    , McpSkillEntry(..)
    , McpSkillRegistration(..)
    , McpSkillResource(..)
    , McpSkillResources(..)
    )
import Agent.OsPath (toText, unsafeToFilePath)
import Agent.OpenAI.Compaction (hasUserTextContaining)
import Agent.Responses.Types (ResponseItem)
import Agent.Skills
import Agent.Runtime.Startup.Context (mergeSkillCatalogs)
import Agent.Runtime.Skills.Content (loadMcpSkillsCatalog, resolveSkillContent, verifyMcpSkillContent)
import Agent.Tools.Types (ToolEnv, setToolSkillRoots)
import Crypto.Hash (Digest, SHA256, hash)
import Data.Aeson qualified as Aeson
import Data.ByteString qualified as BS
import Control.Monad (void, when)
import Data.IORef (IORef, atomicModifyIORef', writeIORef)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import Paths_agent_cli (getDataFileName)
import qualified System.Directory as Directory
import qualified System.Environment as Environment
import qualified System.FilePath as FilePath
import Agent.CLI.TerminalDiagnostics (getTerminalStderr)
import System.OsPath (OsPath, takeDirectory, unsafeEncodeUtf, (</>))

reservedSlashNames :: [Text]
reservedSlashNames =
    concatMap
        (\command -> command.slashName : command.slashAliases)
        slashCommands

resolvePromptSkillMentions
    :: Bool
    -> [SkillInvocation]
    -> Text
    -> Either Text [SkillInvocation]
resolvePromptSkillMentions pasted invocations prompt =
    Right (snd (resolvePromptSkillMentionsWithWarnings pasted invocations prompt))

resolvePromptSkillMentionsWithWarnings
    :: Bool
    -> [SkillInvocation]
    -> Text
    -> ([Text], [SkillInvocation])
resolvePromptSkillMentionsWithWarnings pasted invocations prompt
    | pasted = ([], [])
    | otherwise = resolveSkillMentionsWithWarnings invocations prompt

loadSkillsCatalog
    :: CliOptions
    -> OsPath
    -> OsPath
    -> OsPath
    -> Bool
    -> IO SkillCatalog
loadSkillsCatalog options home projectRoot cwd report
    | not options.optSkills = pure (SkillCatalog [] [])
    | otherwise = do
        catalog <- loadSkillsCatalogQuiet options home projectRoot cwd
        when report do
            stderr <- getTerminalStderr
            color <- resolveColor stderr
            let count = length catalog.catalogSkills
            putTextLn stderr
                (roleMuted color
                    (glyphSession
                        <> "skills: loaded "
                        <> Text.pack (show count)
                        <> if count == 1 then " skill" else " skills"))
            mapM_ (reportSkillWarning color) catalog.catalogWarnings
        pure catalog

loadSkillsCatalogQuiet
    :: CliOptions
    -> OsPath
    -> OsPath
    -> OsPath
    -> IO SkillCatalog
loadSkillsCatalogQuiet options home projectRoot cwd
    | not options.optSkills = pure (SkillCatalog [] [])
    | otherwise = do
        builtinRoot <- packagedSkillsRoot cwd
        discoverSkills SkillDiscoverOptions
            { skillsHome = home
            , skillsProjectRoot = projectRoot
            , skillsCwd = cwd
            , skillsMaxDepth = skillDiscoveryDepth
            , skillsBuiltinRoots =
                [(AgentSkills, unsafeEncodeUtf builtinRoot)]
            }

-- | Skills from directories that the embedding's operator controls, such as
-- a deployment's product skills. They are trusted built-in skills and load
-- independently of workspace discovery and of 'optSkills', which governs
-- only workspace, user and MCP skills.
loadOperatorSkillsCatalog :: [OsPath] -> IO SkillCatalog
loadOperatorSkillsCatalog roots =
    discoverBuiltinSkills skillDiscoveryDepth
        [ (AgentSkills, root) | root <- roots ]

skillDiscoveryDepth :: Int
skillDiscoveryDepth = 6

-- | Convert the untrusted metadata advertised by Skills-over-MCP servers into
-- lightweight catalog entries. Invalid entries are omitted and surfaced using
-- the existing catalog warning channel.


packagedSkillsRoot :: OsPath -> IO FilePath
packagedSkillsRoot cwd = do
    installedSkill <- getDataFileName "skills/add-model/SKILL.md"
    executable <- Environment.getExecutablePath
    let roots =
            take 16 (iterate FilePath.takeDirectory executable)
                <> take 8
                    (iterate FilePath.takeDirectory (unsafeToFilePath cwd))
        candidates =
            FilePath.takeDirectory (FilePath.takeDirectory installedSkill)
                : [ root FilePath.</> "packages/agent-cli/skills"
                  | root <- roots
                  ]
    firstExisting candidates >>= \case
        Just path -> pure path
        Nothing ->
            pure (FilePath.takeDirectory (FilePath.takeDirectory installedSkill))
  where
    firstExisting = \case
        [] -> pure Nothing
        path : rest ->
            Directory.doesDirectoryExist path >>= \case
                True -> pure (Just path)
                False -> firstExisting rest

reportSkillWarning :: Bool -> SkillWarning -> IO ()
reportSkillWarning color warning = do
    stderr <- getTerminalStderr
    putTextLn stderr $
        roleWarn color
            (glyphWarn
                <> "skill ignored: "
                <> toText warning.skillWarningPath
                <> ": "
                <> warning.skillWarningMessage)

-- | Whether a resumed session lacks the current skill catalog context:
-- neither the pending startup context nor a user message in the transcript
-- carries its rendering. That is the case for a session that predates the
-- catalog and after a skill changed, so such a session receives the catalog
-- once more instead of running without its always-active skills.
skillCatalogContextMissing
    :: SkillCatalog
    -> Maybe Text
    -> [ResponseItem]
    -> Bool
skillCatalogContextMissing catalog pending transcript =
    case fst (formatSkillCatalogContext defaultSkillCatalogMaxChars catalog) of
        Nothing -> False
        Just context ->
            let carried = Text.strip context
            in not
                (maybe False (Text.isInfixOf carried) pending
                    || hasUserTextContaining carried transcript)

queueSkillCatalogContext :: IORef (Maybe Text) -> SkillCatalog -> IO ()
queueSkillCatalogContext contextRef catalog = do
    stderr <- getTerminalStderr
    omitted <- queueSkillCatalogContextWithOmissions contextRef catalog
    when (omitted > 0) do
        color <- resolveColor stderr
        putTextLn stderr $
            roleWarn color
                (glyphWarn
                    <> "skills: "
                    <> Text.pack (show omitted)
                    <> " omitted from model context due to the catalog budget")

queueSkillCatalogContextWithOmissions
    :: IORef (Maybe Text)
    -> SkillCatalog
    -> IO Int
queueSkillCatalogContextWithOmissions contextRef catalog =
    case formatSkillCatalogContext defaultSkillCatalogMaxChars catalog of
        (Nothing, omitted) -> pure omitted
        (Just text, omitted) -> do
            atomicModifyIORef' contextRef \current ->
                ( Just $ case current of
                    Nothing -> text
                    Just existing -> existing <> "\n\n" <> text
                , ()
                )
            pure omitted

-- | Publish a freshly discovered catalog to all session consumers. Keeping
-- this transition in one helper lets fullscreen startup begin with empty refs
-- and install the complete catalog once background discovery finishes.
installSkillCatalog
    :: [Text]
    -> Bool
    -> IORef (Maybe Text)
    -> IORef SkillCatalog
    -> IORef [SkillInvocation]
    -> SkillCatalog
    -> IO ()
installSkillCatalog reservedNames queueContext contextRef catalogRef invocationsRef catalog = do
    void $
        installSkillCatalogWithOmissions
            reservedNames
            queueContext
            contextRef
            catalogRef
            invocationsRef
            catalog

installSkillCatalogWithOmissions
    :: [Text]
    -> Bool
    -> IORef (Maybe Text)
    -> IORef SkillCatalog
    -> IORef [SkillInvocation]
    -> SkillCatalog
    -> IO Int
installSkillCatalogWithOmissions reservedNames queueContext contextRef catalogRef invocationsRef catalog = do
    writeIORef catalogRef catalog
    writeIORef invocationsRef (buildSkillInvocations reservedNames catalog)
    if queueContext
        then queueSkillCatalogContextWithOmissions contextRef catalog
        else pure 0

-- | Expose only the directories belonging to the current catalog. This lets
-- models read SKILL.md and skill-relative resources even when packaged skills
-- live outside the worktree (for example under /nix/store).
installSkillToolRoots :: ToolEnv -> SkillCatalog -> IO ()
installSkillToolRoots env catalog =
    setToolSkillRoots
        env
        (mapMaybe filesystemDirectory catalog.catalogSkills <> sharedRoots)
  where
    filesystemDirectory skill =
        case skill.skillSource of
            FilesystemSkillSource{skillDirectory} -> Just skillDirectory
            McpSkillSource{} -> Nothing
    sharedRoots =
        case filter isBuiltinExternalResume catalog.catalogSkills of
            skill : _ ->
                [ takeDirectory directory
                    </> unsafeEncodeUtf "shared/resume-session"
                | Just directory <- [filesystemDirectory skill]
                ]
            [] -> []

    isBuiltinExternalResume skill =
        case skill.skillSource of
            FilesystemSkillSource{skillScope = BuiltinSkill} ->
                skill.skillName `elem`
                    [ "resume-claude"
                    , "resume-codex"
                    , "resume-cursor"
                    , "resume-grok"
                    ]
            _ -> False

skillInvocationCommand :: SkillInvocation -> SkillCommand
skillInvocationCommand invocation =
    SkillCommand
        { skillCommandName = invocation.invocationName
        , skillCommandSummary =
            fromMaybe
                invocation.invocationSkill.skillDescription
                invocation.invocationSkill.skillShortDescription
        , skillCommandArgumentHint =
            invocation.invocationSkill.skillArgumentHint
        , skillCommandSource = skillSourceLabel invocation.invocationSkill
        }

skillSourceLabel :: Skill -> Text
skillSourceLabel skill =
    case skill.skillSource of
        McpSkillSource server _ _ -> "MCP · " <> server
        FilesystemSkillSource{skillScope, skillOrigin} ->
            scopeLabel skillScope <> " · " <> originLabel skillOrigin
  where
    scopeLabel = \case
        BuiltinSkill -> "built-in"
        UserSkill -> "user"
        RepositorySkill _ True -> "local"
        RepositorySkill _ False -> "repo"
    originLabel = \case
        HaskellAgentSkills -> "haskell-agent"
        AgentSkills -> "agents"

formatSkillsListing
    :: Bool
    -> SkillCatalog
    -> [SkillInvocation]
    -> Text
formatSkillsListing color catalog invocations =
    case catalog.catalogSkills of
        [] -> roleMuted color "skills: (none)"
        skills ->
            Text.intercalate "\n" $
                rolePrompt color ("Skills (" <> Text.pack (show (length skills)) <> ")")
                    : map render skills
  where
    namesFor skill =
        [ "/" <> invocation.invocationName
        | invocation <- invocations
        , invocation.invocationSkill == skill
        , skill.skillUserInvocable
        ]
    dollarNamesFor skill =
        [ "$" <> invocation.invocationName
        | invocation <- invocations
        , invocation.invocationSkill == skill
        ]
    render skill =
        let slashNames = namesFor skill
            dollarNames = dollarNamesFor skill
            invocationText =
                case dollarNames of
                    dollar : _ ->
                        Text.intercalate ", "
                            (dollar : slashNames)
                    [] ->
                        if null slashNames
                            then "(model-only)"
                            else Text.intercalate ", " slashNames
        in rolePrompt color invocationText
            <> "  "
            <> roleMuted color
                ( skill.skillDescription
                    <> " · "
                    <> skillSourceLabel skill
                    <> " · "
                    <> skillLocation skill
                )
    skillLocation skill =
        case skill.skillSource of
            FilesystemSkillSource{skillPath} -> toText skillPath
            McpSkillSource _ uri _ -> uri
