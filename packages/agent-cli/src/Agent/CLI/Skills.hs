-- | CLI presentation and lifecycle helpers for filesystem Agent Skills.
module Agent.CLI.Skills
    ( formatSkillsListing
    , installSkillCatalog
    , installSkillCatalogWithOmissions
    , loadSkillsCatalog
    , loadSkillsCatalogQuiet
    , queueSkillCatalogContext
    , queueSkillCatalogContextWithOmissions
    , reservedSlashNames
    , skillInvocationCommand
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
import Agent.OsPath (toText)
import Agent.Skills
import Control.Monad (void, when)
import Data.IORef (IORef, modifyIORef', writeIORef)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import System.IO (stderr)
import System.OsPath (OsPath)
import qualified Paths_agent_cli as Paths

reservedSlashNames :: [Text]
reservedSlashNames =
    concatMap
        (\command -> command.slashName : command.slashAliases)
        slashCommands

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
        discovered <- discoverSkills SkillDiscoverOptions
            { skillsHome = home
            , skillsProjectRoot = projectRoot
            , skillsCwd = cwd
            , skillsMaxDepth = 6
            }
        bundledPath <- Paths.getDataFileName "skills/telegram-agent/SKILL.md"
        bundled <- loadSkillFile BuiltinSkill AgentSkills bundledPath
        pure discovered
            { catalogSkills =
                either (const discovered.catalogSkills)
                    (: discovered.catalogSkills)
                    bundled
            , catalogWarnings =
                either (: discovered.catalogWarnings)
                    (const discovered.catalogWarnings)
                    bundled
            }

reportSkillWarning :: Bool -> SkillWarning -> IO ()
reportSkillWarning color warning =
    putTextLn stderr $
        roleWarn color
            (glyphWarn
                <> "skill ignored: "
                <> toText warning.skillWarningPath
                <> ": "
                <> warning.skillWarningMessage)

queueSkillCatalogContext :: IORef (Maybe Text) -> SkillCatalog -> IO ()
queueSkillCatalogContext contextRef catalog = do
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
            modifyIORef' contextRef \current ->
                Just $ case current of
                    Nothing -> text
                    Just existing -> existing <> "\n\n" <> text
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
    scope <> " · " <> origin
  where
    scope = case skill.skillScope of
        BuiltinSkill -> "built-in"
        UserSkill -> "user"
        RepositorySkill _ True -> "local"
        RepositorySkill _ False -> "repo"
    origin = case skill.skillOrigin of
        AgentSkills -> "agents"
        GrokSkills -> "grok"
        CodexSkills -> "codex"

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
        , invocation.invocationSkill.skillPath == skill.skillPath
        , skill.skillUserInvocable
        ]
    dollarNamesFor skill =
        [ "$" <> invocation.invocationName
        | invocation <- invocations
        , invocation.invocationSkill.skillPath == skill.skillPath
        ]
    render skill =
        let names = namesFor skill
            invocationText =
                if null names
                    then case dollarNamesFor skill of
                        dollar : _ -> dollar <> " only"
                        [] -> "(model-only)"
                    else Text.intercalate ", " names
        in rolePrompt color invocationText
            <> "  "
            <> roleMuted color
                ( skill.skillDescription
                    <> " · "
                    <> skillSourceLabel skill
                    <> " · "
                    <> toText skill.skillPath
                )
