-- | CLI presentation and lifecycle helpers for filesystem Agent Skills.
module Agent.CLI.Skills
    ( formatSkillsListing
    , installSkillCatalog
    , loadSkillsCatalog
    , queueSkillCatalogContext
    , reservedSlashNames
    , skillInvocationCommand
    ) where

import Agent.CLI.Command
    ( SkillCommand(..)
    , SlashCommand(..)
    , slashCommands
    )
import Agent.CLI.Options (CliOptions(..))
import Agent.CLI.Style
    ( glyphSession
    , glyphWarn
    , roleMuted
    , rolePrompt
    , roleWarn
    )
import Agent.CLI.Render (putTextLn)
import Agent.CLI.Terminal (resolveColor)
import Agent.OsPath (toText)
import Agent.Skills
import Control.Monad (when)
import Data.IORef (IORef, modifyIORef', writeIORef)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import System.IO (stderr)
import System.OsPath (OsPath)

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
        catalog <- discoverSkills SkillDiscoverOptions
            { skillsHome = home
            , skillsProjectRoot = projectRoot
            , skillsCwd = cwd
            , skillsMaxDepth = 6
            }
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
queueSkillCatalogContext contextRef catalog =
    case formatSkillCatalogContext defaultSkillCatalogMaxChars catalog of
        (Nothing, _) -> pure ()
        (Just text, omitted) -> do
            modifyIORef' contextRef \current ->
                Just $ case current of
                    Nothing -> text
                    Just existing -> existing <> "\n\n" <> text
            when (omitted > 0) do
                color <- resolveColor stderr
                putTextLn stderr $
                    roleWarn color
                        (glyphWarn
                            <> "skills: "
                            <> Text.pack (show omitted)
                            <> " omitted from model context due to the catalog budget")

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
    writeIORef catalogRef catalog
    writeIORef invocationsRef (buildSkillInvocations reservedNames catalog)
    when queueContext $
        queueSkillCatalogContext contextRef catalog

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
