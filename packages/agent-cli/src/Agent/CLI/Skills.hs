-- | CLI presentation and lifecycle helpers for Agent Skills.
module Agent.CLI.Skills
    ( formatSkillsListing
    , installSkillCatalog
    , installSkillCatalogWithOmissions
    , installSkillToolRoots
    , loadMcpSkillsCatalog
    , loadSkillsCatalog
    , loadSkillsCatalogQuiet
    , mergeSkillCatalogs
    , queueSkillCatalogContext
    , queueSkillCatalogContextWithOmissions
    , reservedSlashNames
    , resolvePromptSkillMentions
    , resolvePromptSkillMentionsWithWarnings
    , resolveSkillContent
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
import Agent.Skills
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
import System.IO (stderr)
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
            , skillsMaxDepth = 6
            , skillsBuiltinRoots =
                [(AgentSkills, unsafeEncodeUtf builtinRoot)]
            }

-- | Convert the untrusted metadata advertised by Skills-over-MCP servers into
-- lightweight catalog entries. Invalid entries are omitted and surfaced using
-- the existing catalog warning channel.
loadMcpSkillsCatalog :: McpFleet -> IO SkillCatalog
loadMcpSkillsCatalog fleet = do
    registrations <- MCP.mcpFleetSkillRegistrations fleet
    pure $
        SkillCatalog
            (mapMaybe (either (const Nothing) Just . loadRegistration) registrations)
            [ SkillWarning
                (unsafeEncodeUtf
                    (Text.unpack registration.mcpSkillEntry.mcpSkillUri))
                ("MCP skill ignored: " <> err)
            | registration <- registrations
            , Left err <- [loadRegistration registration]
            ]
  where
    loadRegistration registration = do
        value <-
            either
                (Left . Text.pack)
                Right
                (Aeson.eitherDecodeStrict'
                    (rawJsonBytes registration.mcpSkillEntry.mcpSkillFrontmatter))
        loadMcpSkillMetadata
            registration.mcpSkillServer
            registration.mcpSkillEntry.mcpSkillUri
            (entryResourceUris registration.mcpSkillEntry)
            value

mergeSkillCatalogs :: SkillCatalog -> SkillCatalog -> SkillCatalog
mergeSkillCatalogs local remote =
    SkillCatalog
        (local.catalogSkills <> remote.catalogSkills)
        (local.catalogWarnings <> remote.catalogWarnings)

-- | Resolve local content immediately or fetch and verify an MCP skill on
-- demand. Remote instructions are never trusted merely because they appeared
-- in @skills/list@.
resolveSkillContent
    :: Maybe McpFleet
    -> Skill
    -> IO (Either Text SkillContent)
resolveSkillContent fleet skill =
    case filesystemSkillContent skill of
        Just content -> pure (Right content)
        Nothing ->
            case (fleet, skill.skillSource) of
                (Nothing, McpSkillSource{}) ->
                    pure (Left "MCP skill server is unavailable")
                (Just activeFleet, McpSkillSource server uri _) ->
                    resolveMcpSkillContent activeFleet server uri skill
                _ -> pure (Left "unsupported skill source")

resolveMcpSkillContent
    :: McpFleet
    -> Text
    -> Text
    -> Skill
    -> IO (Either Text SkillContent)
resolveMcpSkillContent fleet server uri advertised =
    MCP.mcpFleetGetSkill fleet server uri >>= \case
        Left err -> pure (Left err)
        Right entry ->
            MCP.mcpFleetReadResource fleet server uri >>= \case
                Left err -> pure (Left err)
                Right contents ->
                    pure (verifyMcpSkillContent advertised server entry contents)

-- | Validate the complete response chain used to activate an MCP skill.
-- Kept pure so integrity and identity failures can be tested without a live
-- transport.
verifyMcpSkillContent
    :: Skill
    -> Text
    -> McpSkillEntry
    -> [McpResourceContent]
    -> Either Text SkillContent
verifyMcpSkillContent advertised server entry contents = do
    case advertised.skillSource of
        McpSkillSource advertisedServer advertisedUri _
            | advertisedServer /= server ->
                Left "MCP skill server does not match its catalog entry"
            | entry.mcpSkillUri /= advertisedUri ->
                Left "MCP skills/get returned a different skill URI"
            | otherwise -> pure ()
        FilesystemSkillSource{} ->
            Left "expected an MCP skill catalog entry"
    (checkedSkill, manifest) <- validateFetchedEntry advertised server entry
    text <- selectTextResource entry.mcpSkillUri contents
    verifyManifest manifest text
    loadMcpSkillDocument checkedSkill text

validateFetchedEntry
    :: Skill
    -> Text
    -> McpSkillEntry
    -> Either Text (Skill, McpSkillResource)
validateFetchedEntry advertised server entry = do
    resources <- case entry.mcpSkillResources of
        McpSkillResourcesDynamic ->
            Left "MCP skills/get did not provide a resource manifest"
        McpSkillResourcesListed listed -> Right listed
    manifest <-
        case filter
            ((== entry.mcpSkillUri) . (.mcpSkillResourceUri))
            resources of
            [resource] -> Right resource
            [] -> Left "MCP skill manifest does not contain its SKILL.md URI"
            _ -> Left "MCP skill manifest contains duplicate SKILL.md entries"
    value <-
        either
            (Left . Text.pack)
            Right
            (Aeson.eitherDecodeStrict' (rawJsonBytes entry.mcpSkillFrontmatter))
    fetched <-
        loadMcpSkillMetadata
            server
            entry.mcpSkillUri
            (map (.mcpSkillResourceUri) resources)
            value
    let checked =
            advertised
                { skillSource =
                    McpSkillSource
                        server
                        entry.mcpSkillUri
                        (map (.mcpSkillResourceUri) resources)
                }
        sameMetadata =
            fetched { skillSource = advertised.skillSource }
                == advertised
    if sameMetadata
        then Right (checked, manifest)
        else Left "MCP skills/get metadata does not match its catalog entry"

selectTextResource
    :: Text
    -> [McpResourceContent]
    -> Either Text Text
selectTextResource uri contents =
    case [text | content <- contents, content.mcpResourceUri == uri
               , Just text <- [content.mcpResourceText]] of
        [text] -> Right text
        [] -> Left "MCP resources/read returned no textual SKILL.md"
        _ -> Left "MCP resources/read returned duplicate SKILL.md content"

verifyManifest :: McpSkillResource -> Text -> Either Text ()
verifyManifest resource text
    | resource.mcpSkillResourceSize /= BS.length bytes =
        Left "MCP SKILL.md size does not match its manifest"
    | not ("sha256:" `Text.isPrefixOf` digest)
        || Text.length expected /= 64
        || Text.any (not . isHexDigit) expected =
            Left "MCP SKILL.md manifest has an invalid SHA-256 digest"
    | Text.toLower expected /= actual =
        Left "MCP SKILL.md SHA-256 digest does not match its manifest"
    | otherwise = Right ()
  where
    bytes = TextEncoding.encodeUtf8 text
    digest = Text.toLower resource.mcpSkillResourceDigest
    expected = Text.drop 7 digest
    actual = Text.pack (show (hash bytes :: Digest SHA256))
    isHexDigit c =
        ('0' <= c && c <= '9')
            || ('a' <= c && c <= 'f')

entryResourceUris :: McpSkillEntry -> [Text]
entryResourceUris entry =
    case entry.mcpSkillResources of
        McpSkillResourcesDynamic -> []
        McpSkillResourcesListed resources ->
            map (.mcpSkillResourceUri) resources

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
