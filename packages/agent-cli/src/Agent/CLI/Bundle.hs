-- | Versioned, provider-neutral agent bundles.
module Agent.CLI.Bundle
    ( AgentBundle(..)
    , BundleAgent(..)
    , BundleEnvironment(..)
    , BundleSkill(..)
    , BundleTools(..)
    , BundleWorkspace(..)
    , LoadedBundle(..)
    , PreparedBundle(..)
    , decodeAgentBundle
    , formatAgentBundle
    , loadAgentBundle
    , prepareBundleRun
    ) where

import Agent.CLI.Options
    ( BundleRunOptions(..)
    , CliOptions(..)
    , defaultCliOptions
    )
import Agent.OsPath (unsafeToFilePath)
import Agent.ReasoningEffort
    ( ReasoningEffort
    , parseReasoningEffort
    , reasoningEffortText
    )
import Control.Exception.Safe (displayException, tryAny)
import Control.Monad (forM_, unless)
import Data.Aeson
    ( FromJSON(..)
    , Object
    , eitherDecodeStrict'
    , withObject
    , (.:)
    , (.:?)
    , (.!=)
    )
import Data.Aeson.Key (Key)
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Aeson.Types (Parser)
import qualified Data.ByteString as ByteString
import Data.ByteString (ByteString)
import Data.Char (isAlphaNum, isAscii)
import Data.List (intercalate, nub)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified System.Directory as Directory
import qualified System.FilePath as FilePath
import System.OsPath (OsPath, unsafeEncodeUtf)

data AgentBundle = AgentBundle
    { bundleFormat :: !Text
    , bundleVersion :: !Int
    , bundleSystem :: !Text
    , bundleName :: !Text
    , bundleDefaultAgent :: !Text
    , bundleEnvironments :: !(Map Text BundleEnvironment)
    , bundleSkills :: !(Map Text BundleSkill)
    , bundleAgents :: !(Map Text BundleAgent)
    } deriving (Eq, Show)

data BundleEnvironment = BundleEnvironment
    { bundleEnvironmentPath :: !Text
    } deriving (Eq, Show)

data BundleSkill = BundleSkill
    { bundleSkillPath :: !Text
    } deriving (Eq, Show)

data BundleAgent = BundleAgent
    { bundleAgentDescription :: !(Maybe Text)
    , bundleAgentInstructions :: !Text
    , bundleAgentModel :: !(Maybe Text)
    , bundleAgentEffort :: !(Maybe ReasoningEffort)
    , bundleAgentEnvironment :: !(Maybe Text)
    , bundleAgentSkills :: ![Text]
    , bundleAgentTools :: !BundleTools
    , bundleAgentWorkspace :: !BundleWorkspace
    , bundleAgentMaxTurns :: !(Maybe Int)
    } deriving (Eq, Show)

data BundleTools = BundleTools
    { bundleToolBash :: !Bool
    , bundleToolGhci :: !Bool
    , bundleToolComputerUse :: !Bool
    , bundleToolCodeMode :: !Bool
    } deriving (Eq, Show)

data BundleWorkspace = BundleWorkspace
    { bundleWorkspaceWorktree :: !Bool
    , bundleWorkspaceAgentsMd :: !Bool
    , bundleWorkspaceAmbientSkills :: !Bool
    } deriving (Eq, Show)

data LoadedBundle = LoadedBundle
    { loadedBundleManifest :: !AgentBundle
    , loadedBundleBytes :: !ByteString
    , loadedBundleManifestPath :: !FilePath
    } deriving (Eq, Show)

data PreparedBundle = PreparedBundle
    { preparedBundleOptions :: !CliOptions
    , preparedBundleAgentName :: !Text
    } deriving (Eq, Show)

defaultBundleTools :: BundleTools
defaultBundleTools = BundleTools
    { bundleToolBash = True
    , bundleToolGhci = False
    , bundleToolComputerUse = False
    , bundleToolCodeMode = False
    }

defaultBundleWorkspace :: BundleWorkspace
defaultBundleWorkspace = BundleWorkspace
    { bundleWorkspaceWorktree = False
    , bundleWorkspaceAgentsMd = False
    , bundleWorkspaceAmbientSkills = False
    }

instance FromJSON AgentBundle where
    parseJSON = withObject "AgentBundle" \object -> do
        rejectUnknown "agent bundle"
            [ "format"
            , "version"
            , "system"
            , "name"
            , "default_agent"
            , "environments"
            , "skills"
            , "agents"
            ]
            object
        bundle <- AgentBundle
            <$> object .: "format"
            <*> object .: "version"
            <*> object .: "system"
            <*> object .: "name"
            <*> object .: "default_agent"
            <*> object .:? "environments" .!= Map.empty
            <*> object .:? "skills" .!= Map.empty
            <*> object .: "agents"
        either fail pure (validateAgentBundle bundle)

instance FromJSON BundleEnvironment where
    parseJSON = withObject "BundleEnvironment" \object -> do
        rejectUnknown "bundle environment" ["path"] object
        BundleEnvironment <$> object .: "path"

instance FromJSON BundleSkill where
    parseJSON = withObject "BundleSkill" \object -> do
        rejectUnknown "bundle skill" ["path"] object
        BundleSkill <$> object .: "path"

instance FromJSON BundleAgent where
    parseJSON = withObject "BundleAgent" \object -> do
        rejectUnknown "bundle agent"
            [ "description"
            , "instructions"
            , "model"
            , "effort"
            , "environment"
            , "skills"
            , "tools"
            , "workspace"
            , "max_turns"
            ]
            object
        effortText <- object .:? "effort"
        effort <- traverse parseEffortValue effortText
        BundleAgent
            <$> object .:? "description"
            <*> object .: "instructions"
            <*> object .:? "model"
            <*> pure effort
            <*> object .:? "environment"
            <*> object .:? "skills" .!= []
            <*> object .:? "tools" .!= defaultBundleTools
            <*> object .:? "workspace" .!= defaultBundleWorkspace
            <*> object .:? "max_turns"

instance FromJSON BundleTools where
    parseJSON = withObject "BundleTools" \object -> do
        rejectUnknown "bundle tools"
            ["bash", "ghci", "computer_use", "code_mode"]
            object
        BundleTools
            <$> object .:? "bash" .!= True
            <*> object .:? "ghci" .!= False
            <*> object .:? "computer_use" .!= False
            <*> object .:? "code_mode" .!= False

instance FromJSON BundleWorkspace where
    parseJSON = withObject "BundleWorkspace" \object -> do
        rejectUnknown "bundle workspace"
            ["worktree", "agents_md", "ambient_skills"]
            object
        BundleWorkspace
            <$> object .:? "worktree" .!= False
            <*> object .:? "agents_md" .!= False
            <*> object .:? "ambient_skills" .!= False

parseEffortValue :: Text -> Parser ReasoningEffort
parseEffortValue value =
    either (fail . Text.unpack) pure (parseReasoningEffort value)

rejectUnknown :: String -> [Key] -> Object -> Parser ()
rejectUnknown label allowed object =
    unless (null unknown) $
        fail
            (label
                <> " has unknown field(s): "
                <> intercalate ", " (map (Text.unpack . Key.toText) unknown))
  where
    unknown = filter (`notElem` allowed) (KeyMap.keys object)

decodeAgentBundle :: ByteString -> Either String AgentBundle
decodeAgentBundle = eitherDecodeStrict'

validateAgentBundle :: AgentBundle -> Either String AgentBundle
validateAgentBundle bundle = do
    require
        (bundle.bundleFormat == "haskell-agent-bundle")
        "format must be \"haskell-agent-bundle\""
    require
        (bundle.bundleVersion == 1)
        ("unsupported bundle version "
            <> show bundle.bundleVersion
            <> " (expected 1)")
    requireNonBlank "system" bundle.bundleSystem
    validateIdentifier "bundle name" bundle.bundleName
    require
        (not (Map.null bundle.bundleAgents))
        "agents must contain at least one agent"
    validateIdentifier "default agent" bundle.bundleDefaultAgent
    require
        (Map.member bundle.bundleDefaultAgent bundle.bundleAgents)
        ("default agent \""
            <> Text.unpack bundle.bundleDefaultAgent
            <> "\" is not declared")
    forM_ (Map.toList bundle.bundleEnvironments) \(name, environment) -> do
        validateIdentifier "environment name" name
        requireNonBlank
            ("environment \"" <> Text.unpack name <> "\" path")
            environment.bundleEnvironmentPath
        let pathEntries =
                FilePath.splitSearchPath
                    (Text.unpack environment.bundleEnvironmentPath)
        require
            (not (null pathEntries) && all FilePath.isAbsolute pathEntries)
            ("environment \""
                <> Text.unpack name
                <> "\" path must contain only absolute entries")
    forM_ (Map.toList bundle.bundleSkills) \(name, skill) -> do
        validateIdentifier "skill name" name
        requireNonBlank
            ("skill \"" <> Text.unpack name <> "\" path")
            skill.bundleSkillPath
        require
            (FilePath.isAbsolute (Text.unpack skill.bundleSkillPath))
            ("skill \""
                <> Text.unpack name
                <> "\" path must be absolute")
    forM_ (Map.toList bundle.bundleAgents) \(name, agent) -> do
        validateIdentifier "agent name" name
        requireNonBlank
            ("agent \"" <> Text.unpack name <> "\" instructions")
            agent.bundleAgentInstructions
        forM_ agent.bundleAgentModel $
            requireNonBlank ("agent \"" <> Text.unpack name <> "\" model")
        forM_ agent.bundleAgentMaxTurns \maxTurns ->
            require
                (maxTurns > 0)
                ("agent \""
                    <> Text.unpack name
                    <> "\" max_turns must be positive")
        forM_ agent.bundleAgentEnvironment \environment ->
            require
                (Map.member environment bundle.bundleEnvironments)
                ("agent \""
                    <> Text.unpack name
                    <> "\" references unknown environment \""
                    <> Text.unpack environment
                    <> "\"")
        forM_ agent.bundleAgentSkills \skill ->
            require
                (Map.member skill bundle.bundleSkills)
                ("agent \""
                    <> Text.unpack name
                    <> "\" references unknown skill \""
                    <> Text.unpack skill
                    <> "\"")
        require
            (nub agent.bundleAgentSkills == agent.bundleAgentSkills)
            ("agent \""
                <> Text.unpack name
                <> "\" contains duplicate skill references")
    pure bundle

validateIdentifier :: String -> Text -> Either String ()
validateIdentifier label value =
    require
        (case Text.uncons value of
            Nothing -> False
            Just (first, rest) ->
                validAlphaNumeric first
                    && Text.all validRest rest)
        (label
            <> " must start with an ASCII letter or digit and contain only "
            <> "letters, digits, '.', '_', or '-'")
  where
    validAlphaNumeric character = isAscii character && isAlphaNum character
    validRest character =
        validAlphaNumeric character || character `elem` ("._-" :: String)

requireNonBlank :: String -> Text -> Either String ()
requireNonBlank label value =
    require (not (Text.null (Text.strip value))) (label <> " must not be empty")

require :: Bool -> String -> Either String ()
require condition message =
    if condition then Right () else Left message

loadAgentBundle :: OsPath -> IO (Either String LoadedBundle)
loadAgentBundle source = do
    resolveBundleManifest source >>= \case
        Left err -> pure (Left err)
        Right manifestPath ->
            tryAny (ByteString.readFile manifestPath) >>= \case
                Left err ->
                    pure
                        (Left
                            ("could not read bundle manifest "
                                <> manifestPath
                                <> ": "
                                <> displayException err))
                Right bytes ->
                    if ByteString.length bytes > maxBundleManifestBytes
                        then pure
                            (Left
                                ("bundle manifest exceeds "
                                    <> show maxBundleManifestBytes
                                    <> " bytes: "
                                    <> manifestPath))
                        else case decodeAgentBundle bytes of
                            Left err ->
                                pure
                                    (Left
                                        ("invalid bundle manifest "
                                            <> manifestPath
                                            <> ": "
                                            <> err))
                            Right manifest ->
                                validateBundleAssets manifest >>= \case
                                    Left err ->
                                        pure
                                            (Left
                                                ("invalid bundle assets for "
                                                    <> manifestPath
                                                    <> ": "
                                                    <> err))
                                    Right () ->
                                        pure
                                            (Right LoadedBundle
                                                { loadedBundleManifest =
                                                    manifest
                                                , loadedBundleBytes = bytes
                                                , loadedBundleManifestPath =
                                                    manifestPath
                                                })

validateBundleAssets :: AgentBundle -> IO (Either String ())
validateBundleAssets bundle = do
    inspected <- tryAny (checkDirectories directories)
    pure case inspected of
        Left err ->
            Left
                ("could not inspect bundle assets: "
                    <> displayException err)
        Right result -> result
  where
    directories =
        [ ( "environment \"" <> Text.unpack name <> "\" PATH entry"
          , path
          )
        | (name, environment) <- Map.toList bundle.bundleEnvironments
        , path <- FilePath.splitSearchPath
            (Text.unpack environment.bundleEnvironmentPath)
        ]
            <> [ ( "skill \"" <> Text.unpack name <> "\" root"
                 , Text.unpack skill.bundleSkillPath
                 )
               | (name, skill) <- Map.toList bundle.bundleSkills
               ]

    checkDirectories = \case
        [] -> pure (Right ())
        (label, path) : rest ->
            Directory.doesDirectoryExist path >>= \case
                True -> checkDirectories rest
                False ->
                    pure
                        (Left
                            (label
                                <> " is not an existing directory: "
                                <> path))

maxBundleManifestBytes :: Int
maxBundleManifestBytes = 1024 * 1024

resolveBundleManifest :: OsPath -> IO (Either String FilePath)
resolveBundleManifest source = do
    let sourcePath = unsafeToFilePath source
    inspected <- tryAny do
        isDirectory <- Directory.doesDirectoryExist sourcePath
        if isDirectory
            then
                firstExisting
                    [ sourcePath
                        FilePath.</> "share"
                        FilePath.</> "haskell-agent"
                        FilePath.</> "bundle.json"
                    , sourcePath FilePath.</> "manifest.json"
                    , sourcePath FilePath.</> "bundle.json"
                    ] >>= \case
                        Just manifestPath -> pure (Right manifestPath)
                        Nothing ->
                            pure
                                (Left
                                    ("bundle directory has no manifest: "
                                        <> sourcePath))
            else do
                exists <- Directory.doesFileExist sourcePath
                pure
                    (if exists
                        then Right sourcePath
                        else Left ("bundle does not exist: " <> sourcePath))
    pure case inspected of
        Left err ->
            Left
                ("could not inspect bundle "
                    <> sourcePath
                    <> ": "
                    <> displayException err)
        Right result -> result
  where
    firstExisting = \case
        [] -> pure Nothing
        candidate : rest ->
            Directory.doesFileExist candidate >>= \case
                True -> pure (Just candidate)
                False -> firstExisting rest

formatAgentBundle :: AgentBundle -> Text
formatAgentBundle bundle =
    Text.unlines $
        [ "Bundle: " <> bundle.bundleName
        , "Format: " <> bundle.bundleFormat
            <> " v"
            <> Text.pack (show bundle.bundleVersion)
        , "System: " <> bundle.bundleSystem
        , "Default agent: " <> bundle.bundleDefaultAgent
        , "Environments:"
        ]
            <> namedValues
                (.bundleEnvironmentPath)
                bundle.bundleEnvironments
            <> ["Skills:"]
            <> namedValues (.bundleSkillPath) bundle.bundleSkills
            <> ["Agents:"]
            <> concatMap formatAgent (Map.toList bundle.bundleAgents)
  where
    namedValues project values
        | Map.null values = ["  (none)"]
        | otherwise =
            [ "  - " <> name <> ": " <> project value
            | (name, value) <- Map.toList values
            ]
    formatAgent (name, agent) =
        [ "  - "
            <> name
            <> (if name == bundle.bundleDefaultAgent
                    then " (default)"
                    else "")
            <> maybe "" (" — " <>) agent.bundleAgentDescription
        , "    Model alias: "
            <> fromMaybe "(runtime default)" agent.bundleAgentModel
        , "    Effort: "
            <> maybe "(runtime default)" reasoningEffortText
                agent.bundleAgentEffort
        , "    Environment: "
            <> fromMaybe "(host)" agent.bundleAgentEnvironment
        , "    Skills: "
            <> (if null agent.bundleAgentSkills
                    then "(none)"
                    else Text.intercalate ", " agent.bundleAgentSkills)
        , "    Tools: "
            <> enabledNames
                [ ("bash", agent.bundleAgentTools.bundleToolBash)
                , ("ghci", agent.bundleAgentTools.bundleToolGhci)
                , ("computer-use",
                    agent.bundleAgentTools.bundleToolComputerUse)
                , ("code-mode",
                    agent.bundleAgentTools.bundleToolCodeMode)
                ]
        , "    Workspace: "
            <> enabledNames
                [ ("worktree",
                    agent.bundleAgentWorkspace.bundleWorkspaceWorktree)
                , ("AGENTS.md",
                    agent.bundleAgentWorkspace.bundleWorkspaceAgentsMd)
                , ("ambient-skills",
                    agent.bundleAgentWorkspace.bundleWorkspaceAmbientSkills)
                ]
        , "    Max turns: "
            <> maybe "(default)" (Text.pack . show)
                agent.bundleAgentMaxTurns
        , "    Instructions:"
        ]
            <> map ("      " <>) (Text.lines agent.bundleAgentInstructions)
    enabledNames values =
        case [label | (label, True) <- values] of
            [] -> "(none)"
            names -> Text.intercalate ", " names

prepareBundleRun
    :: BundleRunOptions
    -> AgentBundle
    -> Either String PreparedBundle
prepareBundleRun runOptions bundle = do
    require
        (not (runOptions.bundleRunYolo && runOptions.bundleRunNoYolo))
        "use either --yolo or --no-yolo, not both"
    require
        (not
            (maybe False (const True) runOptions.bundleRunPrompt
                && maybe False (const True) runOptions.bundleRunPromptFile))
        "use only one of -p/--prompt or --prompt-file"
    agent <- maybe
        (Left
            ("bundle has no agent named \""
                <> Text.unpack agentName
                <> "\""))
        Right
        (Map.lookup agentName bundle.bundleAgents)
    pathPrefix <- traverse
        (\environmentName ->
            maybe
                (Left
                    ("bundle has no environment named \""
                        <> Text.unpack environmentName
                        <> "\""))
                (Right . Text.unpack . (.bundleEnvironmentPath))
                (Map.lookup environmentName bundle.bundleEnvironments))
        agent.bundleAgentEnvironment
    skillRoots <- traverse
        (\skillName ->
            maybe
                (Left
                    ("bundle has no skill named \""
                        <> Text.unpack skillName
                        <> "\""))
                (Right . unsafeEncodeUtf . Text.unpack . (.bundleSkillPath))
                (Map.lookup skillName bundle.bundleSkills))
        agent.bundleAgentSkills
    let tools = agent.bundleAgentTools
        workspace = agent.bundleAgentWorkspace
        yolo = runOptions.bundleRunYolo
        options = defaultCliOptions
            { optProvider = Nothing
            , optModel = agent.bundleAgentModel
            , optCwd = runOptions.bundleRunCwd
            , optWorktree = workspace.bundleWorkspaceWorktree
            , optYolo = yolo
            , optNoYolo = not yolo
            , optMaxTurns =
                fromMaybe defaultCliOptions.optMaxTurns
                    agent.bundleAgentMaxTurns
            , optEffort = agent.bundleAgentEffort
            , optPrompt = runOptions.bundleRunPrompt
            , optPromptFile = runOptions.bundleRunPromptFile
            , optAgentsMd = workspace.bundleWorkspaceAgentsMd
            , optSkills =
                workspace.bundleWorkspaceAmbientSkills
                    || not (null skillRoots)
            , optBundleContext =
                Just (formatBundleContext bundle.bundleName agentName agent)
            , optBundleSkillRoots = skillRoots
            , optBundlePathPrefix = pathPrefix
            , optAmbientSkills = workspace.bundleWorkspaceAmbientSkills
            , optGhci = tools.bundleToolGhci
            , optBash = tools.bundleToolBash
            , optComputerUse = tools.bundleToolComputerUse
            , optCodeMode = tools.bundleToolCodeMode
            }
    pure PreparedBundle
        { preparedBundleOptions = options
        , preparedBundleAgentName = agentName
        }
  where
    agentName = fromMaybe bundle.bundleDefaultAgent runOptions.bundleRunAgent

formatBundleContext :: Text -> Text -> BundleAgent -> Text
formatBundleContext bundleName agentName agent =
    Text.unlines
        [ "<agent_bundle>"
        , "bundle: " <> bundleName
        , "agent: " <> agentName
        , ""
        , agent.bundleAgentInstructions
        , "</agent_bundle>"
        ]
