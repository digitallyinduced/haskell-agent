-- | Discover, parse, select, and format filesystem and remote Agent Skills.
module Agent.Skills
    ( Skill(..)
    , SkillSource(..)
    , SkillContent(..)
    , SkillCatalog(..)
    , SkillDiscoverOptions(..)
    , SkillInvocation(..)
    , SkillContextMode(..)
    , SkillOrigin(..)
    , SkillScope(..)
    , SkillWarning(..)
    , defaultSkillCatalogMaxChars
    , discoverSkills
    , loadSkillFile
    , loadMcpSkillMetadata
    , loadMcpSkillDocument
    , filesystemSkillContent
    , buildSkillInvocations
    , modelVisibleSkills
    , formatSkillCatalogContext
    , formatSkillActivation
    , resolveSkillInvocation
    , resolveSkillMentions
    , resolveSkillMentionsWithWarnings
    ) where

import Agent.Concurrent (mapConcurrentlyBounded)
import Agent.FileRetry (retryOnFileBusy)
import Agent.OsPath (directoryChain, toText, unsafeToFilePath)
import Control.Applicative ((<|>))
import Control.Concurrent.Async (Concurrently(..))
import Control.Concurrent.STM
    ( atomically
    , modifyTVar'
    , newTVarIO
    , readTVar
    )
import Control.Exception.Safe (SomeException, displayException, tryAny)
import Control.Monad (filterM)
import Data.Aeson
    ( FromJSON(..)
    , Value(..)
    , withObject
    , (.:)
    , (.:?)
    , (.!=)
    )
import System.OsPath (OsPath, unsafeEncodeUtf)
import Data.Aeson.Types (Parser, parseEither)
import qualified Data.ByteString as BS
import Data.Char (isAlphaNum)
import Data.Containers.ListUtils (nubOrdOn)
import Data.List (sort, sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Ord (Down(..))
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as Text
import Data.Yaml (decodeEither', prettyPrintParseException)
import System.Directory
    ( canonicalizePath
    , doesDirectoryExist
    , doesFileExist
    , listDirectory
    )
import System.FilePath
    ( takeDirectory
    , takeFileName
    , (</>)
    )

-- | Filesystem skill tree.
-- @HaskellAgentSkills@ is this product's home (@.haskell-agent/skills@).
-- @AgentSkills@ is the shared Agent Skills location (@.agents/skills@).
-- Vendor trees such as @.codex/skills@, @.grok/skills@, and @.claude/skills@
-- are not discovered.
data SkillOrigin
    = AgentSkills
    | HaskellAgentSkills
    deriving (Eq, Ord, Show)

data SkillScope
    = BuiltinSkill
    | UserSkill
    | RepositorySkill
        { skillDepth :: !Int
        , skillAtCwd :: !Bool
        }
    deriving (Eq, Ord, Show)

data SkillContextMode
    = SkillContextOnDemand
    | SkillContextAlways
    deriving (Eq, Ord, Show)

data Skill = Skill
    { skillName :: !Text
    , skillDescription :: !Text
    , skillDisplayName :: !(Maybe Text)
    , skillShortDescription :: !(Maybe Text)
    , skillDefaultPrompt :: !(Maybe Text)
    , skillWhenToUse :: !(Maybe Text)
    , skillContextMode :: !SkillContextMode
    , skillArgumentHint :: !(Maybe Text)
    , skillUserInvocable :: !Bool
    , skillModelInvocable :: !Bool
    , skillAllowedTools :: ![Text]
    , skillModelOverride :: !(Maybe Text)
    , skillEffortOverride :: !(Maybe Text)
    , skillLicense :: !(Maybe Text)
    , skillCompatibility :: !(Maybe Text)
    , skillMetadata :: !(Map Text Text)
    , skillSource :: !SkillSource
    } deriving (Eq, Show)

data SkillSource
    = FilesystemSkillSource
        { skillPath :: !OsPath
        , skillDirectory :: !OsPath
        , skillBody :: !Text
        , skillFileText :: !Text
        , skillScope :: !SkillScope
        , skillOrigin :: !SkillOrigin
        }
    | McpSkillSource
        { skillMcpServer :: !Text
        , skillMcpUri :: !Text
        , skillMcpResourceUris :: ![Text]
        }
    deriving (Eq, Show)

data SkillContent = SkillContent
    { skillContentBody :: !Text
    , skillContentFileText :: !Text
    , skillContentFile :: !Text
    , skillContentDirectory :: !(Maybe Text)
    , skillContentResourceUris :: ![Text]
    } deriving (Eq, Show)

data SkillWarning = SkillWarning
    { skillWarningPath :: !OsPath
    , skillWarningMessage :: !Text
    } deriving (Eq, Show)

data SkillCatalog = SkillCatalog
    { catalogSkills :: ![Skill]
    , catalogWarnings :: ![SkillWarning]
    } deriving (Eq, Show)

data SkillDiscoverOptions = SkillDiscoverOptions
    { skillsHome :: !OsPath
    , skillsProjectRoot :: !OsPath
    , skillsCwd :: !OsPath
    , skillsMaxDepth :: !Int
    , skillsBuiltinRoots :: ![(SkillOrigin, OsPath)]
    } deriving (Eq, Show)

data SkillInvocation = SkillInvocation
    { invocationName :: !Text
    , invocationSkill :: !Skill
    , invocationBare :: !Bool
    } deriving (Eq, Show)

data Frontmatter = Frontmatter
    { fmName :: !Text
    , fmDescription :: !Text
    , fmWhenToUse :: !(Maybe Text)
    , fmContextMode :: !SkillContextMode
    , fmArgumentHint :: !(Maybe Text)
    , fmUserInvocable :: !Bool
    , fmDisableModelInvocation :: !Bool
    , fmAllowedTools :: ![Text]
    , fmModel :: !(Maybe Text)
    , fmEffort :: !(Maybe Text)
    , fmLicense :: !(Maybe Text)
    , fmCompatibility :: !(Maybe Text)
    , fmMetadata :: !(Map Text Text)
    }

instance FromJSON Frontmatter where
    parseJSON = withObject "Skill frontmatter" \o -> do
        metadata <- o .:? "metadata" .!= Map.empty
        Frontmatter
            <$> o .: "name"
            <*> o .: "description"
            <*> o .:? "when-to-use"
            <*> (o .:? "activation" .!= SkillContextOnDemand)
            <*> o .:? "argument-hint"
            <*> (o .:? "user-invocable" .!= True)
            <*> (o .:? "disable-model-invocation" .!= False)
            <*> ((.unAllowedTools) <$> (o .:? "allowed-tools" .!= AllowedTools []))
            <*> o .:? "model"
            <*> o .:? "effort"
            <*> o .:? "license"
            <*> o .:? "compatibility"
            <*> pure metadata

instance FromJSON SkillContextMode where
    parseJSON = \case
        String "always" -> pure SkillContextAlways
        String "on-demand" -> pure SkillContextOnDemand
        String value ->
            fail
                ("activation must be `always` or `on-demand`, got "
                    <> Text.unpack value)
        _ -> fail "activation must be a string"

newtype AllowedTools = AllowedTools { unAllowedTools :: [Text] }

instance FromJSON AllowedTools where
    parseJSON = \case
        String text ->
            pure $ AllowedTools $
                filter (not . Text.null)
                    (Text.words (Text.replace "," " " text))
        Array values ->
            AllowedTools <$> traverse
                (\case
                    String text -> pure text
                    _ -> fail "allowed-tools entries must be strings")
                (foldr (:) [] values)
        _ -> fail "allowed-tools must be a string or list of strings"

data OpenAiInterface = OpenAiInterface
    { interfaceDisplayName :: !(Maybe Text)
    , interfaceShortDescription :: !(Maybe Text)
    , interfaceDefaultPrompt :: !(Maybe Text)
    , interfaceArgumentHint :: !(Maybe Text)
    }

instance FromJSON OpenAiInterface where
    parseJSON = withObject "agents/openai.yaml interface" \o ->
        OpenAiInterface
            <$> o .:? "display_name"
            <*> o .:? "short_description"
            <*> o .:? "default_prompt"
            <*> o .:? "argument_hint"

newtype OpenAiPolicy = OpenAiPolicy
    { policyAllowImplicitInvocation :: Maybe Bool
    }

instance FromJSON OpenAiPolicy where
    parseJSON = withObject "agents/openai.yaml policy" \o ->
        OpenAiPolicy <$> o .:? "allow_implicit_invocation"

data OpenAiMetadata = OpenAiMetadata
    { openAiDisplayName :: !(Maybe Text)
    , openAiShortDescription :: !(Maybe Text)
    , openAiDefaultPrompt :: !(Maybe Text)
    , openAiArgumentHint :: !(Maybe Text)
    , openAiAllowImplicit :: !(Maybe Bool)
    }

instance FromJSON OpenAiMetadata where
    parseJSON = withObject "agents/openai.yaml" \o -> do
        interface <- (o .:? "interface" :: Parser (Maybe OpenAiInterface))
        policy <- (o .:? "policy" :: Parser (Maybe OpenAiPolicy))
        pure OpenAiMetadata
            { openAiDisplayName = interface >>= (.interfaceDisplayName)
            , openAiShortDescription =
                interface >>= (.interfaceShortDescription)
            , openAiDefaultPrompt = interface >>= (.interfaceDefaultPrompt)
            , openAiArgumentHint = interface >>= (.interfaceArgumentHint)
            , openAiAllowImplicit =
                policy >>= (.policyAllowImplicitInvocation)
            }

defaultSkillCatalogMaxChars :: Int
defaultSkillCatalogMaxChars = 8000

discoverSkills :: SkillDiscoverOptions -> IO SkillCatalog
discoverSkills options = do
    roots <- skillRoots options
    discovered <-
        mapConcurrentlyBounded skillRootConcurrency
            discoverRoot
            roots
    let skills = concatMap fst discovered
        warnings = concatMap snd discovered
    pure SkillCatalog
        { catalogSkills = sortOn skillSortKey skills
        , catalogWarnings = warnings
        }
  where
    discoverRoot (scope, origin, root) = do
        exists <- doesDirectoryExist root
        if not exists
            then pure ([], [])
            else do
                (files, walkWarnings) <-
                    findSkillFiles options.skillsMaxDepth root
                loaded <-
                    mapConcurrentlyBounded skillFileConcurrency
                        (loadSkillFile scope origin)
                        files
                pure
                    ( [skill | Right skill <- loaded]
                    , walkWarnings <> [warning | Left warning <- loaded]
                    )

skillRoots :: SkillDiscoverOptions -> IO [(SkillScope, SkillOrigin, FilePath)]
skillRoots options = do
    (projectRoot, cwd, home) <- runConcurrently $
        (,,)
            <$> Concurrently (canonicalizePath (unsafeToFilePath options.skillsProjectRoot))
            <*> Concurrently (canonicalizePath (unsafeToFilePath options.skillsCwd))
            <*> Concurrently (canonicalizePath (unsafeToFilePath options.skillsHome))
    let dirs =
            map unsafeToFilePath $
                directoryChain (unsafeEncodeUtf projectRoot) (unsafeEncodeUtf cwd)
        projectRoots =
            [ ( RepositorySkill depth (dir == cwd)
              , origin
              , dir </> relativeRoot
              )
            | (depth, dir) <- zip [0..] dirs
            , (origin, relativeRoot) <- filesystemSkillRoots
            ]
        userRoots =
            [ (UserSkill, origin, home </> relativeRoot)
            | (origin, relativeRoot) <- filesystemSkillRoots
            ]
        builtinRoots =
            [ (BuiltinSkill, origin, unsafeToFilePath root)
            | (origin, root) <- options.skillsBuiltinRoots
            ]
    pure (projectRoots <> userRoots <> builtinRoots)

filesystemSkillRoots :: [(SkillOrigin, FilePath)]
filesystemSkillRoots =
    [ (HaskellAgentSkills, ".haskell-agent" </> "skills")
    , (AgentSkills, ".agents" </> "skills")
    ]

findSkillFiles :: Int -> FilePath -> IO ([FilePath], [SkillWarning])
findSkillFiles maxDepth root = do
    seen <- newTVarIO Set.empty
    (files, warnings) <- go seen 0 [root]
    pure (sort files, warnings)
  where
    go _ depth _ | depth > maxDepth = pure ([], [])
    go _ _ [] = pure ([], [])
    go seen depth dirs = do
        let orderedDirs = sort dirs
        canonicalized <-
            mapConcurrentlyBounded skillDirectoryConcurrency
                (\dir ->
                    tryAny (canonicalizePath dir) >>= \case
                        Left err ->
                            pure (Left (skillIoWarning dir err))
                        Right canonical ->
                            pure (Right (dir, canonical)))
                orderedDirs
        let walkWarnings = [warning | Left warning <- canonicalized]
        claimed <- atomically do
            visited <- readTVar seen
            let claim (current, selected) = \case
                    Left _ -> (current, selected)
                    Right (dir, canonical)
                        | canonical `Set.member` current ->
                            (current, selected)
                        | otherwise ->
                            (Set.insert canonical current, dir : selected)
                (updated, selected) =
                    foldl claim (visited, []) canonicalized
            modifyTVar' seen (const updated)
            pure (reverse selected)
        inspected <-
            mapConcurrentlyBounded skillDirectoryConcurrency
                inspectDirectory
                claimed
        let found = concatMap (\(files, _, _) -> files) inspected
            children = concatMap (\(_, nested, _) -> nested) inspected
            inspectWarnings = concatMap (\(_, _, warnings) -> warnings) inspected
        (nested, nestedWarnings) <- go seen (depth + 1) children
        pure
            ( found <> nested
            , walkWarnings <> inspectWarnings <> nestedWarnings
            )

    inspectDirectory dir = do
        entriesResult <- tryAny (listDirectory dir)
        case entriesResult of
            Left err ->
                pure ([], [], [skillIoWarning dir err])
            Right entries -> do
                let skillPath = dir </> "SKILL.md"
                hasSkill <- doesFileExist skillPath
                children <-
                    filterM doesDirectoryExist
                        [dir </> entry | entry <- sort entries]
                pure ([skillPath | hasSkill], children, [])

skillIoWarning :: FilePath -> SomeException -> SkillWarning
skillIoWarning path err =
    SkillWarning
        (unsafeEncodeUtf path)
        (Text.pack (displayException err))

skillRootConcurrency :: Int
skillRootConcurrency = 4

skillDirectoryConcurrency :: Int
skillDirectoryConcurrency = 8

skillFileConcurrency :: Int
skillFileConcurrency = 8

loadSkillFile
    :: SkillScope
    -> SkillOrigin
    -> FilePath
    -> IO (Either SkillWarning Skill)
loadSkillFile scope origin path = do
    result <- tryAny (retryOnFileBusy (Text.readFile path))
    case result of
        Left err ->
            pure $ Left (warning (Text.pack (displayException err)))
        Right fileText ->
            case splitFrontmatter fileText of
                Left err -> pure (Left (warning err))
                Right (yamlText, body) ->
                    case decodeEither' (Text.encodeUtf8 yamlText) of
                        Left err ->
                            pure $ Left
                                (warning
                                    (Text.pack (prettyPrintParseException err)))
                        Right frontmatter ->
                            case validateFrontmatter scope path frontmatter of
                                Left err -> pure (Left (warning err))
                                Right () ->
                                    loadOpenAiMetadata (takeDirectory path) >>= \case
                                        Left err ->
                                            pure $ Left
                                                (warning
                                                    ("agents/openai.yaml: " <> err))
                                        Right openAi -> do
                                            let argumentHint =
                                                    frontmatter.fmArgumentHint
                                                        <|> (openAi >>= (.openAiArgumentHint))
                                                modelInvocable =
                                                    not frontmatter.fmDisableModelInvocation
                                                        && fromMaybe True
                                                            (openAi >>= (.openAiAllowImplicit))
                                            pure $ Right
                                                (skillFromFrontmatter
                                                    (FilesystemSkillSource
                                                        { skillPath = unsafeEncodeUtf path
                                                        , skillDirectory =
                                                            unsafeEncodeUtf
                                                                (takeDirectory path)
                                                        , skillBody = Text.strip body
                                                        , skillFileText = fileText
                                                        , skillScope = scope
                                                        , skillOrigin = origin
                                                        })
                                                    frontmatter)
                                                    { skillDisplayName =
                                                        openAi >>= (.openAiDisplayName)
                                                    , skillShortDescription =
                                                        (openAi >>= (.openAiShortDescription))
                                                            <|> Map.lookup "short-description"
                                                                frontmatter.fmMetadata
                                                    , skillDefaultPrompt =
                                                        openAi >>= (.openAiDefaultPrompt)
                                                    , skillArgumentHint = argumentHint
                                                    , skillModelInvocable = modelInvocable
                                                    }
  where
    warning message = SkillWarning (unsafeEncodeUtf path) message

-- | Build a lightweight, untrusted MCP catalog entry from the frontmatter
-- advertised by @skills/list@. The complete document must still be loaded and
-- checked with 'loadMcpSkillDocument' before its instructions are used.
loadMcpSkillMetadata
    :: Text
    -- ^ Configured MCP server name.
    -> Text
    -- ^ Skill document URI.
    -> [Text]
    -- ^ Advertised resource URIs.
    -> Value
    -> Either Text Skill
loadMcpSkillMetadata server uri resourceUris value = do
    frontmatter <-
        firstText (parseEither parseJSON value)
    validateMcpFrontmatter frontmatter
    pure $
        skillFromFrontmatter
            (McpSkillSource server uri resourceUris)
            frontmatter

-- | Parse a fetched MCP SKILL.md and require its advertised identity to remain
-- stable. Transport-level manifest verification belongs to the MCP adapter.
loadMcpSkillDocument :: Skill -> Text -> Either Text SkillContent
loadMcpSkillDocument advertised fileText =
    case advertised.skillSource of
        FilesystemSkillSource{} ->
            Left "expected an MCP-backed skill"
        McpSkillSource _server uri resourceUris -> do
            (yamlText, body) <- splitFrontmatter fileText
            frontmatter <-
                either
                    (Left . Text.pack . prettyPrintParseException)
                    Right
                    (decodeEither' (Text.encodeUtf8 yamlText))
            validateMcpFrontmatter frontmatter
            let fetched = skillFromFrontmatter advertised.skillSource frontmatter
            if fetched /= advertised
                then Left "fetched MCP skill frontmatter does not match its catalog entry"
                else
                    Right SkillContent
                        { skillContentBody = Text.strip body
                        , skillContentFileText = fileText
                        , skillContentFile = uri
                        , skillContentDirectory = Nothing
                        , skillContentResourceUris = resourceUris
                        }

filesystemSkillContent :: Skill -> Maybe SkillContent
filesystemSkillContent skill =
    case skill.skillSource of
        FilesystemSkillSource path directory body fileText _ _ ->
            Just SkillContent
                { skillContentBody = body
                , skillContentFileText = fileText
                , skillContentFile = toText path
                , skillContentDirectory = Just (toText directory)
                , skillContentResourceUris = []
                }
        McpSkillSource{} -> Nothing

skillFromFrontmatter :: SkillSource -> Frontmatter -> Skill
skillFromFrontmatter source frontmatter =
    Skill
        { skillName = frontmatter.fmName
        , skillDescription = frontmatter.fmDescription
        , skillDisplayName = Nothing
        , skillShortDescription =
            Map.lookup "short-description" frontmatter.fmMetadata
        , skillDefaultPrompt = Nothing
        , skillWhenToUse = frontmatter.fmWhenToUse
        , skillContextMode = frontmatter.fmContextMode
        , skillArgumentHint = frontmatter.fmArgumentHint
        , skillUserInvocable = frontmatter.fmUserInvocable
        , skillModelInvocable = not frontmatter.fmDisableModelInvocation
        , skillAllowedTools = frontmatter.fmAllowedTools
        , skillModelOverride = frontmatter.fmModel
        , skillEffortOverride = frontmatter.fmEffort
        , skillLicense = frontmatter.fmLicense
        , skillCompatibility = frontmatter.fmCompatibility
        , skillMetadata = frontmatter.fmMetadata
        , skillSource = source
        }

validateMcpFrontmatter :: Frontmatter -> Either Text ()
validateMcpFrontmatter frontmatter
    | not (validSkillName frontmatter.fmName) =
        Left "skill name must be 1-64 lowercase letters, digits, or hyphens without edge/consecutive hyphens"
    | Text.length frontmatter.fmDescription < 1
        || Text.length frontmatter.fmDescription > 1024 =
        Left "skill description must be 1-1024 characters"
    | frontmatter.fmContextMode == SkillContextAlways =
        Left "activation `always` is reserved for trusted built-in skills"
    | otherwise = Right ()

firstText :: Either String a -> Either Text a
firstText = either (Left . Text.pack) Right

loadOpenAiMetadata :: FilePath -> IO (Either Text (Maybe OpenAiMetadata))
loadOpenAiMetadata dir = do
    let path = dir </> "agents" </> "openai.yaml"
    exists <- doesFileExist path
    if not exists
        then pure (Right Nothing)
        else do
            result <- tryAny (retryOnFileBusy (BS.readFile path))
            pure $ case result of
                Left err ->
                    Left (Text.pack (displayException err))
                Right bytes ->
                    case decodeEither' bytes of
                        Left err ->
                            Left (Text.pack (prettyPrintParseException err))
                        Right metadata ->
                            Right (Just metadata)

splitFrontmatter :: Text -> Either Text (Text, Text)
splitFrontmatter text =
    case Text.lines text of
        "---" : rest ->
            let (yamlLines, after) = break (== "---") rest
            in case after of
                [] -> Left "SKILL.md has no closing YAML frontmatter delimiter"
                _ : body -> Right (Text.unlines yamlLines, Text.unlines body)
        _ -> Left "SKILL.md must start with YAML frontmatter"

validateFrontmatter :: SkillScope -> FilePath -> Frontmatter -> Either Text ()
validateFrontmatter scope path frontmatter
    | not (validSkillName frontmatter.fmName) =
        Left "skill name must be 1-64 lowercase letters, digits, or hyphens without edge/consecutive hyphens"
    | Text.length frontmatter.fmDescription < 1
        || Text.length frontmatter.fmDescription > 1024 =
        Left "skill description must be 1-1024 characters"
    | Text.pack (takeFileName (takeDirectory path)) /= frontmatter.fmName =
        Left "skill name must match its parent directory"
    | frontmatter.fmContextMode == SkillContextAlways
        && scope /= BuiltinSkill =
        Left "activation `always` is reserved for trusted built-in skills"
    | otherwise = Right ()

validSkillName :: Text -> Bool
validSkillName name = case Text.unpack name of
    [] -> False
    chars@(first:_) ->
        length chars <= 64
            && first /= '-'
            && last chars /= '-'
            && not ("--" `Text.isInfixOf` name)
            && all (\c -> isAsciiLower c || isAsciiDigit c || c == '-') chars
  where
    isAsciiLower c = c >= 'a' && c <= 'z'
    isAsciiDigit c = c >= '0' && c <= '9'

skillSortKey :: Skill -> (Down Int, Down Int, Down Int, Text, Text)
skillSortKey skill =
    ( Down scopeRank
    , Down depth
    , Down originRank
    , skill.skillName
    , skillSourceIdentity skill
    )
  where
    (scopeRank, depth, originRank) = case skill.skillSource of
        McpSkillSource{} -> (-2, 0, 0)
        FilesystemSkillSource _ _ _ _ scope origin ->
            let (rank, sourceDepth) = case scope of
                    BuiltinSkill -> (-1, 0)
                    UserSkill -> (0, 0)
                    RepositorySkill d _ -> (1, d)
                sourceOriginRank = case origin of
                    HaskellAgentSkills -> 2
                    AgentSkills -> 1
            in (rank, sourceDepth, sourceOriginRank)

modelVisibleSkills :: SkillCatalog -> [Skill]
modelVisibleSkills catalog =
    filter (.skillModelInvocable) catalog.catalogSkills

contextSkills :: SkillCatalog -> [Skill]
contextSkills catalog =
    alwaysSkills <> onDemandSkills
  where
    alwaysSkills =
        Map.elems $
            Map.fromListWith preferHigherPrecedence
                [ (skill.skillName, skill)
                | skill <- catalog.catalogSkills
                , skill.skillContextMode == SkillContextAlways
                ]
    onDemandSkills =
        [ skill
        | skill <- modelVisibleSkills catalog
        , skill.skillContextMode == SkillContextOnDemand
        ]
    preferHigherPrecedence left right
        | skillSortKey left <= skillSortKey right = left
        | otherwise = right

buildSkillInvocations :: [Text] -> SkillCatalog -> [SkillInvocation]
buildSkillInvocations reserved catalog =
    concatMap bindings groups
  where
    reservedSet = Set.fromList (map Text.toLower reserved)
    groups = Map.toList $
        Map.fromListWith (<>)
            [(skill.skillName, [skill]) | skill <- catalog.catalogSkills]
    bindings (name, skills) =
        let ordered = sortOn skillSortKey skills
            bare =
                [ SkillInvocation name skill True
                | skill <- take 1 ordered
                , Text.toLower name `Set.notMember` reservedSet
                ]
            needsQualified =
                length ordered > 1
                    || Text.toLower name `Set.member` reservedSet
                    || any isMcpSkill ordered
            qualified =
                if needsQualified
                    then zipWith (qualifiedInvocation name ordered) [0 :: Int ..] ordered
                    else []
        in bare <> qualified

qualifiedInvocation :: Text -> [Skill] -> Int -> Skill -> SkillInvocation
qualifiedInvocation name siblings index skill =
    SkillInvocation
        { invocationName =
            uniqueQualifier
                <> ":"
                <> name
        , invocationSkill = skill
        , invocationBare = False
        }
  where
    base = scopeQualifier skill
    sameScope =
        [ sibling
        | sibling <- siblings
        , scopeQualifier sibling == base
        ]
    sameOriginBefore =
        length
            [ sibling
            | sibling <- take index siblings
            , scopeQualifier sibling == base
            , sourceSlug sibling == sourceSlug skill
            ]
    uniqueQualifier
        | length sameScope == 1 = base
        | otherwise =
            base
                <> "-"
                <> sourceSlug skill
                <> if sameOriginBefore == 0
                    then ""
                    else "-" <> Text.pack (show (sameOriginBefore + 1))

scopeQualifier :: Skill -> Text
scopeQualifier skill = case skill.skillSource of
    McpSkillSource server _ _ -> "mcp-" <> qualifierSlug server
    FilesystemSkillSource _ _ _ _ scope _ -> case scope of
        BuiltinSkill -> "builtin"
        UserSkill -> "user"
        RepositorySkill _ True -> "local"
        RepositorySkill _ False -> "repo"

originSlug :: SkillOrigin -> Text
originSlug = \case
    HaskellAgentSkills -> "haskell-agent"
    AgentSkills -> "agents"

sourceSlug :: Skill -> Text
sourceSlug skill = case skill.skillSource of
    McpSkillSource server _ _ -> qualifierSlug server
    FilesystemSkillSource _ _ _ _ _ origin -> originSlug origin

qualifierSlug :: Text -> Text
qualifierSlug =
    Text.dropAround (== '-')
        . Text.intercalate "-"
        . filter (not . Text.null)
        . Text.split (== '-')
        . Text.map
            (\c ->
                if isAlphaNum c
                    then c
                    else '-')
        . Text.toLower

isMcpSkill :: Skill -> Bool
isMcpSkill skill = case skill.skillSource of
    McpSkillSource{} -> True
    FilesystemSkillSource{} -> False

resolveSkillInvocation
    :: [SkillInvocation]
    -> Text
    -> Either Text SkillInvocation
resolveSkillInvocation invocations rawName =
    case Map.lookup (Text.toLower rawName) (skillInvocationsByName invocations) of
        Just invocation -> Right invocation
        Nothing ->
            Left $
                "unknown skill: "
                    <> rawName
                    <> availableSuffix invocations

skillInvocationsByName :: [SkillInvocation] -> Map Text SkillInvocation
skillInvocationsByName invocations =
    Map.fromList
        [ (Text.toLower invocation.invocationName, invocation)
        | invocation <- invocations
        ]

resolveSkillMentions
    :: [SkillInvocation]
    -> Text
    -> Either Text [SkillInvocation]
resolveSkillMentions invocations text =
    dedupe <$> traverse resolve (skillMentionNames text)
  where
    resolve = resolveSkillInvocation invocations

resolveSkillMentionsWithWarnings
    :: [SkillInvocation]
    -> Text
    -> ([Text], [SkillInvocation])
resolveSkillMentionsWithWarnings invocations text =
    ( [warning | Left warning <- resolved]
    , dedupe [invocation | Right invocation <- resolved]
    )
  where
    resolved =
        map (resolveSkillInvocation invocations) (skillMentionNames text)

skillMentionNames :: Text -> [Text]
skillMentionNames text =
    [ name
    | token <- Text.words text
    , "$" `Text.isPrefixOf` token
    , let name = Text.drop 1 token
    , not (Text.null name)
    , Text.all mentionChar name
    ]
  where
    mentionChar c = isAlphaNum c || c `elem` ['-', ':']

dedupe :: [SkillInvocation] -> [SkillInvocation]
dedupe = nubOrdOn (skillSourceIdentity . (.invocationSkill))

skillSourceIdentity :: Skill -> Text
skillSourceIdentity skill = case skill.skillSource of
    FilesystemSkillSource path _ _ _ _ _ -> "file:" <> toText path
    McpSkillSource server uri _ -> "mcp:" <> server <> ":" <> uri

availableSuffix :: [SkillInvocation] -> Text
availableSuffix invocations =
    case map (.invocationName) invocations of
        [] -> " (no user-invocable skills are available)"
        names -> " (available: " <> Text.intercalate ", " names <> ")"

formatSkillCatalogContext :: Int -> SkillCatalog -> (Maybe Text, Int)
formatSkillCatalogContext maxChars catalog
    | maxChars <= 0 || null skills = (Nothing, 0)
    | otherwise =
        let header = Text.unlines
                [ "## Skills"
                , "The following reusable skills are available in this session."
                , "Always-active skills are included in full below and must be followed for every matching turn."
                , "Use a skill when the user names it or the task clearly matches its description."
                , "Users can explicitly invoke a skill with `$skill-name`."
                , "For on-demand skills, call `view_skill` with the listed name to load the full instructions."
                , "Resolve relative scripts, references, and assets from the skill directory."
                , "Load only the resources needed for the task; do not carry skills across turns unless relevant again."
                , "Briefly state which skill(s) you are using. If a skill cannot be read, say so and continue with the best fallback."
                , ""
                , "### Available skills"
                ]
            room = max 0 (maxChars - Text.length header)
            (kept, omitted) = fitSkillLines room skills
            text = Text.take maxChars (header <> Text.unlines kept)
        in (Just text, omitted)
  where
    skills = contextSkills catalog

renderSkillLine :: Skill -> Text
renderSkillLine skill =
    case skill.skillContextMode of
        SkillContextAlways ->
            case filesystemSkillContent skill of
                Nothing -> ""
                Just content ->
                    Text.unlines
                        [ "### Always-active skill: " <> skill.skillName
                        , "SKILL.md: " <> content.skillContentFile
                        , neutralizeSkillTags content.skillContentBody
                        ]
        SkillContextOnDemand ->
            "- $"
                <> skill.skillName
                <> ": "
                <> Text.replace "\n" " " skill.skillDescription
                <> maybe "" (\trigger -> " Trigger: " <> Text.replace "\n" " " trigger)
                    skill.skillWhenToUse

fitSkillLines :: Int -> [Skill] -> ([Text], Int)
fitSkillLines budget = go budget []
  where
    go _ kept [] = (reverse kept, 0)
    go remaining kept allSkills@(skill:rest)
        | remaining <= 1 = (reverse kept, length allLines)
        | Text.length fullLine + 1 <= remaining =
            go (remaining - Text.length fullLine - 1) (fullLine : kept) rest
        | otherwise =
            case renderShortenedSkillLine remaining skill of
                Just shortened -> (reverse (shortened : kept), length rest)
                Nothing -> (reverse kept, length allSkills)
      where
        allLines = allSkills
        fullLine = renderSkillLine skill

renderShortenedSkillLine :: Int -> Skill -> Maybe Text
renderShortenedSkillLine remaining skill =
    case skill.skillContextMode of
        SkillContextAlways -> Nothing
        SkillContextOnDemand ->
            let prefix = "- $" <> skill.skillName <> ": "
                available = remaining - Text.length prefix - 2
            in if available < 12
                then Nothing
                else Just $
                    prefix
                        <> Text.take available
                            (Text.replace "\n" " " skill.skillDescription)
                        <> "…"

formatSkillActivation :: SkillInvocation -> SkillContent -> Text -> Text
formatSkillActivation invocation content arguments =
    Text.concat
        [ "# Skill instructions: "
        , invocation.invocationSkill.skillName
        , "\n\n"
        , "SKILL.md: "
        , singleLine content.skillContentFile
        , maybe
            ""
            ("\nSkill directory: " <>)
            content.skillContentDirectory
        , case invocation.invocationSkill.skillSource of
            McpSkillSource server _ _ ->
                "\nMCP server: " <> singleLine server
                    <> renderResourceUris content.skillContentResourceUris
            FilesystemSkillSource{} -> ""
        , "\nInvocation arguments: "
        , if Text.null (Text.strip arguments) then "(none)" else arguments
        , "\n\n<SKILL_INSTRUCTIONS>\n"
        , neutralizeSkillTags content.skillContentFileText
        , "\n</SKILL_INSTRUCTIONS>\n\n"
        , case invocation.invocationSkill.skillSource of
            McpSkillSource{} ->
                "Follow these instructions for this turn. Read listed resources with mcp_read_resource when needed. "
            FilesystemSkillSource{} ->
                "Follow these instructions for this turn. Resolve relative resource paths from the skill directory above. "
        , "Normal tool approval, sandboxing, and plan-mode restrictions still apply."
        ]

renderResourceUris :: [Text] -> Text
renderResourceUris [] = ""
renderResourceUris uris =
    "\nMCP skill resources:\n"
        <> Text.unlines (map (("- " <>) . singleLine) uris)

singleLine :: Text -> Text
singleLine =
    neutralizeSkillTags
        . Text.replace "\n" "\\n"
        . Text.replace "\r" "\\r"

neutralizeSkillTags :: Text -> Text
neutralizeSkillTags =
    Text.replace "<SKILL_INSTRUCTIONS" "&lt;SKILL_INSTRUCTIONS"
        . Text.replace "</SKILL_INSTRUCTIONS" "&lt;/SKILL_INSTRUCTIONS"
