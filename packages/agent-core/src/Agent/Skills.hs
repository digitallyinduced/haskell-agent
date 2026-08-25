-- | Discover, parse, select, and format filesystem-backed Agent Skills.
module Agent.Skills
    ( Skill(..)
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
    , buildSkillInvocations
    , modelVisibleSkills
    , formatSkillCatalogContext
    , formatSkillActivation
    , resolveSkillInvocation
    , resolveSkillMentions
    ) where

import Agent.Concurrent (mapConcurrentlyBounded)
import Agent.FileRetry (retryOnFileBusy)
import Agent.OsPath (directoryChain, toText, unsafeToFilePath)
import Control.Applicative ((<|>))
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
    , Object
    , Value(..)
    , withObject
    , (.:)
    , (.:?)
    , (.!=)
    )
import Data.Aeson.Key (Key)
import System.OsPath (OsPath, unsafeEncodeUtf)
import Data.Aeson.Types (Parser)
import qualified Data.ByteString as BS
import Data.Char (isAlphaNum)
import Data.List (find, sort, sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Ord (Down(..))
import Data.Set (Set)
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

data SkillOrigin
    = AgentSkills
    | GrokSkills
    | CodexSkills
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
    , skillPath :: !OsPath
    , skillDirectory :: !OsPath
    , skillBody :: !Text
    , skillFileText :: !Text
    , skillScope :: !SkillScope
    , skillOrigin :: !SkillOrigin
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
        allowed <- o .:? "allowed-tools"
        metadata <- o .:? "metadata" .!= Map.empty
        Frontmatter
            <$> o .: "name"
            <*> o .: "description"
            <*> o .:? "when-to-use"
            <*> (o .:? "activation" .!= SkillContextOnDemand)
            <*> o .:? "argument-hint"
            <*> (o .:? "user-invocable" .!= True)
            <*> (o .:? "disable-model-invocation" .!= False)
            <*> parseAllowedTools allowed
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

parseAllowedTools :: Maybe Value -> Parser [Text]
parseAllowedTools = \case
    Nothing -> pure []
    Just (String text) ->
        pure (filter (not . Text.null) (Text.words (Text.replace "," " " text)))
    Just (Array values) ->
        traverse
            (\case
                String text -> pure text
                _ -> fail "allowed-tools entries must be strings")
            (foldr (:) [] values)
    Just _ -> fail "allowed-tools must be a string or list of strings"

data OpenAiMetadata = OpenAiMetadata
    { openAiDisplayName :: !(Maybe Text)
    , openAiShortDescription :: !(Maybe Text)
    , openAiDefaultPrompt :: !(Maybe Text)
    , openAiArgumentHint :: !(Maybe Text)
    , openAiAllowImplicit :: !(Maybe Bool)
    }

instance FromJSON OpenAiMetadata where
    parseJSON = withObject "agents/openai.yaml" \o -> do
        interface <- o .:? "interface"
        policy <- o .:? "policy"
        let interfaceFields = interface >>= valueObject
            policyFields = policy >>= valueObject
        OpenAiMetadata
            <$> optionalField interfaceFields "display_name"
            <*> optionalField interfaceFields "short_description"
            <*> optionalField interfaceFields "default_prompt"
            <*> optionalField interfaceFields "argument_hint"
            <*> optionalField policyFields "allow_implicit_invocation"

valueObject :: Value -> Maybe Object
valueObject (Object fields) = Just fields
valueObject _ = Nothing

optionalField :: FromJSON a => Maybe Object -> Key -> Parser (Maybe a)
optionalField fields key =
    maybe (pure Nothing) (\object -> object .:? key) fields

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
    canonical <- mapConcurrentlyBounded 3 canonicalizePath
        [ unsafeToFilePath options.skillsProjectRoot
        , unsafeToFilePath options.skillsCwd
        , unsafeToFilePath options.skillsHome
        ]
    let (projectRoot, cwd, home) = case canonical of
            [project, current, userHome] -> (project, current, userHome)
            _ -> error "skillRoots: canonical path count changed"
    let dirs =
            map unsafeToFilePath $
                directoryChain (unsafeEncodeUtf projectRoot) (unsafeEncodeUtf cwd)
        projectRoots =
            [ ( RepositorySkill depth (dir == cwd)
              , origin
              , dir </> relativeRoot origin
              )
            | (depth, dir) <- zip [0..] dirs
            , origin <- origins
            ]
        userRoots =
            [ (UserSkill, origin, home </> userRoot origin)
            | origin <- origins
            ]
        builtinRoots =
            [ (BuiltinSkill, origin, unsafeToFilePath root)
            | (origin, root) <- options.skillsBuiltinRoots
            ]
    pure (projectRoots <> userRoots <> builtinRoots)
  where
    origins = [AgentSkills, GrokSkills, CodexSkills]
    relativeRoot = \case
        AgentSkills -> ".agents" </> "skills"
        GrokSkills -> ".grok" </> "skills"
        CodexSkills -> ".codex" </> "skills"
    userRoot = \case
        AgentSkills -> ".agents" </> "skills"
        GrokSkills -> ".grok" </> "skills"
        CodexSkills -> ".codex" </> "skills"

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
                                            pure $ Right Skill
                                                { skillName = frontmatter.fmName
                                                , skillDescription = frontmatter.fmDescription
                                                , skillDisplayName =
                                                    openAi >>= (.openAiDisplayName)
                                                , skillShortDescription =
                                                    (openAi >>= (.openAiShortDescription))
                                                        <|> Map.lookup "short-description"
                                                            frontmatter.fmMetadata
                                                , skillDefaultPrompt =
                                                    openAi >>= (.openAiDefaultPrompt)
                                                , skillWhenToUse = frontmatter.fmWhenToUse
                                                , skillContextMode = frontmatter.fmContextMode
                                                , skillArgumentHint = argumentHint
                                                , skillUserInvocable = frontmatter.fmUserInvocable
                                                , skillModelInvocable = modelInvocable
                                                , skillAllowedTools = frontmatter.fmAllowedTools
                                                , skillModelOverride = frontmatter.fmModel
                                                , skillEffortOverride = frontmatter.fmEffort
                                                , skillLicense = frontmatter.fmLicense
                                                , skillCompatibility = frontmatter.fmCompatibility
                                                , skillMetadata = frontmatter.fmMetadata
                                                , skillPath = unsafeEncodeUtf path
                                                , skillDirectory = unsafeEncodeUtf (takeDirectory path)
                                                , skillBody = Text.strip body
                                                , skillFileText = fileText
                                                , skillScope = scope
                                                , skillOrigin = origin
                                                }
  where
    warning message = SkillWarning (unsafeEncodeUtf path) message

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
    , toText skill.skillPath
    )
  where
    (scopeRank, depth) = case skill.skillScope of
        BuiltinSkill -> (-1, 0)
        UserSkill -> (0, 0)
        RepositorySkill d _ -> (1, d)
    originRank = case skill.skillOrigin of
        AgentSkills -> 3
        GrokSkills -> 2
        CodexSkills -> 1

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
                length ordered > 1 || Text.toLower name `Set.member` reservedSet
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
            , sibling.skillOrigin == skill.skillOrigin
            ]
    uniqueQualifier
        | length sameScope == 1 = base
        | otherwise =
            base
                <> "-"
                <> originSlug skill.skillOrigin
                <> if sameOriginBefore == 0
                    then ""
                    else "-" <> Text.pack (show (sameOriginBefore + 1))

scopeQualifier :: Skill -> Text
scopeQualifier skill = case skill.skillScope of
    BuiltinSkill -> "builtin"
    UserSkill -> "user"
    RepositorySkill _ True -> "local"
    RepositorySkill _ False -> "repo"

originSlug :: SkillOrigin -> Text
originSlug = \case
    AgentSkills -> "agents"
    GrokSkills -> "grok"
    CodexSkills -> "codex"

resolveSkillInvocation
    :: [SkillInvocation]
    -> Text
    -> Either Text SkillInvocation
resolveSkillInvocation invocations rawName =
    case find ((== Text.toLower rawName) . Text.toLower . (.invocationName)) invocations of
        Just invocation -> Right invocation
        Nothing ->
            Left $
                "unknown skill: "
                    <> rawName
                    <> availableSuffix invocations

resolveSkillMentions
    :: [SkillInvocation]
    -> Text
    -> Either Text [SkillInvocation]
resolveSkillMentions invocations text =
    dedupe <$> traverse resolve mentioned
  where
    mentioned =
        [ Text.drop 1 token
        | token <- Text.words text
        , "$" `Text.isPrefixOf` token
        , let name = Text.drop 1 token
        , not (Text.null name)
        , Text.all mentionChar name
        ]
    mentionChar c = isAlphaNum c || c `elem` ['-', ':']
    resolve = resolveSkillInvocation invocations
    dedupe = go Set.empty
    go :: Set OsPath -> [SkillInvocation] -> [SkillInvocation]
    go _ [] = []
    go seen (item:rest)
        | item.invocationSkill.skillPath `Set.member` seen = go seen rest
        | otherwise =
            item : go (Set.insert item.invocationSkill.skillPath seen) rest

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
            Text.unlines
                [ "### Always-active skill: " <> skill.skillName
                , "SKILL.md: " <> toText skill.skillPath
                , neutralizeSkillTags skill.skillBody
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

formatSkillActivation :: SkillInvocation -> Text -> Text
formatSkillActivation invocation arguments =
    Text.concat
        [ "# Skill instructions: "
        , invocation.invocationSkill.skillName
        , "\n\n"
        , "SKILL.md: "
        , toText invocation.invocationSkill.skillPath
        , "\nSkill directory: "
        , toText invocation.invocationSkill.skillDirectory
        , "\nInvocation arguments: "
        , if Text.null (Text.strip arguments) then "(none)" else arguments
        , "\n\n<SKILL_INSTRUCTIONS>\n"
        , neutralizeSkillTags invocation.invocationSkill.skillFileText
        , "\n</SKILL_INSTRUCTIONS>\n\n"
        , "Follow these instructions for this turn. Resolve relative resource paths from the skill directory above. "
        , "Normal tool approval, sandboxing, and plan-mode restrictions still apply."
        ]

neutralizeSkillTags :: Text -> Text
neutralizeSkillTags =
    Text.replace "<SKILL_INSTRUCTIONS" "&lt;SKILL_INSTRUCTIONS"
        . Text.replace "</SKILL_INSTRUCTIONS" "&lt;/SKILL_INSTRUCTIONS"
