-- | Model-facing tools and startup context for reusable learned skills.
module Agent.CLI.LearnedSkills
    ( LearnedSkillToolsEnv(..)
    , LearnedSkillCreateRequest(..)
    , LearnedSkillUpdateRequest(..)
    , LearnedSkillArchiveRequest(..)
    , LearnedSkillRollbackRequest(..)
    , learnedSkillTools
    , defaultLearnedSkillContextMaxChars
    , formatLearnedSkillContext
    , queueLearnedSkillContextWithOmissions
    ) where

import Agent.CLI.Database (DatabaseScope(..))
import Agent.OsPath (toText)
import Agent.Skills
    ( Skill(..)
    , SkillInvocation(..)
    , resolveSkillInvocation
    )
import Agent.Store.Postgres.Scope
    ( Scope(..)
    , ScopeKind(..)
    , scopeKindText
    )
import Agent.Store.Postgres.Skill
    ( LearnedSkill(..)
    , LearnedSkillActivation(..)
    , LearnedSkillStatus(..)
    , learnedSkillActivationText
    )
import Agent.ToolDSL (PropertySchema(..), PropertyType(..))
import Agent.ToolDispatch (typedTool)
import Agent.Tools.Types
    ( AppTool
    , ToolExecutionPolicy(..)
    , jsonTool
    )
import Data.Aeson
    ( FromJSON(..)
    , Value
    , (.:)
    , (.:?)
    , withObject
    )
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (Parser)
import qualified Data.ByteString.Lazy as LBS
import Data.Int (Int64)
import Data.IORef (IORef, modifyIORef', readIORef)
import Data.List (sortOn)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust)
import Data.Ord (Down(..))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

data LearnedSkillCreateRequest = LearnedSkillCreateRequest
    { createRequestScope :: !DatabaseScope
    , createRequestSlug :: !Text
    , createRequestTitle :: !Text
    , createRequestDescription :: !Text
    , createRequestAppliesWhen :: !Text
    , createRequestInstructions :: !Text
    , createRequestActivation :: !LearnedSkillActivation
    , createRequestPriority :: !Int
    , createRequestChangeSummary :: !Text
    , createRequestEvidence :: !Text
    }
    deriving (Eq, Show)

data LearnedSkillUpdateRequest = LearnedSkillUpdateRequest
    { updateRequestScope :: !DatabaseScope
    , updateRequestSlug :: !Text
    , updateRequestExpectedRevision :: !Integer
    , updateRequestTitle :: !(Maybe Text)
    , updateRequestDescription :: !(Maybe Text)
    , updateRequestAppliesWhen :: !(Maybe Text)
    , updateRequestInstructions :: !(Maybe Text)
    , updateRequestActivation :: !(Maybe LearnedSkillActivation)
    , updateRequestPriority :: !(Maybe Int)
    , updateRequestChangeSummary :: !Text
    , updateRequestEvidence :: !Text
    }
    deriving (Eq, Show)

data LearnedSkillArchiveRequest = LearnedSkillArchiveRequest
    { archiveRequestScope :: !DatabaseScope
    , archiveRequestSlug :: !Text
    , archiveRequestExpectedRevision :: !Integer
    , archiveRequestChangeSummary :: !Text
    , archiveRequestEvidence :: !Text
    }
    deriving (Eq, Show)

data LearnedSkillRollbackRequest = LearnedSkillRollbackRequest
    { rollbackRequestScope :: !DatabaseScope
    , rollbackRequestSlug :: !Text
    , rollbackRequestExpectedRevision :: !Integer
    , rollbackRequestTargetRevision :: !Integer
    , rollbackRequestChangeSummary :: !Text
    , rollbackRequestEvidence :: !Text
    }
    deriving (Eq, Show)

data LearnedSkillToolsEnv = LearnedSkillToolsEnv
    { learnedSkillSearch
        :: !(Text -> Int -> IO (Either Text Value))
    , learnedSkillRead
        :: !(DatabaseScope -> Text -> Maybe Integer -> IO (Either Text Value))
    , learnedSkillCreate
        :: !(LearnedSkillCreateRequest -> IO (Either Text Value))
    , learnedSkillUpdate
        :: !(LearnedSkillUpdateRequest -> IO (Either Text Value))
    , learnedSkillArchive
        :: !(LearnedSkillArchiveRequest -> IO (Either Text Value))
    , learnedSkillRollback
        :: !(LearnedSkillRollbackRequest -> IO (Either Text Value))
    }

data SearchArgs = SearchArgs !Text !Int

instance FromJSON SearchArgs where
    parseJSON = withObject "SearchArgs" \object ->
        SearchArgs
            <$> object .: "query"
            <*> (object .:? "limit" >>= pure . fromMaybe 10)

data ViewArgs = ViewArgs !Text !(Maybe DatabaseScope) !(Maybe Integer)

instance FromJSON ViewArgs where
    parseJSON = withObject "ViewArgs" \object ->
        ViewArgs
            <$> object .: "name"
            <*> object .:? "scope"
            <*> object .:? "revision"

instance FromJSON LearnedSkillCreateRequest where
    parseJSON = withObject "LearnedSkillCreateRequest" \object -> do
        activation <- parseOptionalActivation object
        LearnedSkillCreateRequest
            <$> object .: "scope"
            <*> object .: "slug"
            <*> object .: "title"
            <*> object .: "description"
            <*> object .: "applies_when"
            <*> object .: "instructions"
            <*> pure (fromMaybe SkillRelevant activation)
            <*> (object .:? "priority" >>= pure . fromMaybe 0)
            <*> object .: "change_summary"
            <*> object .: "evidence"

instance FromJSON LearnedSkillUpdateRequest where
    parseJSON = withObject "LearnedSkillUpdateRequest" \object ->
        LearnedSkillUpdateRequest
            <$> object .: "scope"
            <*> object .: "slug"
            <*> object .: "expected_revision"
            <*> object .:? "title"
            <*> object .:? "description"
            <*> object .:? "applies_when"
            <*> object .:? "instructions"
            <*> parseOptionalActivation object
            <*> object .:? "priority"
            <*> object .: "change_summary"
            <*> object .: "evidence"

instance FromJSON LearnedSkillArchiveRequest where
    parseJSON = withObject "LearnedSkillArchiveRequest" \object ->
        LearnedSkillArchiveRequest
            <$> object .: "scope"
            <*> object .: "slug"
            <*> object .: "expected_revision"
            <*> object .: "change_summary"
            <*> object .: "evidence"

instance FromJSON LearnedSkillRollbackRequest where
    parseJSON = withObject "LearnedSkillRollbackRequest" \object ->
        LearnedSkillRollbackRequest
            <$> object .: "scope"
            <*> object .: "slug"
            <*> object .: "expected_revision"
            <*> object .: "target_revision"
            <*> object .: "change_summary"
            <*> object .: "evidence"

learnedSkillTools :: IORef [SkillInvocation] -> LearnedSkillToolsEnv -> [AppTool]
learnedSkillTools invocationsRef env =
    [ searchTool env
    , viewTool invocationsRef env
    , createTool env
    , updateTool env
    , archiveTool env
    , rollbackTool env
    ]

searchTool :: LearnedSkillToolsEnv -> AppTool
searchTool env = jsonTool
    "skill_search"
    ( "Search active reusable skills learned from earlier sessions across the "
        <> "current user, repository, and checkout scopes. Search before "
        <> "creating a skill so you update an existing lesson instead of "
        <> "creating a duplicate. Results are summaries; use view_skill for "
        <> "the complete instructions and revision history."
    )
    [ PropertySchema "query" PropertyString True $ Just
        "Words describing the task, constraint, preference, or procedure."
    , PropertySchema "limit" PropertyInteger False $ Just
        "Maximum matches, from 1 to 50. Defaults to 10."
    ]
    True
    ParallelSafe
    (typedTool "skill_search" \(SearchArgs query limit) ->
        case validateSearch query limit of
            Left err -> pure (Left err)
            Right () -> encodeResult <$> env.learnedSkillSearch query limit)

viewTool :: IORef [SkillInvocation] -> LearnedSkillToolsEnv -> AppTool
viewTool invocationsRef env = jsonTool
    "view_skill"
    ( "Load one skill's complete instructions on demand. For filesystem skills, "
        <> "pass its catalog name without a scope. For learned skills, pass the "
        <> "slug as name and its user, repository, or checkout scope. Pass an "
        <> "earlier revision to inspect learned-skill history before rollback."
    )
    [ PropertySchema "name" PropertyString True $ Just
        "Filesystem skill name or learned-skill slug."
    , PropertySchema
        "scope"
        (PropertyEnum ["user", "repository", "checkout"])
        False
        (Just "Required for learned skills; omit for filesystem skills.")
    , PropertySchema "revision" PropertyInteger False $ Just
        "Learned-skill revision to inspect; omit for current."
    ]
    True
    ParallelSafe
    (typedTool "view_skill" \(ViewArgs name scope revision) ->
        case scope of
            Nothing ->
                case revision of
                    Just _ ->
                        pure
                            (Left
                                "revision requires a learned-skill scope")
                    Nothing -> do
                        invocations <- readIORef invocationsRef
                        pure $
                            encodeResult $
                                filesystemSkillValue
                                    <$> resolveSkillInvocation
                                        invocations
                                        (normalizeFilesystemSkillName name)
            Just selected ->
                case do
                    validateSlug name
                    maybe
                        (Right ())
                        (validatePositiveRevision "revision")
                        revision
                of
                    Left err -> pure (Left err)
                    Right () ->
                        encodeResult
                            <$> env.learnedSkillRead selected name revision)

filesystemSkillValue :: SkillInvocation -> Value
filesystemSkillValue invocation =
    let skill = invocation.invocationSkill
    in Aeson.object
        [ "kind" Aeson..= ("filesystem" :: Text)
        , "name" Aeson..= invocation.invocationName
        , "title" Aeson..= skill.skillDisplayName
        , "description" Aeson..= skill.skillDescription
        , "when_to_use" Aeson..= skill.skillWhenToUse
        , "instructions" Aeson..= skill.skillBody
        , "skill_file" Aeson..= toText skill.skillPath
        , "skill_directory" Aeson..= toText skill.skillDirectory
        ]

normalizeFilesystemSkillName :: Text -> Text
normalizeFilesystemSkillName raw =
    case Text.uncons (Text.strip raw) of
        Just (prefix, name)
            | prefix == '$' || prefix == '/' -> name
        _ -> Text.strip raw

createTool :: LearnedSkillToolsEnv -> AppTool
createTool env = jsonTool
    "skill_create"
    ( "Create a durable, versioned reusable skill from evidence in the current "
        <> "conversation. Promote only actionable behavior that will help in "
        <> "future sessions; ordinary facts remain in conversation history. "
        <> "Search first, prefer the narrowest correct scope, and use "
        <> "activation=always only for stable instructions that should be "
        <> "injected in every new applicable session. This mutating tool "
        <> "requires approval."
    )
    [ scopeProperty
    , slugProperty
    , PropertySchema "title" PropertyString True $ Just
        "Short human-readable skill title."
    , PropertySchema "description" PropertyString True $ Just
        "Concise summary used in search results and startup indexes."
    , PropertySchema "applies_when" PropertyString True $ Just
        "Conditions or task types for which the instructions are relevant."
    , PropertySchema "instructions" PropertyString True $ Just
        "Self-contained, actionable instructions for future agents."
    , activationProperty False
    , priorityProperty False
    , changeSummaryProperty
    , evidenceProperty
    ]
    False
    TurnSequential
    (typedTool "skill_create" \request ->
        case validateCreate request of
            Left err -> pure (Left err)
            Right () -> encodeResult <$> env.learnedSkillCreate request)

updateTool :: LearnedSkillToolsEnv -> AppTool
updateTool env = jsonTool
    "skill_update"
    ( "Create a new immutable revision of an existing learned skill. View the "
        <> "skill first and pass its current revision for optimistic "
        <> "concurrency. Supply only fields that should change, plus evidence "
        <> "and a change summary. Use skill_archive instead of rewriting a "
        <> "skill merely to disable it. This mutating tool requires approval."
    )
    [ scopeProperty
    , slugProperty
    , expectedRevisionProperty
    , PropertySchema "title" PropertyString False $ Just
        "Replacement title, or null to keep the current title."
    , PropertySchema "description" PropertyString False $ Just
        "Replacement description, or null to keep it."
    , PropertySchema "applies_when" PropertyString False $ Just
        "Replacement applicability guidance, or null to keep it."
    , PropertySchema "instructions" PropertyString False $ Just
        "Replacement actionable instructions, or null to keep them."
    , activationProperty False
    , priorityProperty False
    , changeSummaryProperty
    , evidenceProperty
    ]
    False
    TurnSequential
    (typedTool "skill_update" \request ->
        case validateUpdate request of
            Left err -> pure (Left err)
            Right () -> encodeResult <$> env.learnedSkillUpdate request)

archiveTool :: LearnedSkillToolsEnv -> AppTool
archiveTool env = jsonTool
    "skill_archive"
    ( "Archive an obsolete learned skill so it is no longer searched or "
        <> "loaded into future sessions. The immutable revision history is "
        <> "retained and can be restored with skill_rollback. This mutating "
        <> "tool requires approval."
    )
    [ scopeProperty
    , slugProperty
    , expectedRevisionProperty
    , changeSummaryProperty
    , evidenceProperty
    ]
    False
    TurnSequential
    (typedTool "skill_archive" \request ->
        case validateArchive request of
            Left err -> pure (Left err)
            Right () -> encodeResult <$> env.learnedSkillArchive request)

rollbackTool :: LearnedSkillToolsEnv -> AppTool
rollbackTool env = jsonTool
    "skill_rollback"
    ( "Restore the contents and status of an earlier learned-skill revision "
        <> "as a new immutable revision. View the skill history first and pass "
        <> "the current revision for optimistic concurrency. This mutating "
        <> "tool requires approval."
    )
    [ scopeProperty
    , slugProperty
    , expectedRevisionProperty
    , PropertySchema "target_revision" PropertyInteger True $ Just
        "Earlier revision number whose contents should be restored."
    , changeSummaryProperty
    , evidenceProperty
    ]
    False
    TurnSequential
    (typedTool "skill_rollback" \request ->
        case validateRollback request of
            Left err -> pure (Left err)
            Right () -> encodeResult <$> env.learnedSkillRollback request)

scopeProperty :: PropertySchema
scopeProperty = PropertySchema
    "scope"
    (PropertyEnum ["user", "repository", "checkout"])
    True
    (Just
        ( "Durability scope: user across projects, repository across clones "
            <> "and worktrees, or checkout only for this worktree."
        ))

slugProperty :: PropertySchema
slugProperty = PropertySchema
    "slug"
    PropertyString
    True
    (Just
        "Stable lowercase identifier using letters, digits, and single hyphens.")

activationProperty :: Bool -> PropertySchema
activationProperty required = PropertySchema
    "activation"
    (PropertyEnum ["always", "relevant", "manual"])
    required
    (Just
        ( "always injects full instructions into every new applicable session; "
            <> "relevant lists the skill for task-based retrieval and is the "
            <> "default; manual lists it for explicit retrieval only."
        ))

priorityProperty :: Bool -> PropertySchema
priorityProperty required = PropertySchema
    "priority"
    PropertyInteger
    required
    (Just "Ordering priority from -100 to 100. Defaults to 0.")

expectedRevisionProperty :: PropertySchema
expectedRevisionProperty = PropertySchema
    "expected_revision"
    PropertyInteger
    True
    (Just "Current revision returned by view_skill; prevents lost updates.")

changeSummaryProperty :: PropertySchema
changeSummaryProperty = PropertySchema
    "change_summary"
    PropertyString
    True
    (Just "Concise reason for this revision.")

evidenceProperty :: PropertySchema
evidenceProperty = PropertySchema
    "evidence"
    PropertyString
    True
    (Just
        ( "Concrete evidence from the conversation supporting this reusable "
            <> "lesson, decision, preference, or procedure."
        ))

parseOptionalActivation
    :: Aeson.Object
    -> Parser (Maybe LearnedSkillActivation)
parseOptionalActivation object = do
    value <- object .:? "activation" :: Parser (Maybe Text)
    traverse parseActivation value
  where
    parseActivation :: Text -> Parser LearnedSkillActivation
    parseActivation = \case
        "always" -> pure SkillAlways
        "relevant" -> pure SkillRelevant
        "manual" -> pure SkillManual
        value ->
            fail
                ("unknown skill activation "
                    <> show value
                    <> "; expected always, relevant, or manual")

validateSearch :: Text -> Int -> Either Text ()
validateSearch query limit = do
    validateRequiredText "skill search query" 2000 query
    if limit < 1 || limit > 50
        then Left "skill search limit must be between 1 and 50"
        else Right ()

validateCreate :: LearnedSkillCreateRequest -> Either Text ()
validateCreate request = do
    validateSlug request.createRequestSlug
    validateRequiredText "skill title" 200 request.createRequestTitle
    validateRequiredText
        "skill description"
        1000
        request.createRequestDescription
    validateRequiredText
        "skill applies_when"
        2000
        request.createRequestAppliesWhen
    validateRequiredText
        "skill instructions"
        30000
        request.createRequestInstructions
    validatePriority request.createRequestPriority
    validateRevisionMetadata
        request.createRequestChangeSummary
        request.createRequestEvidence

validateUpdate :: LearnedSkillUpdateRequest -> Either Text ()
validateUpdate request = do
    validateSlug request.updateRequestSlug
    validatePositiveRevision
        "expected_revision"
        request.updateRequestExpectedRevision
    validateOptionalText "skill title" 200 request.updateRequestTitle
    validateOptionalText
        "skill description"
        1000
        request.updateRequestDescription
    validateOptionalText
        "skill applies_when"
        2000
        request.updateRequestAppliesWhen
    validateOptionalText
        "skill instructions"
        30000
        request.updateRequestInstructions
    maybe (Right ()) validatePriority request.updateRequestPriority
    if or
        [ isJust request.updateRequestTitle
        , isJust request.updateRequestDescription
        , isJust request.updateRequestAppliesWhen
        , isJust request.updateRequestInstructions
        , isJust request.updateRequestActivation
        , isJust request.updateRequestPriority
        ]
        then Right ()
        else Left "skill update must change at least one field"
    validateRevisionMetadata
        request.updateRequestChangeSummary
        request.updateRequestEvidence

validateArchive :: LearnedSkillArchiveRequest -> Either Text ()
validateArchive request = do
    validateSlug request.archiveRequestSlug
    validatePositiveRevision
        "expected_revision"
        request.archiveRequestExpectedRevision
    validateRevisionMetadata
        request.archiveRequestChangeSummary
        request.archiveRequestEvidence

validateRollback :: LearnedSkillRollbackRequest -> Either Text ()
validateRollback request = do
    validateSlug request.rollbackRequestSlug
    validatePositiveRevision
        "expected_revision"
        request.rollbackRequestExpectedRevision
    validatePositiveRevision
        "target_revision"
        request.rollbackRequestTargetRevision
    if request.rollbackRequestTargetRevision
        >= request.rollbackRequestExpectedRevision
        then
            Left
                "target_revision must be earlier than expected_revision"
        else Right ()
    validateRevisionMetadata
        request.rollbackRequestChangeSummary
        request.rollbackRequestEvidence

validateSlug :: Text -> Either Text ()
validateSlug slug
    | Text.null slug =
        Left "skill slug must not be empty"
    | Text.length slug > 80 =
        Left "skill slug must be at most 80 characters"
    | all validSegment (Text.splitOn "-" slug) =
        Right ()
    | otherwise =
        Left
            "skill slug must use lowercase ASCII letters, digits, and single hyphens"
  where
    validSegment segment =
        not (Text.null segment) && Text.all validCharacter segment
    validCharacter character =
        (character >= 'a' && character <= 'z')
            || (character >= '0' && character <= '9')

validateRequiredText :: Text -> Int -> Text -> Either Text ()
validateRequiredText label maximum value
    | Text.null (Text.strip value) =
        Left (label <> " must not be empty")
    | Text.length value > maximum =
        Left
            (label
                <> " must be at most "
                <> Text.pack (show maximum)
                <> " characters")
    | otherwise = Right ()

validateOptionalText :: Text -> Int -> Maybe Text -> Either Text ()
validateOptionalText label maximum =
    maybe (Right ()) (validateRequiredText label maximum)

validatePriority :: Int -> Either Text ()
validatePriority priority
    | priority < -100 || priority > 100 =
        Left "skill priority must be between -100 and 100"
    | otherwise = Right ()

validatePositiveRevision :: Text -> Integer -> Either Text ()
validatePositiveRevision label revision
    | revision < 1 = Left (label <> " must be at least 1")
    | revision > fromIntegral (maxBound :: Int64) =
        Left (label <> " is too large")
    | otherwise = Right ()

validateRevisionMetadata :: Text -> Text -> Either Text ()
validateRevisionMetadata summary evidence = do
    validateRequiredText "skill change_summary" 2000 summary
    validateRequiredText "skill evidence" 8000 evidence

encodeResult :: Either Text Value -> Either Text Text
encodeResult =
    fmap (Text.decodeUtf8 . LBS.toStrict . Aeson.encode)

defaultLearnedSkillContextMaxChars :: Int
defaultLearnedSkillContextMaxChars = 8000

formatLearnedSkillContext
    :: Int
    -> [LearnedSkill]
    -> (Maybe Text, Int)
formatLearnedSkillContext maximum skills
    | null active = (Nothing, 0)
    | available <= 0 = (Nothing, length active)
    | null selected = (Nothing, omitted)
    | otherwise =
        ( Just
            (contextHeader
                <> Text.intercalate "\n\n" selected
                <> contextFooter)
        , omitted
        )
  where
    active =
        sortOn learnedSkillSortKey
            (Map.elems $
                Map.fromListWith preferNarrowerScope
                    [ (skill.learnedSkillSlug, skill)
                    | skill <- skills
                    , skill.learnedSkillStatus == SkillActive
                    ])
    available =
        maximum - Text.length contextHeader - Text.length contextFooter
    (selected, _, omitted) =
        foldl selectEntry ([], 0, 0) (map renderLearnedSkill active)
    selectEntry (entries, used, skipped) entry =
        let separatorLength = if null entries then 0 else 2
            additional = separatorLength + Text.length entry
        in if used + additional <= available
            then (entries <> [entry], used + additional, skipped)
            else (entries, used, skipped + 1)

queueLearnedSkillContextWithOmissions
    :: Int
    -> IORef (Maybe Text)
    -> [LearnedSkill]
    -> IO Int
queueLearnedSkillContextWithOmissions maximum contextRef skills =
    case formatLearnedSkillContext
        maximum
        skills of
        (Nothing, omitted) -> pure omitted
        (Just context, omitted) -> do
            modifyIORef' contextRef \current ->
                Just $ case current of
                    Nothing -> context
                    Just existing -> existing <> "\n\n" <> context
            pure omitted

contextHeader :: Text
contextHeader =
    "<learned-skills>\n\
    \These are durable, reusable instructions learned from earlier sessions. \
    \Apply every skill marked `always`. For `relevant` skills, call view_skill \
    \with its name and scope when its applicability matches the task. Apply \
    \`manual` skills only when explicitly requested or intentionally selected. \
    \When instructions conflict, checkout scope overrides repository scope, \
    \which overrides user scope.\n\n"

contextFooter :: Text
contextFooter = "\n</learned-skills>"

learnedSkillSortKey :: LearnedSkill -> (Int, Down Int, Int, Text)
learnedSkillSortKey skill =
    ( activationOrder skill.learnedSkillActivation
    , Down (fromIntegral skill.learnedSkillPriority)
    , learnedSkillScopeOrder skill.learnedSkillScope.scopeKind
    , skill.learnedSkillSlug
    )
  where
    activationOrder = \case
        SkillAlways -> 0
        SkillRelevant -> 1
        SkillManual -> 2

preferNarrowerScope :: LearnedSkill -> LearnedSkill -> LearnedSkill
preferNarrowerScope left right
    | learnedSkillScopeOrder left.learnedSkillScope.scopeKind
        <= learnedSkillScopeOrder right.learnedSkillScope.scopeKind =
            left
    | otherwise = right

learnedSkillScopeOrder :: ScopeKind -> Int
learnedSkillScopeOrder = \case
    CheckoutScope -> 0
    RepositoryScope -> 1
    UserScope -> 2

renderLearnedSkill :: LearnedSkill -> Text
renderLearnedSkill skill =
    case skill.learnedSkillActivation of
        SkillAlways ->
            "## "
                <> identity
                <> "\nTitle: "
                <> oneLine skill.learnedSkillTitle
                <> "\nDescription: "
                <> oneLine skill.learnedSkillDescription
                <> "\nApplies when: "
                <> oneLine skill.learnedSkillAppliesWhen
                <> "\nInstructions:\n"
                <> Text.strip skill.learnedSkillInstructions
        activation ->
            "- "
                <> identity
                <> " ["
                <> learnedSkillActivationText activation
                <> "]: "
                <> oneLine skill.learnedSkillTitle
                <> " — "
                <> oneLine skill.learnedSkillDescription
                <> " Applies when: "
                <> oneLine skill.learnedSkillAppliesWhen
  where
    identity =
        "["
            <> scopeKindText skill.learnedSkillScope.scopeKind
            <> "] "
            <> skill.learnedSkillSlug
            <> " (revision "
            <> Text.pack (show skill.learnedSkillRevision)
            <> ", priority "
            <> Text.pack (show skill.learnedSkillPriority)
            <> ")"

oneLine :: Text -> Text
oneLine = Text.unwords . Text.words
