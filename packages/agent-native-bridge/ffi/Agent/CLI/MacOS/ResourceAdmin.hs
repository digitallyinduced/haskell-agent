{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Agent.CLI.MacOS.ResourceAdmin () where

import Agent.CLI.Database.Store
    ( DatabaseScopes
    , deriveDatabaseScopes
    )
import Agent.CLI.Project (resolveProjectRoot)
import Agent.CLI.ResourceAdmin
    ( ResourceAdminError(..)
    , ResourceScope(..)
    , ResourceSkill(..)
    , ResourceSkillDraft(..)
    , ResourceSkillRevision(..)
    , archiveResourceSkill
    , createResourceSkill
    , historyResourceSkill
    , listResourceSkills
    , readResourceSkill
    , restoreResourceSkill
    , rollbackResourceSkill
    , updateResourceSkill
    , validateResourceSlug
    , validateResourceSummary
    , validateResourceSkillDraft
    )
import Agent.CLI.Session (sessionsRoot)
import Agent.CLI.SessionAdmin (managedPostgresConfigForHome)
import Agent.Store.Postgres
    ( Store
    , closeStore
    , openStore
    )
import Agent.Store.Postgres.Skill
    ( LearnedSkillActivation(..) )
import Control.Exception.Safe (bracket, tryAny)
import Control.Monad (forM_)
import qualified Data.ByteString as BS
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Word (Word8, Word64)
import Foreign
    ( FunPtr
    , Ptr
    , castPtr
    , nullFunPtr
    , nullPtr
    )
import Foreign.C.String (CString)
import Foreign.C.Types (CInt(..), CLLong(..), CSize(..))
import System.Directory.OsPath (getHomeDirectory)
import System.OsPath
    ( decodeFS
    , takeDirectory
    , unsafeEncodeUtf
    )
import Control.Concurrent (forkIO)

-- status: 0 item/success, 1 list completion, -1 generic error, -2 not found,
-- -3 revision conflict, -4 already exists, -5 revision not found.
type ResourceSkillCallback =
    Ptr () -> CInt -> CInt
    -> CString -> CSize -> CLLong
    -> CString -> CSize -> CString -> CSize -> CString -> CSize
    -> CString -> CSize -> CInt -> CInt -> CInt
    -> CLLong -> CLLong -> CString -> CSize -> IO ()

type ResourceRevisionCallback =
    Ptr () -> CInt -> CLLong
    -> CString -> CSize -> CString -> CSize -> CString -> CSize
    -> CString -> CSize -> CInt -> CInt -> CInt
    -> CString -> CSize -> CLLong -> CString -> CSize -> IO ()

type ResourceResultCallback =
    Ptr () -> CInt -> CLLong -> CString -> CSize -> IO ()

foreign import ccall "dynamic"
    invokeResourceSkillCallback
        :: FunPtr ResourceSkillCallback -> ResourceSkillCallback

foreign import ccall "dynamic"
    invokeResourceRevisionCallback
        :: FunPtr ResourceRevisionCallback -> ResourceRevisionCallback

foreign import ccall "dynamic"
    invokeResourceResultCallback
        :: FunPtr ResourceResultCallback -> ResourceResultCallback

foreign export ccall ha_learned_skill_list
    :: Ptr Word8 -> CSize -> CInt -> CSize
    -> FunPtr ResourceSkillCallback -> Ptr () -> IO CInt

foreign export ccall ha_learned_skill_read
    :: Ptr Word8 -> CSize -> CInt -> Ptr Word8 -> CSize -> Word64
    -> FunPtr ResourceSkillCallback -> Ptr () -> IO CInt

foreign export ccall ha_learned_skill_create
    :: Ptr Word8 -> CSize -> CInt
    -> Ptr Word8 -> CSize -> Ptr Word8 -> CSize
    -> Ptr Word8 -> CSize -> Ptr Word8 -> CSize -> Ptr Word8 -> CSize
    -> CInt -> CInt -> Ptr Word8 -> CSize
    -> FunPtr ResourceResultCallback -> Ptr () -> IO CInt

foreign export ccall ha_learned_skill_update
    :: Ptr Word8 -> CSize -> CInt -> Ptr Word8 -> CSize -> Word64
    -> Ptr Word8 -> CSize -> Ptr Word8 -> CSize
    -> Ptr Word8 -> CSize -> Ptr Word8 -> CSize
    -> CInt -> CInt -> Ptr Word8 -> CSize
    -> FunPtr ResourceResultCallback -> Ptr () -> IO CInt

foreign export ccall ha_learned_skill_archive
    :: Ptr Word8 -> CSize -> CInt -> Ptr Word8 -> CSize -> Word64
    -> Ptr Word8 -> CSize
    -> FunPtr ResourceResultCallback -> Ptr () -> IO CInt

foreign export ccall ha_learned_skill_restore
    :: Ptr Word8 -> CSize -> CInt -> Ptr Word8 -> CSize -> Word64
    -> Ptr Word8 -> CSize
    -> FunPtr ResourceResultCallback -> Ptr () -> IO CInt

foreign export ccall ha_learned_skill_rollback
    :: Ptr Word8 -> CSize -> CInt -> Ptr Word8 -> CSize -> Word64 -> Word64
    -> Ptr Word8 -> CSize
    -> FunPtr ResourceResultCallback -> Ptr () -> IO CInt

foreign export ccall ha_learned_skill_history
    :: Ptr Word8 -> CSize -> CInt -> Ptr Word8 -> CSize -> CSize
    -> FunPtr ResourceRevisionCallback -> Ptr () -> IO CInt

ha_learned_skill_list
    :: Ptr Word8 -> CSize -> CInt -> CSize
    -> FunPtr ResourceSkillCallback -> Ptr () -> IO CInt
ha_learned_skill_list cwdPointer cwdLength rawScope rawLimit
        callback context
    | callback == nullFunPtr = pure 1
    | rawLimit < 1 || rawLimit > 1000 = pure 2
    | otherwise =
        case parseOptionalScope rawScope of
            Nothing -> pure 2
            Just selected ->
                decodeRequired "cwd" maxPathBytes cwdPointer cwdLength >>= \case
                    Left _ -> pure 2
                    Right cwd -> do
                        runResourceAsync
                            (withResourceStore cwd \store scopes ->
                                listResourceSkills store scopes selected
                                    (fromIntegral rawLimit))
                            (emitSkillList callback context)
                        pure 0

ha_learned_skill_read
    :: Ptr Word8 -> CSize -> CInt -> Ptr Word8 -> CSize -> Word64
    -> FunPtr ResourceSkillCallback -> Ptr () -> IO CInt
ha_learned_skill_read cwdPointer cwdLength rawScope
        slugPointer slugLength rawRevision callback context
    | callback == nullFunPtr = pure 1
    | otherwise =
        case parseScope rawScope of
            Nothing -> pure 2
            Just scope ->
                decodeCommon cwdPointer cwdLength slugPointer slugLength
                    >>= \case
                        Left _ -> pure 2
                        Right (cwd, slug) ->
                            case ( validateResourceSlug slug
                                 , word64Revision rawRevision
                                 ) of
                                (Left _, _) -> pure 2
                                (_, Left _) -> pure 2
                                (Right _, Right revision) -> do
                                    runResourceAsync
                                        (withResourceStore cwd \store scopes ->
                                            readResourceSkill store scopes scope
                                                slug revision)
                                        (emitSkillOne callback context)
                                    pure 0

ha_learned_skill_create
    :: Ptr Word8 -> CSize -> CInt
    -> Ptr Word8 -> CSize -> Ptr Word8 -> CSize
    -> Ptr Word8 -> CSize -> Ptr Word8 -> CSize -> Ptr Word8 -> CSize
    -> CInt -> CInt -> Ptr Word8 -> CSize
    -> FunPtr ResourceResultCallback -> Ptr () -> IO CInt
ha_learned_skill_create cwdPointer cwdLength rawScope
        slugPointer slugLength titlePointer titleLength
        descriptionPointer descriptionLength
        appliesPointer appliesLength
        instructionsPointer instructionsLength
        rawActivation (CInt priority)
        summaryPointer summaryLength callback context
    | callback == nullFunPtr = pure 1
    | otherwise =
        case (parseScope rawScope, parseActivation rawActivation) of
            (Just scope, Just activation) ->
                decodeDraft
                    cwdPointer cwdLength slugPointer slugLength
                    titlePointer titleLength descriptionPointer descriptionLength
                    appliesPointer appliesLength
                    instructionsPointer instructionsLength
                    summaryPointer summaryLength
                    >>= \case
                        Left _ -> pure 2
                        Right (cwd, slug, title, description, applies,
                                instructions, summary) -> do
                            let draft = ResourceSkillDraft
                                    { resourceDraftTitle = title
                                    , resourceDraftDescription = description
                                    , resourceDraftAppliesWhen = applies
                                    , resourceDraftInstructions = instructions
                                    , resourceDraftActivation = activation
                                    , resourceDraftPriority = priority
                                    }
                            case validatePayload slug draft summary of
                                Left _ -> pure 2
                                Right () -> do
                                    runResourceAsync
                                        (withResourceStore cwd \store scopes ->
                                            createResourceSkill store scopes
                                                scope slug draft summary)
                                        (emitMutation callback context)
                                    pure 0
            _ -> pure 2

ha_learned_skill_update
    :: Ptr Word8 -> CSize -> CInt -> Ptr Word8 -> CSize -> Word64
    -> Ptr Word8 -> CSize -> Ptr Word8 -> CSize
    -> Ptr Word8 -> CSize -> Ptr Word8 -> CSize
    -> CInt -> CInt -> Ptr Word8 -> CSize
    -> FunPtr ResourceResultCallback -> Ptr () -> IO CInt
ha_learned_skill_update cwdPointer cwdLength rawScope
        slugPointer slugLength rawExpected
        titlePointer titleLength
        descriptionPointer descriptionLength
        appliesPointer appliesLength
        instructionsPointer instructionsLength
        rawActivation (CInt priority)
        summaryPointer summaryLength callback context
    | callback == nullFunPtr = pure 1
    | otherwise =
        case
            ( parseScope rawScope
            , parseActivation rawActivation
            , positiveWord64Revision rawExpected
            )
          of
            (Just scope, Just activation, Right expected) ->
                decodeDraft
                    cwdPointer cwdLength slugPointer slugLength
                    titlePointer titleLength descriptionPointer descriptionLength
                    appliesPointer appliesLength
                    instructionsPointer instructionsLength
                    summaryPointer summaryLength
                    >>= \case
                        Left _ -> pure 2
                        Right (cwd, slug, title, description, applies,
                                instructions, summary) -> do
                            let draft = ResourceSkillDraft
                                    { resourceDraftTitle = title
                                    , resourceDraftDescription = description
                                    , resourceDraftAppliesWhen = applies
                                    , resourceDraftInstructions = instructions
                                    , resourceDraftActivation = activation
                                    , resourceDraftPriority = priority
                                    }
                            case validatePayload slug draft summary of
                                Left _ -> pure 2
                                Right () -> do
                                    runResourceAsync
                                        (withResourceStore cwd \store scopes ->
                                            updateResourceSkill store scopes
                                                scope slug expected draft summary)
                                        (emitMutation callback context)
                                    pure 0
            _ -> pure 2

ha_learned_skill_archive
    :: Ptr Word8 -> CSize -> CInt -> Ptr Word8 -> CSize -> Word64
    -> Ptr Word8 -> CSize
    -> FunPtr ResourceResultCallback -> Ptr () -> IO CInt
ha_learned_skill_archive =
    statusMutation archiveResourceSkill

ha_learned_skill_restore
    :: Ptr Word8 -> CSize -> CInt -> Ptr Word8 -> CSize -> Word64
    -> Ptr Word8 -> CSize
    -> FunPtr ResourceResultCallback -> Ptr () -> IO CInt
ha_learned_skill_restore =
    statusMutation restoreResourceSkill

ha_learned_skill_rollback
    :: Ptr Word8 -> CSize -> CInt -> Ptr Word8 -> CSize -> Word64 -> Word64
    -> Ptr Word8 -> CSize
    -> FunPtr ResourceResultCallback -> Ptr () -> IO CInt
ha_learned_skill_rollback cwdPointer cwdLength rawScope
        slugPointer slugLength rawExpected rawTarget
        summaryPointer summaryLength callback context
    | callback == nullFunPtr = pure 1
    | otherwise =
        case
            ( parseScope rawScope
            , positiveWord64Revision rawExpected
            , positiveWord64Revision rawTarget
            )
          of
            (Just scope, Right expected, Right target) ->
                decodeMutation
                    cwdPointer cwdLength slugPointer slugLength
                    summaryPointer summaryLength
                    >>= \case
                        Left _ -> pure 2
                        Right (cwd, slug, summary)
                            | target >= expected -> pure 2
                            | otherwise ->
                                case validateMutationPayload slug summary of
                                    Left _ -> pure 2
                                    Right () -> do
                                        runResourceAsync
                                            (withResourceStore cwd \store scopes ->
                                                rollbackResourceSkill store
                                                    scopes scope slug expected
                                                    target summary)
                                            (emitMutation callback context)
                                        pure 0
            _ -> pure 2

ha_learned_skill_history
    :: Ptr Word8 -> CSize -> CInt -> Ptr Word8 -> CSize -> CSize
    -> FunPtr ResourceRevisionCallback -> Ptr () -> IO CInt
ha_learned_skill_history cwdPointer cwdLength rawScope
        slugPointer slugLength rawLimit callback context
    | callback == nullFunPtr = pure 1
    | rawLimit < 1 || rawLimit > 1000 = pure 2
    | otherwise =
        case parseScope rawScope of
            Nothing -> pure 2
            Just scope ->
                decodeCommon cwdPointer cwdLength slugPointer slugLength
                    >>= \case
                        Left _ -> pure 2
                        Right (cwd, slug) ->
                            case validateResourceSlug slug of
                                Left _ -> pure 2
                                Right _ -> do
                                    runResourceAsync
                                        (withResourceStore cwd \store scopes ->
                                            historyResourceSkill store scopes
                                                scope slug
                                                (fromIntegral rawLimit))
                                        (emitRevisionList callback context)
                                    pure 0

statusMutation
    :: (Store -> DatabaseScopes -> ResourceScope -> Text -> Int64 -> Text
        -> IO (Either ResourceAdminError ResourceSkill))
    -> Ptr Word8 -> CSize -> CInt -> Ptr Word8 -> CSize -> Word64
    -> Ptr Word8 -> CSize
    -> FunPtr ResourceResultCallback -> Ptr () -> IO CInt
statusMutation mutation cwdPointer cwdLength rawScope
        slugPointer slugLength rawExpected
        summaryPointer summaryLength callback context
    | callback == nullFunPtr = pure 1
    | otherwise =
        case (parseScope rawScope, positiveWord64Revision rawExpected) of
            (Just scope, Right expected) ->
                decodeMutation
                    cwdPointer cwdLength slugPointer slugLength
                    summaryPointer summaryLength
                    >>= \case
                        Left _ -> pure 2
                        Right (cwd, slug, summary) ->
                            case validateMutationPayload slug summary of
                                Left _ -> pure 2
                                Right () -> do
                                    runResourceAsync
                                        (withResourceStore cwd \store scopes ->
                                            mutation store scopes scope slug
                                                expected summary)
                                        (emitMutation callback context)
                                    pure 0
            _ -> pure 2

-- Store setup errors and unexpected exceptions intentionally collapse to a
-- fixed error. PostgreSQL connection details and process environments must not
-- cross the public bridge.
withResourceStore
    :: Text
    -> (Store -> DatabaseScopes
        -> IO (Either ResourceAdminError value))
    -> IO (Either ResourceAdminError value)
withResourceStore cwd action = do
    home <- getHomeDirectory
    projectRoot <- resolveProjectRoot (unsafeEncodeUtf (Text.unpack cwd))
    stateDirectory <- decodeFS (takeDirectory (sessionsRoot home))
    projectRootPath <- decodeFS projectRoot
    deriveDatabaseScopes stateDirectory projectRootPath >>= \case
        Left _ -> pure (Left ResourceAdminUnavailable)
        Right scopes -> do
            config <- managedPostgresConfigForHome home
            openStore config >>= \case
                Left _ -> pure (Left ResourceAdminUnavailable)
                Right opened ->
                    bracket (pure opened) closeStore (`action` scopes)

runResourceAsync
    :: IO (Either ResourceAdminError value)
    -> (Either ResourceAdminError value -> IO ())
    -> IO ()
runResourceAsync action emit = do
    _ <- forkIO $
        tryAny action >>= \case
            Left _ -> emit (Left ResourceAdminUnavailable)
            Right result -> emit result
    pure ()

emitSkillList
    :: FunPtr ResourceSkillCallback
    -> Ptr ()
    -> Either ResourceAdminError [ResourceSkill]
    -> IO ()
emitSkillList callback context = \case
    Left err -> emitSkillError callback context err
    Right skills -> do
        forM_ skills (emitSkillItem callback context)
        invokeResourceSkillCallback callback context 1 0
            nullPtr 0 0
            nullPtr 0 nullPtr 0 nullPtr 0 nullPtr 0
            0 0 0 0 0 nullPtr 0

emitSkillOne
    :: FunPtr ResourceSkillCallback
    -> Ptr ()
    -> Either ResourceAdminError ResourceSkill
    -> IO ()
emitSkillOne callback context =
    either (emitSkillError callback context)
        (emitSkillItem callback context)

emitSkillItem
    :: FunPtr ResourceSkillCallback -> Ptr () -> ResourceSkill -> IO ()
emitSkillItem callback context skill =
    withText skill.resourceSkillSlug \slug slugLength ->
    withText skill.resourceSkillTitle \title titleLength ->
    withText skill.resourceSkillDescription \description descriptionLength ->
    withText skill.resourceSkillAppliesWhen \applies appliesLength ->
    withText skill.resourceSkillInstructions \instructions instructionsLength ->
        invokeResourceSkillCallback callback context 0
            (scopeCode skill.resourceSkillScope)
            slug slugLength
            (fromIntegral skill.resourceSkillRevision)
            title titleLength description descriptionLength
            applies appliesLength instructions instructionsLength
            (activationCode skill.resourceSkillActivation)
            (fromIntegral skill.resourceSkillPriority)
            (if skill.resourceSkillArchived then 1 else 0)
            (timestampMillis skill.resourceSkillCreatedAt)
            (timestampMillis skill.resourceSkillUpdatedAt)
            nullPtr 0

emitSkillError
    :: FunPtr ResourceSkillCallback
    -> Ptr ()
    -> ResourceAdminError
    -> IO ()
emitSkillError callback context err =
    withText (errorMessage err) \pointer length ->
        invokeResourceSkillCallback callback context (errorStatus err) 0
            nullPtr 0 (conflictRevision err)
            nullPtr 0 nullPtr 0 nullPtr 0 nullPtr 0
            0 0 0 0 0 pointer length

emitRevisionList
    :: FunPtr ResourceRevisionCallback
    -> Ptr ()
    -> Either ResourceAdminError [ResourceSkillRevision]
    -> IO ()
emitRevisionList callback context = \case
    Left err -> emitRevisionError callback context err
    Right revisions -> do
        forM_ revisions (emitRevisionItem callback context)
        invokeResourceRevisionCallback callback context 1 0
            nullPtr 0 nullPtr 0 nullPtr 0 nullPtr 0
            0 0 0 nullPtr 0 0 nullPtr 0

emitRevisionItem
    :: FunPtr ResourceRevisionCallback
    -> Ptr ()
    -> ResourceSkillRevision
    -> IO ()
emitRevisionItem callback context revision =
    withText revision.resourceRevisionTitle \title titleLength ->
    withText revision.resourceRevisionDescription \description descriptionLength ->
    withText revision.resourceRevisionAppliesWhen \applies appliesLength ->
    withText revision.resourceRevisionInstructions \instructions instructionsLength ->
    withText revision.resourceRevisionSummary \summary summaryLength ->
        invokeResourceRevisionCallback callback context 0
            (fromIntegral revision.resourceRevisionNumber)
            title titleLength description descriptionLength
            applies appliesLength instructions instructionsLength
            (activationCode revision.resourceRevisionActivation)
            (fromIntegral revision.resourceRevisionPriority)
            (if revision.resourceRevisionArchived then 1 else 0)
            summary summaryLength
            (timestampMillis revision.resourceRevisionCreatedAt)
            nullPtr 0

emitRevisionError
    :: FunPtr ResourceRevisionCallback
    -> Ptr ()
    -> ResourceAdminError
    -> IO ()
emitRevisionError callback context err =
    withText (errorMessage err) \pointer length ->
        invokeResourceRevisionCallback callback context (errorStatus err)
            (conflictRevision err)
            nullPtr 0 nullPtr 0 nullPtr 0 nullPtr 0
            0 0 0 nullPtr 0 0 pointer length

emitMutation
    :: FunPtr ResourceResultCallback
    -> Ptr ()
    -> Either ResourceAdminError ResourceSkill
    -> IO ()
emitMutation callback context = \case
    Right skill ->
        invokeResourceResultCallback callback context 0
            (fromIntegral skill.resourceSkillRevision) nullPtr 0
    Left err -> withText (errorMessage err) \pointer length ->
        invokeResourceResultCallback callback context (errorStatus err)
            (conflictRevision err) pointer length

decodeDraft
    :: Ptr Word8 -> CSize -> Ptr Word8 -> CSize
    -> Ptr Word8 -> CSize -> Ptr Word8 -> CSize
    -> Ptr Word8 -> CSize -> Ptr Word8 -> CSize
    -> Ptr Word8 -> CSize
    -> IO (Either Text (Text, Text, Text, Text, Text, Text, Text))
decodeDraft cwdPointer cwdLength slugPointer slugLength
        titlePointer titleLength descriptionPointer descriptionLength
        appliesPointer appliesLength instructionsPointer instructionsLength
        summaryPointer summaryLength =
    sequence7
        <$> decodeRequired "cwd" maxPathBytes cwdPointer cwdLength
        <*> decodeRequired "slug" 320 slugPointer slugLength
        <*> decodeRequired "title" 800 titlePointer titleLength
        <*> decodeRequired "description" 4000
            descriptionPointer descriptionLength
        <*> decodeOptional "applies_when" 8000 appliesPointer appliesLength
        <*> decodeRequired "instructions" 120000
            instructionsPointer instructionsLength
        <*> decodeRequired "change_summary" 4000 summaryPointer summaryLength

decodeMutation
    :: Ptr Word8 -> CSize -> Ptr Word8 -> CSize -> Ptr Word8 -> CSize
    -> IO (Either Text (Text, Text, Text))
decodeMutation cwdPointer cwdLength slugPointer slugLength
        summaryPointer summaryLength =
    sequence3
        <$> decodeRequired "cwd" maxPathBytes cwdPointer cwdLength
        <*> decodeRequired "slug" 320 slugPointer slugLength
        <*> decodeRequired "change_summary" 4000 summaryPointer summaryLength

decodeCommon
    :: Ptr Word8 -> CSize -> Ptr Word8 -> CSize
    -> IO (Either Text (Text, Text))
decodeCommon cwdPointer cwdLength slugPointer slugLength =
    sequence2
        <$> decodeRequired "cwd" maxPathBytes cwdPointer cwdLength
        <*> decodeRequired "slug" 320 slugPointer slugLength

decodeRequired
    :: Text -> CSize -> Ptr Word8 -> CSize -> IO (Either Text Text)
decodeRequired label maximum pointer length
    | pointer == nullPtr || length == 0 =
        pure (Left (label <> " must not be empty"))
    | otherwise = decodeOptional label maximum pointer length

decodeOptional
    :: Text -> CSize -> Ptr Word8 -> CSize -> IO (Either Text Text)
decodeOptional label maximum pointer length
    | length > maximum || toInteger length > toInteger (maxBound :: Int) =
        pure (Left (label <> " is too large"))
    | pointer == nullPtr && length > 0 =
        pure (Left (label <> " pointer is null"))
    | length == 0 = pure (Right "")
    | otherwise = do
        bytes <- BS.packCStringLen (castPtr pointer, fromIntegral length)
        pure $ case TextEncoding.decodeUtf8' bytes of
            Left _ -> Left (label <> " is not valid UTF-8")
            Right value
                | Text.any (== '\NUL') value ->
                    Left (label <> " contains NUL")
                | otherwise -> Right value

parseScope :: CInt -> Maybe ResourceScope
parseScope = \case
    0 -> Just ResourceUserScope
    1 -> Just ResourceRepositoryScope
    2 -> Just ResourceCheckoutScope
    _ -> Nothing

parseOptionalScope :: CInt -> Maybe (Maybe ResourceScope)
parseOptionalScope (-1) = Just Nothing
parseOptionalScope value = Just <$> parseScope value

parseActivation :: CInt -> Maybe LearnedSkillActivation
parseActivation = \case
    0 -> Just SkillAlways
    1 -> Just SkillRelevant
    2 -> Just SkillManual
    _ -> Nothing

scopeCode :: ResourceScope -> CInt
scopeCode = \case
    ResourceUserScope -> 0
    ResourceRepositoryScope -> 1
    ResourceCheckoutScope -> 2

activationCode :: LearnedSkillActivation -> CInt
activationCode = \case
    SkillAlways -> 0
    SkillRelevant -> 1
    SkillManual -> 2

word64Revision :: Word64 -> Either Text (Maybe Int64)
word64Revision 0 = Right Nothing
word64Revision value = Just <$> positiveWord64Revision value

positiveWord64Revision :: Word64 -> Either Text Int64
positiveWord64Revision value
    | value == 0 || value > fromIntegral (maxBound :: Int64) =
        Left "revision is out of range"
    | otherwise = Right (fromIntegral value)

timestampMillis :: UTCTime -> CLLong
timestampMillis =
    fromIntegral . (floor :: Rational -> Int64) . (* 1000)
        . toRational . utcTimeToPOSIXSeconds

errorStatus :: ResourceAdminError -> CInt
errorStatus = \case
    ResourceAdminNotFound -> -2
    ResourceAdminConflict _ -> -3
    ResourceAdminAlreadyExists -> -4
    ResourceAdminRevisionNotFound -> -5
    _ -> -1

conflictRevision :: ResourceAdminError -> CLLong
conflictRevision = \case
    ResourceAdminConflict revision -> fromIntegral revision
    _ -> 0

errorMessage :: ResourceAdminError -> Text
errorMessage = \case
    ResourceAdminInvalid message -> message
    ResourceAdminNotFound -> "resource not found"
    ResourceAdminAlreadyExists -> "resource already exists"
    ResourceAdminConflict _ -> "resource revision conflict"
    ResourceAdminRevisionNotFound -> "resource revision not found"
    ResourceAdminUnavailable -> "resource store unavailable"

withText :: Text -> (CString -> CSize -> IO value) -> IO value
withText value action =
    BS.useAsCStringLen (TextEncoding.encodeUtf8 value) \(pointer, length) ->
        action pointer (fromIntegral length)

maxPathBytes :: CSize
maxPathBytes = 1024 * 1024

validatePayload
    :: Text
    -> ResourceSkillDraft
    -> Text
    -> Either ResourceAdminError ()
validatePayload slug draft summary = do
    _ <- validateResourceSlug slug
    _ <- validateResourceSkillDraft draft
    _ <- validateResourceSummary summary
    pure ()

validateMutationPayload
    :: Text -> Text -> Either ResourceAdminError ()
validateMutationPayload slug summary = do
    _ <- validateResourceSlug slug
    _ <- validateResourceSummary summary
    pure ()

sequence2
    :: Either error a -> Either error b -> Either error (a, b)
sequence2 left right = (,) <$> left <*> right

sequence3
    :: Either error a -> Either error b -> Either error c
    -> Either error (a, b, c)
sequence3 one two three = (,,) <$> one <*> two <*> three

sequence7
    :: Either error a -> Either error b -> Either error c
    -> Either error d -> Either error e -> Either error f
    -> Either error g -> Either error (a, b, c, d, e, f, g)
sequence7 one two three four five six seven =
    (,,,,,,) <$> one <*> two <*> three <*> four <*> five <*> six <*> seven
