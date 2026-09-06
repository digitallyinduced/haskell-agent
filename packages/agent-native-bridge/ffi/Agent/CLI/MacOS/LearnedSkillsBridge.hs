{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Read-only learned-skill catalog callbacks, with request-scoped stores.
module Agent.CLI.MacOS.LearnedSkillsBridge () where

import Agent.CLI.Database.Store (applicableDatabaseScopes, deriveDatabaseScopes)
import Agent.CLI.MacOS.Marshalling (decodeInput, withText)
import Agent.CLI.Project (resolveProjectRoot)
import Agent.CLI.Session (sessionsRoot)
import Agent.CLI.SessionAdmin (managedPostgresConfigForHome)
import Agent.Store.Postgres (closeStore, openStore, trustedPool)
import Agent.Store.Postgres.Scope (Scope(..), scopeKindText)
import Agent.Store.Postgres.Skill
    ( LearnedSkill(..), learnedSkillActivationText, learnedSkillStatusText
    , listAllLearnedSkillsLimited )
import Agent.Store.Types (renderStoreError)
import Control.Concurrent (forkIO)
import Control.Exception.Safe (bracket, tryAny)
import Control.Monad (forM_)
import Data.Bifunctor (first)
import qualified Data.Text as Text
import Data.Word (Word8)
import Foreign.C.String (CString)
import Foreign.C.Types (CInt(..), CLLong(..), CSize(..))
import Foreign.Ptr (FunPtr, Ptr, nullFunPtr, nullPtr)
import System.Directory.OsPath (getHomeDirectory)
import System.OsPath (decodeFS, takeDirectory, unsafeEncodeUtf)

type LearnedSkillsListCallback =
    Ptr () -> CInt
    -> CString -> CSize -> CString -> CSize -> CLLong
    -> CString -> CSize -> CString -> CSize -> CString -> CSize
    -> CString -> CSize -> CString -> CSize -> CString -> CSize
    -> CInt -> CString -> CSize -> CString -> CSize -> IO ()

foreign import ccall "dynamic"
    invokeLearnedSkillsListCallback
        :: FunPtr LearnedSkillsListCallback -> LearnedSkillsListCallback

foreign export ccall ha_learned_skills_list
    :: Ptr Word8 -> CSize -> FunPtr LearnedSkillsListCallback -> Ptr () -> IO CInt

ha_learned_skills_list
    :: Ptr Word8 -> CSize -> FunPtr LearnedSkillsListCallback -> Ptr () -> IO CInt
ha_learned_skills_list cwdBytes (CSize cwdLength) callback context
    | callback == nullFunPtr = pure 1
    | cwdBytes == nullPtr && cwdLength > 0 = pure 2
    | otherwise = do
        cwd <- decodeInput cwdBytes cwdLength
        _ <- forkIO do
            tryAny (listLearnedSkillsFor (Text.unpack cwd)) >>= \case
                Left exception ->
                    withText (Text.pack (show exception)) $ \errorPtr errorLength ->
                        learnedSkillsTerminal callback context (-1) errorPtr errorLength
                Right (Left err) ->
                    withText err $ \errorPtr errorLength ->
                        learnedSkillsTerminal callback context (-1) errorPtr errorLength
                Right (Right skills) -> do
                    forM_ skills \skill ->
                        withLearnedSkillStrings skill $
                            invokeLearnedSkillsListCallback callback context 0
                    learnedSkillsTerminal callback context 1 nullPtr 0
        pure 0
  where
    listLearnedSkillsFor cwd = do
        home <- getHomeDirectory
        projectRoot <- resolveProjectRoot (unsafeEncodeUtf cwd)
        stateDirectory <- decodeFS (takeDirectory (sessionsRoot home))
        projectRootPath <- decodeFS projectRoot
        scopes <- deriveDatabaseScopes stateDirectory projectRootPath
        case scopes of
            Left err -> pure (Left err)
            Right databaseScopes -> do
                store <- openStore =<< managedPostgresConfigForHome home
                case store of
                    Left err -> pure (Left (renderStoreError err))
                    Right opened ->
                        bracket (pure opened) closeStore \store ->
                            first renderStoreError
                                <$> listAllLearnedSkillsLimited
                                    (trustedPool store)
                                    (applicableDatabaseScopes databaseScopes)
                                    Nothing
                                    1000

type LearnedSkillItemCallback =
    CString -> CSize -> CString -> CSize -> CLLong
    -> CString -> CSize -> CString -> CSize -> CString -> CSize
    -> CString -> CSize -> CString -> CSize -> CString -> CSize
    -> CInt -> CString -> CSize -> CString -> CSize -> IO ()

withLearnedSkillStrings :: LearnedSkill -> LearnedSkillItemCallback -> IO ()
withLearnedSkillStrings skill action =
    withText (scopeKindText skill.learnedSkillScope.scopeKind) $ \scope scopeLength ->
    withText skill.learnedSkillSlug $ \slug slugLength ->
    withText skill.learnedSkillTitle $ \title titleLength ->
    withText skill.learnedSkillDescription $ \description descriptionLength ->
    withText skill.learnedSkillAppliesWhen $ \applies appliesLength ->
    withText skill.learnedSkillInstructions $ \instructions instructionsLength ->
    withText (learnedSkillActivationText skill.learnedSkillActivation) $ \activation activationLength ->
    withText (learnedSkillStatusText skill.learnedSkillStatus) $ \status statusLength ->
    withText (Text.pack (show skill.learnedSkillUpdatedAt)) $ \updated updatedLength ->
        action scope scopeLength slug slugLength
            (fromIntegral skill.learnedSkillRevision)
            title titleLength description descriptionLength
            applies appliesLength instructions instructionsLength
            activation activationLength status statusLength
            (fromIntegral skill.learnedSkillPriority) updated updatedLength
            nullPtr 0

learnedSkillsTerminal
    :: FunPtr LearnedSkillsListCallback
    -> Ptr () -> CInt -> CString -> CSize -> IO ()
learnedSkillsTerminal callback context status errorPtr errorLength =
    invokeLearnedSkillsListCallback callback context status
        nullPtr 0 nullPtr 0 0
        nullPtr 0 nullPtr 0 nullPtr 0 nullPtr 0
        nullPtr 0 nullPtr 0
        0 nullPtr 0 errorPtr errorLength
