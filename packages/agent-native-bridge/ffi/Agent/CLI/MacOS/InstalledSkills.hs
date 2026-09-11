{-# LANGUAGE ForeignFunctionInterface #-}

-- | Read-only filesystem skill catalog using the same discovery as sessions.
module Agent.CLI.MacOS.InstalledSkills () where

import Agent.CLI.Options (defaultCliOptions)
import Agent.CLI.Project (resolveProjectRoot)
import Agent.CLI.Skills (loadSkillsCatalogQuiet)
import Agent.OsPath (toText)
import Agent.Skills
import Control.Concurrent (forkIO)
import Control.Exception.Safe (displayException, tryAny)
import Control.Monad (forM_, unless, void)
import qualified Data.ByteString as BS
import Data.List (find)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import Data.Word (Word8)
import Foreign (FunPtr, Ptr, castPtr, nullFunPtr, nullPtr)
import Foreign.C.String (CString)
import Foreign.C.Types (CInt(..), CSize(..))
import System.Directory.OsPath (doesDirectoryExist, getHomeDirectory)
import System.OsPath (unsafeEncodeUtf)

type InstalledSkillCallback =
    Ptr () -> CInt -> CInt
    -> CString -> CSize -> CString -> CSize -> CString -> CSize
    -> CString -> CSize -> CString -> CSize -> IO ()

foreign import ccall "dynamic"
    invokeInstalledSkillCallback :: FunPtr InstalledSkillCallback -> InstalledSkillCallback

foreign export ccall ha_installed_skill_list
    :: Ptr Word8 -> CSize -> FunPtr InstalledSkillCallback -> Ptr () -> IO CInt

foreign export ccall ha_installed_skill_read
    :: Ptr Word8 -> CSize -> Ptr Word8 -> CSize
    -> FunPtr InstalledSkillCallback -> Ptr () -> IO CInt

ha_installed_skill_list
    :: Ptr Word8 -> CSize -> FunPtr InstalledSkillCallback -> Ptr () -> IO CInt
ha_installed_skill_list cwdPointer cwdLength callback context
    | callback == nullFunPtr = pure 1
    | otherwise = decodePath cwdPointer cwdLength >>= \case
        Nothing -> pure 2
        Just cwd -> runCatalog cwd Nothing callback context >> pure 0

ha_installed_skill_read
    :: Ptr Word8 -> CSize -> Ptr Word8 -> CSize
    -> FunPtr InstalledSkillCallback -> Ptr () -> IO CInt
ha_installed_skill_read cwdPointer cwdLength identityPointer identityLength callback context
    | callback == nullFunPtr = pure 1
    | otherwise = do
        cwd <- decodePath cwdPointer cwdLength
        identity <- decodePath identityPointer identityLength
        case (cwd, identity) of
            (Just directory, Just selected) ->
                runCatalog directory (Just selected) callback context >> pure 0
            _ -> pure 2

-- Copy and validate before scheduling work. Absolute paths make workspace
-- selection independent of the process working directory.
decodePath :: Ptr Word8 -> CSize -> IO (Maybe Text)
decodePath pointer count
    | pointer == nullPtr || count == 0 || count > 32768 = pure Nothing
    | otherwise = do
        bytes <- BS.packCStringLen (castPtr pointer, fromIntegral count)
        pure $ case Encoding.decodeUtf8' bytes of
            Right value
                | Text.isPrefixOf "/" value && not (Text.any (== '\0') value) ->
                    Just value
            _ -> Nothing

runCatalog :: Text -> Maybe Text -> FunPtr InstalledSkillCallback -> Ptr () -> IO ()
runCatalog directory selected callback context = void $ forkIO do
    result <- tryAny do
        let cwd = unsafeEncodeUtf (Text.unpack directory)
        exists <- doesDirectoryExist cwd
        unless exists (ioError (userError "The selected skill workspace directory does not exist."))
        home <- getHomeDirectory
        projectRoot <- resolveProjectRoot cwd
        loadSkillsCatalogQuiet defaultCliOptions home projectRoot cwd
    case result of
        Left exception -> emitStatus callback context (-1) (Text.pack (displayException exception))
        Right catalog -> do
            forM_ catalog.catalogWarnings \warning ->
                emitStatus callback context 2
                    (toText warning.skillWarningPath <> ": " <> warning.skillWarningMessage)
            case selected of
                Nothing -> do
                    forM_ catalog.catalogSkills (emitSkill callback context False)
                    emitStatus callback context 1 ""
                Just identity ->
                    case find ((== Just identity) . skillIdentity) catalog.catalogSkills of
                        Nothing -> emitStatus callback context (-2) "Installed skill is no longer available in this workspace."
                        Just skill -> do
                            emitSkill callback context True skill
                            emitStatus callback context 1 ""

skillIdentity :: Skill -> Maybe Text
skillIdentity skill = case skill.skillSource of
    FilesystemSkillSource{skillPath} -> Just (toText skillPath)
    McpSkillSource{} -> Nothing

emitSkill :: FunPtr InstalledSkillCallback -> Ptr () -> Bool -> Skill -> IO ()
emitSkill callback context includeInstructions skill =
    case skill.skillSource of
        McpSkillSource{} -> pure ()
        FilesystemSkillSource{skillPath, skillScope, skillFileText} ->
            withText (toText skillPath) \identity identityLength ->
            withText skill.skillName \name nameLength ->
            withText skill.skillDescription \description descriptionLength ->
            withText (if includeInstructions then skillFileText else "") \instructions instructionsLength ->
                invokeInstalledSkillCallback callback context 0 (scopeValue skillScope)
                    identity identityLength name nameLength description descriptionLength
                    instructions instructionsLength nullPtr 0

scopeValue :: SkillScope -> CInt
scopeValue BuiltinSkill = 0
scopeValue UserSkill = 1
scopeValue RepositorySkill{} = 2

emitStatus :: FunPtr InstalledSkillCallback -> Ptr () -> CInt -> Text -> IO ()
emitStatus callback context status message =
    withText message \pointer count ->
        invokeInstalledSkillCallback callback context status (-1)
            nullPtr 0 nullPtr 0 nullPtr 0 nullPtr 0 pointer count

withText :: Text -> (CString -> CSize -> IO a) -> IO a
withText text action = BS.useAsCStringLen (Encoding.encodeUtf8 text) \(pointer, count) ->
    action pointer (fromIntegral count)
