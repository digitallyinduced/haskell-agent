-- | Secure, harness-mediated secret entry for model tool calls.
--
-- The secret itself is supplied by a trusted host hook and is never present in
-- the model's tool arguments or result. The model receives only the path of a
-- private, temporary file containing the exact bytes entered by the user.
module Agent.Tools.Secret
    ( SecretPrompt(..)
    , SecretPromptHooks(..)
    , SecretStore
    , newSecretStore
    , closeSecretStore
    , askSecretTool
    ) where

import Agent.OsPath (unsafeToFilePath)
import Agent.ToolArgs (optText, reqText)
import Agent.ToolDSL
    ( PropertySchema(..)
    , PropertyType(..)
    )
import Agent.ToolDispatch (typedTool)
import Agent.Tools.Types
    ( AppTool
    , ToolEnv(..)
    , ToolExecutionPolicy(..)
    , jsonTool
    )
import Control.Concurrent.MVar
    ( MVar
    , modifyMVar
    , newMVar
    )
import Control.Exception.Safe
    ( SomeException
    , bracketOnError
    , displayException
    , mask
    , onException
    , tryAny
    , tryIO
    )
import Control.Monad (void)
import Data.Aeson
    ( FromJSON(..)
    , Value
    , object
    , withObject
    , (.=)
    )
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.IORef (readIORef)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import System.Directory
    ( doesDirectoryExist
    , removeDirectory
    , removeFile
    )
import System.FilePath ((</>))
import System.IO (Handle, hClose)
import System.Posix.Temp (mkdtemp, mkstemp)
import System.Posix.IO
    ( FdOption(CloseOnExec)
    , closeFd
    , fdToHandle
    , handleToFd
    , setFdOption
    )

-- | Non-secret context shown by the trusted host UI.
data SecretPrompt = SecretPrompt
    { secretPromptMessage :: !Text
    , secretPromptPurpose :: !(Maybe Text)
    }
    deriving (Eq, Show)

-- | Host interaction used by 'askSecretTool'.
--
-- @Left@ reports that secure entry is unavailable, @Right Nothing@ means the
-- user cancelled, and @Right (Just value)@ supplies the exact secret.
newtype SecretPromptHooks = SecretPromptHooks
    { promptSecret :: SecretPrompt -> IO (Either Text (Maybe Text))
    }

data SecretArtifact = SecretArtifact
    { artifactDirectory :: !FilePath
    , artifactFile :: !FilePath
    }

data SecretStoreState
    = SecretStoreOpen ![SecretArtifact]
    | SecretStoreClosed

-- | Session-scoped owner of temporary secret files.
data SecretStore = SecretStore
    { secretToolEnv :: !ToolEnv
    , secretHooks :: !SecretPromptHooks
    , secretState :: !(MVar SecretStoreState)
    }

newSecretStore :: ToolEnv -> SecretPromptHooks -> IO SecretStore
newSecretStore env hooks = do
    state <- newMVar (SecretStoreOpen [])
    pure SecretStore
        { secretToolEnv = env
        , secretHooks = hooks
        , secretState = state
        }

-- | Remove every secret file still owned by the store. Idempotent.
closeSecretStore :: SecretStore -> IO ()
closeSecretStore store = do
    artifacts <- modifyMVar store.secretState \case
        SecretStoreClosed -> pure (SecretStoreClosed, [])
        SecretStoreOpen current -> pure (SecretStoreClosed, current)
    mapM_ removeSecretArtifact artifacts

data AskSecretArgs = AskSecretArgs
    { prompt :: !Text
    , purpose :: !(Maybe Text)
    }

instance FromJSON AskSecretArgs where
    parseJSON = withObject "ask_secret" \input -> AskSecretArgs
        <$> reqText input "prompt"
        <*> optText input "purpose"

askSecretTool :: SecretStore -> AppTool
askSecretTool store = jsonTool "ask_secret" askSecretDescription
    [ PropertySchema "prompt" PropertyString True $ Just
        "The trusted prompt shown to the user. Never include a secret value."
    , PropertySchema "purpose" PropertyString False $ Just
        "Optional explanation of why the secret is needed."
    ]
    -- The handler performs an explicit trusted user interaction. A second
    -- generic mutating-tool approval would be redundant.
    True
    TurnSequential
    (typedTool "ask_secret" (runAskSecret store))

askSecretDescription :: Text
askSecretDescription =
    "Ask the user for a secret without placing it in chat or tool arguments. "
        <> "The harness writes the exact value to a private temporary file and "
        <> "returns only its path. Never read, print, or echo the file contents."

runAskSecret :: SecretStore -> AskSecretArgs -> IO (Either Text Text)
runAskSecret store args
    | Text.null (Text.strip args.prompt) =
        pure (Left "Secret prompt must not be empty.")
    | otherwise = do
        prompted <- tryAny $
            store.secretHooks.promptSecret SecretPrompt
            { secretPromptMessage = args.prompt
            , secretPromptPurpose = args.purpose
            }
        case prompted of
            Left _ -> pure (Left "Secure secret entry failed.")
            Right (Left err) -> pure (Left err)
            Right (Right Nothing) -> pure (Left "Secret entry cancelled.")
            Right (Right (Just secret))
                | Text.null secret -> pure (Left "No secret was entered.")
                | otherwise -> storeSecret store secret

storeSecret :: SecretStore -> Text -> IO (Either Text Text)
storeSecret store secret =
    createSecretArtifact store.secretToolEnv secret >>= \case
        Left err -> pure (Left err)
        Right artifact -> do
            accepted <- modifyMVar store.secretState \case
                SecretStoreClosed -> pure (SecretStoreClosed, False)
                SecretStoreOpen current ->
                    pure (SecretStoreOpen (artifact : current), True)
            if not accepted
                then do
                    removeSecretArtifact artifact
                    pure (Left "Secret storage is already closed.")
                else pure $ Right $ encodeJson $ object
                    [ "secret_file" .= Text.pack artifact.artifactFile
                    , "message" .=
                        ( "Secret saved to a private temporary file. Pass this "
                            <> "path directly to the consuming program without "
                            <> "reading or echoing the contents. Delete the file "
                            <> "as soon as it has been consumed."
                            :: Text
                        )
                    ]

createSecretArtifact
    :: ToolEnv
    -> Text
    -> IO (Either Text SecretArtifact)
createSecretArtifact env secret =
    readIORef env.toolSessionTmp >>= \case
        Nothing ->
            pure (Left "Secure secret entry requires a session temporary directory.")
        Just temp -> do
            let tempPath = unsafeToFilePath temp
            exists <- doesDirectoryExist tempPath
            if not exists
                then pure (Left "The session temporary directory does not exist.")
                else do
                    result <- tryAny (createUnder tempPath)
                    pure $ case result of
                        Left err -> Left
                            ("Failed to store secret securely: " <> exceptionText err)
                        Right artifact -> Right artifact
  where
    createUnder :: FilePath -> IO SecretArtifact
    createUnder tempPath = mask \restore -> do
        directory <- mkdtemp (tempPath </> ".agent-secret-")
        let removeDirectoryOnly = void (tryIO (removeDirectory directory))
        (file, initialHandle) <-
            mkstemp (directory </> "secret-")
                `onException` removeDirectoryOnly
        handle <- do
            fd <-
                handleToFd initialHandle
                    `onException` void (tryIO (hClose initialHandle))
            bracketOnError
                (pure fd)
                closeFd
                \ownedFd -> do
                    setFdOption ownedFd CloseOnExec True
                    fdToHandle ownedFd
        let artifact = SecretArtifact
                { artifactDirectory = directory
                , artifactFile = file
                }
            rollback = closeAndRemove handle artifact
        restore
            (BS.hPut handle (Text.encodeUtf8 secret) >> hClose handle)
            `onException` rollback
        pure artifact

removeSecretArtifact :: SecretArtifact -> IO ()
removeSecretArtifact artifact = do
    void (tryIO (removeFile artifact.artifactFile))
    void (tryIO (removeDirectory artifact.artifactDirectory))

closeAndRemove :: Handle -> SecretArtifact -> IO ()
closeAndRemove handle artifact = do
    void (tryIO (hClose handle))
    removeSecretArtifact artifact

exceptionText :: SomeException -> Text
exceptionText = Text.pack . displayException

encodeJson :: Value -> Text
encodeJson = Text.decodeUtf8 . LBS.toStrict . Aeson.encode
