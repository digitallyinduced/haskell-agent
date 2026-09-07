module Agent.Tools.SecretSpec (spec) where

import qualified Agent.Json.Decode as Json
import Agent.ToolDispatch
    ( ToolCallResult(..)
    , ToolDispatchConfig(..)
    , dispatchToolCall
    , functionToolCall
    )
import Agent.ToolDSL (PropertySchema(..))
import Agent.Tools.Secret
import Agent.Tools.Types
    ( AppTool(..)
    , ApprovalRule(..)
    , ToolEnv
    , defaultToolEnv
    , jsonToolParameters
    , setToolHumanInputWaitHooks
    , setToolSessionTmp
    )
import Control.Exception.Safe (bracket, throwString)
import qualified Data.ByteString as BS
import Data.Bits ((.&.))
import Data.IORef
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import System.Directory
    ( doesDirectoryExist
    , doesFileExist
    , getTemporaryDirectory
    , listDirectory
    , removeDirectoryRecursive
    )
import System.FilePath (takeDirectory, (</>))
import System.OsPath (unsafeEncodeUtf)
import System.Posix.Files (fileMode, getFileStatus)
import System.Posix.Temp (mkdtemp)
import Test.Hspec

spec :: Spec
spec = describe "Agent.Tools.Secret" do
    it "advertises only non-secret prompt context" do
        withStore (const (pure (Right Nothing))) \_ tool _ -> do
            let parameters = fromMaybe [] (jsonToolParameters tool)
            map (.propertyName) parameters `shouldBe` ["prompt", "purpose"]
            map (.required) parameters `shouldBe` [True, False]
            case tool.appToolApproval of
                AlwaysReadOnly -> pure ()
                _ -> expectationFailure "ask_secret should self-authorize"

    it "passes prompt context to the trusted hook" do
        seen <- newIORef Nothing
        let hook request = do
                writeIORef seen (Just request)
                pure (Right Nothing)
        withStore hook \_ tool _ -> do
            _ <- runTool tool
                "{\"prompt\":\"Bot token\",\"purpose\":\"Configure Telegram\"}"
            readIORef seen `shouldReturn` Just SecretPrompt
                { secretPromptMessage = "Bot token"
                , secretPromptPurpose = Just "Configure Telegram"
                }

    it "creates a private exact-content file and never returns the secret" do
        let secret = "token with spaces\nand-a-second-line"
        withStore (const (pure (Right (Just secret)))) \_ tool _ -> do
            output <- runTool tool "{\"prompt\":\"Token\"}"
            output `shouldNotSatisfy` Text.isInfixOf secret
            path <- resultSecretPath output
            BS.readFile path `shouldReturn` Text.encodeUtf8 secret
            modeOf path `shouldReturn` 0o600
            modeOf (takeDirectory path) `shouldReturn` 0o700

    it "creates unique files for repeated requests" do
        withStore (const (pure (Right (Just "secret")))) \_ tool _ -> do
            first <- runTool tool "{\"prompt\":\"First\"}" >>= resultSecretPath
            second <- runTool tool "{\"prompt\":\"Second\"}" >>= resultSecretPath
            first `shouldNotBe` second

    it "removes owned files and directories when closed" do
        withStoreManual (const (pure (Right (Just "secret")))) \store tool _ -> do
            path <- runTool tool "{\"prompt\":\"Token\"}" >>= resultSecretPath
            let directory = takeDirectory path
            doesFileExist path `shouldReturn` True
            closeSecretStore store
            doesFileExist path `shouldReturn` False
            doesDirectoryExist directory `shouldReturn` False
            closeSecretStore store

    it "creates no artifact when the user cancels" do
        withStore (const (pure (Right Nothing))) \_ tool temp -> do
            output <- runTool tool "{\"prompt\":\"Token\"}"
            output `shouldBe` "ERR Secret entry cancelled."
            listDirectory temp `shouldReturn` []

    it "fails closed when secure entry is unavailable" do
        withStore
            (const (pure (Left "Secure secret entry requires an interactive terminal.")))
            \_ tool temp -> do
                output <- runTool tool "{\"prompt\":\"Token\"}"
                output `shouldBe`
                    "ERR Secure secret entry requires an interactive terminal."
                listDirectory temp `shouldReturn` []

    it "does not expose host prompt exceptions" do
        events <- newIORef ([] :: [Text])
        let record event = modifyIORef' events (<> [event])
            hook _ = do
                record "prompt"
                throwString "host detail that must stay private"
        withStoreEnv hook \env _ tool temp -> do
            setToolHumanInputWaitHooks
                env
                (record "begin")
                (record "end")
            output <- runTool tool "{\"prompt\":\"Token\"}"
            output `shouldBe` "ERR Secure secret entry failed."
            output `shouldNotSatisfy` Text.isInfixOf "host detail"
            readIORef events `shouldReturn` ["begin", "prompt", "end"]
            listDirectory temp `shouldReturn` []

    it "requires a configured session temporary directory" do
        root <- getTemporaryDirectory
        bracket
            (mkdtemp (root </> "agent-secret-no-temp-"))
            removeDirectoryRecursive
            \workspace -> do
                env <- defaultToolEnv (unsafeEncodeUtf workspace)
                store <- newSecretStore env
                    (SecretPromptHooks (const (pure (Right (Just "secret")))))
                let tool = askSecretTool store
                runTool tool "{\"prompt\":\"Token\"}"
                    `shouldReturn`
                        "ERR Secure secret entry requires a session temporary directory."
                closeSecretStore store

withStore
    :: (SecretPrompt -> IO (Either Text (Maybe Text)))
    -> (SecretStore -> AppTool -> FilePath -> IO a)
    -> IO a
withStore hook action =
    withStoreManual hook \store tool temp ->
        action store tool temp

withStoreManual
    :: (SecretPrompt -> IO (Either Text (Maybe Text)))
    -> (SecretStore -> AppTool -> FilePath -> IO a)
    -> IO a
withStoreManual hook action =
    withStoreEnv hook \_ store tool temp ->
        action store tool temp

withStoreEnv
    :: (SecretPrompt -> IO (Either Text (Maybe Text)))
    -> (ToolEnv -> SecretStore -> AppTool -> FilePath -> IO a)
    -> IO a
withStoreEnv hook action = do
    root <- getTemporaryDirectory
    bracket
        (mkdtemp (root </> "agent-secret-test-"))
        removeDirectoryRecursive
        \workspace -> do
            temp <- mkdtemp (workspace </> "session-tmp-")
            env <- defaultToolEnv (unsafeEncodeUtf workspace)
            setToolSessionTmp env (Just (unsafeEncodeUtf temp))
            bracket
                (newSecretStore env (SecretPromptHooks hook))
                closeSecretStore
                (\store -> action env store (askSecretTool store) temp)

runTool :: AppTool -> Text -> IO Text
runTool tool arguments = do
    result <- dispatchToolCall testDispatchConfig
        [tool.appToolHandler]
        (functionToolCall "secret-1" "ask_secret" arguments)
    pure result.output

resultSecretPath :: Text -> IO FilePath
resultSecretPath output =
    case Json.decodeText
            (Json.object (Json.atKey "secret_file" Json.text))
            output of
        Right path -> pure (Text.unpack path)
        Left _ -> expectationFailure ("missing secret_file in output: " <> Text.unpack output)
            >> pure ""

modeOf :: FilePath -> IO Integer
modeOf path =
    fromIntegral . (.&. 0o777) . fileMode <$> getFileStatus path

testDispatchConfig :: ToolDispatchConfig
testDispatchConfig = ToolDispatchConfig
    { toolDispatchUnknownTool = \name -> "unknown:" <> name
    , toolDispatchFormatResult = either ("ERR " <>) id
    , toolDispatchFormatException = \name _ -> "EX " <> name
    , toolDispatchOnException = \_ _ -> pure ()
    , toolDispatchOnOutput = \_ _ -> pure ()
    , toolDispatchFinalizeOutput = \_call output -> pure output
    }
