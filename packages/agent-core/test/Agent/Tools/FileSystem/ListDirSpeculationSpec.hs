module Agent.Tools.FileSystem.ListDirSpeculationSpec (spec) where

import Agent.ToolDispatch
    ( ToolArgumentStreamEvent(..)
    , ToolCallStreamRef(..)
    , functionToolCall
    )
import Agent.Tools.FileSystem.ListDir
    ( ListDirSpeculation
    , closeListDirSpeculation
    , listDirToolWithSpeculation
    , newListDirSpeculation
    , waitForListDirSpeculation
    )
import Agent.Tools.Speculation
    ( ToolSpeculationRuntime
    , closeToolSpeculationRuntime
    , newToolSpeculationRuntime
    , observeToolArgumentEvent
    , retainToolSpeculation
    , takeToolSpeculation
    , waitForToolSpeculation
    )
import Agent.Tools.Types (defaultToolEnv)
import Control.Exception.Safe (bracket, finally)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import System.Directory
    ( createDirectoryIfMissing
    , getTemporaryDirectory
    , removeDirectoryRecursive
    )
import System.Exit (ExitCode(..))
import System.FilePath ((</>))
import System.OsPath (unsafeEncodeUtf)
import System.Posix.Temp (mkdtemp)
import System.Process (readProcessWithExitCode)
import Test.Hspec

spec :: Spec
spec = describe "list_dir speculation" do
    it "prefetches a complete directory from argument deltas" do
        withListSpeculation \dir _cache runtime -> do
            createDirectoryIfMissing True (dir </> "alpha")
            Text.writeFile (dir </> "alpha" </> "note.txt") "hi"
            let callId = "call-list"
                arguments = listArguments "alpha"
            streamList runtime callId arguments
            waitForToolSpeculation runtime
            waitForListDirSpeculation _cache
            retainList runtime callId arguments
            takeList runtime callId arguments
                `shouldReturn` Just
                    (Right "Directory listing for alpha:\n- note.txt\n")

    it "predicts a unique workspace directory from a streamed prefix" do
        withPreparedList
            (\dir -> do
                createDirectoryIfMissing True (dir </> "src" </> "unique-listing")
                createDirectoryIfMissing True (dir </> "src" </> "other-listing")
                Text.writeFile
                    (dir </> "src" </> "unique-listing" </> "a.txt")
                    "a"
                Text.writeFile
                    (dir </> "src" </> "other-listing" </> "b.txt")
                    "b"
                initializeGitRepository dir)
            \_dir cache runtime -> do
                let callId = "call-prefix"
                    prefix = "{\"target_directory\":\"src/uni"
                    arguments = listArguments "src/unique-listing"
                observeToolArgumentEvent runtime $
                    ToolArgumentsStarted
                        { argumentStreamRefs = [ToolCallStreamItem "item-prefix"]
                        , argumentStreamCallId = callId
                        , argumentStreamName = Just "list_dir"
                        , argumentStreamArguments = prefix
                        }
                waitForToolSpeculation runtime
                waitForListDirSpeculation cache
                retainList runtime callId arguments
                result <- takeList runtime callId arguments
                result `shouldSatisfy` \case
                    Just (Right output) ->
                        "unique-listing" `Text.isInfixOf` output
                            && "a.txt" `Text.isInfixOf` output
                    _ -> False

    it "falls back when the directory changes after prefetch" do
        withListSpeculation \dir cache runtime -> do
            createDirectoryIfMissing True (dir </> "stale")
            Text.writeFile (dir </> "stale" </> "old.txt") "old"
            let callId = "call-stale"
                arguments = listArguments "stale"
            streamList runtime callId arguments
            waitForToolSpeculation runtime
            waitForListDirSpeculation cache
            Text.writeFile (dir </> "stale" </> "new.txt") "new"
            retainList runtime callId arguments
            result <- takeList runtime callId arguments
            result `shouldSatisfy` \case
                Just (Right output) -> "new.txt" `Text.isInfixOf` output
                _ -> False

withListSpeculation
    :: (FilePath -> ListDirSpeculation -> ToolSpeculationRuntime -> IO a)
    -> IO a
withListSpeculation = withPreparedList (const (pure ()))

withPreparedList
    :: (FilePath -> IO ())
    -> (FilePath -> ListDirSpeculation -> ToolSpeculationRuntime -> IO a)
    -> IO a
withPreparedList prepare action =
    withTempDir \dir -> do
        prepare dir
        env <- defaultToolEnv (unsafeEncodeUtf dir)
        cache <- newListDirSpeculation env
        let tool = listDirToolWithSpeculation env (Just cache)
        bracket
            (newToolSpeculationRuntime [tool])
            closeToolSpeculationRuntime
            (\runtime ->
                action dir cache runtime
                    `finally` closeListDirSpeculation cache)

withTempDir :: (FilePath -> IO a) -> IO a
withTempDir action = do
    tmp <- getTemporaryDirectory
    bracket
        (mkdtemp (tmp </> "agent-list-speculation-XXXXXX"))
        removeDirectoryRecursive
        action

initializeGitRepository :: FilePath -> IO ()
initializeGitRepository dir = do
    (exitCode, _, stderrText) <-
        readProcessWithExitCode "git" ["-C", dir, "init", "-q"] ""
    case exitCode of
        ExitSuccess -> pure ()
        ExitFailure code ->
            expectationFailure $
                "git init failed with exit "
                    <> show code
                    <> ": "
                    <> stderrText

streamList :: ToolSpeculationRuntime -> Text -> Text -> IO ()
streamList runtime callId arguments = do
    observeToolArgumentEvent runtime $
        ToolArgumentsStarted
            { argumentStreamRefs = [ToolCallStreamItem "item-list"]
            , argumentStreamCallId = callId
            , argumentStreamName = Just "list_dir"
            , argumentStreamArguments = ""
            }
    observeToolArgumentEvent runtime $
        ToolArgumentsDelta
            { argumentStreamRefs = [ToolCallStreamItem "item-list"]
            , argumentStreamDelta = arguments
            }

retainList :: ToolSpeculationRuntime -> Text -> Text -> IO ()
retainList runtime callId arguments =
    retainToolSpeculation runtime
        [functionToolCall callId "list_dir" arguments]

takeList
    :: ToolSpeculationRuntime
    -> Text
    -> Text
    -> IO (Maybe (Either Text Text))
takeList runtime callId arguments =
    takeToolSpeculation runtime (functionToolCall callId "list_dir" arguments)

listArguments :: Text -> Text
listArguments target =
    "{\"target_directory\":\"" <> target <> "\"}"
