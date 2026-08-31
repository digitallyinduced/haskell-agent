module Agent.GrokBuild.SearchReplaceSpec (spec) where

import Agent.GrokBuild.Dialect.SearchReplace
    ( searchReplaceToolWithPrefetch
    )
import Agent.Tools.FileSystem.FilePrefetch
    ( FilePrefetch
    , closeFilePrefetch
    , newFilePrefetch
    , waitForFilePrefetch
    )
import Agent.ToolDispatch
    ( ToolArgumentStreamEvent(..)
    , ToolCallStreamRef(..)
    , functionToolCall
    )
import Agent.Tools.PlanMode (newPlanModeEnv)
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
    ( getTemporaryDirectory
    , removeDirectoryRecursive
    )
import System.FilePath ((</>))
import System.OsPath (unsafeEncodeUtf)
import System.Posix.Temp (mkdtemp)
import Test.Hspec

spec :: Spec
spec = describe "search_replace speculation" do
    it "applies a replace using a file prefetched from the streamed path" do
        withReplaceRuntime \dir prefetch runtime -> do
            Text.writeFile (dir </> "alpha.hs") "hello\n"
            let callId = "call-replace"
                arguments =
                    "{\"file_path\":\"alpha.hs\",\"old_string\":\"hello\",\"new_string\":\"world\"}"
            streamPath runtime callId "alpha.hs"
            waitForToolSpeculation runtime
            waitForFilePrefetch prefetch
            retainReplace runtime callId arguments
            takeReplace runtime callId arguments
                `shouldReturn` Just
                    (Right "The file alpha.hs has been updated successfully.")
            Text.readFile (dir </> "alpha.hs") `shouldReturn` "world\n"

    it "re-reads when the prefetched snapshot is stale" do
        withReplaceRuntime \dir prefetch runtime -> do
            Text.writeFile (dir </> "stale.hs") "hello\n"
            let callId = "call-stale"
                arguments =
                    "{\"file_path\":\"stale.hs\",\"old_string\":\"hello\",\"new_string\":\"world\"}"
            streamPath runtime callId "stale.hs"
            waitForToolSpeculation runtime
            waitForFilePrefetch prefetch
            Text.writeFile (dir </> "stale.hs") "goodbye\n"
            retainReplace runtime callId arguments
            result <- takeReplace runtime callId arguments
            result `shouldSatisfy` \case
                Just (Left message) ->
                    "not found" `Text.isInfixOf` Text.toLower message
                        || "multiple times" `Text.isInfixOf` message
                Just (Right _) -> False
                Nothing -> False

withReplaceRuntime
    :: (FilePath -> FilePrefetch -> ToolSpeculationRuntime -> IO a)
    -> IO a
withReplaceRuntime action = do
    tmp <- getTemporaryDirectory
    bracket
        (mkdtemp (tmp </> "agent-replace-speculation-XXXXXX"))
        removeDirectoryRecursive
        \dir -> do
            env <- defaultToolEnv (unsafeEncodeUtf dir)
            plan <- newPlanModeEnv (unsafeEncodeUtf dir) Nothing
            prefetch <- newFilePrefetch env
            let tool = searchReplaceToolWithPrefetch env plan (Just prefetch)
            (bracket
                (newToolSpeculationRuntime [tool])
                closeToolSpeculationRuntime
                (action dir prefetch))
                `finally` closeFilePrefetch prefetch

streamPath :: ToolSpeculationRuntime -> Text -> Text -> IO ()
streamPath runtime callId path = do
    observeToolArgumentEvent runtime $
        ToolArgumentsStarted
            { argumentStreamRefs = [ToolCallStreamItem "item-replace"]
            , argumentStreamCallId = callId
            , argumentStreamName = Just "search_replace"
            , argumentStreamArguments = "{\"file_path\":\"" <> path
            }
    waitForToolSpeculation runtime

retainReplace :: ToolSpeculationRuntime -> Text -> Text -> IO ()
retainReplace runtime callId arguments =
    retainToolSpeculation runtime
        [functionToolCall callId "search_replace" arguments]

takeReplace
    :: ToolSpeculationRuntime
    -> Text
    -> Text
    -> IO (Maybe (Either Text Text))
takeReplace runtime callId arguments =
    takeToolSpeculation runtime
        (functionToolCall callId "search_replace" arguments)
