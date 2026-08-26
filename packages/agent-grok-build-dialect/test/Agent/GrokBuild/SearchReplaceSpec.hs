module Agent.GrokBuild.SearchReplaceSpec (spec) where

import Agent.GrokBuild.Dialect.Runtime
    ( GrokCodingTools(..)
    , newGrokCodingTools
    )
import Agent.ToolDispatch
    ( ToolCallResult(..)
    , ToolDispatchConfig(..)
    , dispatchToolCall
    , functionToolCall
    )
import Agent.Tools.Types (AppTool(..), defaultToolEnv)
import Data.IORef (newIORef)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import System.Directory (createDirectory)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.OsPath (unsafeEncodeUtf)
import Test.Hspec

spec :: Spec
spec = describe "search_replace" do
    it "echoes workspace-relative paths when the model used an absolute path" do
        withSystemTempDirectory "agent-search-replace" \dir -> do
            let sourceDir = dir </> "src"
                absolute = Text.pack (sourceDir </> "Main.hs")
            createDirectory sourceDir
            env <- defaultToolEnv (unsafeEncodeUtf dir)
            typesRef <- newIORef Map.empty
            coding <- newGrokCodingTools env Nothing Nothing typesRef
            result <-
                dispatchToolCall testConfig
                    (map (.appToolHandler) coding.grokAppTools)
                    (functionToolCall
                        "edit-abs"
                        "search_replace"
                        ("{\"file_path\":\""
                            <> absolute
                            <> "\",\"old_string\":\"\",\"new_string\":\"hi\\n\"}"))
            coding.grokClose
            result.output
                `shouldBe` "The file src/Main.hs has been created successfully."

testConfig :: ToolDispatchConfig
testConfig = ToolDispatchConfig
    { toolDispatchUnknownTool = \name -> "unknown:" <> name
    , toolDispatchFormatResult = either ("ERR " <>) id
    , toolDispatchFormatException = \name _ -> "EX " <> name
    , toolDispatchOnException = \_ _ -> pure ()
    , toolDispatchOnOutput = \_ _ -> pure ()
    }
