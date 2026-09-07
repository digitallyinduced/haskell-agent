module Agent.Integrations.RuntimeSpec (spec) where

import Agent.Integrations
import Agent.Tools.Types (ToolEnv(..), defaultToolEnv)
import Data.IORef (readIORef)
import qualified Data.Text as Text
import System.Directory.OsPath (doesDirectoryExist, getCurrentDirectory)
import Test.Hspec

spec :: Spec
spec = describe "integration runtime supervisor" do
    it "owns and removes its private process scratch directory" do
        toolEnv <- defaultToolEnv =<< getCurrentDirectory
        supervisor <- newIntegrationSupervisor toolEnv
        scratch <- readIORef toolEnv.toolSessionTmp
        scratch `shouldSatisfy` maybe False (const True)
        maybe (pure ()) ((`shouldReturn` True) . doesDirectoryExist) scratch
        sessionEnv <- defaultToolEnv =<< getCurrentDirectory
        prepareIntegrationSupervisorForSession supervisor sessionEnv
        sessionRoots <- readIORef sessionEnv.toolAllowedRoots
        sessionRoots `shouldBe` maybe [] pure scratch

        closeIntegrationSupervisor supervisor

        maybe (pure ()) ((`shouldReturn` False) . doesDirectoryExist) scratch
        acquireIntegrationRuntime supervisor LocalIntegrationAuthority >>= \case
            Left message ->
                message `shouldSatisfy` Text.isInfixOf "already closed"
            Right _ ->
                expectationFailure "closed supervisor returned a runtime"
