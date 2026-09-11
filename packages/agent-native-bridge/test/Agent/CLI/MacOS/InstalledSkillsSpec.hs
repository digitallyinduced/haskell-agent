{-# LANGUAGE ForeignFunctionInterface #-}

module Agent.CLI.MacOS.InstalledSkillsSpec (spec) where

import Agent.CLI.MacOS.InstalledSkills ()
import Control.Exception.Safe (bracket)
import Foreign.C.String (CString, withCString)
import Foreign.C.Types (CInt(..))
import System.Directory
    ( canonicalizePath, createDirectory, createDirectoryIfMissing
    , getTemporaryDirectory, removeFile, removePathForcibly )
import System.FilePath ((</>))
import System.IO (hClose, openTempFile)
import Test.Hspec

foreign import ccall safe "ha_installed_skills_abi_smoke"
    installedSkillsSmoke :: CString -> IO CInt

spec :: Spec
spec = describe "Installed filesystem skills native ABI" do
    it "lists, reads, reports warnings, rejects invalid inputs and confines reads to the catalog" $
        bracket createFixture removePathForcibly \directory ->
            withCString directory \pointer ->
                installedSkillsSmoke pointer `shouldReturn` 0
  where
    createFixture = do
        temporary <- getTemporaryDirectory
        (path, handle) <- openTempFile temporary "installed-skills-catalog"
        hClose handle
        removeFile path
        createDirectory path
        createDirectory (path </> ".git")
        let valid = path </> ".agents" </> "skills" </> "catalog-fixture"
            invalid = path </> ".agents" </> "skills" </> "invalid-fixture"
        createDirectoryIfMissing True valid
        createDirectoryIfMissing True invalid
        writeFile (valid </> "SKILL.md")
            "---\nname: catalog-fixture\ndescription: Catalog fixture description\n---\nFixture instructions\n"
        writeFile (invalid </> "SKILL.md") "Missing required frontmatter"
        canonicalizePath path
