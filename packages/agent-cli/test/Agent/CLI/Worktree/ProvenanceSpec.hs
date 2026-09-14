module Agent.CLI.Worktree.ProvenanceSpec (spec) where

import Agent.Runtime.Session.Types (SessionMeta(..))
import Agent.Runtime.SessionLock (sessionLockPath, sessionActivityLockPath)
import Agent.CLI.Worktree.Provenance
import Agent.CLI.Worktree.ReadOnlyLock (withExistingReadOnlyLock)
import Agent.Dialect (DialectId(..))
import Agent.Provider (Provider(..))
import Data.Either (isLeft)
import qualified Data.Map.Strict as Map
import Data.Time.Clock (UTCTime, addUTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import qualified System.Directory as Dir
import System.FileLock (withFileLock, SharedExclusive(..))
import System.IO.Temp (withSystemTempDirectory)
import System.OsPath (OsPath, unsafeEncodeUtf, (</>))
import Test.Hspec

spec :: Spec
spec = describe "worktree saved-session provenance" do
    it "does not create missing session locks or directories" $
        withSystemTempDirectory "worktree-provenance" \dir -> do
            let absent = dir <> "/absent/lock"
            existingSessionLockActive absent `shouldReturn` False
            Dir.listDirectory dir `shouldReturn` []

    it "observes real filelock leases and releases its read-only probe" $
        withSystemTempDirectory "worktree-provenance" \dir -> do
            let path = dir <> "/lock"
                os = path
            writeFile path "unchanged"
            existingSessionLockActive os `shouldReturn` False
            withFileLock path Exclusive \_ ->
                existingSessionLockActive os `shouldReturn` True
            existingSessionLockActive os `shouldReturn` False
            readFile path `shouldReturn` "unchanged"

    it "retains symlink and directory lock paths" $
        withSystemTempDirectory "worktree-provenance" \dir -> do
            existingSessionLockActive dir `shouldReturn` True
            Dir.createFileLink (dir <> "/missing") (dir <> "/link")
            existingSessionLockActive (dir <> "/link") `shouldReturn` True

    it "holds read-only flock through the callback and releases it after callback failure" $
        withSystemTempDirectory "worktree-provenance" \dir -> do
            let path = dir <> "/lock"
                os = unsafeEncodeUtf path
            writeFile path "unchanged"
            withExistingReadOnlyLock os (existingSessionLockActive path)
                `shouldReturn` Right True
            failed <- withExistingReadOnlyLock os (ioError (userError "injected") :: IO ())
            failed `shouldSatisfy` isLeft
            existingSessionLockActive path `shouldReturn` False
            readFile path `shouldReturn` "unchanged"

    it "retains old active sessions without a worktree lease, for either session lock" $
        withSystemTempDirectory "worktree-provenance" \dir -> do
            let sessionRoot = unsafeEncodeUtf dir
                meta = sampleMeta checkout old
                sessionDir = sessionRoot </> unsafeEncodeUtf "saved-session"
                initial = buildWorktreeActivity root [meta]
            Dir.createDirectory (dir <> "/saved-session")
            mapM_ (\lockPath -> do
                let path = lockPath sessionDir
                withFileLock path Exclusive \_ -> do
                    result <- protectActiveSessions sessionRoot root [meta] initial
                    Map.lookup checkout result `shouldSatisfy` maybe False isLeft
                protectActiveSessions sessionRoot root [meta] initial `shouldReturn` initial)
                [sessionLockPath, sessionActivityLockPath]

    it "retains symlinked session directories and session roots even with absent locks" $
        withSystemTempDirectory "worktree-provenance" \dir -> do
            let meta = sampleMeta checkout old
                initial = buildWorktreeActivity root [meta]
                blocked path = do
                    result <- protectActiveSessions (unsafeEncodeUtf path) root [meta] initial
                    Map.lookup checkout result `shouldSatisfy` maybe False isLeft
            Dir.createDirectory (dir <> "/target")
            Dir.createDirectoryLink (dir <> "/target") (dir <> "/saved-session")
            blocked dir
            Dir.createDirectoryLink (dir <> "/target") (dir <> "/root-link")
            blocked (dir <> "/root-link")

    it "allows missing session parents without creating them" $
        withSystemTempDirectory "worktree-provenance" \dir -> do
            let meta = sampleMeta checkout old
                initial = buildWorktreeActivity root [meta]
            protectActiveSessions (unsafeEncodeUtf (dir <> "/missing")) root [meta] initial
                `shouldReturn` initial
            Dir.listDirectory dir `shouldReturn` []

    it "preserves persisted activity rather than adopting with today's time" do
        buildWorktreeActivity root [sampleMeta checkout old]
            `shouldBe` Map.singleton checkout (Right old)

    it "uses newest session activity regardless of listing order" do
        let older = sampleMeta checkout old
            newer = (sampleMeta checkout recent) { metaId = "other-session" }
        buildWorktreeActivity root [older, newer]
            `shouldBe` Map.singleton checkout (Right recent)
        buildWorktreeActivity root [newer, older]
            `shouldBe` Map.singleton checkout (Right recent)

    it "includes activity from sessions resumed in checkout subdirectories" do
        buildWorktreeActivity root
            [sampleMeta checkout old, sampleMeta (checkout </> unsafeEncodeUtf "src") recent]
            `shouldBe` Map.singleton checkout (Right recent)

    it "does not infer activity for a checkout without a saved session" do
        Map.lookup checkout (buildWorktreeActivity root []) `shouldBe` Nothing

    it "does not match sibling root prefixes or relative paths" do
        buildWorktreeActivity root
            [ sampleMeta (unsafeEncodeUtf "/managed/worktrees-other/repo/2020-01-01-deadbeef") old
            , sampleMeta (unsafeEncodeUtf "managed/worktrees/repo/2020-01-01-deadbeef") old
            , sampleMeta root old
            , sampleMeta (root </> unsafeEncodeUtf "repo") old
            ] `shouldBe` Map.empty

    it "does not let valid older sessions conceal invalid activity" do
        let invalid = (sampleMeta checkout old) { metaCreatedAt = recent }
            valid = sampleMeta checkout recent
        Map.lookup checkout (buildWorktreeActivity root [invalid, valid])
            `shouldSatisfy` maybe False isLeft
        Map.lookup checkout (buildWorktreeActivity root [valid, invalid])
            `shouldSatisfy` maybe False isLeft

    it "retains uncertain traversing session paths instead of using their timestamp" do
        Map.lookup checkout (buildWorktreeActivity root
            [sampleMeta (checkout </> unsafeEncodeUtf "src/..") old])
            `shouldSatisfy` maybe False isLeft

    it "retains invalid session provenance" do
        Map.lookup checkout (buildWorktreeActivity root
            [(sampleMeta checkout old) { metaId = "" }])
            `shouldSatisfy` maybe False isLeft
        Map.lookup checkout (buildWorktreeActivity root
            [(sampleMeta checkout old) { metaVersion = 2 }])
            `shouldSatisfy` maybe False isLeft

    it "fails closed on incomplete listings instead of hiding a possibly newer session" do
        worktreeActivityFromListing root ([sampleMeta checkout old], ["cannot decode another row"])
            `shouldSatisfy` isLeft

    it "does not filter valid sessions by provider, gateway route or title" do
        let routed = (sampleMeta checkout recent)
                { metaGatewayIdentity = Just "another-route", metaTitle = "archived session" }
        worktreeActivityFromListing root ([sampleMeta checkout old, routed], [])
            `shouldBe` Right (Map.singleton checkout (Right recent))

    it "rejects ambiguous managed roots" do
        worktreeActivityFromListing (unsafeEncodeUtf "relative") ([], [])
            `shouldSatisfy` isLeft
        worktreeActivityFromListing (unsafeEncodeUtf "/managed/../worktrees") ([], [])
            `shouldSatisfy` isLeft

root, checkout :: OsPath
root = unsafeEncodeUtf "/managed/worktrees"
checkout = root </> unsafeEncodeUtf "repo/2020-01-01-deadbeef"

old, recent :: UTCTime
old = posixSecondsToUTCTime 1000000000
recent = addUTCTime 86400 old

sampleMeta :: OsPath -> UTCTime -> SessionMeta
sampleMeta cwd updated =
    SessionMeta
        { metaVersion = 1
        , metaId = "saved-session"
        , metaCreatedAt = old
        , metaUpdatedAt = updated
        , metaProvider = XAIProvider
        , metaConnection = "xai"
        , metaGatewayIdentity = Nothing
        , metaModel = "grok-4.6"
        , metaTransportModel = Nothing
        , metaDialect = GrokBuildDialect
        , metaLegacySubagentTarget = Nothing
        , metaCwd = cwd
        , metaEffort = "high"
        , metaTitle = ""
        , metaTitleIsManual = False
        , metaTitleRefreshIndex = 0
        , metaTitleUserTurns = 0
        , metaLastResponseId = Nothing
        , metaInputTokens = 0
        , metaOutputTokens = 0
        , metaCachedTokens = 0
        , metaLastRecap = Nothing
        , metaLastTurnSummary = Nothing
        , metaLastRecapMainTurns = 0
        , metaPromptSnapshot = Nothing
        }
