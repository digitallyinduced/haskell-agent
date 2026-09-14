module Agent.Runtime.Tools.LocalStartupSpec (spec) where

import Agent.Dialect (DialectId(..))
import Agent.Provider (Provider(..))
import Agent.ResourceScope (allocateAcquire)
import Agent.Runtime.Config (HarnessConfig(..), WebFetchConfig(..), defaultHarnessConfig)
import Agent.Runtime.Lsp (LspStartup(..))
import Agent.Runtime.Tools.LocalStartup
import Agent.Runtime.Tools.Resources
import Agent.Runtime.Tools.Startup
import Agent.Tools.Types (defaultToolEnv)
import Control.Concurrent.Async (cancel, withAsync)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, readMVar, takeMVar)
import Control.Exception.Safe (finally, throwIO, tryAny)
import Control.Monad (forM_, void)
import Data.Acquire (Acquire, mkAcquire)
import Data.Either (isLeft)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Maybe (isNothing)
import Data.Text (Text)
import System.Timeout (timeout)
import System.OsPath (unsafeEncodeUtf)
import Test.Hspec

spec :: Spec
spec = describe "local tool startup" do
    it "requires both host extensions and the Grok dialect for web-fetch and LSP" do
        forM_ [False, True] \host ->
            forM_ [CodexDialect, GrokBuildDialect] \dialect ->
                let policy = resolveLocalToolPolicy settings
                        { localHostExtensions = host, localDialectId = dialect }
                in policy.localGrokToolsEnabled
                    `shouldBe` (host && dialect == GrokBuildDialect)

    it "only makes computer use available for OpenAI on Linux and macOS" do
        forM_ [OpenAIProvider, XAIProvider, OpenRouterProvider, GeminiProvider, ClaudeCodeProvider] \provider ->
            forM_ ["linux", "darwin", "mingw32", "freebsd"] \platform ->
                let policy = resolveLocalToolPolicy settings
                        { localProvider = provider, localPlatform = platform }
                in policy.localComputerUseAvailable
                    `shouldBe` (provider == OpenAIProvider && platform `elem` ["linux", "darwin"])

    it "keeps computer availability independent of host extensions, dialect, and exposure" do
        let policy = resolveLocalToolPolicy settings
                { localHostExtensions = False
                , localDialectId = CodexDialect
                , localComputerUseEnabled = False
                }
        policy `shouldBe` LocalToolPolicy False True False

    it "never exposes computer use without a supported runtime" do
        let policy = resolveLocalToolPolicy settings { localPlatform = "mingw32" }
        policy.localComputerToolExposed `shouldBe` False
        (resolveLocalToolPolicy settings).localComputerToolExposed `shouldBe` True

    it "does not evaluate disabled acquisition factories or finalizers" do
        let disabled = selectLocalToolAcquisitions (LocalToolPolicy False False False)
                ("empty LSP" :: Text)
                (error "disabled factories were evaluated" :: LocalToolAcquisitions Text Text Text)
        withSessionResourceScopes \scopes -> do
            resources <- acquireToolStartup scopes (startupFor disabled (pure ()))
            resources.startupWebFetch `shouldBe` Nothing
            resources.startupLsp `shouldBe` "empty LSP"
            resources.startupComputerUse `shouldBe` Nothing

    it "preserves disabled web-fetch and LSP configuration through the concrete factories" do
        env <- defaultToolEnv (unsafeEncodeUtf ".")
        let config = defaultHarnessConfig
            selected = localToolAcquisitions settings { localProvider = XAIProvider }
                config.configWebFetch config.configLsp env
                (\_ -> expectationFailure "unexpected web-fetch failure" >> pure Nothing)
        withSessionResourceScopes \scopes -> do
            resources <- acquireToolStartup scopes (startupFor selected (pure ()))
            isNothing resources.startupWebFetch `shouldBe` True
            isNothing resources.startupLsp.lspStartupRuntime `shouldBe` True
            resources.startupLsp.lspStartupWarnings `shouldBe` []
            isNothing resources.startupComputerUse `shouldBe` True

    it "passes web-fetch initialization failure to the host without advertising a tool" do
        env <- defaultToolEnv (unsafeEncodeUtf ".")
        errors <- newIORef []
        let config = defaultHarnessConfig
            webConfig = config.configWebFetch
                { webFetchEnabled = True, webFetchAllowedDomains = [""] }
            selected = localToolAcquisitions settings { localProvider = XAIProvider }
                webConfig config.configLsp env
                (\err -> record errors err >> pure Nothing)
        withSessionResourceScopes \scopes -> do
            resources <- acquireToolStartup scopes (startupFor selected (pure ()))
            isNothing resources.startupWebFetch `shouldBe` True
        readIORef errors `shouldReturn` ["webFetch allowedDomains must not contain empty entries"]

    it "retains an available computer runtime even when the tool is not exposed" do
        released <- newIORef []
        let policy = resolveLocalToolPolicy settings
                { localHostExtensions = False, localComputerUseEnabled = False }
            selected = selectLocalToolAcquisitions policy "empty LSP" $
                factories released
        withSessionResourceScopes \scopes -> do
            resources <- acquireToolStartup scopes (startupFor selected (pure ()))
            resources.startupComputerUse `shouldBe` Just "computer"
            resources.startupWebFetch `shouldBe` Nothing
            resources.startupLsp `shouldBe` "empty LSP"
            readIORef released `shouldReturn` []
        readIORef released `shouldReturn` ["computer"]

    it "retains typed results and tears down computer, LSP, web-fetch, then scratch" do
        released <- newIORef []
        withSessionResourceScopes \scopes -> do
            retainScratch scopes released
            let selected = selectLocalToolAcquisitions
                    (resolveLocalToolPolicy settings) "empty LSP" (factories released)
            resources <- acquireToolStartup scopes (startupFor selected (pure ()))
            resources.startupWebFetch `shouldBe` Just "web"
            resources.startupLsp `shouldBe` "lsp"
            resources.startupComputerUse `shouldBe` Just "computer"
            readIORef released `shouldReturn` []
        readIORef released `shouldReturn` ["computer", "lsp", "web", "scratch"]

    it "releases completed local acquisitions after a sibling startup fails" do
        released <- newIORef []
        webReady <- newEmptyMVar
        computerReady <- newEmptyMVar
        let candidates :: LocalToolAcquisitions Text Text Text
            candidates = LocalToolAcquisitions
                { localAcquireWebFetch = mkAcquire
                    (putMVar webReady () >> pure (Just "web"))
                    (const (record released "web"))
                , localAcquireLsp = mkAcquire
                    (takeMVar webReady >> takeMVar computerReady
                        >> throwIO (userError "LSP startup failed") :: IO Text)
                    (const (record released "unacquired LSP"))
                , localAcquireComputerUse = mkAcquire
                    (putMVar computerReady () >> pure (Just "computer"))
                    (const (record released "computer"))
                }
        result <- timeout 2000000 $ tryAny $
            withSessionResourceScopes \scopes -> do
                retainScratch scopes released
                void $ acquireToolStartup scopes (startupFor candidates (pure ()))
        result `shouldSatisfy` maybe False isLeft
        readIORef released `shouldReturn` ["computer", "web", "scratch"]

    it "joins a cancelled local acquisition before releasing completed siblings and scratch" do
        released <- newIORef []
        webReady <- newEmptyMVar
        computerReady <- newEmptyMVar
        lspReady <- newEmptyMVar
        blocked <- newEmptyMVar
        let candidates :: LocalToolAcquisitions Text Text Text
            candidates = LocalToolAcquisitions
                { localAcquireWebFetch = mkAcquire
                    (putMVar webReady () >> pure (Just "web"))
                    (const (record released "web"))
                , localAcquireLsp = mkAcquire
                    ((putMVar lspReady () >> readMVar blocked :: IO Text)
                        `finally` record released "LSP acquisition stopped")
                    (const (record released "unacquired LSP"))
                , localAcquireComputerUse = mkAcquire
                    (putMVar computerReady () >> pure (Just "computer"))
                    (const (record released "computer"))
                }
            run = withSessionResourceScopes \scopes -> do
                retainScratch scopes released
                void $ acquireToolStartup scopes (startupFor candidates (pure ()))
        result <- timeout 2000000 $ withAsync run \worker -> do
            mapM_ takeMVar [webReady, computerReady, lspReady]
            cancel worker
        result `shouldBe` Just ()
        readIORef released
            `shouldReturn` ["LSP acquisition stopped", "computer", "web", "scratch"]

settings :: LocalToolSettings
settings = LocalToolSettings
    { localHostExtensions = True
    , localDialectId = GrokBuildDialect
    , localProvider = OpenAIProvider
    , localPlatform = "linux"
    , localComputerUseEnabled = True
    }

factories :: IORef [Text] -> LocalToolAcquisitions Text Text Text
factories released = LocalToolAcquisitions
    { localAcquireWebFetch = owned "web" (Just "web")
    , localAcquireLsp = owned "lsp" "lsp"
    , localAcquireComputerUse = owned "computer" (Just "computer")
    }
  where
    owned :: Text -> a -> Acquire a
    owned name value = mkAcquire (pure value) (const (record released name))

startupFor
    :: LocalToolAcquisitions web lsp computer
    -> IO context
    -> ToolAcquisitions () () (Maybe web) lsp (Maybe computer) context
startupFor acquisitions context = ToolAcquisitions
    { acquireMcp = pure ()
    , acquireCoding = pure ()
    , acquireWebFetch = acquisitions.localAcquireWebFetch
    , acquireLsp = acquisitions.localAcquireLsp
    , acquireComputerUse = acquisitions.localAcquireComputerUse
    , preloadContext = context
    }

retainScratch :: SessionResourceScopes -> IORef [Text] -> IO ()
retainScratch scopes released =
    void $ allocateAcquire scopes.scratchResources $
        mkAcquire (pure ()) (const (record released "scratch"))

record :: IORef [Text] -> Text -> IO ()
record events event =
    atomicModifyIORef' events \previous -> (previous <> [event], ())
