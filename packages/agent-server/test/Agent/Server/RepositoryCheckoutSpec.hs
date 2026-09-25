module Agent.Server.RepositoryCheckoutSpec (spec) where

import Control.Exception (bracket_, toException)
import Data.List (isInfixOf)
import Data.Maybe (fromMaybe)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.Environment (getEnvironment, setEnv)
import System.Exit (ExitCode(..))
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Posix.Files (setFileMode)
import System.Process
    ( CreateProcess(..)
    , proc
    , readCreateProcessWithExitCode
    )
import Test.Hspec
import Agent.Server.RepositoryCheckout
    ( PreparedRepositoryLayout(..)
    , checkoutExceptionDiagnostic
    , completeRepositoryCheckout
    , credentialHelperScript
    , ghWrapperScript
    , gitDiagnosticFromStderr
    , prepareRepositoryLayout
    , redactGitDiagnostic
    , validateDescriptor
    )
import Agent.Server.Types (RepositoryDescriptor(..))

spec :: Spec
spec =
    describe "repository checkout descriptors" do
        it "accepts a canonical GitHub repository with an HTTPS broker" do
            validDescriptor `shouldSatisfy` isValid

        it "rejects a clone URL that does not match the repository" do
            validDescriptor
                { repositoryCloneUrl = "https://github.com/other/repository.git"
                }
                `shouldSatisfy` isInvalid

        it "rejects broker URLs with userinfo or fragments" do
            validDescriptor
                { repositoryCredentialBrokerUrl = "https://gateway.example@attacker.example/token"
                }
                `shouldSatisfy` isInvalid
            validDescriptor
                { repositoryCredentialBrokerUrl = "https://gateway.example/token#ignored"
                }
                `shouldSatisfy` isInvalid

        it "uses the JSON lease broker contract without exposing the lease as bearer auth" do
            credentialHelperScript `shouldSatisfy` isInfixOf "'{lease: $lease}'"
            credentialHelperScript `shouldSatisfy` isInfixOf "Content-Type: application/json"
            credentialHelperScript `shouldSatisfy` isInfixOf ".password"
            credentialHelperScript `shouldSatisfy` isInfixOf "key=${line%%=*}"
            credentialHelperScript `shouldSatisfy` isInfixOf "set +e"
            credentialHelperScript `shouldSatisfy` isInfixOf "credential broker response did not include a password"
            credentialHelperScript `shouldSatisfy` (not . isInfixOf "Authorization: Bearer")
            ghWrapperScript `shouldSatisfy` isInfixOf "'{lease: $lease}'"
            ghWrapperScript `shouldSatisfy` isInfixOf ".password"

        it "accepts Git capability lines and reports a broker body without a password" do
            withHelper \helper bin -> do
                writeExecutable
                    (bin </> "curl")
                    (unlines
                        [ "#!/bin/sh"
                        , "printf '%s\\n' '{\"username\":\"x-access-token\",\"password\":\"secret-token\",\"expiresAt\":\"2030-01-01T00:00:00Z\"}'"
                        ])
                (okExit, okOut, _) <-
                    runHelper helper bin credentialAttributes
                okExit `shouldBe` ExitSuccess
                okOut `shouldBe` "username=x-access-token\npassword=secret-token\n"
                writeExecutable
                    (bin </> "curl")
                    (unlines
                        [ "#!/bin/sh"
                        , "printf '%s\\n' '{\"username\":\"x-access-token\"}'"
                        ])
                (badExit, badOut, badErr) <-
                    runHelper helper bin credentialAttributes
                badExit `shouldBe` ExitFailure 1
                badOut `shouldSatisfy` (not . isInfixOf "password=")
                badErr `shouldBe` "credential broker response did not include a password\n"

        it "redacts credential material in git diagnostics" do
            let raw =
                    Text.unlines
                        [ "fatal: Authentication failed for 'https://github.com/Prokapi/traumimmo.git/'"
                        , "password=ghs_abcdefghijklmnopqrstuvwxyz0123456789"
                        , "Authorization: Bearer ghp_abcdefghijklmnopqrstuvwxyz"
                        , "broker cdb_live_abcdefghijklmnopqrstuvwxyz"
                        ]
                redacted = redactGitDiagnostic raw
            redacted `shouldSatisfy` Text.isInfixOf "Authentication failed"
            redacted `shouldSatisfy` Text.isInfixOf "password=<redacted>"
            redacted `shouldSatisfy` Text.isInfixOf "Authorization: <redacted>"
            redacted `shouldNotSatisfy` Text.isInfixOf "ghs_"
            redacted `shouldNotSatisfy` Text.isInfixOf "ghp_"
            redacted `shouldNotSatisfy` Text.isInfixOf "cdb_live_"
            let thrown =
                    checkoutExceptionDiagnostic
                        (toException (userError "password=ghs_SECRETVALUE"))
            thrown `shouldSatisfy` Text.isInfixOf "password=<redacted>"
            thrown `shouldNotSatisfy` Text.isInfixOf "ghs_SECRETVALUE"

        it "drops a token split by the stderr cap" do
            let fatal = "\nfatal: Authentication failed"
                input =
                    "ghs_"
                        <> Text.replicate 20000 "s"
                        <> Text.pack fatal
                diagnostic =
                    gitDiagnosticFromStderr (TextEncoding.encodeUtf8 input)
            diagnostic `shouldSatisfy` Text.isInfixOf "Authentication failed"
            diagnostic `shouldNotSatisfy` Text.isInfixOf "ghs_"
            diagnostic `shouldNotSatisfy` Text.isInfixOf (Text.replicate 40 "s")

        it "retries git and keeps the redacted diagnostic and credential files" $
            withSystemTempDirectory "agent-server-checkout-retry" retryKeepsCredentials

        it "retries until git succeeds without removing credential files" $
            withSystemTempDirectory "agent-server-checkout-retry-ok" retryUntilGitSucceeds

        it "rejects refs and broker URLs containing unsafe characters" do
            validDescriptor
                { repositoryDefaultBranch = "--upload-pack=malicious"
                }
                `shouldSatisfy` isInvalid
            validDescriptor
                { repositoryCredentialBrokerUrl = "https://gateway.example/token\nAuthorization: injected"
                }
                `shouldSatisfy` isInvalid

        it "allows loopback HTTP only for local brokers" do
            validDescriptor
                { repositoryCredentialBrokerUrl = "http://127.0.0.1:8080/token"
                }
                `shouldSatisfy` isValid
            validDescriptor
                { repositoryCredentialBrokerUrl = "http://gateway.example/token"
                }
                `shouldSatisfy` isInvalid

validDescriptor :: RepositoryDescriptor
validDescriptor =
    RepositoryDescriptor
        { repositoryFullName = "digitallyinduced/haskell-agent"
        , repositoryCloneUrl = "https://github.com/digitallyinduced/haskell-agent.git"
        , repositoryDefaultBranch = "main"
        , repositoryCredentialBrokerUrl = "https://gateway.example/api/v1/github/repository-token"
        , repositoryCredentialLease = "opaque-lease"
        }

isValid :: RepositoryDescriptor -> Bool
isValid = either (const False) (const True) . validateDescriptor

isInvalid :: RepositoryDescriptor -> Bool
isInvalid = not . isValid

retryKeepsCredentials :: FilePath -> IO ()
retryKeepsCredentials root = do
    layout <-
        prepareRepositoryLayout root "01testcorrelation" validDescriptor
            >>= either (fail . show) pure
    let bin = root </> "bin"
        logPath = root </> "git.log"
    createDirectoryIfMissing True bin
    writeExecutable (bin </> "git") (failingGit logPath)
    result <-
        withPath bin $
            completeRepositoryCheckout layout validDescriptor (\_ _ -> pure ())
    case result of
        Right _ -> expectationFailure "expected checkout failure"
        Left message -> do
            message `shouldSatisfy` Text.isInfixOf "clone:"
            message `shouldSatisfy` Text.isInfixOf "Authentication failed"
            message `shouldSatisfy` Text.isInfixOf "exit 128"
            message `shouldSatisfy` Text.isInfixOf "password=<redacted>"
            message `shouldNotSatisfy` Text.isInfixOf "ghs_SUPERSECRET"
            message `shouldNotSatisfy` Text.isInfixOf "ghp_OTHERTOKEN"
            message `shouldNotSatisfy` Text.isInfixOf "cdb_live_"
            message `shouldNotSatisfy` Text.isInfixOf "checkout still present"
    calls <- readFile logPath
    length (lines calls) `shouldBe` 3
    doesFileExist layout.layoutHelperPath `shouldReturn` True
    doesFileExist (layout.layoutRoot </> "credentials" </> "lease")
        `shouldReturn` True
    layout.layoutCleanup

retryUntilGitSucceeds :: FilePath -> IO ()
retryUntilGitSucceeds root = do
    layout <-
        prepareRepositoryLayout root "01testcorrelation" validDescriptor
            >>= either (fail . show) pure
    let bin = root </> "bin"
        logPath = root </> "git.log"
    createDirectoryIfMissing True bin
    writeExecutable (bin </> "git") (flakyGit logPath)
    result <-
        withPath bin $
            completeRepositoryCheckout layout validDescriptor (\_ _ -> pure ())
    case result of
        Left message -> expectationFailure (Text.unpack message)
        Right _ -> pure ()
    calls <- readFile logPath
    length (lines calls) `shouldBe` 5
    doesFileExist layout.layoutHelperPath `shouldReturn` True
    layout.layoutCleanup

credentialAttributes :: String
credentialAttributes =
    unlines
        [ "capability[]=authtype"
        , "capability[]=state"
        , "protocol=https"
        , "host=github.com"
        , "wwwauth[]=Basic realm=\"a=b\""
        , "path=digitallyinduced/haskell-agent.git"
        , ""
        ]

withHelper :: (FilePath -> FilePath -> IO ()) -> IO ()
withHelper action =
    withSystemTempDirectory "agent-server-credential-helper" \dir -> do
        let helper = dir </> "git-credential-github-app"
            bin = dir </> "bin"
        createDirectoryIfMissing True bin
        writeExecutable helper credentialHelperScript
        writeFile (dir </> "repository") "digitallyinduced/haskell-agent"
        writeFile (dir </> "lease") "opaque-lease"
        writeFile (dir </> "endpoint") "https://broker.example/token"
        writeExecutable (bin </> "jq") jqMock
        action helper bin

jqMock :: String
jqMock =
    unlines
        [ "#!/bin/sh"
        , "if printf '%s\\n' \"$@\" | grep -q -- --arg; then"
        , "  printf '%s\\n' '{\"lease\":\"opaque-lease\"}'"
        , "  exit 0"
        , "fi"
        , "input=$(cat)"
        , "if printf '%s' \"$input\" | grep -q '\"password\":\"secret-token\"'; then"
        , "  printf '%s\\n' 'secret-token'"
        , "  exit 0"
        , "fi"
        , "exit 1"
        ]

runHelper
    :: FilePath
    -> FilePath
    -> String
    -> IO (ExitCode, String, String)
runHelper helper bin input = do
    environment <- getEnvironment
    let patched =
            ("PATH", bin <> ":/bin:/usr/bin")
                : filter (\(name, _) -> name /= "PATH") environment
    readCreateProcessWithExitCode
        (proc helper ["get"]){env = Just patched}
        input

withPath :: FilePath -> IO a -> IO a
withPath bin action = do
    environment <- getEnvironment
    let original = fromMaybe "" (lookup "PATH" environment)
    bracket_
        (setEnv "PATH" (bin <> ":" <> original))
        (setEnv "PATH" original)
        action

writeExecutable :: FilePath -> String -> IO ()
writeExecutable path contents = do
    writeFile path contents
    setFileMode path 0o755

failingGit :: FilePath -> String
failingGit logPath =
    unlines
        [ "#!/bin/sh"
        , "set -eu"
        , "log=" <> shellQuote logPath
        , "count=" <> shellQuote (logPath <> ".count")
        , "printf '%s\\n' \"$*\" >> \"$log\""
        , "n=0"
        , "if [ -f \"$count\" ]; then n=$(cat \"$count\"); fi"
        , "n=$((n + 1))"
        , "printf '%s\\n' \"$n\" > \"$count\""
        , "dest="
        , "is_clone=0"
        , "for arg in \"$@\"; do"
        , "  dest=$arg"
        , "  if [ \"$arg\" = clone ]; then is_clone=1; fi"
        , "done"
        , "if [ \"$is_clone\" -eq 1 ] && [ \"$n\" -gt 1 ] && [ -e \"$dest\" ]; then"
        , "  printf '%s\\n' 'checkout still present on retry' >&2"
        , "  exit 1"
        , "fi"
        , "if [ \"$is_clone\" -eq 1 ]; then mkdir -p \"$dest\"; fi"
        , "printf '%s\\n' 'fatal: Authentication failed for password=ghs_SUPERSECRET Authorization: Bearer ghp_OTHERTOKEN broker cdb_live_GATEWAYSECRET' >&2"
        , "exit 128"
        ]

flakyGit :: FilePath -> String
flakyGit logPath =
    unlines
        [ "#!/bin/sh"
        , "set -eu"
        , "log=" <> shellQuote logPath
        , "count=" <> shellQuote (logPath <> ".count")
        , "printf '%s\\n' \"$*\" >> \"$log\""
        , "n=0"
        , "if [ -f \"$count\" ]; then n=$(cat \"$count\"); fi"
        , "n=$((n + 1))"
        , "printf '%s\\n' \"$n\" > \"$count\""
        , "if [ \"$n\" -eq 1 ]; then"
        , "  printf '%s\\n' 'fatal: password=ghs_SUPERSECRET' >&2"
        , "  exit 128"
        , "fi"
        , "exit 0"
        ]

shellQuote :: String -> String
shellQuote value = "'" <> concatMap quote value <> "'"
  where
    quote '\'' = "'\\''"
    quote character = [character]
