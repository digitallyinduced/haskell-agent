module Agent.Server.RepositoryCheckout
    ( RepositoryCheckout(..)
    , PreparedRepositoryLayout(..)
    , RepositoryCheckoutOperation(..)
    , CheckoutOperationStatus(..)
    , prepareRepositoryLayout
    , repositoryCheckoutOperations
    , completeRepositoryCheckout
    , prepareRepositoryCheckout
    , cleanupRepositoryCheckout
    , validateDescriptor
    , credentialHelperScript
    , ghWrapperScript
    , redactGitDiagnostic
    , gitDiagnosticFromStderr
    , checkoutExceptionDiagnostic
    ) where

import Control.Concurrent.Async (Async, async, cancel, waitCatch)
import Control.Exception (SomeException, displayException)
import Control.Exception.Safe (tryAny)
import Control.Monad (void)
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.Char (isAlphaNum, isAscii, isControl, isSpace)
import Data.List (isPrefixOf)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import Data.Text.Encoding.Error qualified as TextError
import Data.Word (Word8)
import System.Directory
    ( createDirectoryIfMissing
    , getPermissions
    , removePathForcibly
    , setOwnerExecutable
    , setPermissions
    )
import System.Environment (getEnvironment)
import System.Exit (ExitCode(..))
import System.FilePath (normalise, splitDirectories, takeDirectory, (</>))
import System.IO (BufferMode(..), Handle, hClose, hSetBinaryMode, hSetBuffering)
import System.Posix.Files (setFileMode)
import System.Process
    ( CreateProcess(..)
    , StdStream(CreatePipe, NoStream)
    , createProcess
    , interruptProcessGroupOf
    , proc
    , ProcessHandle
    , terminateProcess
    , waitForProcess
    )
import System.Timeout qualified as Timeout
import Agent.Server.Types (RepositoryDescriptor(..))

data RepositoryCheckout = RepositoryCheckout
    { checkoutPath :: !FilePath
    , checkoutBranch :: !Text
    , cleanupCheckout :: !(IO ())
    }

-- | Credential files and the empty checkout directory, before any Git
-- network operations. Session creation uses this so the working directory
-- exists while clone and branch creation run in the background.
data PreparedRepositoryLayout = PreparedRepositoryLayout
    { layoutWorkspaceRoot :: !FilePath
    , layoutRoot :: !FilePath
    , layoutCheckoutPath :: !FilePath
    , layoutHelperPath :: !FilePath
    , layoutBranch :: !Text
    , layoutCleanup :: !(IO ())
    }

data RepositoryCheckoutOperation = RepositoryCheckoutOperation
    { checkoutOperationId :: !Text
    , checkoutOperationCommand :: !Text
    , checkoutOperationArguments :: ![String]
    }

data CheckoutOperationStatus
    = CheckoutOperationRunning
    | CheckoutOperationCompleted
    | CheckoutOperationFailed
    deriving (Eq, Show)

prepareRepositoryLayout
    :: FilePath
    -> Text
    -> RepositoryDescriptor
    -> IO (Either Text PreparedRepositoryLayout)
prepareRepositoryLayout workspaceRoot correlation descriptor =
    case validateDescriptor descriptor of
        Left err -> pure (Left err)
        Right () -> do
            let root = workspaceRoot </> ".haskell-agent" </> "repositories" </> Text.unpack correlation
                checkout = root </> "checkout"
                credentials = root </> "credentials"
                helper = credentials </> "git-credential-github-app"
                ghWrapper = credentials </> "gh"
                branch = "agent/" <> correlation
                cleanup = removePathForcibly root
            outcome <- tryAny do
                createDirectoryIfMissing True credentials
                setFileMode credentials 0o700
                writeFile (credentials </> "endpoint") (Text.unpack descriptor.repositoryCredentialBrokerUrl)
                writeFile (credentials </> "lease") (Text.unpack descriptor.repositoryCredentialLease)
                writeFile (credentials </> "repository") (Text.unpack descriptor.repositoryFullName)
                setFileMode (credentials </> "endpoint") 0o600
                setFileMode (credentials </> "lease") 0o600
                setFileMode (credentials </> "repository") 0o600
                writeFile helper credentialHelperScript
                makeExecutable helper
                writeFile ghWrapper ghWrapperScript
                makeExecutable ghWrapper
                writeFile
                    (root </> "README")
                    ("GitHub CLI wrapper: " <> ghWrapper <> "\n")
            case outcome of
                Left _ -> do
                    _ <- tryAny cleanup
                    pure (Left "could not prepare repository checkout")
                Right () ->
                    pure $
                        Right
                            PreparedRepositoryLayout
                                { layoutWorkspaceRoot = workspaceRoot
                                , layoutRoot = root
                                , layoutCheckoutPath = checkout
                                , layoutHelperPath = helper
                                , layoutBranch = branch
                                , layoutCleanup = cleanup
                                }

repositoryCheckoutOperations
    :: PreparedRepositoryLayout
    -> RepositoryDescriptor
    -> [RepositoryCheckoutOperation]
repositoryCheckoutOperations layout descriptor =
    [ RepositoryCheckoutOperation
        { checkoutOperationId = "clone"
        , checkoutOperationCommand =
            "git clone --single-branch --branch "
                <> descriptor.repositoryDefaultBranch
                <> " "
                <> descriptor.repositoryCloneUrl
        , checkoutOperationArguments =
            [ "-c", "credential.helper=" <> layout.layoutHelperPath
            , "-c", "credential.useHttpPath=true"
            , "clone", "--single-branch"
            , "--branch", Text.unpack descriptor.repositoryDefaultBranch
            , Text.unpack descriptor.repositoryCloneUrl
            , layout.layoutCheckoutPath
            ]
        }
    , RepositoryCheckoutOperation
        { checkoutOperationId = "credential-helper"
        , checkoutOperationCommand =
            "git config credential.helper git-credential-github-app"
        , checkoutOperationArguments =
            ["-C", layout.layoutCheckoutPath, "config", "credential.helper", layout.layoutHelperPath]
        }
    , RepositoryCheckoutOperation
        { checkoutOperationId = "http-path"
        , checkoutOperationCommand = "git config credential.useHttpPath true"
        , checkoutOperationArguments =
            ["-C", layout.layoutCheckoutPath, "config", "credential.useHttpPath", "true"]
        }
    , RepositoryCheckoutOperation
        { checkoutOperationId = "switch-branch"
        , checkoutOperationCommand = "git switch -c " <> layout.layoutBranch
        , checkoutOperationArguments =
            ["-C", layout.layoutCheckoutPath, "switch", "-c", Text.unpack layout.layoutBranch]
        }
    ]

completeRepositoryCheckout
    :: PreparedRepositoryLayout
    -> RepositoryDescriptor
    -> (RepositoryCheckoutOperation -> CheckoutOperationStatus -> IO ())
    -> IO (Either Text RepositoryCheckout)
completeRepositoryCheckout layout descriptor onStep =
    attempt repositoryCheckoutAttempts
  where
    -- A failed attempt removes only the checkout directory. Credential files
    -- stay in place so the next attempt can ask the broker again.
    attempt remaining =
        runCheckoutOperations layout descriptor onStep >>= \case
            Right () ->
                pure $
                    Right
                        RepositoryCheckout
                            { checkoutPath = layout.layoutCheckoutPath
                            , checkoutBranch = layout.layoutBranch
                            , cleanupCheckout = layout.layoutCleanup
                            }
            Left message
                | remaining <= 1 -> pure (Left message)
                | otherwise -> do
                    _ <- tryAny (removePathForcibly layout.layoutCheckoutPath)
                    attempt (remaining - 1)

-- | Create the layout and run every Git operation before returning. Callers
-- that must not block on the network should use 'prepareRepositoryLayout'
-- and 'completeRepositoryCheckout' separately.
prepareRepositoryCheckout
    :: FilePath
    -> Text
    -> RepositoryDescriptor
    -> IO (Either Text RepositoryCheckout)
prepareRepositoryCheckout workspaceRoot correlation descriptor =
    prepareRepositoryLayout workspaceRoot correlation descriptor >>= \case
        Left err -> pure (Left err)
        Right layout ->
            completeRepositoryCheckout layout descriptor (\_ _ -> pure ()) >>= \case
                Left err -> do
                    _ <- tryAny layout.layoutCleanup
                    pure (Left err)
                Right checkout -> pure (Right checkout)

-- | Remove only checkouts created by 'prepareRepositoryCheckout'. The strict
-- shape check prevents an arbitrary session cwd from becoming a deletion
-- target.
cleanupRepositoryCheckout :: FilePath -> IO ()
cleanupRepositoryCheckout checkoutPath =
    case reverse (splitDirectories (normalise checkoutPath)) of
        "checkout" : correlation : "repositories" : ".haskell-agent" : _
            | validCorrelation correlation -> do
                _ <- tryAny (removePathForcibly (takeDirectory checkoutPath))
                pure ()
        _ -> pure ()
  where
    validCorrelation value =
        not (null value)
            && length value <= 64
            && all (\char -> isAlphaNum char || char == '-') value

validateDescriptor :: RepositoryDescriptor -> Either Text ()
validateDescriptor descriptor
    | descriptor.repositoryCloneUrl /= expectedCloneUrl =
        Left "repository clone URL does not match the GitHub repository name"
    | not (validFullName descriptor.repositoryFullName) =
        Left "repository full name is invalid"
    | not (validRef descriptor.repositoryDefaultBranch) =
        Left "repository default branch is invalid"
    | not (validBrokerUrl descriptor.repositoryCredentialBrokerUrl) =
        Left "repository credential broker URL must use HTTPS or loopback HTTP"
    | Text.null descriptor.repositoryCredentialLease
        || Text.length descriptor.repositoryCredentialLease > 512
        || Text.any isControl descriptor.repositoryCredentialLease =
        Left "repository credential lease is invalid"
    | otherwise = Right ()
  where
    expectedCloneUrl =
        "https://github.com/" <> descriptor.repositoryFullName <> ".git"

validFullName :: Text -> Bool
validFullName value =
    case Text.splitOn "/" value of
        [owner, repository] -> validPart owner && validPart repository
        _ -> False
  where
    validPart part =
        not (Text.null part)
            && Text.length part <= 100
            && Text.all (\char -> isAscii char && (isAlphaNum char || char `elem` ("._-" :: String))) part

validRef :: Text -> Bool
validRef ref =
    not (Text.null ref)
        && Text.length ref <= 255
        && not (Text.isPrefixOf "-" ref)
        && not (Text.isPrefixOf "/" ref)
        && not (Text.isSuffixOf "/" ref)
        && not (".." `Text.isInfixOf` ref)
        && Text.all (\char -> isAscii char && (isAlphaNum char || char `elem` ("._/-" :: String))) ref

validBrokerUrl :: Text -> Bool
validBrokerUrl url =
    Text.all (\char -> isAscii char && not (isControl char) && not (isSpace char)) url
        && not ("@" `Text.isInfixOf` url)
        && not ("#" `Text.isInfixOf` url)
        && ( "https://" `Text.isPrefixOf` url
                || "http://127.0.0.1:" `Text.isPrefixOf` url
                || "http://localhost:" `Text.isPrefixOf` url
           )

makeExecutable :: FilePath -> IO ()
makeExecutable path = do
    permissions <- getPermissions path
    setPermissions path (setOwnerExecutable True permissions)
    setFileMode path 0o700

-- | How many times to run the clone, config, and branch steps.
repositoryCheckoutAttempts :: Int
repositoryCheckoutAttempts = 3

-- | Bytes of Git stderr kept from the end of the stream.
maximumGitStderrBytes :: Int
maximumGitStderrBytes = 4096

gitCommandTimeoutMicros :: Int
gitCommandTimeoutMicros = 120 * 1_000_000

data StderrCapture = StderrCapture
    { stderrBytes :: !ByteString
    , stderrTruncated :: !Bool
    , stderrCutInsideToken :: !Bool
    }

emptyStderrCapture :: StderrCapture
emptyStderrCapture =
    StderrCapture
        { stderrBytes = ByteString.empty
        , stderrTruncated = False
        , stderrCutInsideToken = False
        }

runCheckoutOperations
    :: PreparedRepositoryLayout
    -> RepositoryDescriptor
    -> (RepositoryCheckoutOperation -> CheckoutOperationStatus -> IO ())
    -> IO (Either Text ())
runCheckoutOperations layout descriptor onStep =
    go (repositoryCheckoutOperations layout descriptor)
  where
    go [] = pure (Right ())
    go (operation : rest) = do
        onStep operation CheckoutOperationRunning
        runGit layout.layoutWorkspaceRoot operation.checkoutOperationArguments >>= \case
            Left message -> do
                onStep operation CheckoutOperationFailed
                pure (Left (operation.checkoutOperationId <> ": " <> message))
            Right () -> do
                onStep operation CheckoutOperationCompleted
                go rest

runGit :: FilePath -> [String] -> IO (Either Text ())
runGit cwd arguments = do
    inherited <- getEnvironment
    let safeEnvironment =
            filter
                (\(name, _) ->
                    not ("GIT_" `isPrefixOf` name)
                        && name `notElem` ["GH_TOKEN", "GITHUB_TOKEN", "SSH_AUTH_SOCK"])
                inherited
        command =
            (proc "git" arguments)
                { cwd = Just cwd
                , env = Just (("GIT_TERMINAL_PROMPT", "0") : safeEnvironment)
                , std_in = NoStream
                , std_out = NoStream
                , std_err = CreatePipe
                , create_group = True
                }
    tryAny (createProcess command) >>= \case
        Left exception ->
            pure (Left (gitExceptionDiagnostic exception))
        Right (_, _, Just stderrHandle, processHandle) ->
            finishGitProcess stderrHandle processHandle
        Right (_, _, Nothing, processHandle) -> do
            stopGitProcess processHandle
            pure (Left "git command failed")

finishGitProcess :: Handle -> ProcessHandle -> IO (Either Text ())
finishGitProcess stderrHandle processHandle = do
    hSetBinaryMode stderrHandle True
    hSetBuffering stderrHandle NoBuffering
    drained <- async (readBoundedStderr stderrHandle)
    let collect = collectStderr drained
    Timeout.timeout gitCommandTimeoutMicros (waitForProcess processHandle) >>= \case
        Nothing -> do
            stopGitProcess processHandle
            captured <- collect
            pure (Left (gitTimedOutMessage captured))
        Just ExitSuccess -> do
            _ <- collect
            pure (Right ())
        Just (ExitFailure code) -> do
            captured <- collect
            pure (Left (gitFailedMessage code captured))

readBoundedStderr :: Handle -> IO StderrCapture
readBoundedStderr handle = do
    result <- tryAny (loop emptyStderrCapture)
    void (tryAny (hClose handle))
    pure $ case result of
        Left _ -> emptyStderrCapture
        Right captured -> captured
  where
    loop captured = do
        chunk <- ByteString.hGetSome handle 4096
        if ByteString.null chunk
            then pure captured
            else loop (appendStderr captured chunk)

appendStderr :: StderrCapture -> ByteString -> StderrCapture
appendStderr captured chunk =
    let combined = captured.stderrBytes <> chunk
    in if ByteString.length combined <= maximumGitStderrBytes
        then captured{stderrBytes = combined}
        else
            let extra = ByteString.length combined - maximumGitStderrBytes
                discarded = ByteString.take extra combined
                kept = ByteString.drop extra combined
            in StderrCapture
                { stderrBytes = kept
                , stderrTruncated = True
                , stderrCutInsideToken = boundarySplitsToken discarded kept
                }

boundarySplitsToken :: ByteString -> ByteString -> Bool
boundarySplitsToken discarded kept =
    not (ByteString.null discarded)
        && not (ByteString.null kept)
        && isTokenByte (ByteString.last discarded)
        && isTokenByte (ByteString.head kept)

collectStderr :: Async StderrCapture -> IO StderrCapture
collectStderr drained =
    Timeout.timeout (2 * 1_000_000) (waitCatch drained) >>= \case
        Just (Right captured) -> pure captured
        _ -> do
            cancel drained
            void (tryAny (waitCatch drained))
            pure emptyStderrCapture

stopGitProcess :: ProcessHandle -> IO ()
stopGitProcess processHandle = do
    void (tryAny (interruptProcessGroupOf processHandle))
    Timeout.timeout (5 * 1_000_000) (waitForProcess processHandle) >>= \case
        Just _ -> pure ()
        Nothing -> do
            void (tryAny (terminateProcess processHandle))
            void (Timeout.timeout (5 * 1_000_000) (waitForProcess processHandle))

gitFailedMessage :: Int -> StderrCapture -> Text
gitFailedMessage code captured =
    withGitDetail
        ("git command failed (exit " <> Text.pack (show code) <> ")")
        captured

gitTimedOutMessage :: StderrCapture -> Text
gitTimedOutMessage = withGitDetail "git command timed out"

withGitDetail :: Text -> StderrCapture -> Text
withGitDetail summary captured =
    let detail = gitStderrDiagnostic captured
    in if Text.null detail then summary else summary <> ": " <> detail

gitExceptionDiagnostic :: SomeException -> Text
gitExceptionDiagnostic exception =
    let rendered = redactGitDiagnostic (Text.pack (displayException exception))
    in if Text.null rendered then "git command failed" else rendered

-- | Text shown when checkout itself throws. Secrets from the exception
-- text are removed; an empty result falls back to the generic failure.
checkoutExceptionDiagnostic :: SomeException -> Text
checkoutExceptionDiagnostic exception =
    let rendered = redactGitDiagnostic (Text.pack (displayException exception))
    in if Text.null rendered
        then "could not prepare repository checkout"
        else rendered

-- | Redact credential material from a Git or helper diagnostic.
redactGitDiagnostic :: Text -> Text
redactGitDiagnostic value =
    Text.strip $
        Text.unlines $
            map
                (redactLongTokenRuns . redactTokenPrefixes . redactPasswordValues . redactAuthorization)
                (Text.lines (Text.map sanitizeGitControl value))

-- | Apply the same stderr cap used for a live Git process.
gitDiagnosticFromStderr :: ByteString -> Text
gitDiagnosticFromStderr bytes =
    gitStderrDiagnostic (appendStderr emptyStderrCapture bytes)

gitStderrDiagnostic :: StderrCapture -> Text
gitStderrDiagnostic captured =
    let decoded =
            TextEncoding.decodeUtf8With
                TextError.lenientDecode
                captured.stderrBytes
        dropped =
            if captured.stderrCutInsideToken
                then Text.dropWhile isTokenChar decoded
                else decoded
        redacted = redactGitDiagnostic dropped
    in if captured.stderrTruncated && not (Text.null redacted)
        then "…" <> redacted
        else redacted

sanitizeGitControl :: Char -> Char
sanitizeGitControl character
    | character == '\n' || character == '\t' = character
    | character == '\r' = '\n'
    | isControl character = ' '
    | otherwise = character

redactAuthorization :: Text -> Text
redactAuthorization line =
    let lowered = Text.toLower line
    in case Text.breakOn "authorization" lowered of
        (before, rest)
            | Text.null rest -> line
            | otherwise ->
                Text.take (Text.length before) line <> "Authorization: <redacted>"

redactPasswordValues :: Text -> Text
redactPasswordValues = go
  where
    marker = "password="
    go remaining =
        case Text.breakOn marker remaining of
            (before, rest)
                | Text.null rest -> remaining
                | otherwise ->
                    let afterName = Text.drop (Text.length marker) rest
                        (_secret, after) = Text.span (not . isSpace) afterName
                    in before <> "password=<redacted>" <> go after

redactTokenPrefixes :: Text -> Text
redactTokenPrefixes =
    redactTokenPrefix "cdb_live_"
        . redactTokenPrefix "ghs_"
        . redactTokenPrefix "ghp_"

redactTokenPrefix :: Text -> Text -> Text
redactTokenPrefix prefix = go
  where
    go remaining =
        case Text.breakOn prefix remaining of
            (before, rest)
                | Text.null rest -> remaining
                | otherwise ->
                    let afterPrefix = Text.drop (Text.length prefix) rest
                        (_body, after) = Text.span isTokenChar afterPrefix
                    in before <> "<redacted>" <> go after

-- Drop a long opaque run. Checkout diagnostics do not need one, and a
-- token body is at least this long.
redactLongTokenRuns :: Text -> Text
redactLongTokenRuns = go
  where
    go remaining =
        let (plain, rest) = Text.span (not . isTokenChar) remaining
            (run, after) = Text.span isTokenChar rest
        in if Text.null rest
            then remaining
            else
                plain
                    <> (if Text.length run >= 32 then "<redacted>" else run)
                    <> go after

isTokenChar :: Char -> Bool
isTokenChar character =
    isAscii character && (isAlphaNum character || character == '_' || character == '-')

isTokenByte :: Word8 -> Bool
isTokenByte byte =
    (byte >= 48 && byte <= 57)
        || (byte >= 65 && byte <= 90)
        || (byte >= 97 && byte <= 122)
        || byte == 95
        || byte == 45

credentialHelperScript :: String
credentialHelperScript =
    unlines
        [ "#!/bin/sh"
        , "set -eu"
        , "[ \"${1:-}\" = get ] || exit 0"
        , "dir=$(CDPATH= cd -- \"$(dirname -- \"$0\")\" && pwd)"
        , "protocol= host= path="
        , "# Git sends capability[]= and other keys whose values may contain '='."
        , "# Leave errexit off so EOF from read cannot skip the broker exchange."
        , "set +e"
        , "while IFS= read -r line; do"
        , "  [ -n \"$line\" ] || continue"
        , "  key=${line%%=*}"
        , "  value=${line#*=}"
        , "  case \"$key\" in"
        , "    protocol) protocol=$value ;;"
        , "    host) host=$value ;;"
        , "    path) path=$value ;;"
        , "  esac"
        , "done"
        , "set -e"
        , "[ \"$protocol\" = https ] || exit 0"
        , "[ \"$host\" = github.com ] || exit 0"
        , "[ \"${path%.git}\" = \"$(cat \"$dir/repository\")\" ] || exit 0"
        , "payload=$(jq -cn --arg lease \"$(cat \"$dir/lease\")\" '{lease: $lease}')"
        , "response=$(curl --fail --silent --show-error --max-time 15 \\"
        , "  -H 'Content-Type: application/json' -H 'Accept: application/json' \\"
        , "  --data \"$payload\" \"$(cat \"$dir/endpoint\")\")"
        , "token=$(printf '%s' \"$response\" | jq -er '.password | select(type == \"string\" and length > 0)' 2>/dev/null) || {"
        , "  printf '%s\\n' 'credential broker response did not include a password' >&2"
        , "  exit 1"
        , "}"
        , "printf 'username=x-access-token\\npassword=%s\\n' \"$token\""
        ]

ghWrapperScript :: String
ghWrapperScript =
    unlines
        [ "#!/bin/sh"
        , "set -eu"
        , "dir=$(CDPATH= cd -- \"$(dirname -- \"$0\")\" && pwd)"
        , "payload=$(jq -cn --arg lease \"$(cat \"$dir/lease\")\" '{lease: $lease}')"
        , "response=$(curl --fail --silent --show-error --max-time 15 \\"
        , "  -H 'Content-Type: application/json' -H 'Accept: application/json' \\"
        , "  --data \"$payload\" \"$(cat \"$dir/endpoint\")\")"
        , "GH_TOKEN=$(printf '%s' \"$response\" | jq -er '.password | select(type == \"string\" and length > 0)')"
        , "export GH_TOKEN"
        , "exec gh \"$@\""
        ]
