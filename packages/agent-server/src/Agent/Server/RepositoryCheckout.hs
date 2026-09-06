module Agent.Server.RepositoryCheckout
    ( RepositoryCheckout(..)
    , prepareRepositoryCheckout
    , cleanupRepositoryCheckout
    , validateDescriptor
    , credentialHelperScript
    , ghWrapperScript
    ) where

import Control.Exception.Safe (tryAny)
import Data.Char (isAlphaNum, isAscii, isControl, isSpace)
import Data.List (isPrefixOf)
import Data.Text (Text)
import Data.Text qualified as Text
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
import System.Posix.Files (setFileMode)
import System.Process
    ( CreateProcess(..)
    , StdStream(NoStream)
    , createProcess
    , interruptProcessGroupOf
    , proc
    , waitForProcess
    )
import System.Timeout qualified as Timeout
import Agent.Server.Types (RepositoryDescriptor(..))

data RepositoryCheckout = RepositoryCheckout
    { checkoutPath :: !FilePath
    , checkoutBranch :: !Text
    , cleanupCheckout :: !(IO ())
    }

prepareRepositoryCheckout
    :: FilePath
    -> Text
    -> RepositoryDescriptor
    -> IO (Either Text RepositoryCheckout)
prepareRepositoryCheckout workspaceRoot correlation descriptor =
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
                runGit workspaceRoot
                    [ "-c", "credential.helper=" <> helper
                    , "-c", "credential.useHttpPath=true"
                    , "clone", "--single-branch"
                    , "--branch", Text.unpack descriptor.repositoryDefaultBranch
                    , Text.unpack descriptor.repositoryCloneUrl
                    , checkout
                    ]
                runGit workspaceRoot ["-C", checkout, "config", "credential.helper", helper]
                runGit workspaceRoot ["-C", checkout, "config", "credential.useHttpPath", "true"]
                runGit workspaceRoot ["-C", checkout, "switch", "-c", Text.unpack branch]
                writeFile
                    (root </> "README")
                    ("GitHub CLI wrapper: " <> ghWrapper <> "\n")
            case outcome of
                Left _ -> do
                    _ <- tryAny cleanup
                    pure (Left "could not prepare repository checkout")
                Right () ->
                    pure (Right RepositoryCheckout{checkoutPath = checkout, checkoutBranch = branch, cleanupCheckout = cleanup})

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

runGit :: FilePath -> [String] -> IO ()
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
                , std_err = NoStream
                , create_group = True
                }
    (_, _, _, processHandle) <- createProcess command
    Timeout.timeout (120 * 1_000_000) (waitForProcess processHandle) >>= \case
        Nothing -> do
            interruptProcessGroupOf processHandle
            _ <- waitForProcess processHandle
            fail "git command timed out"
        Just ExitSuccess -> pure ()
        Just (ExitFailure _) -> fail "git command failed"

credentialHelperScript :: String
credentialHelperScript =
    unlines
        [ "#!/bin/sh"
        , "set -eu"
        , "[ \"${1:-}\" = get ] || exit 0"
        , "dir=$(CDPATH= cd -- \"$(dirname -- \"$0\")\" && pwd)"
        , "protocol= host= path="
        , "while IFS='=' read -r key value; do"
        , "  case \"$key\" in"
        , "    protocol) protocol=$value ;;"
        , "    host) host=$value ;;"
        , "    path) path=$value ;;"
        , "  esac"
        , "done"
        , "[ \"$protocol\" = https ] || exit 0"
        , "[ \"$host\" = github.com ] || exit 0"
        , "[ \"${path%.git}\" = \"$(cat \"$dir/repository\")\" ] || exit 0"
        , "payload=$(jq -cn --arg lease \"$(cat \"$dir/lease\")\" '{lease: $lease}')"
        , "response=$(curl --fail --silent --show-error --max-time 15 \\"
        , "  -H 'Content-Type: application/json' -H 'Accept: application/json' \\"
        , "  --data \"$payload\" \"$(cat \"$dir/endpoint\")\")"
        , "token=$(printf '%s' \"$response\" | jq -er '.password | select(type == \"string\" and length > 0)')"
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
