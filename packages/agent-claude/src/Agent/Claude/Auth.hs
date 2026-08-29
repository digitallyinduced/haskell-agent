{-# LANGUAGE OverloadedStrings #-}

module Agent.Claude.Auth
    ( ClaudeCodeAuth(..)
    , loadClaudeCodeAuth
    , parseClaudeCodeAuthStatus
    ) where

import Agent.Claude.Internal.Environment
    ( sanitizedClaudeEnvironment
    )
import Agent.Claude.Transport
    ( ClaudeCodeTransport(..)
    )
import qualified Agent.Json.Decode as Json
import Control.Exception.Safe (displayException, tryAny)
import Data.ByteString (ByteString)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import System.Directory (findExecutable)
import System.Environment (lookupEnv)
import System.Exit (ExitCode(..))
import System.Process
    ( CreateProcess(env)
    , proc
    , readCreateProcessWithExitCode
    )

data ClaudeCodeAuth = ClaudeCodeAuth
    { executable :: FilePath
    , accountLabel :: Text
    , subscriptionType :: Maybe Text
    , transport :: ClaudeCodeTransport
    } deriving (Eq, Show)

-- | Verify that the installed Claude Code CLI is signed into a first-party
-- claude.ai subscription. This only reads the CLI's status metadata and never
-- reads or returns credential material.
loadClaudeCodeAuth :: IO (Either Text ClaudeCodeAuth)
loadClaudeCodeAuth = do
    gatewayUrl <- nonEmptyEnvironment "HASKELL_AGENT_GATEWAY_URL"
    gatewayToken <- nonEmptyEnvironment "HASKELL_AGENT_GATEWAY_TOKEN"
    case (gatewayUrl, gatewayToken) of
        (Nothing, Nothing) -> loadLocalClaudeCodeAuth
        (Just url, Just token) ->
            resolveClaudeExecutable >>= \case
                Left err -> pure (Left err)
                Right executablePath ->
                    pure $
                        Right ClaudeCodeAuth
                            { executable = executablePath
                            , accountLabel = "Claude via gateway"
                            , subscriptionType = Nothing
                            , transport =
                                ClaudeCodeGateway
                                    { gatewayBaseUrl = url
                                    , gatewayToken = token
                                    }
                            }
        _ ->
            pure $
                Left
                    "Claude gateway mode requires both HASKELL_AGENT_GATEWAY_URL and HASKELL_AGENT_GATEWAY_TOKEN."

loadLocalClaudeCodeAuth :: IO (Either Text ClaudeCodeAuth)
loadLocalClaudeCodeAuth =
    resolveClaudeExecutable >>= \case
        Left err -> pure (Left err)
        Right executablePath -> do
            cleanEnvironment <- sanitizedClaudeEnvironment
            statusResult <- tryAny $
                readCreateProcessWithExitCode
                    (proc executablePath ["auth", "status", "--json"])
                        { env = Just cleanEnvironment }
                    ""
            pure case statusResult of
                Left exception ->
                    Left
                        ( "Unable to query Claude Code authentication status: "
                            <> Text.pack (displayException exception)
                        )
                Right (ExitFailure exitCode, _stdout, stderrText) ->
                    Left
                        ( "Claude Code authentication status failed (exit "
                            <> Text.pack (show exitCode)
                            <> "): "
                            <> conciseProcessError stderrText
                        )
                Right (ExitSuccess, stdoutText, _stderrText) ->
                    parseClaudeCodeAuthStatus
                        executablePath
                        (TextEncoding.encodeUtf8 (Text.pack stdoutText))

nonEmptyEnvironment :: String -> IO (Maybe Text)
nonEmptyEnvironment name =
    lookupEnv name >>= pure . (>>= nonEmptyText . Text.pack)

parseClaudeCodeAuthStatus
    :: FilePath
    -> ByteString
    -> Either Text ClaudeCodeAuth
parseClaudeCodeAuthStatus executablePath bytes =
    case Json.decodeEither authStatusDecoder bytes of
        Left _ ->
            Left "Claude Code returned an unreadable authentication status."
        Right status
            | status.authLoggedIn /= Just True ->
                Left "Claude Code is not logged in."
            | status.authMethod /= Just "claude.ai" ->
                Left
                    "Claude Code is not using a claude.ai subscription login."
            | status.authProvider /= Just "firstParty" ->
                Left
                    "Claude Code is configured to use a third-party API provider."
            | Nothing <- status.authSubscription ->
                Left
                    "Claude Code did not report an active subscription type."
            | Just subscription <- status.authSubscription ->
                Right ClaudeCodeAuth
                    { executable = executablePath
                    , accountLabel =
                        firstNonEmpty
                            [ status.authEmail
                            , status.authAccountLabel
                            , status.authOrgName
                            , status.authOrganizationName
                            ]
                            ("Claude Code (" <> subscription <> ")")
                    , subscriptionType = Just subscription
                    , transport = ClaudeCodeLocalSubscription
                    }

data AuthStatus = AuthStatus
    { authLoggedIn :: !(Maybe Bool)
    , authMethod :: !(Maybe Text)
    , authProvider :: !(Maybe Text)
    , authSubscription :: !(Maybe Text)
    , authEmail :: !(Maybe Text)
    , authAccountLabel :: !(Maybe Text)
    , authOrgName :: !(Maybe Text)
    , authOrganizationName :: !(Maybe Text)
    }

authStatusDecoder :: Json.Decoder AuthStatus
authStatusDecoder = Json.withType \case
    Json.VObject -> Json.object do
        authLoggedIn <- optionalBool "loggedIn"
        authMethod <- optionalNonEmptyText "authMethod"
        authProvider <- optionalNonEmptyText "apiProvider"
        authSubscription <- optionalNonEmptyText "subscriptionType"
        authEmail <- optionalNonEmptyText "email"
        authAccountLabel <- optionalNonEmptyText "accountLabel"
        authOrgName <- optionalNonEmptyText "orgName"
        authOrganizationName <- optionalNonEmptyText "organizationName"
        pure AuthStatus{..}
    _ -> fail "authentication status must be an object"

optionalBool :: Text -> Json.FieldsDecoder (Maybe Bool)
optionalBool key =
    (Json.atKeyOptional key $
        Json.withType \case
            Json.VBoolean -> Just <$> Json.bool
            _ -> pure Nothing)
        >>= pure . (>>= id)

optionalNonEmptyText :: Text -> Json.FieldsDecoder (Maybe Text)
optionalNonEmptyText key = do
    value <- Json.atKeyOptional key $
        Json.withType \case
            Json.VString -> Just <$> Json.text
            _ -> pure Nothing
    pure (value >>= id >>= nonEmptyText)

nonEmptyText :: Text -> Maybe Text
nonEmptyText value =
    let stripped = Text.strip value
    in if Text.null stripped then Nothing else Just stripped

resolveClaudeExecutable :: IO (Either Text FilePath)
resolveClaudeExecutable = do
    configured <- lookupEnv "CLAUDE_CODE_EXECUTABLE"
    case configured of
        Just path
            | not (null (trimString path)) ->
                pure (Right (trimString path))
        _ ->
            findExecutable "claude" >>= \case
                Just path -> pure (Right path)
                Nothing ->
                    pure
                        (Left
                            "Claude Code was not found. Install `claude` or set CLAUDE_CODE_EXECUTABLE."
                        )

firstNonEmpty :: [Maybe Text] -> Text -> Text
firstNonEmpty candidates fallback =
    case catMaybes candidates of
        value : _ -> value
        [] -> fallback

conciseProcessError :: String -> Text
conciseProcessError raw =
    let stripped = Text.strip (Text.pack raw)
    in if Text.null stripped
        then "no diagnostic output"
        else Text.take 500 stripped

trimString :: String -> String
trimString = Text.unpack . Text.strip . Text.pack
