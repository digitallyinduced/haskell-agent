{-# LANGUAGE OverloadedStrings #-}

module Agent.ClaudeCode.Auth
    ( ClaudeCodeAuth(..)
    , loadClaudeCodeAuth
    , parseClaudeCodeAuthStatus
    ) where

import Agent.ClaudeCode.Internal.Environment
    ( sanitizedClaudeEnvironment
    )
import Control.Exception.Safe (displayException, tryAny)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.ByteString (ByteString)
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
    } deriving (Eq, Show)

-- | Verify that the installed Claude Code CLI is signed into a first-party
-- claude.ai subscription. This only reads the CLI's status metadata and never
-- reads or returns credential material.
loadClaudeCodeAuth :: IO (Either Text ClaudeCodeAuth)
loadClaudeCodeAuth = do
    resolved <- resolveClaudeExecutable
    case resolved of
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

parseClaudeCodeAuthStatus
    :: FilePath
    -> ByteString
    -> Either Text ClaudeCodeAuth
parseClaudeCodeAuthStatus executablePath bytes = do
    value <- case Aeson.eitherDecodeStrict' bytes of
        Left _ ->
            Left "Claude Code returned an unreadable authentication status."
        Right decoded ->
            Right decoded
    object <- case value of
        Aeson.Object fields -> Right fields
        _ -> Left "Claude Code returned an invalid authentication status."
    requireBool "loggedIn" True object
        "Claude Code is not logged in."
    requireText "authMethod" "claude.ai" object
        "Claude Code is not using a claude.ai subscription login."
    requireText "apiProvider" "firstParty" object
        "Claude Code is configured to use a third-party API provider."
    subscription <- case nonEmptyTextAt "subscriptionType" object of
        Nothing ->
            Left "Claude Code did not report an active subscription type."
        Just valueText ->
            Right valueText
    let label =
            firstNonEmpty
                [ nonEmptyTextAt "email" object
                , nonEmptyTextAt "accountLabel" object
                , nonEmptyTextAt "orgName" object
                , nonEmptyTextAt "organizationName" object
                ]
                ("Claude Code (" <> subscription <> ")")
    Right ClaudeCodeAuth
        { executable = executablePath
        , accountLabel = label
        , subscriptionType = Just subscription
        }

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

requireBool
    :: Text
    -> Bool
    -> Aeson.Object
    -> Text
    -> Either Text ()
requireBool key expected object err =
    case KeyMap.lookup (Key.fromText key) object of
        Just (Aeson.Bool actual)
            | actual == expected -> Right ()
        _ -> Left err

requireText
    :: Text
    -> Text
    -> Aeson.Object
    -> Text
    -> Either Text ()
requireText key expected object err =
    case nonEmptyTextAt key object of
        Just actual
            | actual == expected -> Right ()
        _ -> Left err

nonEmptyTextAt :: Text -> Aeson.Object -> Maybe Text
nonEmptyTextAt key object =
    case KeyMap.lookup (Key.fromText key) object of
        Just (Aeson.String raw) ->
            let stripped = Text.strip raw
            in if Text.null stripped then Nothing else Just stripped
        _ -> Nothing

firstNonEmpty :: [Maybe Text] -> Text -> Text
firstNonEmpty candidates fallback =
    case [value | Just value <- candidates] of
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
