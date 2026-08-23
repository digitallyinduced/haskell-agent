module Claude.Agent.SDK.TestSupport
    ( testSessionId
    , testOptions
    , withFakeClaude
    , oneShotScript
    , shellQuote
    ) where

import Claude.Agent.SDK
    ( ClaudeAgentOptions(..)
    , defaultClaudeAgentOptions
    )
import Control.Exception.Safe (bracket)
import Data.Text (Text)
import qualified Data.Text as Text
import System.Directory
    ( createDirectory
    , getTemporaryDirectory
    , removeFile
    , removePathForcibly
    )
import System.FilePath ((</>))
import System.IO
    ( hClose
    , openTempFile
    )
import System.Posix.Files (setFileMode)

testSessionId :: Text
testSessionId = "123e4567-e89b-42d3-a456-426614174000"

testOptions :: FilePath -> FilePath -> ClaudeAgentOptions
testOptions executable cwd =
    (defaultClaudeAgentOptions executable cwd)
        { sessionId = Just testSessionId
        , environment = Just []
        , promptWriteTimeoutMicros = 2_000_000
        , streamStartupTimeoutMicros = 2_000_000
        , streamInactivityTimeoutMicros = 2_000_000
        , turnTimeoutMicros = 5_000_000
        }

withFakeClaude
    :: String
    -> (FilePath -> FilePath -> IO a)
    -> IO a
withFakeClaude body action =
    withTemporaryDirectory "claude-agent-sdk-test" \directory -> do
        let executable = directory </> "claude"
        writeFile executable body
        setFileMode executable 0o700
        action directory executable

oneShotScript :: [Text] -> String
oneShotScript outputLines =
    Text.unpack $
        Text.unlines $
            [ "#!/bin/sh"
            , "IFS= read -r _query"
            ]
                <> map
                    (\line -> "printf '%s\\n' " <> shellQuote line)
                    outputLines

shellQuote :: Text -> Text
shellQuote value =
    "'" <> Text.replace "'" "'\"'\"'" value <> "'"

withTemporaryDirectory
    :: String
    -> (FilePath -> IO a)
    -> IO a
withTemporaryDirectory label =
    bracket acquire removePathForcibly
  where
    acquire = do
        temporaryRoot <- getTemporaryDirectory
        (path, handle) <-
            openTempFile temporaryRoot (label <> "-")
        hClose handle
        removeFile path
        createDirectory path
        pure path
