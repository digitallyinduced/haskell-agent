-- | Safe helpers for invoking user-selected terminal programs.
--
-- Program specifications are parsed into an executable and arguments and are
-- always passed directly to 'System.Process'; no shell is involved.
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
module Agent.CLI.ExternalProgram
    ( ExternalProgram(..)
    , parseProgramWords
    , parseExternalProgram
    , resolveExternalProgram
    , runExternalProgramOnFile
    , withTemporaryTextFile
    , normalizeEditedText
    ) where

import Control.Exception.Safe
    ( catchAny
    , finally
    , onException
    , tryAny
    )
import Data.Char (isSpace)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import System.Directory
    ( getTemporaryDirectory
    , removeFile
    )
import System.Environment (lookupEnv)
import System.Exit (ExitCode(..))
import System.IO
    ( hClose
    , openTempFile
    )
import System.Process
    ( proc
    , terminateProcess
    , waitForProcess
    , withCreateProcess
    )

data ExternalProgram = ExternalProgram
    { externalProgramExecutable :: !FilePath
    , externalProgramArguments :: ![String]
    }
    deriving (Eq, Show)

data ProgramQuote
    = Unquoted
    | SingleQuoted
    | DoubleQuoted
    deriving (Eq)

-- | Parse a quoted/escaped program specification. Single quotes preserve all
-- characters; double quotes permit backslash escapes; and backslash outside
-- quotes escapes the following character.
parseProgramWords :: Text -> Either Text [Text]
parseProgramWords input =
    map Text.pack <$> go Unquoted False False [] [] (Text.unpack input)
  where
    go quote escaped started current completed = \case
        [] | escaped -> Left "program specification ends with an incomplete escape"
           | quote /= Unquoted ->
                Left "program specification contains an unterminated quote"
           | started -> Right (reverse (reverse current : completed))
           | otherwise -> Right (reverse completed)
        char : rest
            | escaped -> go quote False True (char : current) completed rest
            | quote == SingleQuoted ->
                if char == '\''
                    then go Unquoted False True current completed rest
                    else go quote False True (char : current) completed rest
            | quote == DoubleQuoted ->
                case char of
                    '"' -> go Unquoted False True current completed rest
                    '\\' -> go quote True True current completed rest
                    _ -> go quote False True (char : current) completed rest
            | isSpace char ->
                if started
                    then go Unquoted False False []
                        (reverse current : completed) rest
                    else go Unquoted False False [] completed rest
            | otherwise ->
                case char of
                    '\'' -> go SingleQuoted False True current completed rest
                    '"' -> go DoubleQuoted False True current completed rest
                    '\\' -> go Unquoted True True current completed rest
                    _ -> go Unquoted False True (char : current) completed rest

parseExternalProgram :: Text -> Either Text ExternalProgram
parseExternalProgram specification =
    case parseProgramWords specification of
        Left err -> Left err
        Right [] -> Left "program specification must not be empty"
        Right (executable : arguments)
            | Text.null (Text.strip executable) ->
                Left "program executable must not be empty"
            | otherwise ->
                Right
                    ExternalProgram
                        { externalProgramExecutable = Text.unpack executable
                        , externalProgramArguments = map Text.unpack arguments
                        }

-- | Resolve the first non-empty environment variable, falling back to the
-- supplied program specification. A malformed non-empty variable is an error.
resolveExternalProgram
    :: [(String, Text)]
    -> Text
    -> IO (Either Text ExternalProgram)
resolveExternalProgram variables fallback = go variables
  where
    go [] = pure (parseExternalProgram fallback)
    go ((name, label) : rest) =
        lookupEnv name >>= \case
            Just value
                | not (null value) ->
                    pure $
                        case parseExternalProgram (Text.pack value) of
                            Left err ->
                                Left
                                    (label <> " is invalid: " <> err)
                            Right program -> Right program
            _ -> go rest

-- | Run a program with a temporary file path appended to its arguments.
runExternalProgramOnFile
    :: ExternalProgram
    -> FilePath
    -> IO (Either Text ())
runExternalProgramOnFile program path = do
    result <- tryAny do
        withCreateProcess
            (proc
                program.externalProgramExecutable
                (program.externalProgramArguments <> [path]))
            \_ _ _ processHandle ->
                waitForProcess processHandle `onException` do
                    terminateProcess processHandle
                    _ <- waitForProcess processHandle
                    pure ()
    pure $
        case result of
            Left exception ->
                Left
                    ( "could not launch "
                        <> Text.pack program.externalProgramExecutable
                        <> ": "
                        <> Text.pack (show exception)
                    )
            Right ExitSuccess -> Right ()
            Right (ExitFailure code) ->
                Left
                    ( Text.pack program.externalProgramExecutable
                        <> " exited with status "
                        <> Text.pack (show code)
                    )

-- | Create a temporary UTF-8 text file, invoke an action with its path, and
-- remove the file even when the action fails or throws.
withTemporaryTextFile
    :: String
    -> Text
    -> (FilePath -> IO a)
    -> IO a
withTemporaryTextFile prefix contents action = do
    temporaryDirectory <- getTemporaryDirectory
    bracketTemp temporaryDirectory
  where
    bracketTemp temporaryDirectory = do
        (path, handle) <- openTempFile temporaryDirectory prefix
        (hClose handle >> Text.writeFile path contents >> action path)
            `finally` removeQuietly path
    removeQuietly path =
        removeFile path `catchAny` const (pure ())

normalizeEditedText :: Text -> Text
normalizeEditedText text
    | "\r\n" `Text.isSuffixOf` text = Text.dropEnd 2 text
    | "\n" `Text.isSuffixOf` text = Text.dropEnd 1 text
    | otherwise = text
