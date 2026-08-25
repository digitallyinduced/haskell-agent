{-# LANGUAGE OverloadedStrings #-}

-- | Pure validation and lexical handling for custom PostgreSQL statements.
module Agent.Store.Postgres.Custom.Sql
    ( normalizeCustomExecution
    , normalizeCustomQuery
    ) where

import Data.Char (isAlpha, isAlphaNum, isSpace)
import Data.List (stripPrefix)
import Data.Text (Text)
import qualified Data.Text as Text

-- | Strip whitespace and trailing semicolons before nesting a user query in a
-- single boundary-encoded Hasql statement.
normalizeCustomQuery :: Text -> Either Text Text
normalizeCustomQuery raw =
    let stripped = Text.dropWhileEnd isTrailing (Text.strip raw)
    in if Text.null stripped
        then Left "database query SQL must not be empty"
        else if Text.any (== '\NUL') stripped
            then Left "database query SQL contains a NUL byte"
            else Right stripped
  where
    isTrailing char = char == ';' || char == ' ' || char == '\t'
        || char == '\r' || char == '\n'

normalizeCustomExecution :: Text -> Either Text Text
normalizeCustomExecution raw = do
    sql <- normalizeCustomQuery raw
    statements <- splitTopLevelStatements sql
    keywords <- traverse leadingSqlKeyword statements
    if not (null keywords)
        && all (`elem` allowedExecutionKeywords) keywords
        then pure sql
        else Left $
            "database execution only accepts DDL/DML statements; "
                <> "transaction and session control are not allowed"

allowedExecutionKeywords :: [Text]
allowedExecutionKeywords =
    [ "alter"
    , "analyze"
    , "comment"
    , "create"
    , "delete"
    , "drop"
    , "insert"
    , "merge"
    , "refresh"
    , "reindex"
    , "truncate"
    , "update"
    , "with"
    ]

leadingSqlKeyword :: Text -> Either Text Text
leadingSqlKeyword sql = do
    stripped <- stripLeadingSqlComments sql
    let keyword = Text.toLower (Text.takeWhile isKeywordChar stripped)
    if Text.null keyword
        then Left "database execution SQL does not start with a SQL keyword"
        else Right keyword
  where
    isKeywordChar char = isAlpha char || char == '_'

stripLeadingSqlComments :: Text -> Either Text Text
stripLeadingSqlComments input =
    let stripped = Text.dropWhile isSpace input
    in case Text.stripPrefix "--" stripped of
        Just rest ->
            stripLeadingSqlComments $
                case Text.break (== '\n') rest of
                    (_, remaining) -> Text.drop 1 remaining
        Nothing ->
            case Text.stripPrefix "/*" stripped of
                Nothing -> Right stripped
                Just rest ->
                    consumeBlockComment 1 rest >>= stripLeadingSqlComments

consumeBlockComment :: Int -> Text -> Either Text Text
consumeBlockComment depth input
    | Text.null input =
        Left "database execution SQL contains an unterminated block comment"
    | Just rest <- Text.stripPrefix "/*" input =
        consumeBlockComment (depth + 1) rest
    | Just rest <- Text.stripPrefix "*/" input =
        if depth == 1
            then Right rest
            else consumeBlockComment (depth - 1) rest
    | otherwise =
        consumeBlockComment depth (Text.drop 1 input)

data SqlLexState
    = SqlNormal
    | SqlSingleQuoted !Bool
    | SqlDoubleQuoted
    | SqlDollarQuoted !String
    | SqlLineComment
    | SqlBlockComment !Int
    deriving (Eq, Show)

-- | Split a PostgreSQL batch only at actual top-level terminators. In
-- particular, semicolons in function bodies, strings, quoted identifiers, and
-- nested comments remain part of their containing statement.
splitTopLevelStatements :: Text -> Either Text [Text]
splitTopLevelStatements input = do
    scanned <- go SqlNormal [] [] (Text.unpack input)
    statements <- finish scanned
    pure $
        filter (not . Text.null . Text.strip) (map Text.pack statements)
  where
    go
        :: SqlLexState
        -> [Char]
        -> [[Char]]
        -> [Char]
        -> Either Text (SqlLexState, [Char], [[Char]])
    go state current statements remainingChars =
        case (state, remainingChars) of
            (SqlNormal, []) ->
                Right (state, current, statements)
            (SqlLineComment, []) ->
                Right (SqlNormal, current, statements)
            (SqlSingleQuoted _, []) ->
                Left "database execution SQL contains an unterminated string"
            (SqlDoubleQuoted, []) ->
                Left "database execution SQL contains an unterminated quoted identifier"
            (SqlDollarQuoted _, []) ->
                Left "database execution SQL contains an unterminated dollar quote"
            (SqlBlockComment _, []) ->
                Left "database execution SQL contains an unterminated block comment"

            (SqlNormal, '-' : '-' : rest) ->
                go SqlLineComment ('-' : '-' : current) statements rest
            (SqlNormal, '/' : '*' : rest) ->
                go (SqlBlockComment 1) ('*' : '/' : current) statements rest
            (SqlNormal, '\'' : rest) ->
                go
                    (SqlSingleQuoted (escapeStringPrefix current))
                    ('\'' : current)
                    statements
                    rest
            (SqlNormal, '"' : rest) ->
                go SqlDoubleQuoted ('"' : current) statements rest
            (SqlNormal, '$' : rest)
                | Just (delimiterTail, remaining) <-
                    dollarDelimiter rest ->
                        let delimiter = '$' : delimiterTail
                        in go
                            (SqlDollarQuoted delimiter)
                            (reverse delimiter <> current)
                            statements
                            remaining
            (SqlNormal, ';' : rest) ->
                go SqlNormal [] (reverse current : statements) rest
            (SqlNormal, char : rest) ->
                go SqlNormal (char : current) statements rest

            (SqlLineComment, '\n' : rest) ->
                go SqlNormal ('\n' : current) statements rest
            (SqlLineComment, char : rest) ->
                go SqlLineComment (char : current) statements rest

            (SqlBlockComment depth, '/' : '*' : rest) ->
                go
                    (SqlBlockComment (depth + 1))
                    ('*' : '/' : current)
                    statements
                    rest
            (SqlBlockComment depth, '*' : '/' : rest)
                | depth == 1 ->
                    go SqlNormal ('/' : '*' : current) statements rest
                | otherwise ->
                    go
                        (SqlBlockComment (depth - 1))
                        ('/' : '*' : current)
                        statements
                        rest
            (SqlBlockComment depth, char : rest) ->
                go (SqlBlockComment depth) (char : current) statements rest

            (SqlSingleQuoted _, '\'' : '\'' : rest) ->
                go state ('\'' : '\'' : current) statements rest
            (SqlSingleQuoted True, '\\' : char : rest) ->
                go state (char : '\\' : current) statements rest
            (SqlSingleQuoted _, '\'' : rest) ->
                go SqlNormal ('\'' : current) statements rest
            (SqlSingleQuoted escaped, char : rest) ->
                go (SqlSingleQuoted escaped) (char : current) statements rest

            (SqlDoubleQuoted, '"' : '"' : rest) ->
                go SqlDoubleQuoted ('"' : '"' : current) statements rest
            (SqlDoubleQuoted, '"' : rest) ->
                go SqlNormal ('"' : current) statements rest
            (SqlDoubleQuoted, char : rest) ->
                go SqlDoubleQuoted (char : current) statements rest

            (SqlDollarQuoted delimiter, remaining)
                | Just rest <- stripPrefix delimiter remaining ->
                    go
                        SqlNormal
                        (reverse delimiter <> current)
                        statements
                        rest
            (SqlDollarQuoted delimiter, char : rest) ->
                go
                    (SqlDollarQuoted delimiter)
                    (char : current)
                    statements
                    rest

    finish (state, current, statements)
        | state /= SqlNormal =
            Left "database execution SQL ended in an invalid lexical state"
        | otherwise =
            Right (reverse (reverse current : statements))

    escapeStringPrefix ['e'] = True
    escapeStringPrefix ('e' : before : _) =
        not (isIdentifierChar before)
    escapeStringPrefix ['E'] = True
    escapeStringPrefix ('E' : before : _) =
        not (isIdentifierChar before)
    escapeStringPrefix _ = False

    isIdentifierChar char = isAlphaNum char || char == '_' || char == '$'

-- Input begins immediately after the first '$'. The returned delimiter tail
-- includes its closing '$'.
dollarDelimiter :: String -> Maybe (String, String)
dollarDelimiter input =
    let (tag, remaining) = span isTagChar input
        validTag = case tag of
            [] -> True
            first : _ -> isAlpha first || first == '_'
    in case remaining of
        '$' : rest
            | validTag -> Just (tag <> "$", rest)
        _ -> Nothing
  where
    isTagChar char = isAlphaNum char || char == '_'
