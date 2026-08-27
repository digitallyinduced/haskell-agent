module Agent.CLI.AgentSessions.Render
    ( renderAgentSession
    ) where

import Agent.CLI.Session
    ( SessionActivity(..)
    , SessionMeta(..)
    , SessionTurn(..)
    )
import Agent.Dialect (dialectSlug)
import Agent.OsPath (unsafeToFilePath)
import Agent.Provider (providerSlug)
import Data.Text (Text)
import qualified Data.Text as Text

renderAgentSession
    :: SessionMeta
    -> Text
    -> Maybe SessionActivity
    -> [SessionTurn]
    -> Text
renderAgentSession meta status activity turns =
    Text.intercalate "\n" $
        [ "Session"
        , "  ID: " <> meta.metaId
        , "  Status: " <> status
        , "  Title: " <> meta.metaTitle
        , "  Provider: " <> providerSlug meta.metaProvider
        , "  Connection: " <> meta.metaConnection
        , "  Model: " <> meta.metaModel
        , "  Dialect: " <> dialectSlug meta.metaDialect
        , "  Reasoning effort: " <> meta.metaEffort
        , "  Working directory: " <> Text.pack (unsafeToFilePath meta.metaCwd)
        , "  Created at: " <> Text.pack (show meta.metaCreatedAt)
        , "  Updated at: " <> Text.pack (show meta.metaUpdatedAt)
        ]
            <> maybe [] renderActivity activity
            <> ["", "Recent turns: " <> Text.pack (show (length turns))]
            <> case turns of
                [] -> ["  (none)"]
                _ -> [""] <> intercalateBlank
                    (zipWith renderSessionTurn [1 :: Int ..] turns)

renderActivity :: SessionActivity -> [Text]
renderActivity activity =
    [ ""
    , "Current activity"
    , "  Kind: " <> activity.activityKind
    , "  Message: " <> activity.activityMessage
    ]
        <> maybe []
            (\retryAt -> ["  Retry at: " <> Text.pack (show retryAt)])
            activity.activityRetryAt
        <> ["  Updated at: " <> Text.pack (show activity.activityUpdatedAt)]

renderSessionTurn :: Int -> SessionTurn -> [Text]
renderSessionTurn index turn =
    [ "Turn " <> Text.pack (show index)
    , "At: " <> Text.pack (show turn.turnAt)
    , "User:"
    , indentText turn.turnUserText
    ]
        <> maybe [] (\text -> ["Assistant:", indentText text])
            turn.turnAssistantText
        <> maybe [] (\text -> ["Error:", indentText text])
            turn.turnError

intercalateBlank :: [[Text]] -> [Text]
intercalateBlank = \case
    [] -> []
    first : rest -> first <> concatMap ("" :) rest

indentText :: Text -> Text
indentText = Text.intercalate "\n" . map ("  " <>) . Text.lines
