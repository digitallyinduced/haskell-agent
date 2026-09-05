-- | Public session representations and pagination codecs.
--
-- Keep wire-format policy independent of runtime acquisition and execution.
module Agent.Server.Runtime.SessionCodec
    ( sessionValue
    , modelOptionValue
    , historyValue
    , storeArchiveFilter
    , encodeCursor
    , decodeCursor
    , integerToInt64
    ) where

import Agent.CLI.Models (ModelOption(..), ModelTarget(..))
import Agent.CLI.Session (SessionMeta(..), SessionTurnPage(..))
import Agent.Dialect (dialectSlug)
import Agent.OsPath (unsafeToFilePath)
import Agent.Provider (providerSlug)
import Agent.Server.Event (projectPublicValue)
import Agent.Server.Types (ApiError(..), SessionArchiveFilter(..))
import Agent.Store.Postgres.Session qualified as StoreSession
import Data.Aeson (Value, object, toJSON, (.=))
import Data.Int (Int64)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time.Clock (UTCTime)
import Data.Time.Format (defaultTimeLocale, formatTime, parseTimeM)

sessionValue :: Bool -> SessionMeta -> Value
sessionValue archived meta = object
    [ "id" .= meta.metaId
    , "createdAt" .= meta.metaCreatedAt
    , "updatedAt" .= meta.metaUpdatedAt
    , "provider" .= providerSlug meta.metaProvider
    , "connection" .= meta.metaConnection
    , "model" .= meta.metaModel
    , "transportModel" .= meta.metaTransportModel
    , "dialect" .= dialectSlug meta.metaDialect
    , "cwd" .= unsafeToFilePath meta.metaCwd
    , "effort" .= meta.metaEffort
    , "title" .= meta.metaTitle
    , "titleIsManual" .= meta.metaTitleIsManual
    , "archived" .= archived
    , "usage" .= object
        [ "input" .= meta.metaInputTokens
        , "output" .= meta.metaOutputTokens
        , "cached" .= meta.metaCachedTokens
        ]
    ]

modelOptionValue :: ModelOption -> Value
modelOptionValue option = object
    [ "id" .= option.modelTarget.targetModelId
    , "provider" .= providerSlug option.modelTarget.targetProvider
    , "connection" .= option.modelTarget.targetConnectionId
    , "transportModel" .= option.modelTarget.targetWireModelId
    , "dialect" .= dialectSlug option.modelTarget.targetDialect
    , "label" .= option.modelLabel
    , "contextWindow" .= option.modelContextWindow
    ]

historyValue :: SessionMeta -> Bool -> SessionTurnPage -> Value
historyValue meta archived page = object
    [ "session" .= sessionValue archived meta
    , "data" .=
        [ object
            [ "index" .= index
            , "turn" .= projectPublicValue (toJSON turn)
            ]
        | (index, turn) <- page.pageTurns
        ]
    , "generationStart" .= page.pageGenerationStart
    , "total" .= page.pageTotalTurns
    , "hasOlder" .= page.pageHasOlder
    , "hasNewer" .= page.pageHasNewer
    , "nextCursor" .=
        if page.pageHasOlder
            then fst <$> listToMaybe page.pageTurns
            else Nothing
    ]

storeArchiveFilter
    :: SessionArchiveFilter
    -> StoreSession.SessionArchiveFilter
storeArchiveFilter = \case
    ActiveSessions -> StoreSession.SessionActive
    ArchivedSessions -> StoreSession.SessionArchived
    AllSessions -> StoreSession.SessionAll

encodeCursor :: StoreSession.SessionListCursor -> Text
encodeCursor cursor =
    Text.pack
        (formatTime
            defaultTimeLocale
            cursorTimestampFormat
            cursor.sessionListCursorUpdatedAt)
        <> "|"
        <> cursor.sessionListCursorKey

decodeCursor
    :: Text
    -> Either ApiError StoreSession.SessionListCursor
decodeCursor raw =
    let (timestamp, separatorAndKey) = Text.breakOn "|" raw
        key = Text.drop 1 separatorAndKey
        parsed =
            parseTimeM
                True
                defaultTimeLocale
                cursorTimestampFormat
                (Text.unpack timestamp)
                :: Maybe UTCTime
    in case parsed of
        Just updatedAt
            | not (Text.null separatorAndKey)
            , not (Text.null key) ->
                Right StoreSession.SessionListCursor
                    { sessionListCursorUpdatedAt = updatedAt
                    , sessionListCursorKey = key
                    }
        _ ->
            Left ApiError
                { apiErrorStatus = 400
                , apiErrorCode = "invalid_cursor"
                , apiErrorMessage = "the session cursor is invalid"
                , apiErrorDetails = Nothing
                }

cursorTimestampFormat :: String
cursorTimestampFormat = "%Y-%m-%dT%H:%M:%S%QZ"

integerToInt64 :: Integer -> Either ApiError Int64
integerToInt64 value
    | value < 0
        || value > toInteger (maxBound :: Int64) =
        Left ApiError
            { apiErrorStatus = 400
            , apiErrorCode = "invalid_turn_cursor"
            , apiErrorMessage =
                "the turn cursor must be a non-negative 64-bit integer"
            , apiErrorDetails = Nothing
            }
    | otherwise = Right (fromInteger value)
