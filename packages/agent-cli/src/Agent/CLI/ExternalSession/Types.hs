{-# LANGUAGE DeriveGeneric #-}

module Agent.CLI.ExternalSession.Types
    ( ContentOmissions(..)
    , ExternalCandidate(..)
    , ExternalProvider(..)
    , ExternalSession(..)
    , ExternalSessionEnv(..)
    , ExternalSessionError(..)
    , ExternalTurn(..)
    , ExternalWarning(..)
    , HistoricalToolCall(..)
    , HistoricalToolResult(..)
    , ResumeOperation(..)
    , ResumeOutcome(..)
    , ResumeRequest(..)
    , emptyOmissions
    , providerText
    ) where

import Agent.Tools.Types (ToolEnv)
import Control.Exception.Safe (Exception)
import Data.Aeson
    ( FromJSON(..)
    , ToJSON(..)
    , object
    , withObject
    , (.:)
    , (.=)
    )
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import GHC.Generics (Generic)

data ExternalProvider
    = ExternalClaude
    | ExternalCodex
    | ExternalCursor
    | ExternalGrok
    deriving (Eq, Ord, Show, Enum, Bounded, Generic)

providerText :: ExternalProvider -> Text
providerText = \case
    ExternalClaude -> "claude"
    ExternalCodex -> "codex"
    ExternalCursor -> "cursor"
    ExternalGrok -> "grok"

instance ToJSON ExternalProvider where
    toJSON = toJSON . providerText

data ExternalCandidate = ExternalCandidate
    { candidateProvider :: !ExternalProvider
    , candidateSource :: !Text
    , candidateSessionId :: !Text
    , candidatePath :: !FilePath
    , candidateTitle :: !Text
    , candidateCwd :: !(Maybe FilePath)
    , candidateCreatedAt :: !(Maybe Text)
    , candidateUpdatedAt :: !(Maybe Text)
    , candidateSortTime :: !Double
    } deriving (Eq, Show, Generic)

instance ToJSON ExternalCandidate where
    toJSON candidate = object
        [ "tool" .= candidate.candidateProvider
        , "source" .= candidate.candidateSource
        , "session_id" .= candidate.candidateSessionId
        , "path" .= candidate.candidatePath
        , "title" .= candidate.candidateTitle
        , "cwd" .= candidate.candidateCwd
        , "created_at" .= candidate.candidateCreatedAt
        , "updated_at" .= candidate.candidateUpdatedAt
        ]

instance FromJSON ExternalTurn where
    parseJSON = withObject "ExternalTurn" \objectValue ->
        ExternalTurn
            <$> objectValue .: "role"
            <*> objectValue .: "text"
            <*> objectValue .: "tool_calls"
            <*> objectValue .: "tool_results"

instance FromJSON HistoricalToolResult where
    parseJSON = withObject "HistoricalToolResult" \objectValue ->
        HistoricalToolResult
            <$> objectValue .: "call_id"
            <*> objectValue .: "output"

instance FromJSON HistoricalToolCall where
    parseJSON = withObject "HistoricalToolCall" \objectValue ->
        HistoricalToolCall
            <$> objectValue .: "call_id"
            <*> objectValue .: "name"
            <*> objectValue .: "arguments"

data HistoricalToolCall = HistoricalToolCall
    { historicalCallId :: !Text
    , historicalCallName :: !Text
    , historicalCallArguments :: !Text
    } deriving (Eq, Show, Generic)

instance ToJSON HistoricalToolCall where
    toJSON call = object
        [ "call_id" .= call.historicalCallId
        , "name" .= call.historicalCallName
        , "arguments" .= call.historicalCallArguments
        ]

data HistoricalToolResult = HistoricalToolResult
    { historicalResultCallId :: !Text
    , historicalResultOutput :: !Text
    } deriving (Eq, Show, Generic)

instance ToJSON HistoricalToolResult where
    toJSON result = object
        [ "call_id" .= result.historicalResultCallId
        , "output" .= result.historicalResultOutput
        , "stale" .= ("true" :: Text)
        ]

data ExternalTurn = ExternalTurn
    { externalTurnRole :: !Text
    , externalTurnText :: !Text
    , externalTurnToolCalls :: ![HistoricalToolCall]
    , externalTurnToolResults :: ![HistoricalToolResult]
    } deriving (Eq, Show, Generic)

instance ToJSON ExternalTurn where
    toJSON turn = object
        [ "role" .= turn.externalTurnRole
        , "text" .= turn.externalTurnText
        , "tool_calls" .= turn.externalTurnToolCalls
        , "tool_results" .= turn.externalTurnToolResults
        , "inert" .= True
        , "trust" .= ("untrusted_external_history" :: Text)
        ]

data ExternalWarning = ExternalWarning
    { externalWarningCode :: !Text
    , externalWarningMessage :: !Text
    } deriving (Eq, Show, Generic)

instance ToJSON ExternalWarning where
    toJSON warning = object
        [ "code" .= warning.externalWarningCode
        , "message" .= warning.externalWarningMessage
        ]

data ContentOmissions = ContentOmissions
    { omittedImages :: !Int
    , omittedAttachments :: !Int
    } deriving (Eq, Show, Generic)

instance Semigroup ContentOmissions where
    left <> right = ContentOmissions
        { omittedImages = left.omittedImages + right.omittedImages
        , omittedAttachments =
            left.omittedAttachments + right.omittedAttachments
        }

instance Monoid ContentOmissions where
    mempty = emptyOmissions

emptyOmissions :: ContentOmissions
emptyOmissions = ContentOmissions 0 0

data ExternalSession = ExternalSession
    { externalSessionCandidate :: !ExternalCandidate
    , externalSessionTurns :: ![ExternalTurn]
    , externalSessionWarnings :: ![ExternalWarning]
    , externalSessionLastUserRequest :: !(Maybe Text)
    , externalSessionLastAssistantAction :: !(Maybe Text)
    } deriving (Eq, Show, Generic)

instance ToJSON ExternalSession where
    toJSON session =
        let candidate = session.externalSessionCandidate
        in object
            [ "tool" .= candidate.candidateProvider
            , "source" .= candidate.candidateSource
            , "session_id" .= candidate.candidateSessionId
            , "path" .= candidate.candidatePath
            , "title" .= candidate.candidateTitle
            , "cwd" .= candidate.candidateCwd
            , "created_at" .= candidate.candidateCreatedAt
            , "updated_at" .= candidate.candidateUpdatedAt
            , "turns" .= session.externalSessionTurns
            , "warnings" .= session.externalSessionWarnings
            , "last_user_request" .= session.externalSessionLastUserRequest
            , "last_assistant_action"
                .= session.externalSessionLastAssistantAction
            ]

data ResumeOutcome
    = ResumeResolved !ExternalSession
    | ResumeAmbiguous !Text ![ExternalCandidate]
    | ResumeListed ![ExternalCandidate]
    deriving (Eq, Show, Generic)

instance ToJSON ResumeOutcome where
    toJSON = \case
        ResumeResolved session -> object
            [ "status" .= ("ok" :: Text)
            , "session" .= session
            ]
        ResumeAmbiguous reference candidates -> object
            [ "status" .= ("ambiguous" :: Text)
            , "reference" .= reference
            , "candidates" .= candidates
            ]
        ResumeListed candidates -> object
            [ "status" .= ("ok" :: Text)
            , "candidates" .= candidates
            ]

data ResumeOperation = ResumeShow | ResumeList
    deriving (Eq, Show, Generic)

data ResumeRequest = ResumeRequest
    { resumeProvider :: !ExternalProvider
    , resumeOperation :: !ResumeOperation
    , resumeReference :: !(Maybe Text)
    , resumeWithinMinutes :: !Int
    , resumeMaxToolChars :: !Int
    } deriving (Eq, Show, Generic)

data ExternalSessionEnv = ExternalSessionEnv
    { externalToolEnv :: !ToolEnv
    , externalCwd :: !FilePath
    , externalScratchDirectory :: !FilePath
    , externalHomeDirectory :: !FilePath
    , externalCodexRoot :: !FilePath
    , externalClaudeRoot :: !FilePath
    , externalCursorRoot :: !FilePath
    , externalCursorDesktopStores :: ![FilePath]
    , externalGrokRoot :: !FilePath
    , externalZstdExecutable :: !FilePath
    , externalNow :: !(IO UTCTime)
    }

data ExternalSessionError
    = InvalidExternalSessionRequest !Text
    | ExternalSessionNotFound !Text
    | ExternalSessionReadFailure !Text
    | ExternalSessionAccessDenied !Text
    deriving (Eq, Show)

instance Exception ExternalSessionError
