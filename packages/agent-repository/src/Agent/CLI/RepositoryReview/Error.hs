module Agent.CLI.RepositoryReview.Error
    ( RepositoryError(..)
    , repositoryErrorText
    ) where

import Data.Text (Text)
import qualified Data.Text as Text

data RepositoryError
    = NotARepository !Text
    | StaleRepositorySnapshot !Text !Text
    | InvalidRepositoryRequest !Text
    | RepositoryCommandFailed !Text !Int !Text
    deriving (Eq, Show)

repositoryErrorText :: RepositoryError -> Text
repositoryErrorText = \case
    NotARepository message -> message
    StaleRepositorySnapshot expected actual ->
        "repository changed (expected "
            <> expected
            <> ", actual "
            <> actual
            <> ")"
    InvalidRepositoryRequest message -> message
    RepositoryCommandFailed command code message ->
        command
            <> " exited "
            <> Text.pack (show code)
            <> if Text.null message then "" else ": " <> message
