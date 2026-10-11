-- | Nonblocking clarification questions, independent of plan mode or any
-- particular delivery surface. Hosts own presentation and reply routing.
module Agent.Tools.AsyncQuestions
    ( AsyncQuestion(..)
    , RequestUserInputAsyncArgs(..)
    , requestUserInputAsyncParameters
    , requestUserInputAsyncDescription
    , requestUserInputAsyncTool
    , executeRequestUserInputAsync
    ) where

import Agent.Json (rawJsonBytes, rawJsonDecoder)
import Agent.Json.Decode (Decoder)
import Agent.ToolDSL (PropertySchema(..), PropertyType(..))
import Agent.ToolDispatch (typedTool)
import Agent.Tools.Types (AppTool, ToolExecutionPolicy(..), jsonTool)
import Control.Monad (when)
import Data.Aeson (FromJSON(..), withObject, (.:), (.:?))
import qualified Data.Aeson as Aeson
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text

data AsyncQuestion = AsyncQuestion
    { title :: !Text
    , options :: ![Text]
    } deriving (Eq, Show)

newtype RequestUserInputAsyncArgs = RequestUserInputAsyncArgs
    { questions :: [AsyncQuestion]
    } deriving (Eq, Show)

instance FromJSON AsyncQuestion where
    parseJSON = withObject "AsyncQuestion" \object -> do
        title <- object .: "title"
        options <- object .:? "options"
        when (Text.null (Text.strip title)) $
            fail "Question title must not be empty"
        case options of
            Just choices | null choices || any (Text.null . Text.strip) choices ->
                fail "Question options must contain nonempty strings"
            _ -> pure (AsyncQuestion title (fromMaybe [] options))

instance FromJSON RequestUserInputAsyncArgs where
    parseJSON = withObject "RequestUserInputAsyncArgs" \object -> do
        questions <- object .: "questions"
        when (null questions) $ fail "questions must not be empty"
        pure (RequestUserInputAsyncArgs questions)

-- Keep one validation implementation for native and Aeson-based hosts.
requestUserInputAsyncDecoder :: Decoder RequestUserInputAsyncArgs
requestUserInputAsyncDecoder = do
    raw <- rawJsonDecoder
    either fail pure (Aeson.eitherDecodeStrict' (rawJsonBytes raw))

requestUserInputAsyncDescription :: Text
requestUserInputAsyncDescription =
    "Ask the user one or more questions without waiting for a reply. "
        <> "Continue independent work after this tool returns. The user replies "
        <> "with an ordinary message, which may steer the active turn. "
        <> "Do not assume an answer, poll for it, or use this tool to obtain approval "
        <> "for an action that requires explicit permission. Use ask_user_question "
        <> "when progress depends on the answer."

requestUserInputAsyncParameters :: [PropertySchema]
requestUserInputAsyncParameters =
    [ PropertySchema "questions" (PropertyArray (PropertyObject
        [ PropertySchema "title" PropertyString True (Just "The question to ask.")
        , PropertySchema "options" (PropertyArray PropertyString) False
            (Just "Optional suggested answers. Omit for a free-text question.")
        ])) True (Just "One or more questions.")
    ]

-- | Delivery must publish the structured questions without waiting for an
-- answer. Replies enter through the host's ordinary user-message path.
-- Acknowledgment means delivery succeeded, not that the user answered.
executeRequestUserInputAsync
    :: (RequestUserInputAsyncArgs -> IO (Either Text ()))
    -> RequestUserInputAsyncArgs
    -> IO (Either Text Text)
executeRequestUserInputAsync deliver request =
    fmap (fmap (const "{\"accepted\":true}")) (deliver request)

requestUserInputAsyncTool
    :: (RequestUserInputAsyncArgs -> IO (Either Text ()))
    -> AppTool
requestUserInputAsyncTool deliver = jsonTool "request_user_input_async"
    requestUserInputAsyncDescription
    requestUserInputAsyncParameters
    True
    TurnSequential
    (typedTool "request_user_input_async" requestUserInputAsyncDecoder
        (executeRequestUserInputAsync deliver))
