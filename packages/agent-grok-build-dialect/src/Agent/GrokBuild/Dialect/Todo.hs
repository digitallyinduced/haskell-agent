-- | Grok Build's structured progress-list tool.
module Agent.GrokBuild.Dialect.Todo
    ( todoWriteTool
    ) where

import Agent.ToolDSL (PropertySchema(..), PropertyType(..))
import Agent.ToolDispatch (ToolCall, typedTool)
import Agent.GrokBuild.Dialect.Common (jsonTool)
import Agent.GrokBuild.Dialect.Json (optionalBool, optionalTextValue)
import qualified Agent.Json.Decode as Json
import Agent.GrokBuild.Dialect.Shell (GrokSession(..))
import Agent.Tools.Scheduling
    ( ToolAccess(..)
    , ToolResource(..)
    , ToolResourceClaim(..)
    )
import Control.Monad (join)
import Agent.Tools.Types
    ( AppTool
    , ToolExecutionPolicy(..)
    , withToolResourceClaims
    )
import Control.Monad (foldM)
import Data.IORef (atomicModifyIORef')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text

data TodoStatus
    = TodoPending
    | TodoInProgress
    | TodoCompleted
    | TodoCancelled
    deriving (Eq, Show)

todoStatusDecoder :: Json.Decoder TodoStatus
todoStatusDecoder = Json.withText \case
        "pending" -> pure TodoPending
        "in_progress" -> pure TodoInProgress
        "completed" -> pure TodoCompleted
        "cancelled" -> pure TodoCancelled
        other ->
            fail $
                "Unknown todo status: "
                    <> Text.unpack other
                    <> ". Expected pending, in_progress, completed, or cancelled."

data TodoUpdate = TodoUpdate
    { todoId :: !Text
    , todoContent :: !(Maybe Text)
    , todoStatus :: !(Maybe TodoStatus)
    }

todoUpdateDecoder :: Json.Decoder TodoUpdate
todoUpdateDecoder = Json.object $
    TodoUpdate
        <$> Json.atKey "id" Json.text
        <*> optionalTextValue "content"
        <*> (join <$> Json.atKeyOptional "status" (Json.nullable todoStatusDecoder))

data TodoWriteArgs = TodoWriteArgs
    { merge :: !Bool
    , todos :: ![TodoUpdate]
    }

todoWriteArgsDecoder :: Json.Decoder TodoWriteArgs
todoWriteArgsDecoder = Json.object $
    TodoWriteArgs
        <$> (fromMaybe True <$> optionalBool "merge")
        <*> Json.atKey "todos" (Json.list todoUpdateDecoder)

todoWriteTool :: GrokSession -> AppTool
todoWriteTool session =
    withToolResourceClaims todoWriteResourceClaims $
    jsonTool "todo_write" todoWriteDescription
    [ PropertySchema "merge" PropertyBoolean False $ Just
        "Optional. When true (default), merges the provided todos into the existing list by id. When false, the provided todos replace the existing list."
    , PropertySchema "todos" (PropertyArray todoUpdateSchema) True $ Just
        "Array of todo items to write to the workspace."
    ]
    True
    TurnSequential
    (typedTool "todo_write" todoWriteArgsDecoder (runTodoWrite session))
  where
    todoUpdateSchema :: PropertyType
    todoUpdateSchema = PropertyObject
        [ PropertySchema "id" PropertyString True $ Just
            "Unique identifier for the todo item."
        , PropertySchema "content" PropertyString False $ Just
            "The description/content of the todo item."
        , PropertySchema "status"
            (PropertyEnum ["pending", "in_progress", "completed", "cancelled"])
            False
            (Just "The status of the todo item.")
        ]

todoWriteResourceClaims
    :: ToolCall
    -> IO (Either Text [ToolResourceClaim])
todoWriteResourceClaims _ =
    pure $ Right
        [ToolResourceClaim ToolWrite (ToolNamedResource "todos")]

todoWriteDescription :: Text
todoWriteDescription =
    "Create and manage a structured task list. The user sees this list live — it is your primary way to show progress.\n\n\
    \Use for any task with 3+ steps. Skip for trivial single-step work."

runTodoWrite :: GrokSession -> TodoWriteArgs -> IO (Either Text Text)
runTodoWrite session args =
    atomicModifyIORef' session.grokTodos \current ->
        case updateTodos (Map.map fromStored current) args of
            Left err -> (current, Left err)
            Right updated ->
                ( Map.map toStored updated
                , Right (formatTodos updated)
                )
  where
    fromStored :: (Text, Text) -> TodoItem
    fromStored (content, status) =
        TodoItem content (statusFromText status)
    toStored :: TodoItem -> (Text, Text)
    toStored item =
        (item.itemContent, statusToText item.itemStatus)

updateTodos :: Map Text TodoItem -> TodoWriteArgs -> Either Text (Map Text TodoItem)
updateTodos current args = do
    ensureUniqueIds args.todos
    if effectiveMerge
        then foldM mergeOne current args.todos
        else replaceAll args.todos
  where
    -- Match Grok Build's resilience for a common model mistake: an explicit
    -- replacement request containing only status flips for existing IDs is
    -- clearly a partial update, so preserve the rest of the list.
    effectiveMerge =
        args.merge
            || ( not (Map.null current)
                && not (null args.todos)
                && all
                    (\update ->
                        noMeaningfulContent update
                            && Map.member update.todoId current)
                    args.todos
               )

data TodoItem = TodoItem
    { itemContent :: !Text
    , itemStatus :: !TodoStatus
    }

ensureUniqueIds :: [TodoUpdate] -> Either Text ()
ensureUniqueIds updates =
    let counts = foldl'
            (\acc update -> Map.insertWith (+) update.todoId (1 :: Int) acc)
            Map.empty
            updates
    in case [todoId | (todoId, count) <- Map.toList counts, count > 1] of
        duplicate : _ -> Left ("Duplicate todo ID: " <> duplicate)
        [] -> Right ()

mergeOne :: Map Text TodoItem -> TodoUpdate -> Either Text (Map Text TodoItem)
mergeOne current update =
    case Map.lookup update.todoId current of
        Nothing -> do
            let item = TodoItem
                    { itemContent = contentOrId update
                    , itemStatus = fromMaybe TodoPending update.todoStatus
                    }
            pure (Map.insert update.todoId item current)
        Just existing ->
            pure $ Map.insert update.todoId TodoItem
                { itemContent = fromMaybe existing.itemContent update.todoContent
                , itemStatus = fromMaybe existing.itemStatus update.todoStatus
                }
                current

replaceAll :: [TodoUpdate] -> Either Text (Map Text TodoItem)
replaceAll updates =
    pure $ Map.fromList (map toItem updates)
  where
    toItem update =
        ( update.todoId
        , TodoItem
            (contentOrId update)
            (fromMaybe TodoPending update.todoStatus)
        )

contentOrId :: TodoUpdate -> Text
contentOrId update =
    case Text.strip <$> update.todoContent of
        Just content | not (Text.null content) -> content
        _ -> update.todoId

noMeaningfulContent :: TodoUpdate -> Bool
noMeaningfulContent update =
    maybe True (Text.null . Text.strip) update.todoContent

formatTodos :: Map Text TodoItem -> Text
formatTodos todos
    | Map.null todos = "No tasks currently tracked."
    | otherwise =
        Text.intercalate "\n"
            [ "- [" <> statusMarker item.itemStatus <> "] "
                <> todoId <> ": " <> item.itemContent
            | (todoId, item) <- Map.toList todos
            ]

statusMarker :: TodoStatus -> Text
statusMarker = \case
    TodoPending -> "pending"
    TodoInProgress -> "in_progress"
    TodoCompleted -> "completed"
    TodoCancelled -> "cancelled"

statusToText :: TodoStatus -> Text
statusToText = \case
    TodoPending -> "pending"
    TodoInProgress -> "in_progress"
    TodoCompleted -> "completed"
    TodoCancelled -> "cancelled"

statusFromText :: Text -> TodoStatus
statusFromText = \case
    "in_progress" -> TodoInProgress
    "completed" -> TodoCompleted
    "cancelled" -> TodoCancelled
    _ -> TodoPending
