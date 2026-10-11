-- | Schema-bound registrations for hosts using native tool handlers. The
-- caller supplies argument decoding and dispatch; this module owns coverage
-- validation and the per-invocation channel for typed runtime effects.
module Agent.Tools.Registration
    ( RegisteredTool
    , RegistrationError(..)
    , registerTool
    , registeredName
    , registeredSchema
    , registeredEffects
    , registeredTakesArguments
    , recordToolEffects
    , dispatchRegistered
    , selectAdvertised
    ) where

import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import qualified Data.List as List
import Data.Maybe (mapMaybe, listToMaybe)
import Data.Text (Text)

data RegisteredTool schema effect handler = RegisteredTool
    { name :: !Text
    , schema :: !schema
    , effects :: ![effect]
    , takesArguments :: !Bool
    , makeHandler :: !(IORef [effect] -> handler)
    }

registeredName :: RegisteredTool schema effect handler -> Text
registeredName = (.name)

registeredSchema :: RegisteredTool schema effect handler -> schema
registeredSchema = (.schema)

registeredEffects :: RegisteredTool schema effect handler -> [effect]
registeredEffects = (.effects)

registeredTakesArguments :: RegisteredTool schema effect handler -> Bool
registeredTakesArguments = (.takesArguments)

data RegistrationError
    = DuplicateHandlers ![Text]
    | MissingHandlers ![Text]
    deriving (Eq, Show)

-- | Derive both dispatch and advertisement names from the same schema.
-- Declared effects describe possible successful outcomes, not authorization.
registerTool
    :: (schema -> Text)
    -> schema
    -> Bool
    -> [effect]
    -> (Text -> IORef [effect] -> handler)
    -> RegisteredTool schema effect handler
registerTool schemaName schema takesArguments effects handler =
    RegisteredTool name schema effects takesArguments (handler name)
  where
    name = schemaName schema

-- | Publish effects only after a successful handler result. Each invocation
-- gets its own reference from 'dispatchRegistered', including parallel calls.
recordToolEffects
    :: IORef [effect]
    -> IO (Either failure (output, [effect]))
    -> IO (Either failure output)
recordToolEffects effectsRef action = action >>= \case
    Left failure -> pure (Left failure)
    Right (output, effects) -> do
        writeIORef effectsRef effects
        pure (Right output)

dispatchRegistered
    :: ([handler] -> call -> IO output)
    -> [RegisteredTool schema effect handler]
    -> call
    -> IO (output, [effect])
dispatchRegistered dispatch registrations call = do
    effectsRef <- newIORef []
    output <- dispatch (map (\registration -> registration.makeHandler effectsRef) registrations) call
    effects <- readIORef effectsRef
    pure (output, effects)

-- | Reject malformed registrations before selecting the advertised subset.
-- The host remains responsible for selecting authorized advertisements and
-- enforcing permissions when each tool runs.
selectAdvertised
    :: [Text]
    -> [RegisteredTool schema effect handler]
    -> Either RegistrationError [RegisteredTool schema effect handler]
selectAdvertised advertised registrations
    | not (null duplicates) = Left (DuplicateHandlers duplicates)
    | not (null missing) = Left (MissingHandlers missing)
    | otherwise = Right (filter (\registration -> registration.name `elem` advertised) registrations)
  where
    names = map (.name) registrations
    missing = filter (`notElem` names) advertised
    duplicates = mapMaybe listToMaybe (filter ((> 1) . length) (List.group (List.sort names)))
