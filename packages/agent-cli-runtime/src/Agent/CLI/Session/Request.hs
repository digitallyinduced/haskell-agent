-- | Validated session request identity and mutable provider options. Wire
-- requests are projections; callers cannot clear a live model or the cache
-- key required by a persistent session through an options update.
module Agent.CLI.Session.Request
    ( SessionRequestState
    , newSessionRequestState
    , readSessionRequestParams
    , readSessionRequestModel
    , modifySessionRequestOptions
    , setSessionRequestModel
    , withPersistentSessionRequest
    ) where

import Agent.CLI.Request (setRequestModel)
import Agent.CLI.Session.Types (Persistence(..), PersistenceState)
import Agent.Provider (Provider)
import Agent.Responses.Types (ResponseCreateParams(..))
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Text (Text)

data RequestConfig cache = RequestConfig
    { requestModel :: !Text
    , requestCacheKey :: !cache
    , requestOptions :: !ResponseCreateParams
    }

data SessionRequestState
    = EphemeralRequest !(IORef (RequestConfig (Maybe Text)))
    | PersistentRequest !(IORef PersistenceState) !(IORef (RequestConfig Text))

newSessionRequestState
    :: Persistence
    -> ResponseCreateParams
    -> IO (Either Text SessionRequestState)
newSessionRequestState persistence params =
    case params.model of
        Nothing -> pure (Left "provider request is missing a model")
        Just model -> case persistence of
            PersistenceDisabled ->
                Right . EphemeralRequest <$>
                    newIORef (RequestConfig model params.promptCacheKey (optionsOnly params))
            PersistenceEnabled slot -> case params.promptCacheKey of
                Nothing -> pure (Left "persistent provider request is missing a cache key")
                Just cacheKey ->
                    Right . PersistentRequest slot <$>
                        newIORef (RequestConfig model cacheKey (optionsOnly params))

readSessionRequestParams :: SessionRequestState -> IO ResponseCreateParams
readSessionRequestParams = \case
    EphemeralRequest ref -> renderRequest id <$> readIORef ref
    PersistentRequest _ ref -> renderRequest Just <$> readIORef ref

readSessionRequestModel :: SessionRequestState -> IO Text
readSessionRequestModel = \case
    EphemeralRequest ref -> (.requestModel) <$> readIORef ref
    PersistentRequest _ ref -> (.requestModel) <$> readIORef ref

-- | Update optional transport settings, preserving validated session identity.
-- Model changes use 'setSessionRequestModel' so Responses Lite conversion and
-- model-specific defaults change together with the model.
modifySessionRequestOptions
    :: SessionRequestState
    -> (ResponseCreateParams -> ResponseCreateParams)
    -> IO ()
modifySessionRequestOptions state change =
    modifyRequest state (\model params -> (model, change params))

setSessionRequestModel :: SessionRequestState -> Provider -> Text -> IO ()
setSessionRequestModel state provider model =
    modifyRequest state (\_ params -> (model, setRequestModel provider model params))

modifyRequest
    :: SessionRequestState
    -> (Text -> ResponseCreateParams -> (Text, ResponseCreateParams))
    -> IO ()
modifyRequest state change = case state of
    EphemeralRequest ref -> atomicModifyIORef' ref (update id)
    PersistentRequest _ ref -> atomicModifyIORef' ref (update Just)
  where
    update toCacheKey config =
        let (model, params) = change config.requestModel (renderRequest toCacheKey config)
        in (config { requestModel = model, requestOptions = optionsOnly params }, ())

-- | Snapshot the model, cache key and wire options together with the durable
-- slot that owns them. An ephemeral session has no persistence action.
withPersistentSessionRequest
    :: SessionRequestState
    -> (IORef PersistenceState -> Text -> Text -> ResponseCreateParams -> IO ())
    -> IO ()
withPersistentSessionRequest state action = case state of
    EphemeralRequest _ -> pure ()
    PersistentRequest slot ref -> do
        config <- readIORef ref
        action slot config.requestModel config.requestCacheKey (renderRequest Just config)

renderRequest :: (cache -> Maybe Text) -> RequestConfig cache -> ResponseCreateParams
renderRequest toCacheKey config =
    config.requestOptions
        { model = Just config.requestModel
        , promptCacheKey = toCacheKey config.requestCacheKey
        }

optionsOnly :: ResponseCreateParams -> ResponseCreateParams
optionsOnly params = params { model = Nothing, promptCacheKey = Nothing }
