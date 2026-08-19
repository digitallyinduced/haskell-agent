module Agent.OpenAI.ResponseMerge
    ( mergeCompletedResponseOutput
    ) where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Vector as Vector

-- | Merge output items observed in streaming @response.output_item.done@ events
-- into a completed response object. The completed event can contain a partial
-- @output@ array, so this must append missing stream items instead of only
-- replacing @output: []@.
mergeCompletedResponseOutput :: [Aeson.Value] -> Aeson.Value -> Aeson.Value
mergeCompletedResponseOutput streamedItems responseVal =
    case responseVal of
        Aeson.Object robj
            | not (null streamedItems) ->
                let finalItems = case KeyMap.lookup "output" robj of
                        Just (Aeson.Array arr) -> Vector.toList arr
                        _ -> []
                    mergedItems = mergeOutputItems finalItems streamedItems
                in Aeson.Object (KeyMap.insert "output" (Aeson.Array (Vector.fromList mergedItems)) robj)
        _ -> responseVal

mergeOutputItems :: [Aeson.Value] -> [Aeson.Value] -> [Aeson.Value]
mergeOutputItems finalItems streamedItems =
    finalItems <> filter (not . alreadyPresent) streamedItems
  where
    finalKeys = Set.fromList (concatMap itemIdentityKeys finalItems)
    alreadyPresent item =
        any (`Set.member` finalKeys) (itemIdentityKeys item)

itemIdentityKeys :: Aeson.Value -> [Text]
itemIdentityKeys (Aeson.Object obj) =
    Maybe.catMaybes
        [ identityKey "id" obj
        , identityKey "call_id" obj
        ]
itemIdentityKeys _ = []

identityKey :: Text -> KeyMap.KeyMap Aeson.Value -> Maybe Text
identityKey field obj = do
    itemType <- textField "type" obj
    value <- textField field obj
    pure (itemType <> ":" <> field <> ":" <> value)

textField :: Text -> KeyMap.KeyMap Aeson.Value -> Maybe Text
textField field obj =
    case KeyMap.lookup (Key.fromText field) obj of
        Just (Aeson.String value) -> Just value
        _ -> Nothing
