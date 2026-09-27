-- | Bounded observational hints, never validation contracts. Scalar response
-- values are not retained, but property names may themselves contain sensitive
-- data. Identifier filtering is not a confidentiality guarantee.
-- Unknown information is widened rather than guessed.
module Agent.Tools.CodeMode.ReturnShape
    ( inferReturnShape
    , mergeReturnShapes
    , renderReturnShapeHint
    ) where

import Data.Aeson (Value(..), encode, object, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Lazy as LBS
import Data.List (foldl')
import qualified Data.Map.Strict as Map
import Data.Scientific (isInteger)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Vector as Vector

data Shape
    = Unknown
    | Scalar !Text
    | Nullable !Shape
    | Collection !Shape
    | Record !(Map.Map Text Shape) !(Set.Set Text)
    deriving (Eq, Show)

inferReturnShape :: Value -> Value
inferReturnShape = schema . fst . infer 0 512

mergeReturnShapes :: Value -> Value -> Value
mergeReturnShapes left right =
    -- Reapply the total-node limit after merging disjoint object observations.
    let combined = merge (fst (readShape 0 512 left)) (fst (readShape 0 512 right))
    in schema (fst (readShape 0 512 (schema combined)))

renderReturnShapeHint :: Value -> Text
renderReturnShapeHint value =
    "Observed return shape (sampled, not a contract; fields may be absent or change): "
        <> Text.decodeUtf8 (LBS.toStrict (encode (schema (fst (readShape 0 512 value)))))

infer :: Int -> Int -> Value -> (Shape, Int)
infer depth budget value
    | budget <= 0 = (Unknown, budget)
    | depth >= 6 = (Unknown, budget - 1)
    | otherwise = case value of
        Null -> result (Scalar "null")
        Bool _ -> result (Scalar "boolean")
        String _ -> result (Scalar "string")
        Number number -> result (Scalar (if isInteger number then "integer" else "number"))
        Array values ->
            let (children, remaining) = traverseBudget (infer (depth + 1))
                    (budget - 1) (Vector.toList (Vector.take 32 values))
            in (Collection (case children of [] -> Unknown; first : rest -> foldl' merge first rest), remaining)
        Object fields ->
            let entries = take 65 (KeyMap.toList fields)
            in if length entries > 64
                then result (Record Map.empty Set.empty)
                else let permitted = [(Key.toText key, child) | (key, child) <- entries, safeKey (Key.toText key)]
                         (children, remaining) = traverseBudget
                             (\available (key, child) -> let (shape, rest) = infer (depth + 1) available child
                                 in ((key, shape), rest)) (budget - 1) permitted
                         properties = Map.fromList children
                     in (Record properties (Map.keysSet properties), remaining)
  where result shape = (shape, budget - 1)

safeKey :: Text -> Bool
safeKey key = Text.length key <= 128 && case Text.uncons key of
    Nothing -> False
    Just (first, rest) -> initial first && Text.all subsequent rest
  where
    initial character = character == '_' || character == '$'
        || character >= 'a' && character <= 'z'
        || character >= 'A' && character <= 'Z'
    subsequent character = initial character || character >= '0' && character <= '9'

traverseBudget :: (Int -> a -> (b, Int)) -> Int -> [a] -> ([b], Int)
traverseBudget _ budget _ | budget <= 0 = ([], budget)
traverseBudget _ budget [] = ([], budget)
traverseBudget action budget (value : remaining) =
    let (converted, next) = action budget value
        (convertedRemaining, final) = traverseBudget action next remaining
    in (converted : convertedRemaining, final)

merge :: Shape -> Shape -> Shape
merge Unknown _ = Unknown
merge _ Unknown = Unknown
merge (Scalar "null") (Scalar "null") = Scalar "null"
merge (Scalar "null") value = nullable value
merge value (Scalar "null") = nullable value
merge (Nullable left) (Nullable right) = nullable (merge left right)
merge (Nullable left) right = nullable (merge left right)
merge left (Nullable right) = nullable (merge left right)
merge (Scalar left) (Scalar right)
    | left == right = Scalar left
    | Set.fromList [left, right] == Set.fromList ["integer", "number"] = Scalar "number"
merge (Collection left) (Collection right) = Collection (merge left right)
merge (Record left leftRequired) (Record right rightRequired)
    | Map.size combined <= 64 = Record combined (Set.intersection leftRequired rightRequired)
    | otherwise = Record Map.empty Set.empty
  where combined = Map.unionWith merge left right
merge _ _ = Unknown

nullable :: Shape -> Shape
nullable Unknown = Unknown
nullable value@(Nullable _) = value
nullable value = Nullable value

schema :: Shape -> Value
schema Unknown = object []
schema (Scalar name) = object ["type" .= name]
schema (Nullable value) = object ["anyOf" .= [schema (Scalar "null"), schema value]]
schema (Collection value) = object ["type" .= ("array" :: Text), "items" .= schema value]
schema (Record properties required) = object
    [ "type" .= ("object" :: Text)
    , "properties" .= Object (KeyMap.fromList [(Key.fromText key, schema value) | (key, value) <- Map.toList properties])
    , "required" .= Set.toAscList required
    , "additionalProperties" .= True
    ]

-- Parse only our small schema vocabulary, dropping all descriptions, examples,
-- defaults and other externally supplied values before displaying a hint.
readShape :: Int -> Int -> Value -> (Shape, Int)
readShape depth budget value
    | budget <= 0 = (Unknown, budget)
    | depth >= 6 = (Unknown, budget - 1)
    | Object fields <- value = case KeyMap.lookup "type" fields of
        Just (String name) | name `elem` ["null", "boolean", "string", "integer", "number"] ->
            (Scalar name, budget - 1)
        Just (String "array") ->
            let (child, remaining) = readShape (depth + 1) (budget - 1)
                    (maybe (object []) id (KeyMap.lookup "items" fields))
            in (Collection child, remaining)
        Just (String "object") ->
            let properties = case KeyMap.lookup "properties" fields of
                    Just (Object entries) -> take 65 (KeyMap.toList entries)
                    _ -> []
                permitted = if length properties > 64 then [] else
                    [(Key.toText key, child) | (key, child) <- properties, safeKey (Key.toText key)]
                (children, remaining) = traverseBudget
                    (\available (key, child) -> let (shape, rest) = readShape (depth + 1) available child
                        in ((key, shape), rest)) (budget - 1) permitted
                required = case KeyMap.lookup "required" fields of
                    Just (Array names) -> Set.fromList [name | String name <- Vector.toList (Vector.take 64 names), safeKey name]
                    _ -> Set.empty
                converted = Map.fromList children
            in (Record converted (Set.intersection required (Map.keysSet converted)), remaining)
        _ -> case KeyMap.lookup "anyOf" fields of
            Just (Array alternatives) | Vector.length alternatives == 2 ->
                let (children, remaining) = traverseBudget (readShape (depth + 1))
                        (budget - 1) (Vector.toList alternatives)
                in (case children of [left, right] -> merge left right; _ -> Unknown, remaining)
            _ -> (Unknown, budget - 1)
    | otherwise = (Unknown, budget - 1)
