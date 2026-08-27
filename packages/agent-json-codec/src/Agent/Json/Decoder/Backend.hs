-- | Backend interpreter API.
--
-- This module is for decoder backend implementations, not application code.
module Agent.Json.Decoder.Backend
    ( Decoder(..)
    , JsonType(..)
    , NamedField(..)
    , UnknownField(..)
    , ObjectPlan(..)
    , PlannedField(..)
    , PlannedFieldMatch(..)
    , matchPlannedField
    , capturePlannedExtension
    , finishObjectPlan
    , objectPlanRequiresRawCapture
    , objectPlanCapturesExtensions
    , unsafeRawJsonFromValidatedBytes
    ) where

import Agent.Json
    ( Extensions
    , insertExtension
    )
import Agent.Json.Internal (RawJson(..))
import qualified Data.ByteString as BS
import Data.Scientific (Scientific)
import Data.Text (Text)

data Decoder a where
    NullDecoder :: a -> Decoder a
    BoolDecoder :: Decoder Bool
    TextDecoder :: Decoder Text
    ScientificDecoder :: Decoder Scientific
    ArrayDecoder :: Decoder a -> Decoder [a]
    ObjectDecoder
        :: state
        -> [NamedField state]
        -> UnknownField state
        -> (state -> Either Text a)
        -> Decoder a
    PlannedObjectDecoder :: ObjectPlan a -> Decoder a
    DiscriminatedObjectDecoder
        :: Text
        -> (Text -> Decoder a)
        -> Decoder a
    NullableDecoder :: Decoder a -> Decoder (Maybe a)
    ByTypeDecoder :: (JsonType -> Decoder a) -> Decoder a
    RawJsonDecoder :: Decoder RawJson
    SkipDecoder :: Decoder ()
    MapDecoder
        :: (a -> Either Text b)
        -> Decoder a
        -> Decoder b

-- | Backend-only constructor for bytes fully validated by a JSON parser.
--
-- Application code must use 'Agent.Json.Decoder.validateRawJson'. A backend
-- must copy input-backed bytes before calling this function if its parser
-- reuses the input buffer.
unsafeRawJsonFromValidatedBytes :: BS.ByteString -> RawJson
unsafeRawJsonFromValidatedBytes = RawJson

data ObjectPlan a where
    PlanPure :: a -> ObjectPlan a
    PlanApply
        :: ObjectPlan (field -> a)
        -> ObjectPlan field
        -> ObjectPlan a
    PlanField
        :: PlannedField a
        -> ObjectPlan a
    PlanExtensions
        :: Extensions
        -> ObjectPlan Extensions

data PlannedField a = PlannedField
    { plannedName :: !Text
    , plannedDecoder :: !(Decoder a)
    , plannedMissing :: !(Either Text a)
    , plannedValue :: !(Maybe a)
    }

instance Functor ObjectPlan where
    fmap transform plan =
        PlanApply (PlanPure transform) plan

instance Applicative ObjectPlan where
    pure = PlanPure
    (<*>) = PlanApply

data PlannedFieldMatch result where
    PlannedFieldMatch
        :: Decoder field
        -> (field -> ObjectPlan result)
        -> PlannedFieldMatch result

matchPlannedField
    :: Text
    -> ObjectPlan result
    -> Maybe (PlannedFieldMatch result)
matchPlannedField key = \case
    PlanPure _ -> Nothing
    PlanExtensions _ -> Nothing
    PlanField field
        | field.plannedName == key ->
            Just (PlannedFieldMatch
                field.plannedDecoder
                (\value -> PlanField field { plannedValue = Just value }))
        | otherwise -> Nothing
    PlanApply functions argument ->
        case matchPlannedField key functions of
            Just (PlannedFieldMatch decoder rebuild) ->
                Just (PlannedFieldMatch decoder
                    (\value -> PlanApply (rebuild value) argument))
            Nothing -> do
                PlannedFieldMatch decoder rebuild <-
                    matchPlannedField key argument
                pure (PlannedFieldMatch decoder
                    (\value -> PlanApply functions (rebuild value)))

capturePlannedExtension
    :: Text
    -> RawJson
    -> ObjectPlan result
    -> ObjectPlan result
capturePlannedExtension key value = \case
    PlanPure result -> PlanPure result
    PlanExtensions fields ->
        PlanExtensions (insertExtension key value fields)
    PlanField field -> PlanField field
    PlanApply functions argument ->
        PlanApply
            (capturePlannedExtension key value functions)
            (capturePlannedExtension key value argument)

finishObjectPlan :: ObjectPlan a -> Either Text a
finishObjectPlan = \case
    PlanPure value -> Right value
    PlanExtensions fields -> Right fields
    PlanField field ->
        case field.plannedValue of
            Just value -> Right value
            Nothing -> field.plannedMissing
    PlanApply functions argument ->
        finishObjectPlan functions <*> finishObjectPlan argument

objectPlanRequiresRawCapture :: ObjectPlan a -> Bool
objectPlanRequiresRawCapture = \case
    PlanPure _ -> False
    PlanExtensions _ -> True
    PlanField field ->
        decoderRequiresRaw field.plannedDecoder
    PlanApply functions argument ->
        objectPlanRequiresRawCapture functions
            || objectPlanRequiresRawCapture argument
  where
    decoderRequiresRaw :: Decoder value -> Bool
    decoderRequiresRaw = \case
        NullDecoder _ -> False
        BoolDecoder -> False
        TextDecoder -> False
        ScientificDecoder -> False
        ArrayDecoder decoder -> decoderRequiresRaw decoder
        ObjectDecoder _ fields unknown _ ->
            any namedRequiresRaw fields || unknownRequiresRaw unknown
        PlannedObjectDecoder plan -> objectPlanRequiresRawCapture plan
        DiscriminatedObjectDecoder{} -> True
        NullableDecoder decoder -> decoderRequiresRaw decoder
        ByTypeDecoder select ->
            any (decoderRequiresRaw . select)
                [JsonNull, JsonBoolean, JsonNumber, JsonString, JsonArray, JsonObject]
        RawJsonDecoder -> True
        SkipDecoder -> False
        MapDecoder _ decoder -> decoderRequiresRaw decoder
    namedRequiresRaw (NamedField _ decoder _) = decoderRequiresRaw decoder
    unknownRequiresRaw (UnknownField decoder _) = decoderRequiresRaw decoder

objectPlanCapturesExtensions :: ObjectPlan a -> Bool
objectPlanCapturesExtensions = \case
    PlanPure _ -> False
    PlanExtensions _ -> True
    PlanField _ -> False
    PlanApply functions argument ->
        objectPlanCapturesExtensions functions
            || objectPlanCapturesExtensions argument
instance Functor Decoder where
    fmap transform =
        MapDecoder (Right . transform)

data JsonType
    = JsonNull
    | JsonBoolean
    | JsonNumber
    | JsonString
    | JsonArray
    | JsonObject
    deriving stock (Eq, Show)

data NamedField state where
    NamedField
        :: Text
        -> Decoder value
        -> (value -> state -> Either Text state)
        -> NamedField state

data UnknownField state where
    UnknownField
        :: Decoder value
        -> (Text -> value -> state -> Either Text state)
        -> UnknownField state
