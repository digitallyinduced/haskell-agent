-- | Typed, versioned protocol for accessibility-first computer use.
--
-- Model arguments are decoded directly into domain types. Only the native
-- callback boundary encodes them again, using one canonical fixed envelope
-- with an independent protocol version.
module Agent.ComputerUse.Protocol
    ( ComputerUseEffect(..)
    , ComputerUseVerdict(..)
    , ComputerUseVerdictDecision(..)
    , computerUseVerdictField
    , observationComputerUseVerdict
    , suspectedNoopComputerUseVerdict
    , unverifiedComputerUseVerdict
    , SemanticComputerAction(..)
    , SemanticComputerOperation(..)
    , SemanticComputerRequest(..)
    , SemanticComputerScalar(..)
    , decodeSemanticComputerRequest
    , decodeSemanticComputerWireRequest
    , encodeSemanticComputerRequest
    , semanticComputerProtocolVersion
    , semanticComputerRequestDecoder
    , semanticComputerRequestOperation
    , semanticComputerRequestSchema
    , semanticComputerRequestWantsScreenshot
    , semanticComputerRequestWireValue
    ) where

import Control.Monad (unless, when)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Scientific (Scientific, fromFloatDigits, toRealFloat)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import qualified Agent.Json.Decode as Json

-- | What the computer backend can honestly establish about a completed
-- request. Transport success is deliberately distinct from proof that the
-- intended UI state changed.
data ComputerUseEffect
    = ComputerUseObservation
    | ComputerUseUnverifiable
    | ComputerUseSuspectedNoop
    deriving (Eq, Show)

data ComputerUseVerdictDecision
    = ComputerUseDone
    | ComputerUseInspectFreshState
    | ComputerUseVerifyFreshState
    deriving (Eq, Show)

data ComputerUseVerdict = ComputerUseVerdict
    { computerUseVerdictEffect :: !ComputerUseEffect
    , computerUseVerdictDecision :: !ComputerUseVerdictDecision
    , computerUseVerdictFreshObservation :: !Bool
    , computerUseVerdictHint :: !Text
    } deriving (Eq, Show)

instance Aeson.ToJSON ComputerUseEffect where
    toJSON = Aeson.String . \case
        ComputerUseObservation -> "observation"
        ComputerUseUnverifiable -> "unverifiable"
        ComputerUseSuspectedNoop -> "suspected_noop"

instance Aeson.FromJSON ComputerUseEffect where
    parseJSON = Aeson.withText "ComputerUseEffect" \case
        "observation" -> pure ComputerUseObservation
        "unverifiable" -> pure ComputerUseUnverifiable
        "suspected_noop" -> pure ComputerUseSuspectedNoop
        _ -> fail "unsupported computer use effect"

instance Aeson.ToJSON ComputerUseVerdictDecision where
    toJSON = Aeson.String . \case
        ComputerUseDone -> "done"
        ComputerUseInspectFreshState -> "inspect_fresh_state"
        ComputerUseVerifyFreshState -> "verify_fresh_state"

instance Aeson.FromJSON ComputerUseVerdictDecision where
    parseJSON = Aeson.withText "ComputerUseVerdictDecision" \case
        "done" -> pure ComputerUseDone
        "inspect_fresh_state" -> pure ComputerUseInspectFreshState
        "verify_fresh_state" -> pure ComputerUseVerifyFreshState
        _ -> fail "unsupported computer use verdict decision"

instance Aeson.ToJSON ComputerUseVerdict where
    toJSON verdict = Aeson.object
        [ "effect" Aeson..= verdict.computerUseVerdictEffect
        , "decision" Aeson..= verdict.computerUseVerdictDecision
        , "fresh_observation"
            Aeson..= verdict.computerUseVerdictFreshObservation
        , "hint" Aeson..= verdict.computerUseVerdictHint
        ]

instance Aeson.FromJSON ComputerUseVerdict where
    parseJSON = Aeson.withObject "ComputerUseVerdict" \object -> do
        verdict <- ComputerUseVerdict
            <$> object Aeson..: "effect"
            <*> object Aeson..: "decision"
            <*> object Aeson..: "fresh_observation"
            <*> object Aeson..: "hint"
        unless (validComputerUseVerdict verdict) $
            fail "inconsistent computer use verdict"
        pure verdict

validComputerUseVerdict :: ComputerUseVerdict -> Bool
validComputerUseVerdict verdict =
    case
        ( verdict.computerUseVerdictEffect
        , verdict.computerUseVerdictDecision
        , verdict.computerUseVerdictFreshObservation
        ) of
        (ComputerUseObservation, ComputerUseDone, True) -> True
        (ComputerUseUnverifiable, ComputerUseInspectFreshState, True) -> True
        (ComputerUseUnverifiable, ComputerUseVerifyFreshState, False) -> True
        (ComputerUseSuspectedNoop, ComputerUseInspectFreshState, True) -> True
        (ComputerUseSuspectedNoop, ComputerUseVerifyFreshState, False) -> True
        _ -> False

computerUseVerdictField :: Text
computerUseVerdictField = "verdict"

observationComputerUseVerdict :: ComputerUseVerdict
observationComputerUseVerdict = ComputerUseVerdict
    { computerUseVerdictEffect = ComputerUseObservation
    , computerUseVerdictDecision = ComputerUseDone
    , computerUseVerdictFreshObservation = True
    , computerUseVerdictHint =
        "A fresh computer observation was returned."
    }

unverifiedComputerUseVerdict :: Bool -> ComputerUseVerdict
unverifiedComputerUseVerdict freshObservation = ComputerUseVerdict
    { computerUseVerdictEffect = ComputerUseUnverifiable
    , computerUseVerdictDecision =
        if freshObservation
            then ComputerUseInspectFreshState
            else ComputerUseVerifyFreshState
    , computerUseVerdictFreshObservation = freshObservation
    , computerUseVerdictHint =
        if freshObservation
            then
                "Input delivery does not prove the intended UI effect. "
                    <> "Inspect the fresh state before retrying."
            else
                "Input delivery does not prove the intended UI effect. "
                    <> "Re-observe the target before retrying; do not repeat "
                    <> "the input blindly."
    }

suspectedNoopComputerUseVerdict :: Bool -> ComputerUseVerdict
suspectedNoopComputerUseVerdict freshObservation = ComputerUseVerdict
    { computerUseVerdictEffect = ComputerUseSuspectedNoop
    , computerUseVerdictDecision =
        if freshObservation
            then ComputerUseInspectFreshState
            else ComputerUseVerifyFreshState
    , computerUseVerdictFreshObservation = freshObservation
    , computerUseVerdictHint =
        if freshObservation
            then
                "The target reported that the input may not have taken effect. "
                    <> "Inspect the fresh state; do not repeat it blindly."
            else
                "The target reported that the input may not have taken effect. "
                    <> "Re-observe the target before retrying; do not repeat "
                    <> "the input blindly."
    }

data SemanticComputerRequest
    = ListComputerTargets
    | BindComputerTarget !Text !Bool
    | ObserveComputerTarget !Bool
    | ActOnComputerTarget !(NonEmpty SemanticComputerAction) !Bool
    deriving (Eq, Show)

data SemanticComputerAction
    = PerformComputerAction !Text !Text
    | SetComputerValue !Text !SemanticComputerScalar
    | ReplaceComputerSelectedText !Text !Text
    deriving (Eq, Show)

data SemanticComputerScalar
    = ComputerText !Text
    | ComputerNumber !Scientific
    | ComputerBool !Bool
    deriving (Eq, Show)

data SemanticComputerOperation
    = ListComputerTargetsOperation
    | BindComputerTargetOperation
    | ObserveOrActOnComputerTargetOperation
    deriving (Eq, Show)

semanticComputerProtocolVersion :: Int
semanticComputerProtocolVersion = 1

semanticComputerRequestOperation
    :: SemanticComputerRequest
    -> SemanticComputerOperation
semanticComputerRequestOperation = \case
    ListComputerTargets -> ListComputerTargetsOperation
    BindComputerTarget{} -> BindComputerTargetOperation
    ObserveComputerTarget{} -> ObserveOrActOnComputerTargetOperation
    ActOnComputerTarget{} -> ObserveOrActOnComputerTargetOperation

semanticComputerRequestWantsScreenshot :: SemanticComputerRequest -> Bool
semanticComputerRequestWantsScreenshot = \case
    ListComputerTargets -> False
    BindComputerTarget _ includeScreenshot -> includeScreenshot
    ObserveComputerTarget includeScreenshot -> includeScreenshot
    ActOnComputerTarget _ includeScreenshot -> includeScreenshot

-- | Strict decoder for the model-facing function arguments.
semanticComputerRequestDecoder :: Json.Decoder SemanticComputerRequest
semanticComputerRequestDecoder = requestDecoder ModelArguments

decodeSemanticComputerRequest :: Text -> Either Text SemanticComputerRequest
decodeSemanticComputerRequest arguments =
    case Json.decodeText semanticComputerRequestDecoder arguments of
        Left (Json.JsonError err) -> Left err
        Right request -> Right request

-- | Decode and version-check the canonical public-runtime to native-host wire
-- representation. This is primarily useful to contract tests and alternate
-- native hosts; normal execution already carries the typed request.
decodeSemanticComputerWireRequest
    :: BS.ByteString
    -> Either Text SemanticComputerRequest
decodeSemanticComputerWireRequest bytes =
    if BS.length bytes > semanticComputerRequestCapacity
        then Left "computer request exceeds the native protocol capacity"
        else case Json.decodeEither (requestDecoder NativeWire) bytes of
            Left (Json.JsonError err) -> Left err
            Right request -> Right request

encodeSemanticComputerRequest :: SemanticComputerRequest -> BS.ByteString
encodeSemanticComputerRequest =
    LBS.toStrict . Aeson.encode . semanticComputerRequestWireValue

semanticComputerRequestWireValue :: SemanticComputerRequest -> Aeson.Value
semanticComputerRequestWireValue request =
    requestValue operation target actions includeScreenshot
  where
    includeScreenshot = semanticComputerRequestWantsScreenshot request
    (operation, target, actions) = case request of
        ListComputerTargets ->
            ("list_targets", Aeson.Null, Aeson.Null)
        BindComputerTarget targetId _ ->
            ("bind", Aeson.String targetId, Aeson.Null)
        ObserveComputerTarget{} ->
            ("observe", Aeson.Null, Aeson.Null)
        ActOnComputerTarget semanticActions _ ->
            ( "act"
            , Aeson.Null
            , Aeson.toJSON
                (map semanticActionValue (NonEmpty.toList semanticActions))
            )

data Envelope
    = ModelArguments
    | NativeWire
    deriving (Eq)

data RequiredField a
    = MissingField
    | PresentField !a

data RequestFields = RequestFields
    { requestVersion :: !(RequiredField Int)
    , requestOperation :: !(RequiredField Text)
    , requestTarget :: !(RequiredField (Maybe Text))
    , requestActions :: !(RequiredField (Maybe [SemanticComputerAction]))
    , requestScreenshot :: !(RequiredField Bool)
    }

emptyRequestFields :: RequestFields
emptyRequestFields = RequestFields
    { requestVersion = MissingField
    , requestOperation = MissingField
    , requestTarget = MissingField
    , requestActions = MissingField
    , requestScreenshot = MissingField
    }

requestDecoder :: Envelope -> Json.Decoder SemanticComputerRequest
requestDecoder envelope = do
    fields <- Json.objectFold emptyRequestFields (decodeRequestField envelope)
    validateRequest envelope fields

decodeRequestField
    :: Envelope
    -> Text
    -> RequestFields
    -> Json.Decoder RequestFields
decodeRequestField envelope key fields =
    case key of
        "protocol_version"
            | envelope == NativeWire ->
                decodeRequired key fields.requestVersion Json.int
                    \field -> fields { requestVersion = field }
        "operation" ->
            decodeRequired key fields.requestOperation Json.text
                \field -> fields { requestOperation = field }
        "target_id" ->
            decodeRequired key fields.requestTarget (Json.nullable Json.text)
                \field -> fields { requestTarget = field }
        "actions" ->
            decodeRequired
                key
                fields.requestActions
                (Json.nullable (Json.list semanticActionDecoder))
                \field -> fields { requestActions = field }
        "include_screenshot" ->
            decodeRequired key fields.requestScreenshot Json.bool
                \field -> fields { requestScreenshot = field }
        _ -> fail ("unexpected computer argument field: " <> Text.unpack key)

validateRequest
    :: Envelope
    -> RequestFields
    -> Json.Decoder SemanticComputerRequest
validateRequest envelope fields = do
    case envelope of
        ModelArguments -> pure ()
        NativeWire -> do
            version <- requireField "protocol_version" fields.requestVersion
            unless (version == semanticComputerProtocolVersion) $
                fail "unsupported computer protocol version"
    operation <- requireField "operation" fields.requestOperation
    target <- requireField "target_id" fields.requestTarget
    actions <- requireField "actions" fields.requestActions
    includeScreenshot <-
        requireField "include_screenshot" fields.requestScreenshot
    request <- case operation of
        "list_targets" -> do
            requireNothing "target_id" target
            requireNothing "actions" actions
            when includeScreenshot $
                fail "list_targets cannot include a screenshot"
            pure ListComputerTargets
        "bind" -> do
            targetId <- requireJust "target_id" target
            validateText "target_id" False 1024 targetId
            requireNothing "actions" actions
            pure (BindComputerTarget targetId includeScreenshot)
        "observe" -> do
            requireNothing "target_id" target
            requireNothing "actions" actions
            pure (ObserveComputerTarget includeScreenshot)
        "act" -> do
            requireNothing "target_id" target
            rawActions <- requireJust "actions" actions
            when (null rawActions) $
                fail "act requires at least one semantic action"
            when (length rawActions > 64) $
                fail "act accepts at most 64 semantic actions"
            case NonEmpty.nonEmpty rawActions of
                Nothing -> fail "act requires at least one semantic action"
                Just semanticActions ->
                    pure (ActOnComputerTarget
                        semanticActions
                        includeScreenshot)
        _ -> fail "unsupported computer operation"
    when (BS.length (encodeSemanticComputerRequest request)
            > semanticComputerRequestCapacity) $
        fail "computer request exceeds the native protocol capacity"
    pure request

data ActionFields = ActionFields
    { actionType :: !(RequiredField Text)
    , actionElementId :: !(RequiredField Text)
    , actionName :: !(RequiredField (Maybe Text))
    , actionValueField :: !(RequiredField (Maybe SemanticComputerScalar))
    , actionText :: !(RequiredField (Maybe Text))
    }

emptyActionFields :: ActionFields
emptyActionFields = ActionFields
    { actionType = MissingField
    , actionElementId = MissingField
    , actionName = MissingField
    , actionValueField = MissingField
    , actionText = MissingField
    }

semanticActionDecoder :: Json.Decoder SemanticComputerAction
semanticActionDecoder = do
    fields <- Json.objectFold emptyActionFields decodeActionField
    validateAction fields

decodeActionField
    :: Text
    -> ActionFields
    -> Json.Decoder ActionFields
decodeActionField key fields =
    case key of
        "type" ->
            decodeRequired key fields.actionType Json.text
                \field -> fields { actionType = field }
        "element_id" ->
            decodeRequired key fields.actionElementId Json.text
                \field -> fields { actionElementId = field }
        "action" ->
            decodeRequired key fields.actionName (Json.nullable Json.text)
                \field -> fields { actionName = field }
        "value" ->
            decodeRequired
                key
                fields.actionValueField
                (Json.nullable semanticScalarDecoder)
                \field -> fields { actionValueField = field }
        "text" ->
            decodeRequired key fields.actionText (Json.nullable Json.text)
                \field -> fields { actionText = field }
        _ -> fail ("unexpected semantic computer action field: "
            <> Text.unpack key)

validateAction :: ActionFields -> Json.Decoder SemanticComputerAction
validateAction fields = do
    actionType <- requireField "type" fields.actionType
    elementId <- requireField "element_id" fields.actionElementId
    validateText "element_id" False 1024 elementId
    action <- requireField "action" fields.actionName
    value <- requireField "value" fields.actionValueField
    replacement <- requireField "text" fields.actionText
    case actionType of
        "perform" -> do
            actionName <- requireJust "action" action
            validateText "action" False 1024 actionName
            requireNothing "value" value
            requireNothing "text" replacement
            pure (PerformComputerAction elementId actionName)
        "set_value" -> do
            requireNothing "action" action
            scalar <- requireJust "value" value
            requireNothing "text" replacement
            pure (SetComputerValue elementId scalar)
        "replace_selected_text" -> do
            requireNothing "action" action
            requireNothing "value" value
            text <- requireJust "text" replacement
            validateText "text" True 65536 text
            pure (ReplaceComputerSelectedText elementId text)
        _ -> fail "unsupported semantic computer action"

semanticScalarDecoder :: Json.Decoder SemanticComputerScalar
semanticScalarDecoder =
    Json.getType >>= \case
        Json.VString -> do
            value <- Json.text
            validateText "value" True 65536 value
            pure (ComputerText value)
        Json.VNumber -> do
            value <- Json.scientific
            validateDouble value
            pure (ComputerNumber value)
        Json.VBoolean -> ComputerBool <$> Json.bool
        _ -> fail "set_value.value must be a string, number, or boolean"

validateText :: Text -> Bool -> Int -> Text -> Json.Decoder ()
validateText name allowEmpty maximumBytes value =
    unless ( (allowEmpty || not (Text.null value))
                && BS.length (TextEncoding.encodeUtf8 value) <= maximumBytes) $
        fail (Text.unpack name <> " is outside its protocol limit")

validateDouble :: Scientific -> Json.Decoder ()
validateDouble value =
    let asDouble = toRealFloat value :: Double
    in unless (not (isInfinite asDouble)
                && not (isNaN asDouble)
                && fromFloatDigits asDouble == value) $
        fail "set_value.value must be a finite IEEE-754 double"

semanticComputerRequestCapacity :: Int
semanticComputerRequestCapacity = 1024 * 1024

decodeRequired
    :: Text
    -> RequiredField a
    -> Json.Decoder a
    -> (RequiredField a -> state)
    -> Json.Decoder state
decodeRequired key current decoder update =
    case current of
        MissingField -> update . PresentField <$> decoder
        PresentField _ ->
            fail ("duplicate computer argument field: " <> Text.unpack key)

requireField
    :: Text
    -> RequiredField value
    -> Json.Decoder value
requireField key = \case
    MissingField -> fail ("missing required field: " <> Text.unpack key)
    PresentField value -> pure value

requireNothing :: Text -> Maybe value -> Json.Decoder ()
requireNothing _ Nothing = pure ()
requireNothing key (Just _) =
    fail (Text.unpack key <> " must be null for this operation")

requireJust :: Text -> Maybe value -> Json.Decoder value
requireJust key = \case
    Nothing -> fail (Text.unpack key <> " must not be null for this operation")
    Just value -> pure value

requestValue
    :: Text
    -> Aeson.Value
    -> Aeson.Value
    -> Bool
    -> Aeson.Value
requestValue operation target actions includeScreenshot =
    Aeson.object
        [ "protocol_version" Aeson..= semanticComputerProtocolVersion
        , "operation" Aeson..= operation
        , "target_id" Aeson..= target
        , "actions" Aeson..= actions
        , "include_screenshot" Aeson..= includeScreenshot
        ]

semanticActionValue :: SemanticComputerAction -> Aeson.Value
semanticActionValue = \case
    PerformComputerAction elementId action ->
        actionValue "perform" elementId
            (Aeson.String action)
            Aeson.Null
            Aeson.Null
    SetComputerValue elementId value ->
        actionValue "set_value" elementId
            Aeson.Null
            (semanticScalarValue value)
            Aeson.Null
    ReplaceComputerSelectedText elementId replacement ->
        actionValue "replace_selected_text" elementId
            Aeson.Null
            Aeson.Null
            (Aeson.String replacement)

actionValue
    :: Text
    -> Text
    -> Aeson.Value
    -> Aeson.Value
    -> Aeson.Value
    -> Aeson.Value
actionValue actionType elementId action value replacement =
    Aeson.object
        [ "type" Aeson..= actionType
        , "element_id" Aeson..= elementId
        , "action" Aeson..= action
        , "value" Aeson..= value
        , "text" Aeson..= replacement
        ]

semanticScalarValue :: SemanticComputerScalar -> Aeson.Value
semanticScalarValue = \case
    ComputerText value -> Aeson.String value
    ComputerNumber value -> Aeson.Number value
    ComputerBool value -> Aeson.Bool value

-- | Strict model-facing JSON schema for 'semanticComputerRequestDecoder'.
semanticComputerRequestSchema :: Aeson.Value
semanticComputerRequestSchema = strictObject
    [ ("operation", Aeson.object
        [ "type" Aeson..= ("string" :: Text)
        , "enum" Aeson..=
            (["list_targets", "bind", "observe", "act"] :: [Text])
        , "description" Aeson..=
            ( "Use list_targets, bind one returned target_id, observe the "
            <> "bound target, then act on element_id values from the fresh "
            <> "accessibility state."
            :: Text
            )
        ])
    , ("target_id", describeParameter
        "Required only for bind; use an exact ID returned by list_targets."
        (nullableStringParameter 1024))
    , ("actions", Aeson.object
        [ "type" Aeson..= (["array", "null"] :: [Text])
        , "minItems" Aeson..= (1 :: Int)
        , "maxItems" Aeson..= (64 :: Int)
        , "items" Aeson..= semanticActionParameters
        , "description" Aeson..=
            ( "Required only for act. Use element IDs from the latest "
            <> "observation and inspect the returned fresh state before "
            <> "retrying."
            :: Text
            )
        ])
    , ("include_screenshot", screenshotParameter)
    ]
    ["operation", "target_id", "actions", "include_screenshot"]

semanticActionParameters :: Aeson.Value
semanticActionParameters = strictObject
    [ ("type", Aeson.object
        [ "type" Aeson..= ("string" :: Text)
        , "enum" Aeson..=
            (["perform", "set_value", "replace_selected_text"] :: [Text])
        , "description" Aeson..=
            ("Accessibility operation to apply." :: Text)
        ])
    , ("element_id", describeParameter
        "Exact stable element ID from the current bound target observation."
        (boundedStringParameter False 1024))
    , ("action", describeParameter
        "Accessibility action name for perform; otherwise null."
        (nullableStringParameter 1024))
    , ("value", Aeson.object
        [ "type" Aeson..=
            (["string", "number", "boolean", "null"] :: [Text])
        , "maxLength" Aeson..= (65536 :: Int)
        , "description" Aeson..=
            ("Scalar value for set_value; otherwise null." :: Text)
        ])
    , ("text", describeParameter
        "Replacement for replace_selected_text; otherwise null."
        (nullableStringParameter 65536))
    ]
    ["type", "element_id", "action", "value", "text"]

strictObject :: [(Text, Aeson.Value)] -> [Text] -> Aeson.Value
strictObject properties requiredFields = Aeson.object
    [ "type" Aeson..= ("object" :: Text)
    , "additionalProperties" Aeson..= False
    , "properties" Aeson..= Aeson.object
        [ Key.fromText name Aeson..= schema | (name, schema) <- properties ]
    , "required" Aeson..= requiredFields
    ]

describeParameter :: Text -> Aeson.Value -> Aeson.Value
describeParameter description = \case
    Aeson.Object object ->
        Aeson.Object
            (KeyMap.insert "description" (Aeson.String description) object)
    value -> value

nullableStringParameter :: Int -> Aeson.Value
nullableStringParameter maximumLength = Aeson.object
    [ "type" Aeson..= (["string", "null"] :: [Text])
    , "maxLength" Aeson..= maximumLength
    ]

boundedStringParameter :: Bool -> Int -> Aeson.Value
boundedStringParameter allowEmpty maximumLength = Aeson.object $
    [ "type" Aeson..= ("string" :: Text)
    , "maxLength" Aeson..= maximumLength
    ]
    <> [ "minLength" Aeson..= (1 :: Int) | not allowEmpty ]

screenshotParameter :: Aeson.Value
screenshotParameter = Aeson.object
    [ "type" Aeson..= ("boolean" :: Text)
    , "description" Aeson..=
        ( "Set false unless visual evidence is necessary; screenshots are "
        <> "never returned implicitly."
        :: Text
        )
    ]
