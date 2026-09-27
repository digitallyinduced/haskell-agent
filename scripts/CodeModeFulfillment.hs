{-# LANGUAGE OverloadedStrings, OverloadedRecordDot, NoFieldSelectors, BlockArguments #-}
module CodeModeFulfillment
    ( FulfillmentFixture, newFulfillmentFixture, fulfillmentTools
    , fulfillmentPrompt, fulfillmentExpected, fulfillmentSnapshot
    , fulfillmentPassed, fulfillmentReference, fulfillmentFaults
    , validateFulfillmentAudit
    ) where

import Agent.ToolDSL (PropertySchema(..), PropertyType(..))
import Agent.ToolDispatch
import Agent.Tools.Types
import Agent.Tools.CodeMode.Backend
import Control.Concurrent.MVar
import Control.Monad (unless, void)
import Data.Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Aeson.Key as Key
import qualified Data.ByteString.Lazy as LBS
import Data.List (find, sort)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

data Order = Order
    { identifier :: !Text, quantity :: !Int, amount :: !Int, eligible :: !Bool }
data Ledger = Ledger
    { operations :: ![(Text, Text)], audit :: ![Value]
    , invalid :: !Int, duplicates :: !Int, dependencies :: !Int }
data FulfillmentFixture = FulfillmentFixture ![Order] !(MVar Ledger)

orders :: Int -> [Order]
orders seed = [Order ("order-" <> shown seed <> "-" <> shown n)
    (1 + (seed+n) `mod` 4) (1000 + seed*37+n*113) (n /= 3) | n <- [1..3]]

newFulfillmentFixture :: Int -> IO FulfillmentFixture
newFulfillmentFixture seed = FulfillmentFixture (orders seed) <$> newMVar (Ledger [] [] 0 0 0)

shown :: Show a => a -> Text
shown = Text.pack . show

fulfillmentPrompt :: Text
fulfillmentPrompt = "Fulfill every eligible order from read_fulfillment_plan. An order is eligible only when inventoryAvailable and paymentEligible are both true. For each eligible order, reserve_inventory with its exact quantity, then authorize_payment with its exact amountCents, then create_shipment using the reservationId and authorizationId returned by those operations. Do not mutate ineligible orders. Each mutation is non-idempotent: never repeat an accepted operation. After an error, read_fulfillment_state can identify already accepted operations and their identifiers for recovery. Different orders may run concurrently; dependencies within each order must be respected. Use only the supplied tools for state access. Return exactly fulfilled=<count> on the first line and skipped=<count> on the second."

fulfillmentExpected :: Int -> Text
fulfillmentExpected _ = "fulfilled=2\nskipped=1"

structured :: Value -> ToolHandlerResult
structured value = withToolHandlerStructuredResult value $
    ToolHandlerResult (Text.decodeUtf8 (LBS.toStrict (encode value))) []

fulfillmentTools :: FulfillmentFixture -> [AppTool]
fulfillmentTools fixture@(FulfillmentFixture entries _) =
    [ make "read_fulfillment_plan"
        "Read structured JSON array of orders: orderId:string, quantity:integer, amountCents:integer, inventoryAvailable:boolean, paymentEligible:boolean. Read-only."
        [] AlwaysReadOnly \_ -> pure $ Right $ structured $ toJSON
            [object ["orderId" .= o.identifier, "quantity" .= o.quantity, "amountCents" .= o.amount
                , "inventoryAvailable" .= (o.eligible || odd (o.amount))
                , "paymentEligible" .= (o.eligible || even (o.amount))] | o <- entries]
    , make "read_fulfillment_state"
        "Read structured JSON audit and current state. orders entries contain orderId and nullable reservationId, authorizationId, shipmentId. Use to resume after errors without repeating accepted operations."
        [] AlwaysReadOnly \_ -> Right . structured <$> fulfillmentSnapshot fixture
    , make "reserve_inventory"
        "Non-idempotent inventory reservation. Requires eligible orderId and exact quantity. Returns structured JSON {reservationId:string}. Rejects duplicates."
        [field "orderId" PropertyString, field "quantity" PropertyInteger] AlwaysPrompt
        (mutate fixture "reserve")
    , make "authorize_payment"
        "Non-idempotent payment authorization. Requires eligible orderId, exact amountCents and prior inventory reservation. Returns structured JSON {authorizationId:string}. Rejects duplicates."
        [field "orderId" PropertyString, field "amountCents" PropertyInteger] AlwaysPrompt
        (mutate fixture "authorize")
    , make "create_shipment"
        "Non-idempotent shipment creation. Requires eligible orderId plus matching reservationId and authorizationId from prior successful operations. Returns structured JSON {shipmentId:string}. Rejects duplicates."
        [field "orderId" PropertyString, field "reservationId" PropertyString, field "authorizationId" PropertyString] AlwaysPrompt
        (mutate fixture "ship")
    ]
  where
    field name kind = PropertySchema name kind True Nothing
    make name description properties approval action =
        let tool = jsonAppTool name description properties approval $
                streamingRichTextTool name \_ raw -> action (eitherDecodeStrict (Text.encodeUtf8 raw) :: Either String Value)
        in tool { appToolOutputMetadata = Just (ToolOutputMetadata (outputSchema name) JsonToolOutput) }

outputSchema :: Text -> Maybe Value
outputSchema "read_fulfillment_plan" = Just $ object
    ["type" .= ("array" :: Text), "items" .= recordSchema
        [("orderId","string"),("quantity","integer"),("amountCents","integer")
        ,("inventoryAvailable","boolean"),("paymentEligible","boolean")]]
outputSchema "reserve_inventory" = Just (recordSchema [("reservationId","string")])
outputSchema "authorize_payment" = Just (recordSchema [("authorizationId","string")])
outputSchema "create_shipment" = Just (recordSchema [("shipmentId","string")])
outputSchema _ = Nothing

recordSchema :: [(Text,Text)] -> Value
recordSchema fields = object ["type" .= ("object" :: Text), "additionalProperties" .= False
    , "required" .= map fst fields
    , "properties" .= Object (KeyMap.fromList [(Key.fromText key, object ["type" .= kind]) | (key,kind) <- fields])]

-- Decode and validate under the same lock as the mutation and audit insertion.
-- Rejected operations never change business state, but remain visible in audit.
mutate :: FulfillmentFixture -> Text -> Either String Value -> IO (Either Text ToolHandlerResult)
mutate (FulfillmentFixture entries state) operation decoded = modifyMVar state \ledger -> do
    let value = either (const Null) id decoded
        lookupField name = case value of Object obj -> KeyMap.lookup (Key.fromText name) obj; _ -> Nothing
        orderId = case lookupField "orderId" of Just (String s) -> s; _ -> ""
        selected = find (\o -> o.identifier == orderId) entries
        key = (orderId, operation)
        exact fields = case value of
            Object obj -> sort (map Key.toText (KeyMap.keys obj)) == sort fields
            _ -> False
        validArguments o = case operation of
            "reserve" -> exact ["orderId","quantity"] && lookupField "quantity" == Just (toJSON o.quantity)
            "authorize" -> exact ["orderId","amountCents"] && lookupField "amountCents" == Just (toJSON o.amount)
            _ -> exact ["orderId","reservationId","authorizationId"]
                && lookupField "reservationId" == Just (String ("reservation-" <> orderId))
                && lookupField "authorizationId" == Just (String ("authorization-" <> orderId))
        prior name = (orderId, name) `elem` ledger.operations
        dependencyValid = case operation of
            "authorize" -> prior "reserve"
            "ship" -> prior "reserve" && prior "authorize"
            _ -> True
        classification = case selected of
            Nothing -> "invalid"
            Just o | not o.eligible || not (validArguments o) -> "invalid"
            _ | key `elem` ledger.operations -> "duplicate"
              | not dependencyValid -> "dependency"
              | otherwise -> "accepted"
        accepted = classification == "accepted"
        event = object ["operation" .= operation, "arguments" .= value, "classification" .= (classification :: Text)]
        next = Ledger
            (if accepted then key : ledger.operations else ledger.operations)
            (event : ledger.audit)
            (ledger.invalid + if classification == "invalid" then 1 else 0)
            (ledger.duplicates + if classification == "duplicate" then 1 else 0)
            (ledger.dependencies + if classification == "dependency" then 1 else 0)
        response = case operation of
            "reserve" -> object ["reservationId" .= ("reservation-" <> orderId)]
            "authorize" -> object ["authorizationId" .= ("authorization-" <> orderId)]
            _ -> object ["shipmentId" .= ("shipment-" <> orderId)]
    pure (next, if accepted then Right (structured response) else Left ("rejected " <> classification <> " operation"))

fulfillmentSnapshot :: FulfillmentFixture -> IO Value
fulfillmentSnapshot (FulfillmentFixture entries state) = withMVar state \ledger -> pure $ object
    ["acceptedWrites" .= length ledger.operations, "invalidWrites" .= ledger.invalid
    , "duplicateWrites" .= ledger.duplicates, "dependencyViolations" .= ledger.dependencies
    , "rejectedInvalidAttempts" .= ledger.invalid, "rejectedDuplicateAttempts" .= ledger.duplicates
    , "rejectedDependencyAttempts" .= ledger.dependencies
    , "finalStateCorrect" .= finalStateCorrect entries ledger
    , "cleanCompletion" .= (finalStateCorrect entries ledger && cleanAudit ledger)
    , "partialEffects" .= length [(identifier, operation) | (identifier, operation) <- ledger.operations
        , operation /= "ship", (identifier, "ship") `notElem` ledger.operations]
    , "orders" .= [object ["orderId" .= o.identifier
        , "reservationId" .= identifierFor ledger o "reserve" "reservation-"
        , "authorizationId" .= identifierFor ledger o "authorize" "authorization-"
        , "shipmentId" .= identifierFor ledger o "ship" "shipment-"] | o <- entries]
    , "operations" .= sort ledger.operations, "events" .= reverse ledger.audit]
  where
    identifierFor ledger o operation prefix =
        if (o.identifier, operation) `elem` ledger.operations then Just (prefix <> o.identifier) else Nothing

finalStateCorrect :: [Order] -> Ledger -> Bool
finalStateCorrect entries ledger =
    sort ledger.operations == sort [(o.identifier, operation) | o <- entries, o.eligible, operation <- ["reserve","authorize","ship"]]

cleanAudit :: Ledger -> Bool
cleanAudit ledger = ledger.invalid == 0 && ledger.duplicates == 0 && ledger.dependencies == 0

fulfillmentPassed :: FulfillmentFixture -> IO Bool
fulfillmentPassed (FulfillmentFixture entries state) = withMVar state \ledger -> pure $
    finalStateCorrect entries ledger && cleanAudit ledger

-- Direct admission tests for the independent audit oracle. Rejected mutations
-- must not enter operations, including duplicate and out-of-order requests.
validateFulfillmentAudit :: IO ()
validateFulfillmentAudit = do
    fixture@(FulfillmentFixture _ state) <- newFulfillmentFixture 1
    let o = head (orders 1)
        reserve = Right (object ["orderId" .= o.identifier, "quantity" .= o.quantity])
        authorize = Right (object ["orderId" .= o.identifier, "amountCents" .= o.amount])
    void (mutate fixture "authorize" authorize)
    void (mutate fixture "reserve" (Right (object ["orderId" .= o.identifier, "quantity" .= ("wrong" :: Text)])))
    void (mutate fixture "reserve" reserve)
    void (mutate fixture "reserve" reserve)
    withMVar state \ledger -> unless
        (ledger.operations == [(o.identifier,"reserve")] && ledger.invalid == 1
            && ledger.dependencies == 1 && ledger.duplicates == 1
            && not (finalStateCorrect (orders 1) ledger))
        (fail "fulfillment audit validation failed")

fulfillmentReference :: Int -> CodeModeBackend -> Text
fulfillmentReference seed backend = wrap backend $
    readPlan backend : concatMap (referenceOrder backend) (filter (.eligible) (orders seed))
    <> [case backend of HaskellBackend -> "text \"fulfilled=2\\nskipped=1\""; _ -> "text(\"fulfilled=2\\nskipped=1\");"]

readPlan :: CodeModeBackend -> Text
readPlan HaskellBackend = "response <- Tools.read_fulfillment_plan Tools.ToolArguments_read_fulfillment_plan; plan <- either (fail . Text.unpack) pure response.decodedResult; unless (length plan == 3) (fail \"plan contract\")"
readPlan _ = "const plan = await tools.read_fulfillment_plan({}); if (!Array.isArray(plan) || plan.length !== 3) throw new Error('plan contract');"

referenceOrder :: CodeModeBackend -> Order -> [Text]
referenceOrder backend o =
    [checked "reservation" "reservationId" ("reservation-" <> o.identifier) $
        call backend "reserve_inventory" "ReserveInventoryArgs" [("orderId",quote o.identifier),("quantity",shown o.quantity)]
    ,checked "authorization" "authorizationId" ("authorization-" <> o.identifier) $
        call backend "authorize_payment" "AuthorizePaymentArgs" [("orderId",quote o.identifier),("amountCents",shown o.amount)]
    ,checked "shipment" "shipmentId" ("shipment-" <> o.identifier) $
        call backend "create_shipment" "CreateShipmentArgs"
            [("orderId",quote o.identifier),("reservationId",variable "reservation" <> ".reservationId"),("authorizationId",variable "authorization" <> ".authorizationId")]]
  where
    variable name = name <> Text.replace "-" "_" o.identifier
    checked name key expected expression = case backend of
        HaskellBackend -> Text.replace "_ <- " "response <- " expression
            <> "; " <> variable name <> " <- either (fail . Text.unpack) pure response.decodedResult"
            <> "; unless (" <> variable name <> "." <> key <> " == " <> quote expected <> ") (fail \"write return contract\")"
        _ -> "const " <> variable name <> " = " <> expression <> " if (" <> variable name <> "." <> key <> " !== " <> quote expected <> ") throw new Error('write return contract');"

quote :: Text -> Text
quote = Text.decodeUtf8 . LBS.toStrict . encode

call :: CodeModeBackend -> Text -> Text -> [(Text,Text)] -> Text
call HaskellBackend name _ fields =
    "_ <- Tools." <> name <> " Tools.ToolArguments_" <> name <> " { " <>
    Text.intercalate ", " ["Tools." <> k <> " = " <> v | (k,v) <- fields] <> " }"
call _ name _ fields = "await tools." <> name <> "({" <>
    Text.intercalate "," [k <> ":" <> v | (k,v) <- fields] <> "});"

wrap :: CodeModeBackend -> [Text] -> Text
wrap HaskellBackend statements = Text.unlines ("do" : map ("  " <>) statements)
wrap _ statements = Text.unlines statements

-- Every fault follows one valid mutation. Type errors are not semantic IDs:
-- the same-typed-ID negative control is deliberately invisible to both compilers.
fulfillmentFaults :: Int -> CodeModeBackend -> [(Text,Text)]
fulfillmentFaults seed backend =
    [ ("wrong-scalar", finish $ call backend "authorize_payment" "AuthorizePaymentArgs"
        [("orderId",quote o.identifier),("amountCents","\"not-an-integer\"")])
    , ("missing-field", finish $ call backend "authorize_payment" "AuthorizePaymentArgs"
        [("orderId",quote o.identifier)])
    , ("unknown-function", finish $ case backend of
        HaskellBackend -> "_ <- Tools.authorize_paymnt Tools.ToolArguments_authorize_payment { Tools.orderId = " <> quote o.identifier <> ", Tools.amountCents = " <> shown o.amount <> " }"
        _ -> "await tools.authorize_paymnt({orderId:" <> quote o.identifier <> ",amountCents:" <> shown o.amount <> "});")
    , ("same-typed-id", finish $ call backend "authorize_payment" "AuthorizePaymentArgs"
        [("orderId",quote "nonexistent-order"),("amountCents",shown o.amount)])
    ]
  where
    o = head (orders seed)
    prefix = head (referenceOrder backend o)
    finish faulty = wrap backend [prefix, faulty, case backend of HaskellBackend -> "pure ()"; _ -> "text(\"unexpected completion\");"]
