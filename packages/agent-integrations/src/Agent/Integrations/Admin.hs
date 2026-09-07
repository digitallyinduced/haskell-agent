-- | Typed administration operations for integration settings UIs.
--
-- Administration handlers use concrete Haskell inputs and outputs. The
-- existential registry performs JSON decoding and encoding only at the native
-- host boundary.
module Agent.Integrations.Admin
    ( IntegrationAdminOperationName
    , integrationAdminOperationName
    , integrationAdminOperationNameText
    , IntegrationAdminDefinition(..)
    , IntegrationAdminOperation(..)
    , SomeIntegrationAdminOperation(..)
    , IntegrationAdminRegistry
    , createIntegrationAdminRegistry
    , integrationAdminDefinitions
    , encodeIntegrationAdminDefinitions
    , callIntegrationAdmin
    , emailAdminOperations
    ) where

import Agent.Integrations.Email.Imap (connectMailImapAccount)
import Agent.Integrations.Email.OAuth
import Agent.Integrations.Email.Store
import Agent.Integrations.Server (IntegrationHost, notifyIntegrationToolsChanged)
import Agent.Integrations.Types
import Agent.Json (RawJson, rawJsonFromEncoding)
import qualified Agent.Json.Decode as Json
import Control.Monad (when)
import Data.Aeson (ToJSON, Value, object, (.=))
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (Pair)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text

newtype IntegrationAdminOperationName =
    IntegrationAdminOperationName Text
    deriving (Eq, Ord, Show)

integrationAdminOperationName
    :: Text
    -> Either Text IntegrationAdminOperationName
integrationAdminOperationName raw
    | not (Text.null checked)
        && all validSegment (Text.splitOn "." checked) =
        Right (IntegrationAdminOperationName checked)
    | otherwise =
        Left
            "integration admin operation name must contain lowercase identifier segments"
  where
    checked = Text.strip raw
    validSegment segment =
        not (Text.null segment)
            && Text.all validCharacter segment
    validCharacter character =
        character == '_'
            || character == '-'
            || character >= 'a' && character <= 'z'
            || character >= '0' && character <= '9'

integrationAdminOperationNameText :: IntegrationAdminOperationName -> Text
integrationAdminOperationNameText (IntegrationAdminOperationName value) = value

data IntegrationAdminDefinition = IntegrationAdminDefinition
    { integrationAdminDefinitionName :: !IntegrationAdminOperationName
    , integrationAdminDefinitionTitle :: !Text
    , integrationAdminDefinitionDescription :: !Text
    , integrationAdminDefinitionInputSchema :: !RawJson
    , integrationAdminDefinitionOutputSchema :: !RawJson
    , integrationAdminDefinitionSensitiveInputFields :: ![Text]
    }

instance ToJSON IntegrationAdminDefinition where
    toJSON definition = object
        [ "name"
            .= integrationAdminOperationNameText
                definition.integrationAdminDefinitionName
        , "title" .= definition.integrationAdminDefinitionTitle
        , "description" .= definition.integrationAdminDefinitionDescription
        , "input_schema" .= definition.integrationAdminDefinitionInputSchema
        , "output_schema" .= definition.integrationAdminDefinitionOutputSchema
        , "sensitive_input_fields"
            .= definition.integrationAdminDefinitionSensitiveInputFields
        ]
    toEncoding definition = Aeson.pairs
        ( "name"
            .= integrationAdminOperationNameText
                definition.integrationAdminDefinitionName
            <> "title" .= definition.integrationAdminDefinitionTitle
            <> "description" .= definition.integrationAdminDefinitionDescription
            <> "input_schema" .= definition.integrationAdminDefinitionInputSchema
            <> "output_schema" .= definition.integrationAdminDefinitionOutputSchema
            <> "sensitive_input_fields"
                .= definition.integrationAdminDefinitionSensitiveInputFields
        )

data IntegrationAdminOperation input output = IntegrationAdminOperation
    { integrationAdminOperationDefinition :: !IntegrationAdminDefinition
    , integrationAdminOperationInput :: !(InputContract input)
    , integrationAdminOperationOutput :: !(OutputContract output)
    , integrationAdminOperationHandler
        :: !(input -> IO (Either IntegrationError output))
    }

data SomeIntegrationAdminOperation =
    forall input output.
    SomeIntegrationAdminOperation (IntegrationAdminOperation input output)

newtype IntegrationAdminRegistry = IntegrationAdminRegistry
    { adminOperations ::
        Map.Map IntegrationAdminOperationName SomeIntegrationAdminOperation
    }

createIntegrationAdminRegistry
    :: [SomeIntegrationAdminOperation]
    -> Either Text IntegrationAdminRegistry
createIntegrationAdminRegistry operations =
    IntegrationAdminRegistry <$> foldlM insertOperation Map.empty operations
  where
    insertOperation operationsByName packed@(SomeIntegrationAdminOperation operation) =
        let name =
                operation.integrationAdminOperationDefinition.integrationAdminDefinitionName
        in if Map.member name operationsByName
            then Left
                ( "duplicate integration admin operation: "
                    <> integrationAdminOperationNameText name
                )
            else Right (Map.insert name packed operationsByName)

integrationAdminDefinitions
    :: IntegrationAdminRegistry
    -> [IntegrationAdminDefinition]
integrationAdminDefinitions registry =
    [ operation.integrationAdminOperationDefinition
    | SomeIntegrationAdminOperation operation <-
        Map.elems registry.adminOperations
    ]

encodeIntegrationAdminDefinitions :: IntegrationAdminRegistry -> RawJson
encodeIntegrationAdminDefinitions =
    rawJsonFromEncoding
        . Aeson.toEncoding
        . integrationAdminDefinitions

callIntegrationAdmin
    :: IntegrationAdminRegistry
    -> Text
    -> RawJson
    -> IO (Either IntegrationError RawJson)
callIntegrationAdmin registry rawName arguments =
    case integrationAdminOperationName rawName of
        Left _ -> invalidOperation
        Right name ->
            case Map.lookup name registry.adminOperations of
                Nothing -> invalidOperation
                Just (SomeIntegrationAdminOperation operation) ->
                    case decodeIntegrationInput
                        operation.integrationAdminOperationInput
                        arguments
                    of
                        Left err -> pure (Left err)
                        Right input ->
                            fmap
                                (fmap
                                    (encodeIntegrationOutput
                                        operation.integrationAdminOperationOutput))
                                (operation.integrationAdminOperationHandler input)
  where
    invalidOperation =
        pure
            (Left
                (IntegrationInvalidInput
                    "The integration admin operation is unknown."))

emailAdminOperations
    :: MailOAuthRuntime
    -> IntegrationHost
    -> Either Text [SomeIntegrationAdminOperation]
emailAdminOperations oauthRuntime host =
    sequence
        [ operation
            "email.accounts.list"
            "Email accounts"
            "List configured email accounts without credential material."
            emptyObjectSchema
            accountsOutputSchema
            []
            (jsonInputContract emptyObjectSchema emptyInputDecoder)
            (aesonOutputContract accountsOutputSchema)
            (\() -> mapError <$> loadMailAccounts)
        , operation
            "email.oauth.start"
            "Connect email with OAuth"
            "Start a Gmail or Microsoft browser authorization flow."
            oauthStartInputSchema
            oauthChallengeOutputSchema
            []
            (jsonInputContract oauthStartInputSchema oauthStartDecoder)
            (jsonOutputContract oauthChallengeOutputSchema oauthChallengeEncoding)
            (startOAuth oauthRuntime)
        , operation
            "email.oauth.poll"
            "Check email authorization"
            "Check a previously started email authorization flow."
            flowInputSchema
            oauthPollOutputSchema
            []
            (jsonInputContract flowInputSchema flowInputDecoder)
            (jsonOutputContract oauthPollOutputSchema oauthPollEncoding)
            (pollOAuth oauthRuntime host)
        , operation
            "email.oauth.cancel"
            "Cancel email authorization"
            "Cancel a previously started email authorization flow."
            flowInputSchema
            successOutputSchema
            []
            (jsonInputContract flowInputSchema flowInputDecoder)
            (jsonOutputContract successOutputSchema successEncoding)
            (cancelOAuth oauthRuntime)
        , operation
            "email.settings.discover"
            "Discover email settings"
            "Discover conservative provider settings from an email address."
            emailInputSchema
            discoveryOutputSchema
            []
            (jsonInputContract emailInputSchema emailInputDecoder)
            (jsonOutputContract discoveryOutputSchema discoveryEncoding)
            discoverSettings
        , operation
            "email.imap.connect"
            "Connect an IMAP account"
            "Verify and store a custom IMAP account."
            imapConnectInputSchema
            identifierOutputSchema
            ["password"]
            (jsonInputContract imapConnectInputSchema imapConnectDecoder)
            (jsonOutputContract identifierOutputSchema identifierEncoding)
            (connectImap host)
        , operation
            "email.account.set_enabled"
            "Enable or disable an email account"
            "Change whether an email account is available to integrations."
            setEnabledInputSchema
            successOutputSchema
            []
            (jsonInputContract setEnabledInputSchema setEnabledDecoder)
            (jsonOutputContract successOutputSchema successEncoding)
            (setEnabled host)
        , operation
            "email.account.delete"
            "Delete an email account"
            "Remove an email account and its stored credential."
            accountInputSchema
            successOutputSchema
            []
            (jsonInputContract accountInputSchema accountInputDecoder)
            (jsonOutputContract successOutputSchema successEncoding)
            (deleteAccount host)
        ]

operation
    :: Text
    -> Text
    -> Text
    -> Value
    -> Value
    -> [Text]
    -> InputContract input
    -> OutputContract output
    -> (input -> IO (Either IntegrationError output))
    -> Either Text SomeIntegrationAdminOperation
operation name title description inputSchema outputSchema sensitive input output handler = do
    checkedName <- integrationAdminOperationName name
    pure . SomeIntegrationAdminOperation $ IntegrationAdminOperation
        { integrationAdminOperationDefinition = IntegrationAdminDefinition
            { integrationAdminDefinitionName = checkedName
            , integrationAdminDefinitionTitle = title
            , integrationAdminDefinitionDescription = description
            , integrationAdminDefinitionInputSchema =
                rawJsonFromEncoding (Aeson.toEncoding inputSchema)
            , integrationAdminDefinitionOutputSchema =
                rawJsonFromEncoding (Aeson.toEncoding outputSchema)
            , integrationAdminDefinitionSensitiveInputFields = sensitive
            }
        , integrationAdminOperationInput = input
        , integrationAdminOperationOutput = output
        , integrationAdminOperationHandler = handler
        }

data OAuthStartInput = OAuthStartInput
    { oauthStartProvider :: !Text
    , oauthStartClientId :: !Text
    }

data FlowInput = FlowInput
    { flowInputId :: !Text
    }

data EmailInput = EmailInput
    { emailInputAddress :: !Text
    }

data ImapConnectInput = ImapConnectInput
    { imapConnectEmail :: !Text
    , imapConnectLabel :: !Text
    , imapConnectHost :: !Text
    , imapConnectPort :: !Int
    , imapConnectTls :: !Text
    , imapConnectUsername :: !Text
    , imapConnectPassword :: !Text
    }

data SetEnabledInput = SetEnabledInput
    { setEnabledAccountId :: !Text
    , setEnabledValue :: !Bool
    }

data AccountInput = AccountInput
    { accountInputId :: !Text
    }

data Success = Success

newtype Identifier = Identifier Text

emptyInputDecoder :: Json.Decoder ()
emptyInputDecoder = Json.object (pure ())

oauthStartDecoder :: Json.Decoder OAuthStartInput
oauthStartDecoder = Json.object $
    OAuthStartInput
        <$> Json.atKey "provider" Json.text
        <*> Json.atKey "client_id" Json.text

flowInputDecoder :: Json.Decoder FlowInput
flowInputDecoder = Json.object $
    FlowInput <$> Json.atKey "flow_id" Json.text

emailInputDecoder :: Json.Decoder EmailInput
emailInputDecoder = Json.object $
    EmailInput <$> Json.atKey "email" Json.text

imapConnectDecoder :: Json.Decoder ImapConnectInput
imapConnectDecoder = Json.object $
    ImapConnectInput
        <$> Json.atKey "email" Json.text
        <*> Json.atKey "label" Json.text
        <*> Json.atKey "host" Json.text
        <*> Json.atKey "port" Json.int
        <*> Json.atKey "tls" Json.text
        <*> Json.atKey "username" Json.text
        <*> Json.atKey "password" Json.text

setEnabledDecoder :: Json.Decoder SetEnabledInput
setEnabledDecoder = Json.object $
    SetEnabledInput
        <$> Json.atKey "account_id" Json.text
        <*> Json.atKey "enabled" Json.bool

accountInputDecoder :: Json.Decoder AccountInput
accountInputDecoder = Json.object $
    AccountInput <$> Json.atKey "account_id" Json.text

startOAuth
    :: MailOAuthRuntime
    -> OAuthStartInput
    -> IO (Either IntegrationError MailOAuthChallenge)
startOAuth runtime input =
    case parseMailProvider input.oauthStartProvider of
        Nothing ->
            pure
                (Left
                    (IntegrationInvalidInput
                        "The email provider is invalid."))
        Just provider ->
            mapError <$> startMailOAuth runtime provider input.oauthStartClientId

pollOAuth
    :: MailOAuthRuntime
    -> IntegrationHost
    -> FlowInput
    -> IO (Either IntegrationError MailOAuthPoll)
pollOAuth runtime host input = do
    result <- mapError <$> pollMailOAuth runtime input.flowInputId
    case result of
        Right (MailOAuthConnected _) ->
            notifyIntegrationToolsChanged host
        _ -> pure ()
    pure result

cancelOAuth
    :: MailOAuthRuntime
    -> FlowInput
    -> IO (Either IntegrationError Success)
cancelOAuth runtime input =
    fmap (fmap (const Success) . mapError)
        (cancelMailOAuth runtime input.flowInputId)

discoverSettings
    :: EmailInput
    -> IO (Either IntegrationError MailAccountDiscovery)
discoverSettings input =
    pure (mapError (discoverMailSettings input.emailInputAddress))

connectImap
    :: IntegrationHost
    -> ImapConnectInput
    -> IO (Either IntegrationError Identifier)
connectImap host input =
    case parseMailTLSMode input.imapConnectTls of
        Nothing ->
            pure
                (Left
                    (IntegrationInvalidInput
                        "The IMAP TLS mode is invalid."))
        Just tlsMode -> do
            result <- fmap (fmap Identifier . mapError) $
                connectMailImapAccount
                    input.imapConnectEmail
                    input.imapConnectLabel
                    MailImapSettings
                        { mailImapHost = input.imapConnectHost
                        , mailImapPort = input.imapConnectPort
                        , mailImapTLSMode = tlsMode
                        , mailImapUsername = input.imapConnectUsername
                        }
                    input.imapConnectPassword
            notifyOnSuccess host result

setEnabled
    :: IntegrationHost
    -> SetEnabledInput
    -> IO (Either IntegrationError Success)
setEnabled host input = do
    result <- fmap (fmap (const Success) . mapError) $
        setMailAccountEnabled input.setEnabledAccountId input.setEnabledValue
    notifyOnSuccess host result

deleteAccount
    :: IntegrationHost
    -> AccountInput
    -> IO (Either IntegrationError Success)
deleteAccount host input = do
    result <- fmap (fmap (const Success) . mapError) $
        deleteMailAccount input.accountInputId
    notifyOnSuccess host result

notifyOnSuccess
    :: IntegrationHost
    -> Either IntegrationError value
    -> IO (Either IntegrationError value)
notifyOnSuccess host result = do
    when (either (const False) (const True) result) $
        notifyIntegrationToolsChanged host
    pure result

mapError :: Either Text value -> Either IntegrationError value
mapError = either (Left . IntegrationOperationFailed) Right

oauthChallengeEncoding :: MailOAuthChallenge -> Aeson.Encoding
oauthChallengeEncoding challenge = Aeson.pairs
    ( "provider" .= mailProviderSlug challenge.mailOAuthProvider
        <> "authorization_url" .= challenge.mailOAuthAuthorizationUrl
        <> "flow_id" .= challenge.mailOAuthFlowId
        <> "expires_in_seconds" .= challenge.mailOAuthExpiresInSeconds
    )

oauthPollEncoding :: MailOAuthPoll -> Aeson.Encoding
oauthPollEncoding poll = Aeson.pairs case poll of
    MailOAuthPending -> "status" .= ("pending" :: Text)
    MailOAuthConnected accountId ->
        "status" .= ("connected" :: Text)
            <> "account_id" .= accountId
    MailOAuthFailed message ->
        "status" .= ("failed" :: Text)
            <> "error" .= message
    MailOAuthCancelled ->
        "status" .= ("cancelled" :: Text)

discoveryEncoding :: MailAccountDiscovery -> Aeson.Encoding
discoveryEncoding = \case
    MailOAuthDiscovery provider -> Aeson.pairs
        ( "kind" .= ("oauth" :: Text)
            <> "provider" .= mailProviderSlug provider
        )
    MailImapDiscovery settings -> Aeson.pairs
        ( "kind" .= ("imap" :: Text)
            <> "host" .= settings.mailImapHost
            <> "port" .= settings.mailImapPort
            <> "tls" .= mailTLSModeSlug settings.mailImapTLSMode
            <> "username" .= settings.mailImapUsername
        )

successEncoding :: Success -> Aeson.Encoding
successEncoding Success = Aeson.pairs ("ok" .= True)

identifierEncoding :: Identifier -> Aeson.Encoding
identifierEncoding (Identifier identifier) =
    Aeson.pairs ("account_id" .= identifier)

emptyObjectSchema :: Value
emptyObjectSchema = closedObject [] []

accountsOutputSchema :: Value
accountsOutputSchema = object
    [ "type" .= ("array" :: Text)
    , "items" .= object ["type" .= ("object" :: Text)]
    ]

oauthStartInputSchema, oauthChallengeOutputSchema, flowInputSchema
    , oauthPollOutputSchema, emailInputSchema, discoveryOutputSchema
    , imapConnectInputSchema, identifierOutputSchema, setEnabledInputSchema
    , accountInputSchema, successOutputSchema :: Value
oauthStartInputSchema = closedObject
    [ "provider" .= stringSchema
    , "client_id" .= stringSchema
    ]
    ["provider", "client_id"]
oauthChallengeOutputSchema = closedObject
    [ "provider" .= stringSchema
    , "authorization_url" .= stringSchema
    , "flow_id" .= stringSchema
    , "expires_in_seconds" .= integerSchema
    ]
    ["provider", "authorization_url", "flow_id", "expires_in_seconds"]
flowInputSchema = closedObject
    ["flow_id" .= stringSchema]
    ["flow_id"]
oauthPollOutputSchema = object ["type" .= ("object" :: Text)]
emailInputSchema = closedObject ["email" .= stringSchema] ["email"]
discoveryOutputSchema = object ["type" .= ("object" :: Text)]
imapConnectInputSchema = closedObject
    [ "email" .= stringSchema
    , "label" .= stringSchema
    , "host" .= stringSchema
    , "port" .= integerSchema
    , "tls" .= stringSchema
    , "username" .= stringSchema
    , "password" .= stringSchema
    ]
    ["email", "label", "host", "port", "tls", "username", "password"]
identifierOutputSchema = closedObject
    ["account_id" .= stringSchema]
    ["account_id"]
setEnabledInputSchema = closedObject
    [ "account_id" .= stringSchema
    , "enabled" .= object ["type" .= ("boolean" :: Text)]
    ]
    ["account_id", "enabled"]
accountInputSchema = closedObject
    ["account_id" .= stringSchema]
    ["account_id"]
successOutputSchema = closedObject
    ["ok" .= object ["const" .= True, "type" .= ("boolean" :: Text)]]
    ["ok"]

closedObject :: [Pair] -> [Text] -> Value
closedObject properties required = object
    [ "type" .= ("object" :: Text)
    , "properties" .= object properties
    , "required" .= required
    , "additionalProperties" .= False
    ]

stringSchema, integerSchema :: Value
stringSchema = object ["type" .= ("string" :: Text)]
integerSchema = object ["type" .= ("integer" :: Text)]

parseMailTLSMode :: Text -> Maybe MailTLSMode
parseMailTLSMode value =
    case Text.toCaseFold (Text.strip value) of
        "tls" -> Just MailImplicitTLS
        "ssl" -> Just MailImplicitTLS
        "implicit_tls" -> Just MailImplicitTLS
        "starttls" -> Just MailStartTLS
        _ -> Nothing

foldlM
    :: (accumulator -> value -> Either error accumulator)
    -> accumulator
    -> [value]
    -> Either error accumulator
foldlM _ accumulator [] = Right accumulator
foldlM step accumulator (value : rest) =
    step accumulator value >>= \next ->
        foldlM step next rest
