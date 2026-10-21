-- | Execution settings transmitted by a local frontend. Presentation options
-- stay in that frontend; hosted servers retain their own execution policy.
module Agent.Runtime.Session.Settings
    ( SessionSettings(..), SessionOperation(..), defaultSessionSettings, validateSessionSettings
    ) where

import Agent.Loop (defaultLoopMaxTurns)
import Agent.Runtime.Request (NativeInteractionMode(..), NativeShellMode(..))
import Agent.Tools.CodeMode.Backend (CodeModeBackend(..))
import Data.Aeson
    ( ToJSON(..), FromJSON(..), object, withObject, (.:?), (.!=), (.=) )
import Data.Text (Text)

data SessionOperation = ConversationTurn | CompactContext | SideQuestion | PrivateQuestion | HarnessCommand
    deriving (Eq, Show)

data SessionSettings = SessionSettings
    { sessionMaxTurns :: !Int
    , sessionMaxConcurrentAgents :: !(Maybe Int)
    , sessionCompactThreshold :: !(Maybe Int)
    , sessionShowRawReasoning :: !Bool
    , sessionAgentsMd :: !Bool
    , sessionSkills :: !Bool
    , sessionComputerUse :: !Bool
    , sessionCodeMode :: !Text
    , sessionCodeModeBackend :: !CodeModeBackend
    , sessionCodeModeRepair :: !Bool
    , sessionOneShot :: !Bool
    , sessionInteractionMode :: !NativeInteractionMode
    , sessionShellMode :: !NativeShellMode
    , sessionReadOnly :: !Bool
    , sessionOperation :: !SessionOperation
    , sessionAllowedTools :: ![Text]
    , sessionFast :: !Bool
    , sessionGoal :: !(Maybe (Text, Maybe Int, Bool))
    }
    deriving (Eq, Show)

defaultSessionSettings :: SessionSettings
defaultSessionSettings = SessionSettings
    { sessionMaxTurns = defaultLoopMaxTurns
    , sessionMaxConcurrentAgents = Nothing
    , sessionCompactThreshold = Nothing
    , sessionShowRawReasoning = False
    , sessionAgentsMd = True
    , sessionSkills = True
    , sessionComputerUse = False
    , sessionCodeMode = "catalog"
    , sessionCodeModeBackend = JavaScriptBackend
    , sessionCodeModeRepair = False
    , sessionOneShot = False
    , sessionInteractionMode = NativeAsk
    , sessionShellMode = NativeShellBash
    , sessionReadOnly = False
    , sessionOperation = ConversationTurn
    , sessionAllowedTools = []
    , sessionFast = False
    , sessionGoal = Nothing
    }

validateSessionSettings :: SessionSettings -> Either Text SessionSettings
validateSessionSettings settings
    | settings.sessionMaxTurns < 1 || settings.sessionMaxTurns > 100000 =
        Left "maxTurns must be between 1 and 100000"
    | maybe False (\n -> n < 1 || n > 256) settings.sessionMaxConcurrentAgents =
        Left "maxConcurrentAgents must be between 1 and 256"
    | maybe False (< 1) settings.sessionCompactThreshold =
        Left "compactThreshold must be positive"
    | settings.sessionCodeMode `notElem` ["catalog", "enabled", "disabled"] =
        Left "codeMode must be catalog, enabled or disabled"
    | otherwise = Right settings

instance ToJSON SessionSettings where
    toJSON settings = object
        [ "maxTurns" .= settings.sessionMaxTurns
        , "maxConcurrentAgents" .= settings.sessionMaxConcurrentAgents
        , "compactThreshold" .= settings.sessionCompactThreshold
        , "showRawReasoning" .= settings.sessionShowRawReasoning
        , "agentsMd" .= settings.sessionAgentsMd
        , "skills" .= settings.sessionSkills
        , "computerUse" .= settings.sessionComputerUse
        , "codeMode" .= settings.sessionCodeMode
        , "codeModeBackend" .= case settings.sessionCodeModeBackend of
            JavaScriptBackend -> ("javascript" :: Text)
            HaskellBackend -> "haskell"
        , "codeModeRepair" .= settings.sessionCodeModeRepair
        , "oneShot" .= settings.sessionOneShot
        , "interactionMode" .= case settings.sessionInteractionMode of
            NativeAsk -> ("ask" :: Text)
            NativeYolo -> "yolo"
            NativePlan -> "plan"
        , "shellMode" .= case settings.sessionShellMode of
            NativeShellNone -> ("none" :: Text)
            NativeShellBash -> "bash"
            NativeShellGhci -> "ghci"
            NativeShellBoth -> "both"
        , "readOnly" .= settings.sessionReadOnly
        , "allowedTools" .= settings.sessionAllowedTools
        , "fast" .= settings.sessionFast
        , "goal" .= settings.sessionGoal
        , "operation" .= case settings.sessionOperation of
            ConversationTurn -> ("turn" :: Text)
            CompactContext -> "compact"
            SideQuestion -> "btw"
            PrivateQuestion -> "private"
            HarnessCommand -> "command"
        ]

instance FromJSON SessionSettings where
    parseJSON = withObject "session options" \value -> do
        backend <- value .:? "codeModeBackend" .!= ("javascript" :: Text)
        sessionCodeModeBackend <- case backend of
            "javascript" -> pure JavaScriptBackend
            "haskell" -> pure HaskellBackend
            _ -> fail "unknown codeModeBackend"
        sessionMaxTurns <- value .:? "maxTurns" .!= defaultLoopMaxTurns
        sessionMaxConcurrentAgents <- value .:? "maxConcurrentAgents"
        sessionCompactThreshold <- value .:? "compactThreshold"
        sessionShowRawReasoning <- value .:? "showRawReasoning" .!= False
        sessionAgentsMd <- value .:? "agentsMd" .!= True
        sessionSkills <- value .:? "skills" .!= True
        sessionComputerUse <- value .:? "computerUse" .!= False
        sessionCodeMode <- value .:? "codeMode" .!= "catalog"
        sessionCodeModeRepair <- value .:? "codeModeRepair" .!= False
        sessionOneShot <- value .:? "oneShot" .!= False
        mode <- value .:? "interactionMode" .!= ("ask" :: Text)
        sessionInteractionMode <- case mode of
            "ask" -> pure NativeAsk
            "yolo" -> pure NativeYolo
            "plan" -> pure NativePlan
            _ -> fail "unknown interactionMode"
        shell <- value .:? "shellMode" .!= ("bash" :: Text)
        sessionShellMode <- case shell of
            "none" -> pure NativeShellNone
            "bash" -> pure NativeShellBash
            "ghci" -> pure NativeShellGhci
            "both" -> pure NativeShellBoth
            _ -> fail "unknown shellMode"
        sessionReadOnly <- value .:? "readOnly" .!= False
        sessionAllowedTools <- value .:? "allowedTools" .!= []
        sessionFast <- value .:? "fast" .!= False
        sessionGoal <- value .:? "goal"
        operation <- value .:? "operation" .!= ("turn" :: Text)
        sessionOperation <- case operation of
            "turn" -> pure ConversationTurn
            "compact" -> pure CompactContext
            "btw" -> pure SideQuestion
            "private" -> pure PrivateQuestion
            "command" -> pure HarnessCommand
            _ -> fail "unknown operation"
        either (fail . show) pure (validateSessionSettings SessionSettings{..})
