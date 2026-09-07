-- | Native plan-mode interactions and tool/root permission requests.
-- Callback locks and pending waiters retain their existing ownership.
module Agent.CLI.MacOS.NativeInteraction
    ( nativePlanModeHooks
    , requestApproval
    , requestApprovalFromClient
    , boundedApprovalArguments
    , requestRootAccessFromClient
    , resolveApproval
    ) where

import Agent.CLI.MacOS.EngineEvents
import Agent.CLI.MacOS.InteractionState
import Agent.CLI.MacOS.NativeRequest
import Agent.CLI.MacOS.TurnState (TurnControl(..))
import Agent.CLI.Permission (PermissionChoice(..))
import Agent.CLI.Render (summarizeToolCall)
import Agent.ToolDispatch
    ( ToolCall(..), ToolCallMode(..), ToolCallKind(..)
    , toolCallMode, isComputerToolCallKind
    )
import Agent.Tools.PlanMode (PlanDecision(..), PlanModeHooks(..))
import Control.Concurrent.MVar (withMVar)
import Control.Concurrent.STM
import Control.Exception.Safe (finally, onException)
import Control.Monad (forM_)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import Data.Word (Word8)
import Foreign (FunPtr, Ptr, allocaArray, castPtr, nullPtr, pokeElemOff)
import Foreign.C.Types (CInt, CSize)
import System.OsPath (OsPath, decodeFS)

withTextBytes :: Text -> (Ptr Word8 -> CSize -> IO a) -> IO a
withTextBytes value action =
    BS.useAsCStringLen (TextEncoding.encodeUtf8 value) \(pointer, length) ->
        action (castPtr pointer) (fromIntegral length)

nativePlanModeHooks
    :: TurnControl
    -> InteractionRuntime
    -> PlanModeHooks
nativePlanModeHooks control interactions = PlanModeHooks
    { planConfirmEnter = \reason ->
        requestNativeInteraction
            control interactions 1 reason
            [ "Enter plan mode"
            , "Stay in normal mode"
            ] >>= \case
                Just resolution ->
                    pure (resolution.interactionSelectedIndex == 0)
                Nothing -> pure False
    , planDecideExit = \planBody ->
        requestNativeInteraction
            control interactions 2 planBody
            [ "Approve and implement"
            , "Request changes"
            , "Cancel plan"
            ] >>= \case
                Just resolution ->
                    pure $ case resolution.interactionSelectedIndex of
                        0 -> PlanApprove
                        1 ->
                            PlanRequestChanges
                                (fromMaybe
                                    "(no notes)"
                                    (nonBlank
                                        resolution.interactionCustomText))
                        _ -> PlanCancel
                Nothing -> pure PlanCancel
    , planAskQuestion = \question options ->
        requestNativeInteraction
            control interactions 3 question options >>= \case
                Nothing -> pure Nothing
                Just resolution
                    | resolution.interactionSelectedIndex >= 0 ->
                        pure $
                            atMay
                                resolution.interactionSelectedIndex
                                options
                    | otherwise ->
                        pure (nonBlank resolution.interactionCustomText)
    }
  where
    nonBlank = (>>= \text ->
        let stripped = Text.strip text
        in if Text.null stripped then Nothing else Just stripped)

requestNativeInteraction
    :: TurnControl
    -> InteractionRuntime
    -> CInt
    -> Text
    -> [Text]
    -> IO (Maybe NativeInteractionResolution)
requestNativeInteraction control interactions kind prompt options = do
    waiter <- newEmptyTMVarIO
    registration <-
        withMVar interactions.interactionCallbackLock \_ -> do
            registered <- atomically do
                target <- readTVar interactions.interactionCallbackTarget
                case target of
                    Nothing -> pure Nothing
                    Just callbackTarget -> do
                        interactionID <- register waiter
                        pure (Just (callbackTarget, interactionID))
            forM_ registered \(callbackTarget, interactionID) ->
                sendNativeInteraction
                    callbackTarget
                    control.turnControlId
                    interactionID
                    kind
                    prompt
                    options
                    `onException`
                        atomically
                            (modifyTVar'
                                interactions.interactionPending
                                (Map.delete
                                    (control.turnControlId, interactionID)))
            pure registered
    case registration of
        Nothing -> pure Nothing
        Just (_, interactionID) -> do
            let cleanup =
                    atomically $ modifyTVar'
                        interactions.interactionPending
                        (Map.delete
                            (control.turnControlId, interactionID))
            (Just <$> atomically (takeTMVar waiter))
                `finally` cleanup
  where
    register waiter = do
        current <- readTVar control.turnControlInteractionCounter
        let next = current + 1
            interactionID =
                control.turnControlId
                    <> "-interaction-"
                    <> Text.pack (show next)
        writeTVar control.turnControlInteractionCounter next
        modifyTVar'
            interactions.interactionPending
            (Map.insert
                (control.turnControlId, interactionID)
                PendingInteraction
                    { pendingInteractionOptionCount = length options
                    , pendingInteractionWaiter = waiter
                    })
        pure interactionID

sendNativeInteraction
    :: InteractionCallbackTarget
    -> Text
    -> Text
    -> CInt
    -> Text
    -> [Text]
    -> IO ()
sendNativeInteraction target turnID interactionID kind prompt options =
    withTextBytes turnID \turnPointer turnLength ->
    withTextBytes interactionID
        \interactionPointer interactionLength ->
    withTextBytes prompt \promptPointer promptLength ->
    withInteractionOptions options \optionPointer optionCount ->
        invokeInteractionCallback
            target.interactionTargetCallback
            target.interactionTargetContext
            turnPointer
            turnLength
            interactionPointer
            interactionLength
            kind
            promptPointer
            promptLength
            optionPointer
            optionCount

withInteractionOptions
    :: [Text]
    -> (Ptr CInteractionOption -> CSize -> IO a)
    -> IO a
withInteractionOptions [] action = action nullPtr 0
withInteractionOptions options action =
    withEncodedOptions options \encoded ->
        allocaArray (length encoded) \pointer -> do
            forM_ (zip [0..] encoded) \(index, (label, labelLength)) ->
                pokeElemOff pointer index CInteractionOption
                    { cInteractionOptionLabel = label
                    , cInteractionOptionLabelLength = labelLength
                    }
            action pointer (fromIntegral (length encoded))

withEncodedOptions
    :: [Text]
    -> ([(Ptr Word8, CSize)] -> IO a)
    -> IO a
withEncodedOptions [] action = action []
withEncodedOptions (option : rest) action =
    withTextBytes option \pointer length ->
        withEncodedOptions rest
            (action . ((pointer, length) :))

atMay :: Int -> [a] -> Maybe a
atMay index values
    | index < 0 = Nothing
    | otherwise = case drop index values of
        value : _ -> Just value
        [] -> Nothing

requestApproval
    :: FunPtr EventCallback
    -> Ptr ()
    -> TurnControl
    -> ToolCall
    -> IO (Maybe PermissionChoice)
requestApproval callback context control call = do
    alreadyAllowed <- Set.member call.name
        <$> readTVarIO control.turnControlAllowedTools
    if alreadyAllowed && not (isComputerToolCallKind call.callKind)
      then pure (Just PermissionAllowOnce)
      else requestApprovalFromClient callback context control call

requestApprovalFromClient
    :: FunPtr EventCallback
    -> Ptr ()
    -> TurnControl
    -> ToolCall
    -> IO (Maybe PermissionChoice)
requestApprovalFromClient callback context control call = do
    waiter <- newEmptyTMVarIO
    approvalId <- atomically do
        current <- readTVar control.turnControlApprovalCounter
        let next = current + 1
            approvalId =
                control.turnControlId
                    <> "-approval-"
                    <> Text.pack (show next)
        writeTVar control.turnControlApprovalCounter next
        modifyTVar'
            control.turnControlApprovals
            (Map.insert approvalId waiter)
        pure approvalId
    let (arguments, truncated) = boundedApprovalArguments call
    sendEvent callback context $
        Aeson.object
            [ "event" Aeson..= ("approval.requested" :: Text)
            , "turnId" Aeson..= control.turnControlId
            , "approval" Aeson..= Aeson.object
                [ "id" Aeson..= approvalId
                , "callId" Aeson..= call.callId
                , "name" Aeson..= call.name
                , "summary" Aeson..= summarizeToolCall call
                , "arguments" Aeson..= arguments
                , "argumentsEncrypted" Aeson..= call.argumentsEncrypted
                , "async" Aeson..= (toolCallMode call == AsyncToolCall)
                , "truncated" Aeson..= truncated
                ]
            ]
    choice <- atomically (takeTMVar waiter)
    atomically $
        modifyTVar'
            control.turnControlApprovals
            (Map.delete approvalId)
    case choice of
        PermissionAllowTool
            | not (isComputerToolCallKind call.callKind) ->
            atomically $
                modifyTVar'
                    control.turnControlAllowedTools
                    (Set.insert call.name)
        _ -> pure ()
    pure (Just choice)

boundedApprovalArguments :: ToolCall -> (Text, Bool)
boundedApprovalArguments call
    | call.argumentsEncrypted = ("", False)
    | call.name == "email_send" =
        boundedText maximumEmailSendApprovalCharacters call.arguments
    | otherwise = boundedEventText call.arguments
  where
    boundedText maximum value =
        let (visible, remainder) = Text.splitAt maximum value
        in (visible, not (Text.null remainder))

maximumEmailSendApprovalCharacters :: Int
maximumEmailSendApprovalCharacters = 1024 * 1024

requestRootAccessFromClient
    :: FunPtr EventCallback
    -> Ptr ()
    -> TurnControl
    -> OsPath
    -> IO Bool
requestRootAccessFromClient callback context control root = do
    path <- Text.pack <$> decodeFS root
    choice <- requestApprovalFromClient callback context control ToolCall
        { callId = control.turnControlId <> "-filesystem-root-access"
        , name = "filesystem_root_access"
        , arguments =
            TextEncoding.decodeUtf8
                . LBS.toStrict
                . Aeson.encode
                $ Aeson.object ["path" Aeson..= path]
        , callKind = FunctionCallKind
        , argumentsEncrypted = False
        }
    -- Root grants are maintained by the filesystem permission store, not by
    -- the per-turn "allow this tool" shortcut used for ordinary tool calls.
    atomically $
        modifyTVar'
            control.turnControlAllowedTools
            (Set.delete "filesystem_root_access")
    pure $ case choice of
        Nothing -> False
        Just PermissionDeny -> False
        Just _ -> True

resolveApproval :: TurnControl -> BridgeRequest -> IO Aeson.Value
resolveApproval control request =
    case (parseParams request
        :: Either Text ApprovalResolution) of
        Left err -> pure (failureEvent request.requestId err)
        Right resolution ->
            case permissionChoice resolution.approvalResolutionDecision of
                Nothing ->
                    pure $ failureEvent
                        request.requestId
                        "unknown approval decision"
                Just choice -> do
                    accepted <- atomically do
                        current <- readTVar control.turnControlApprovals
                        case Map.lookup
                            resolution.approvalResolutionId
                            current of
                                Nothing -> pure False
                                Just waiter -> do
                                    published <- tryPutTMVar waiter choice
                                    if published
                                        then writeTVar
                                            control.turnControlApprovals
                                            (Map.delete
                                                resolution.approvalResolutionId
                                                current)
                                        else pure ()
                                    pure published
                    pure $
                        if accepted
                            then successEvent request.requestId True
                            else failureEvent
                                request.requestId
                                "approval request is no longer active"

permissionChoice :: Text -> Maybe PermissionChoice
permissionChoice = \case
    "allow_once" -> Just PermissionAllowOnce
    "allow_tool" -> Just PermissionAllowTool
    "deny" -> Just PermissionDeny
    _ -> Nothing
