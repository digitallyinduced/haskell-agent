module Agent.TUI.ModelPropertySpec (spec) where

import Agent.Loop (LoopEvent(..))
import Agent.TUI.Model
    ( BlockId(..)
    , BlockState(..)
    , Focus(..)
    , PermissionOverlay(..)
    , UiNotice
    , UiBlock(..)
    , UiEvent(..)
    , UiState(..)
    , errorNotice
    , initialUiState
    , reduceUi
    , warningNotice
    )
import Agent.TUI.TextWidth (graphemeClusters)
import Agent.ToolDispatch
    ( ToolCallResult(..)
    , ToolCallKind(..)
    , functionToolCall
    )
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Foldable as Foldable
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as Text
import Test.Hspec
import Test.Hspec.QuickCheck (modifyMaxSuccess, prop)
import Test.QuickCheck
    ( Arbitrary(..)
    , Gen
    , Property
    , chooseInt
    , conjoin
    , counterexample
    , elements
    , frequency
    , listOf
    , property
    , resize
    , shrinkList
    , (===)
    , (.&&.)
    , (=/=)
    )

spec :: Spec
spec =
    describe "generated fullscreen reducer traces" do
        modifyMaxSuccess (const 500) $
            prop "preserve UiState invariants after every event prefix" $
                \(EventTrace events) -> traceProperty events
        modifyMaxSuccess (const 500) $
            prop "matches navigation and permission reference semantics" $
                \(NavigationTrace commands) ->
                    navigationTraceProperty commands
        modifyMaxSuccess (const 500) $
            prop "keeps state-aware tool lifecycles consistent" $
                \(ToolTrace commands) -> toolTraceProperty commands

newtype EventTrace = EventTrace [UiEvent]
    deriving (Show)

instance Arbitrary EventTrace where
    arbitrary = EventTrace <$> listOf generatedUiEvent

traceProperty :: [UiEvent] -> Property
traceProperty events =
    conjoin
        [ counterexample ("event prefix: " <> show prefix)
            (stateInvariant state)
        | (prefix, state) <- traceStates events
        ]

traceStates :: [UiEvent] -> [([UiEvent], UiState)]
traceStates events =
    drop 1 (scanl step ([], initialUiState) events)
  where
    step (prefix, state) event =
        (prefix <> [event], reduceUi event state)

stateInvariant :: UiState -> Property
stateInvariant state =
    conjoin
        [ counterexample "block IDs must be unique"
            (ids == List.nub ids)
        , counterexample "block index map must match the sequence"
            (state.uiBlockIndices === expectedIndices)
        , counterexample "next block ID must be fresh"
            (all (< state.uiNextBlockId) numericIds)
        , counterexample "selected block and index must agree"
            selectedInvariant
        , counterexample "cursor must stay within the draft"
            (state.uiCursor >= 0
                && state.uiCursor <= Text.length state.uiDraft)
        , counterexample "cursor must stay on a grapheme boundary"
            (state.uiCursor `elem` graphemeBoundaries state.uiDraft)
        , counterexample "permission choices must stay in range"
            permissionInvariant
        ]
  where
    blocks = Foldable.toList state.uiBlocks
    ids = map (.blockId) blocks
    numericIds = [number | BlockId number <- ids]
    expectedIndices =
        Map.fromList
            [ (block.blockId, index)
            | (index, block) <- zip [0 ..] blocks
            ]
    selectedInvariant =
        case state.uiSelectedBlock of
            Nothing -> state.uiSelectedBlockIndex === Nothing
            Just ident ->
                case List.findIndex ((== ident) . (.blockId)) blocks of
                    Nothing -> property False
                    Just index -> state.uiSelectedBlockIndex === Just index
    permissionInvariant =
        case state.uiPermission of
            Nothing -> True
            Just (PermissionOverlay _ index) ->
                index >= 0 && index < 4

graphemeBoundaries :: Text -> [Int]
graphemeBoundaries text =
    scanl (+) 0 (map Text.length (graphemeClusters text))

-- Keep the generator deliberately focused on reducer inputs which do not
-- require constructing provider/tool values. The reducer is still exercised
-- through long mixed traces, including stream boundaries and destructive
-- conversation operations.
generatedUiEvent :: Gen UiEvent
generatedUiEvent = frequency
    [ (5, UiUserSubmitted <$> generatedText)
    , (4, UiAssistantHistory <$> generatedText)
    , (4, UiHistory <$> generatedText)
    , (4, UiSystemMessage <$> generatedText)
    , (4, UiErrorMessage <$> generatedText)
    , (4, UiSetDraft <$> generatedText <*> chooseInt (-20, 120))
    , (3, UiInputQueued <$> generatedText)
    , (3, UiInputPromoted <$> generatedText)
    , (3, UiSetPromptEffort <$> generatedText)
    , (3, UiSetRepository <$> generatedText <*> generatedText)
    , (3, UiSetNotice <$> generatedNotice)
    , (3, UiMoveSelection <$> chooseInt (-20, 20))
    , (3, UiSelectBlock . BlockId <$> chooseInt (-10, 30))
    , (3, UiActivateBlock . BlockId <$> chooseInt (-10, 30))
    , (3, UiFocusChanged <$> elements
        [FocusComposer, FocusScrollback, FocusPermission])
    , (3, UiPermissionShown <$> generatedText)
    , (3, UiPermissionMoved <$> chooseInt (-20, 20))
    , (3, pure UiPermissionHidden)
    , (3, UiRetryCountdown <$> generatedText
        <*> chooseInt (-1000, 120000)
        <*> generatedText)
    , (3, UiSetFollow <$> arbitraryBool)
    , (3, UiSetAwaitingInput <$> arbitraryBool)
    , (3, UiTurnEnded <$> elements
        [BlockComplete, BlockFailed, BlockCancelled, BlockDenied])
    , (3, pure UiTurnRestarted)
    , (2, pure UiConversationCleared)
    , (2, pure UiDraftSubmitted)
    , (2, pure UiQueuedInputStarted)
    , (2, UiLoop <$> generatedLoopEvent)
    ]

generatedLoopEvent :: Gen LoopEvent
generatedLoopEvent = frequency
    [ (4, TextDelta <$> generatedText)
    , (4, ReasoningDelta <$> generatedText)
    , (3, ActivityUpdated <$> generatedText)
    , (3, WarningRaised <$> generatedText)
    , (2, ResponseRestarted <$> generatedText)
    , (2, pure TurnStarted)
    ]

generatedNotice :: Gen (Maybe UiNotice)
generatedNotice = frequency
    [ (2, pure Nothing)
    , (1, Just . warningNotice <$> generatedText)
    , (1, Just . errorNotice <$> generatedText)
    ]

generatedText :: Gen Text
generatedText =
    Text.pack <$> resize 24 (listOf (elements alphabet))
  where
    alphabet =
        "abc XYZ012-_/\n\té界🚀e\769"

arbitraryBool :: Gen Bool
arbitraryBool = elements [False, True]

data NavigationCommand
    = AppendBlock !Text
    | MoveSelection !Int
    | SelectSlot !Int
    | SetFollow !Bool
    | ShowPermission
    | MovePermission !Int
    | HidePermission
    | SetFocus !Focus
    | ClearNavigation
    deriving (Eq, Show)

newtype NavigationTrace = NavigationTrace [NavigationCommand]
    deriving (Eq, Show)

instance Arbitrary NavigationTrace where
    arbitrary =
        NavigationTrace <$> resize 40 (listOf generatedNavigationCommand)
    shrink (NavigationTrace commands) =
        NavigationTrace <$> shrinkList (const []) commands

generatedNavigationCommand :: Gen NavigationCommand
generatedNavigationCommand = frequency
    [ (5, AppendBlock <$> generatedText)
    , (4, MoveSelection <$> chooseInt (-20, 20))
    , (4, SelectSlot <$> chooseInt (-5, 20))
    , (3, SetFollow <$> arbitraryBool)
    , (3, pure ShowPermission)
    , (4, MovePermission <$> chooseInt (-20, 20))
    , (3, pure HidePermission)
    , (2, SetFocus <$> elements
        [FocusComposer, FocusScrollback, FocusPermission])
    , (2, pure ClearNavigation)
    ]

data NavigationModel = NavigationModel
    { navigationBlockIds      :: ![Int]
    , navigationNextBlockId   :: !Int
    , navigationSelectedIndex :: !(Maybe Int)
    , navigationFollow        :: !Bool
    , navigationPermission    :: !(Maybe Int)
    , navigationFocus         :: !Focus
    }
    deriving (Eq, Show)

initialNavigationModel :: NavigationModel
initialNavigationModel = NavigationModel
    { navigationBlockIds = []
    , navigationNextBlockId = 1
    , navigationSelectedIndex = Nothing
    , navigationFollow = True
    , navigationPermission = Nothing
    , navigationFocus = FocusComposer
    }

navigationTraceProperty :: [NavigationCommand] -> Property
navigationTraceProperty commands =
    conjoin
        [ counterexample
            ("navigation command prefix: " <> show prefix)
            (navigationProjection state === expectedProjection expected)
        | (prefix, expected, state) <- traceNavigationStates commands
        ]

traceNavigationStates
    :: [NavigationCommand]
    -> [([NavigationCommand], NavigationModel, UiState)]
traceNavigationStates commands =
    go [] initialNavigationModel initialUiState commands
  where
    go _ _ _ [] = []
    go prefix expected state (command : rest) =
        let event = navigationEvent expected command
            expected' = applyNavigationModel command expected
            state' = reduceUi event state
            prefix' = prefix <> [command]
        in (prefix', expected', state')
            : go prefix' expected' state' rest

applyNavigationModel
    :: NavigationCommand
    -> NavigationModel
    -> NavigationModel
applyNavigationModel command model = case command of
    AppendBlock _ ->
        let index = length model.navigationBlockIds
        in model
            { navigationBlockIds =
                model.navigationBlockIds <> [model.navigationNextBlockId]
            , navigationNextBlockId = model.navigationNextBlockId + 1
            , navigationSelectedIndex = Just index
            }
    MoveSelection delta ->
        case model.navigationBlockIds of
            [] -> model { navigationSelectedIndex = Nothing }
            blockIds ->
                let lastIndex = length blockIds - 1
                    current =
                        maybe lastIndex id model.navigationSelectedIndex
                    selected = max 0 (min lastIndex (current + delta))
                in model
                    { navigationSelectedIndex = Just selected
                    , navigationFollow = selected == lastIndex
                    }
    SelectSlot slot ->
        case selectedSlotIndex slot model.navigationBlockIds of
            Nothing -> model
            Just selected ->
                model
                    { navigationSelectedIndex = Just selected
                    , navigationFollow =
                        selected == length model.navigationBlockIds - 1
                    }
    SetFollow follow ->
        model
            { navigationFollow = follow
            , navigationSelectedIndex =
                if follow
                    then case model.navigationBlockIds of
                        [] -> Nothing
                        blockIds -> Just (length blockIds - 1)
                    else model.navigationSelectedIndex
            }
    ShowPermission ->
        model
            { navigationPermission = Just 0
            , navigationFocus = FocusPermission
            }
    MovePermission delta ->
        model
            { navigationPermission =
                (\index -> (index + delta) `mod` 4)
                    <$> model.navigationPermission
            }
    HidePermission ->
        model
            { navigationPermission = Nothing
            , navigationFocus = FocusComposer
            }
    SetFocus focus ->
        model { navigationFocus = focus }
    ClearNavigation ->
        model
            { navigationBlockIds = []
            , navigationNextBlockId = 1
            , navigationSelectedIndex = Nothing
            }

navigationEvent :: NavigationModel -> NavigationCommand -> UiEvent
navigationEvent model command = case command of
    AppendBlock text -> UiSystemMessage text
    MoveSelection delta -> UiMoveSelection delta
    SelectSlot slot ->
        UiSelectBlock
            (case selectedSlotIndex slot model.navigationBlockIds
                    >>= (`listAt` model.navigationBlockIds) of
                Just ident -> BlockId ident
                Nothing -> BlockId (-1000 - abs slot))
    SetFollow follow -> UiSetFollow follow
    ShowPermission -> UiPermissionShown "generated permission"
    MovePermission delta -> UiPermissionMoved delta
    HidePermission -> UiPermissionHidden
    SetFocus focus -> UiFocusChanged focus
    ClearNavigation -> UiConversationCleared

selectedSlotIndex :: Int -> [Int] -> Maybe Int
selectedSlotIndex slot blockIds
    | slot < 0 || null blockIds = Nothing
    | otherwise = Just (slot `mod` length blockIds)

type NavigationProjection =
    ([BlockId], Int, Maybe BlockId, Maybe Int, Bool, Maybe Int, Focus)

navigationProjection :: UiState -> NavigationProjection
navigationProjection state =
    ( map (.blockId) (Foldable.toList state.uiBlocks)
    , state.uiNextBlockId
    , state.uiSelectedBlock
    , state.uiSelectedBlockIndex
    , state.uiFollow
    , (.permissionIndex) <$> state.uiPermission
    , state.uiFocus
    )

expectedProjection :: NavigationModel -> NavigationProjection
expectedProjection model =
    ( map BlockId model.navigationBlockIds
    , model.navigationNextBlockId
    , BlockId <$> (model.navigationSelectedIndex
        >>= (`listAt` model.navigationBlockIds))
    , model.navigationSelectedIndex
    , model.navigationFollow
    , model.navigationPermission
    , model.navigationFocus
    )

listAt :: Int -> [value] -> Maybe value
listAt index values
    | index < 0 = Nothing
    | otherwise = case drop index values of
        value : _ -> Just value
        [] -> Nothing

-- | Commands are generated with knowledge of which call ids are currently
-- active. Updates and completions therefore exercise meaningful tool
-- lifecycles, while unknown ids provide adversarial noise.
data ToolCommand
    = StartTool !Text
    | UpdateTool !Text !Text
    | FinishTool !Text !Text
    | BeginTurn
    | EndTurn
    | ClearConversation
    | RestartTurn
    deriving (Eq, Show)

newtype ToolTrace = ToolTrace [ToolCommand]
    deriving (Eq, Show)

instance Arbitrary ToolTrace where
    arbitrary = ToolTrace <$> generateToolCommands 24 [] [] 1
    -- The oracle treats inactive IDs as noise, so deleting commands preserves
    -- validity while yielding useful minimal lifecycle counterexamples.
    shrink (ToolTrace commands) =
        ToolTrace <$> shrinkList (const []) commands

generateToolCommands :: Int -> [Text] -> [Text] -> Int -> Gen [ToolCommand]
generateToolCommands remaining active known nextId
    | remaining <= 0 = pure []
    | otherwise = do
        (command, nextActive, nextId') <- frequency
            ( [ (5, do
                    let callId = "generated-" <> Text.pack (show nextId)
                    pure (StartTool callId, callId : active, nextId + 1))
              , (if null active then 1 else 5, do
                    callId <- chooseActiveOrUnknown active known
                    output <- generatedOutput
                    pure (UpdateTool callId output, active, nextId))
              , (if null active then 1 else 5, do
                    callId <- chooseActiveOrUnknown active known
                    output <- generatedOutput
                    pure (FinishTool callId output, deleteOne callId active, nextId))
              , (2, pure (EndTurn, [], nextId))
              , (2, pure (ClearConversation, [], nextId))
              , (2, pure (RestartTurn, [], nextId))
              ]
                <> [ (2, pure (BeginTurn, [], nextId))
                   | null active
                   ])
        let nextKnown = List.nub (known <> case command of
                StartTool callId -> [callId]
                _ -> [])
        rest <- generateToolCommands (remaining - 1) nextActive nextKnown nextId'
        pure (command : rest)

chooseActiveOrUnknown :: [Text] -> [Text] -> Gen Text
chooseActiveOrUnknown active known =
    elements (List.nub (if null active then ["unknown"] else active
        <> known <> ["unknown"]))

deleteOne :: Eq value => value -> [value] -> [value]
deleteOne _ [] = []
deleteOne wanted (value : values)
    | wanted == value = values
    | otherwise = value : deleteOne wanted values

generatedOutput :: Gen Text
generatedOutput =
    Text.pack <$> resize 18 (listOf (elements ("output-012 XYZ\n" :: String)))

toolTraceProperty :: [ToolCommand] -> Property
toolTraceProperty commands =
    conjoin
        [ counterexample
            ("tool command prefix: " <> show prefix)
            (toolStateInvariant expected after
                .&&. toolCommandPostcondition command before after)
        | (prefix, command, expected, before, after) <- traceToolStates commands
        ]

traceToolStates
    :: [ToolCommand]
    -> [([ToolCommand], ToolCommand, Map.Map Text Text, UiState, UiState)]
traceToolStates commands =
    go [] Map.empty initialUiState commands
  where
    go _ _ _ [] = []
    go prefix expected state (command : rest) =
        let expected' = expectedToolState command expected
            state' = reduceUi (toolEvent command) state
            prefix' = prefix <> [command]
        in (prefix', command, expected', state, state')
            : go prefix' expected' state' rest

expectedToolState :: ToolCommand -> Map.Map Text Text -> Map.Map Text Text
expectedToolState command active = case command of
    StartTool callId -> Map.insert callId "" active
    UpdateTool callId output
        | Map.member callId active -> Map.insert callId output active
        | otherwise -> active
    FinishTool callId _ -> Map.delete callId active
    BeginTurn -> Map.empty
    EndTurn -> Map.empty
    ClearConversation -> Map.empty
    RestartTurn -> Map.empty

toolStateInvariant :: Map.Map Text Text -> UiState -> Property
toolStateInvariant expected state =
    conjoin
        [ counterexample "active call IDs must match the reducer map"
            (Map.keysSet expected === Map.keysSet state.uiToolCalls)
        , counterexample "every active call must point to its running block"
            (conjoin (map activeEntryInvariant (Map.toList expected)))
        , counterexample "every running tool block must be active"
            (conjoin
                [ case block.blockCallId of
                    Nothing -> property True
                    Just callId ->
                        case Map.lookup callId state.uiToolCalls of
                            Just (index, _call) ->
                                Seq.lookup index state.uiBlocks === Just block
                            Nothing -> property False
                | block <- Foldable.toList state.uiBlocks
                , block.blockState == BlockRunning
                ])
        ]
  where
    activeEntryInvariant (callId, expectedOutput) =
        case Map.lookup callId state.uiToolCalls of
            Nothing -> property False
            Just (index, _call) ->
                case Seq.lookup index state.uiBlocks of
                    Just block ->
                        conjoin
                            [ block.blockCallId === Just callId
                            , block.blockState === BlockRunning
                            , block.blockBody === expectedOutput
                            ]
                    Nothing -> property False
toolCommandPostcondition :: ToolCommand -> UiState -> UiState -> Property
toolCommandPostcondition command before state = case command of
    FinishTool callId output ->
        if Map.member callId before.uiToolCalls
            then case List.find ((== Just callId) . (.blockCallId))
                    (reverse (Foldable.toList state.uiBlocks)) of
                Just block ->
                    conjoin
                        [ block.blockState =/= BlockRunning
                        , block.blockBody === output
                        ]
                Nothing -> property False
            else property True
    EndTurn ->
        conjoin
            [ Map.null state.uiToolCalls === True
            , all ((/= BlockRunning) . (.blockState))
                (Foldable.toList state.uiBlocks) === True
            ]
    _ -> property True

toolEvent :: ToolCommand -> UiEvent
toolEvent command = case command of
    StartTool callId ->
        UiLoop (ToolStarted (functionToolCall callId "echo" "{}"))
    UpdateTool callId output ->
        UiLoop (ToolOutputUpdated callId output)
    FinishTool callId output ->
        UiLoop (ToolFinished ToolCallResult
            { callId
            , output
            , callKind = FunctionCallKind
            })
    BeginTurn -> UiLoop TurnStarted
    EndTurn -> UiTurnEnded BlockCancelled
    ClearConversation -> UiConversationCleared
    RestartTurn -> UiTurnRestarted
