-- | Pure conversation viewport policy for Grok-style page flips.
module Agent.CLI.TUI.Scroll
    ( ConversationAnchor(..)
    , ConversationPhase(..)
    , ConversationScroll(..)
    , ConversationScrollGesture(..)
    , conversationScrollGesture
    , conversationAnchorSticky
    , followConversationTail
    , reconcileConversationFollow
    , reflowConversationAnchor
    , startConversationAnchor
    ) where

import Agent.TUI.Model (BlockId)
import Data.Text (Text)

data ConversationPhase
    = ConversationFillingPage
    | ConversationFollowingTail
    deriving (Eq, Show)

data ConversationAnchor = ConversationAnchor
    { anchorBlockId :: !BlockId
    , anchorText :: !Text
    , anchorTop :: !Int
    , anchorReserveRows :: !Int
    , anchorViewportTop :: !Int
    , anchorPhase :: !ConversationPhase
    }
    deriving (Eq, Show)

data ConversationScroll
    = KeepConversationPosition
    | ScrollConversationToEnd
    deriving (Eq, Show)

data ConversationScrollGesture
    = IgnoreConversationScroll
    | PauseAndScrollConversation
    | ResumeConversationFollow
    deriving (Eq, Show)

-- | Decide whether a requested scroll can move the conversation viewport.
--
-- Empty sessions intentionally render without a viewport, and a viewport at
-- its top cannot move farther up unless older persisted turns can still be
-- loaded. The latter must pause live following so a just-requested page is
-- not immediately scrolled back to the tail.
conversationScrollGesture
    :: Bool
    -- ^ Whether older persisted turns can still be loaded.
    -> Int
    -- ^ Signed scroll amount.
    -> Maybe (Int, Int, Int)
    -- ^ Viewport top, viewport height, and content height.
    -> ConversationScrollGesture
conversationScrollGesture olderAvailable amount viewport
    | amount == 0 = IgnoreConversationScroll
    | otherwise =
        case viewport of
            Nothing -> IgnoreConversationScroll
            Just (top, height, contentHeight)
                | amount < 0
                , top <= 0
                , olderAvailable ->
                    PauseAndScrollConversation
                | amount < 0
                , top <= 0 ->
                    IgnoreConversationScroll
                | amount > 0
                , top + height + amount >= contentHeight ->
                    ResumeConversationFollow
                | otherwise ->
                    PauseAndScrollConversation

-- | Reconcile the stored follow flag with the viewport that was visible
-- immediately before new conversation output arrived.
--
-- A visible tail repairs a stale paused flag. A viewport above the tail does
-- not itself pause following, because the submitted-prompt page-fill reflow
-- can transiently expose that geometry before its reserve rows are installed.
-- Explicit user scroll gestures remain responsible for pausing follow mode.
-- Before the first render there is no viewport, so retain the stored flag.
reconcileConversationFollow :: Bool -> Maybe (Int, Int, Int) -> Bool
reconcileConversationFollow storedFollow = \case
    Nothing -> storedFollow
    Just (top, height, contentHeight) ->
        storedFollow
            || top + max 0 height >= max 0 contentHeight

startConversationAnchor :: BlockId -> Text -> Int -> ConversationAnchor
startConversationAnchor blockId text top = ConversationAnchor
    { anchorBlockId = blockId
    , anchorText = text
    , anchorTop = max 0 top
    , anchorReserveRows = 0
    , anchorViewportTop = max 0 top
    , anchorPhase = ConversationFillingPage
    }

-- | Recompute the virtual bottom reserve after a render.
--
-- While the response fits below the submitted prompt, shrink that reserve as
-- real content arrives so the viewport bottom remains exactly at the prompt's
-- top. Once real content exceeds one viewport, drop the reserve permanently
-- and switch to ordinary tail following.
reflowConversationAnchor
    :: Bool
    -- ^ Whether the UI is currently following output.
    -> Int
    -- ^ Current viewport top.
    -> Int
    -- ^ Current viewport height.
    -> Int
    -- ^ Unpadded transcript content height.
    -> ConversationAnchor
    -> (ConversationAnchor, ConversationScroll)
reflowConversationAnchor following viewportTop viewportHeight unpaddedContentHeight anchor =
    ( anchor
        { anchorReserveRows = reserveRows
        , anchorViewportTop = predictedTop
        , anchorPhase = phase
        }
    , if following
        then ScrollConversationToEnd
        else KeepConversationPosition
    )
  where
    height = max 1 viewportHeight
    unpaddedHeight =
        max anchor.anchorTop $
            max 0 unpaddedContentHeight
    pageBottom = anchor.anchorTop + height
    overflowed =
        following
            && anchor.anchorPhase == ConversationFillingPage
            && unpaddedHeight > pageBottom
    phase
        | overflowed = ConversationFollowingTail
        | otherwise = anchor.anchorPhase
    reserveRows
        | phase == ConversationFillingPage =
            max 0 (pageBottom - unpaddedHeight)
        | otherwise = 0
    predictedTop
        | not following = max 0 viewportTop
        | phase == ConversationFillingPage = anchor.anchorTop
        | otherwise = max 0 (unpaddedHeight - height)

-- | Explicit bottom gestures leave the page-fill pose and resolve the real
-- transcript tail, matching Grok Build's End/G behavior.
followConversationTail :: ConversationAnchor -> ConversationAnchor
followConversationTail anchor =
    anchor
        { anchorReserveRows = 0
        , anchorPhase = ConversationFollowingTail
        }

conversationAnchorSticky :: ConversationAnchor -> Bool
conversationAnchorSticky anchor =
    anchor.anchorViewportTop > anchor.anchorTop
