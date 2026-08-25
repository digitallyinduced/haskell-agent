-- | Semantic attributes for the retained fullscreen interface.
module Agent.TUI.Theme
    ( assistantAttr
    , baseAttr
    , borderActiveAttr
    , borderAttr
    , errorAttr
    , footerAttr
    , headerAttr
    , codeAttr
    , controlLinkActiveAttr
    , controlLinkAttr
    , controlLinkHoverAttr
    , dimAttr
    , emphasisAttr
    , headingAttr
    , inlineCodeAttr
    , lambdaDimAttr
    , lambdaGlowAttr
    , lambdaSparkAttr
    , lambdaTrailAttr
    , linkAttr
    , mutedAttr
    , selectedAttr
    , strongAttr
    , successAttr
    , syntaxAnnotationAttr
    , syntaxCommentAttr
    , syntaxErrorAttr
    , syntaxFunctionAttr
    , syntaxKeywordAttr
    , syntaxNormalAttr
    , syntaxNumberAttr
    , syntaxOperatorAttr
    , syntaxPreprocessorAttr
    , syntaxStringAttr
    , syntaxTypeAttr
    , syntaxVariableAttr
    , syntaxWarningAttr
    , syntaxClassAttr
    , thinkingAttr
    , todoCancelledAttr
    , todoCompletedAttr
    , todoInProgressAttr
    , todoPendingAttr
    , toolAttr
    , userAttr
    , userMutedAttr
    , waitingDimAttr
    , waitingMidAttr
    , completionFlashAttr
    , monochrome
    , terminalDefault
    ) where

import Agent.Syntax (SyntaxClass(..))
import Brick (AttrMap, AttrName, attrMap, attrName)
import Data.Bits ((.|.))
import qualified Graphics.Vty as V

baseAttr, headerAttr, footerAttr, mutedAttr :: AttrName
userAttr, userMutedAttr, assistantAttr, thinkingAttr, toolAttr :: AttrName
todoPendingAttr, todoInProgressAttr, todoCompletedAttr, todoCancelledAttr :: AttrName
errorAttr, successAttr, selectedAttr, borderAttr, borderActiveAttr :: AttrName
headingAttr, codeAttr, dimAttr, emphasisAttr, inlineCodeAttr, linkAttr, strongAttr :: AttrName
controlLinkAttr, controlLinkHoverAttr, controlLinkActiveAttr :: AttrName
lambdaDimAttr, lambdaTrailAttr, lambdaGlowAttr, lambdaSparkAttr :: AttrName
syntaxNormalAttr, syntaxKeywordAttr, syntaxTypeAttr, syntaxFunctionAttr :: AttrName
syntaxVariableAttr, syntaxStringAttr, syntaxNumberAttr, syntaxCommentAttr :: AttrName
syntaxOperatorAttr, syntaxAnnotationAttr, syntaxPreprocessorAttr :: AttrName
syntaxWarningAttr, syntaxErrorAttr :: AttrName
waitingDimAttr, waitingMidAttr, completionFlashAttr :: AttrName
baseAttr = attrName "base"
headerAttr = attrName "header"
footerAttr = attrName "footer"
mutedAttr = attrName "muted"
userAttr = attrName "user"
userMutedAttr = attrName "user-muted"
assistantAttr = attrName "assistant"
thinkingAttr = attrName "thinking"
toolAttr = attrName "tool"
todoPendingAttr = attrName "todo-pending"
todoInProgressAttr = attrName "todo-in-progress"
todoCompletedAttr = attrName "todo-completed"
todoCancelledAttr = attrName "todo-cancelled"
errorAttr = attrName "error"
successAttr = attrName "success"
selectedAttr = attrName "selected"
borderAttr = attrName "border"
borderActiveAttr = attrName "border-active"
headingAttr = attrName "markdown-heading"
codeAttr = attrName "markdown-code"
dimAttr = attrName "dim"
emphasisAttr = attrName "markdown-emphasis"
inlineCodeAttr = attrName "markdown-inline-code"
lambdaDimAttr = attrName "lambda-dim"
lambdaTrailAttr = attrName "lambda-trail"
lambdaGlowAttr = attrName "lambda-glow"
lambdaSparkAttr = attrName "lambda-spark"
linkAttr = attrName "markdown-link"
strongAttr = attrName "markdown-strong"
controlLinkAttr = attrName "control-link"
controlLinkHoverAttr = attrName "control-link-hover"
controlLinkActiveAttr = attrName "control-link-active"
syntaxNormalAttr = attrName "syntax-normal"
syntaxKeywordAttr = attrName "syntax-keyword"
syntaxTypeAttr = attrName "syntax-type"
syntaxFunctionAttr = attrName "syntax-function"
syntaxVariableAttr = attrName "syntax-variable"
syntaxStringAttr = attrName "syntax-string"
syntaxNumberAttr = attrName "syntax-number"
syntaxCommentAttr = attrName "syntax-comment"
syntaxOperatorAttr = attrName "syntax-operator"
syntaxAnnotationAttr = attrName "syntax-annotation"
syntaxPreprocessorAttr = attrName "syntax-preprocessor"
syntaxWarningAttr = attrName "syntax-warning"
syntaxErrorAttr = attrName "syntax-error"

syntaxClassAttr :: SyntaxClass -> AttrName
syntaxClassAttr = \case
    SyntaxNormal -> syntaxNormalAttr
    SyntaxKeyword -> syntaxKeywordAttr
    SyntaxType -> syntaxTypeAttr
    SyntaxFunction -> syntaxFunctionAttr
    SyntaxVariable -> syntaxVariableAttr
    SyntaxString -> syntaxStringAttr
    SyntaxNumber -> syntaxNumberAttr
    SyntaxComment -> syntaxCommentAttr
    SyntaxOperator -> syntaxOperatorAttr
    SyntaxAnnotation -> syntaxAnnotationAttr
    SyntaxPreprocessor -> syntaxPreprocessorAttr
    SyntaxWarning -> syntaxWarningAttr
    SyntaxError -> syntaxErrorAttr
waitingDimAttr = attrName "waiting-dim"
waitingMidAttr = attrName "waiting-mid"
completionFlashAttr = attrName "completion-flash"

-- | Theme using only the terminal's default foreground/background and its
-- configurable ANSI palette. Ghostty themes therefore apply directly,
-- including automatic light/dark theme switching.
terminalDefault :: AttrMap
terminalDefault =
    attrMap V.defAttr
        [ (baseAttr, V.defAttr)
        , (headerAttr, V.defAttr `V.withStyle` V.bold)
        , (footerAttr, palette V.brightBlack)
        , (mutedAttr, palette V.brightBlack)
        , (userAttr, userPanelAttr `V.withStyle` V.bold)
        , (userMutedAttr, userPanelAttr `V.withStyle` V.dim)
        , (assistantAttr, V.defAttr)
        , (thinkingAttr, palette V.yellow)
        , (waitingDimAttr, palette V.brightBlack)
        , (waitingMidAttr, palette V.yellow)
        , (toolAttr, palette V.cyan)
        , (todoPendingAttr, V.defAttr)
        , (todoInProgressAttr, palette V.yellow `V.withStyle` V.bold)
        , (todoCompletedAttr, palette V.brightBlack)
        , (todoCancelledAttr, palette V.brightBlack `V.withStyle` V.strikethrough)
        , (errorAttr, palette V.red `V.withStyle` V.bold)
        , (successAttr, palette V.green)
        , (completionFlashAttr,
            palette V.brightGreen `V.withStyle` V.bold)
        , (selectedAttr, V.defAttr `V.withStyle` V.reverseVideo)
        , (borderAttr, palette V.brightBlack)
        , (borderActiveAttr, V.defAttr)
        , (headingAttr, palette V.magenta `V.withStyle` V.bold)
        , (codeAttr, palette V.cyan)
        , (dimAttr, palette V.brightBlack)
        , (emphasisAttr, V.defAttr `V.withStyle` V.italic)
        , (inlineCodeAttr, palette V.cyan `V.withStyle` V.reverseVideo)
        , (lambdaDimAttr, palette V.brightBlack)
        , (lambdaTrailAttr, palette V.brightBlack)
        , (lambdaGlowAttr, V.defAttr `V.withStyle` V.bold)
        , (lambdaSparkAttr,
            palette V.brightWhite `V.withStyle` V.bold)
        , (linkAttr, palette V.blue `V.withStyle` V.underline)
        , (strongAttr, V.defAttr `V.withStyle` V.bold)
        , (controlLinkAttr, palette V.brightBlack)
        , (controlLinkHoverAttr, V.defAttr `V.withStyle` V.underline)
        , (controlLinkActiveAttr, V.defAttr `V.withStyle` V.reverseVideo)
        , (syntaxNormalAttr, palette V.cyan)
        , (syntaxKeywordAttr, palette V.magenta)
        , (syntaxTypeAttr, palette V.yellow)
        , (syntaxFunctionAttr, palette V.blue)
        , (syntaxVariableAttr, palette V.cyan)
        , (syntaxStringAttr, palette V.green)
        , (syntaxNumberAttr, palette V.brightMagenta)
        , (syntaxCommentAttr,
            palette V.brightBlack `V.withStyle` V.italic)
        , (syntaxOperatorAttr, palette V.brightYellow)
        , (syntaxAnnotationAttr, palette V.green)
        , (syntaxPreprocessorAttr, palette V.brightYellow)
        , (syntaxWarningAttr,
            palette V.yellow `V.withStyle` V.bold)
        , (syntaxErrorAttr,
            palette V.red `V.withStyle` V.bold)
        ]

monochrome :: AttrMap
monochrome =
    attrMap V.defAttr
        [ (baseAttr, V.defAttr)
        , (headerAttr, V.defAttr `V.withStyle` V.bold)
        , (footerAttr, V.defAttr)
        , (mutedAttr, V.defAttr)
        , (userAttr, V.defAttr `V.withStyle` V.bold)
        , (userMutedAttr, V.defAttr `V.withStyle` V.dim)
        , (assistantAttr, V.defAttr)
        , (thinkingAttr, V.defAttr)
        , (waitingDimAttr, V.defAttr)
        , (waitingMidAttr, V.defAttr)
        , (toolAttr, V.defAttr)
        , (todoPendingAttr, V.defAttr)
        , (todoInProgressAttr, V.defAttr `V.withStyle` V.bold)
        , (todoCompletedAttr, V.defAttr)
        , (todoCancelledAttr, V.defAttr `V.withStyle` V.strikethrough)
        , (errorAttr, V.defAttr `V.withStyle` V.bold)
        , (successAttr, V.defAttr)
        , (completionFlashAttr, V.defAttr `V.withStyle` V.bold)
        , (selectedAttr, V.defAttr `V.withStyle` V.reverseVideo)
        , (borderAttr, V.defAttr)
        , (borderActiveAttr, V.defAttr `V.withStyle` V.bold)
        , (headingAttr, V.defAttr `V.withStyle` V.bold)
        , (codeAttr, V.defAttr)
        , (dimAttr, V.defAttr)
        , (emphasisAttr, V.defAttr `V.withStyle` V.italic)
        , (inlineCodeAttr, V.defAttr `V.withStyle` V.reverseVideo)
        , (lambdaDimAttr, V.defAttr)
        , (lambdaTrailAttr, V.defAttr `V.withStyle` V.bold)
        , (lambdaGlowAttr, V.defAttr `V.withStyle` V.bold)
        , (lambdaSparkAttr, V.defAttr
            `V.withStyle` (V.bold .|. V.reverseVideo))
        , (linkAttr, V.defAttr `V.withStyle` V.underline)
        , (strongAttr, V.defAttr `V.withStyle` V.bold)
        , (controlLinkAttr, V.defAttr `V.withStyle` V.underline)
        , (controlLinkHoverAttr, V.defAttr
            `V.withStyle` (V.underline .|. V.bold))
        , (controlLinkActiveAttr, V.defAttr `V.withStyle` V.reverseVideo)
        , (syntaxNormalAttr, V.defAttr)
        , (syntaxKeywordAttr, V.defAttr `V.withStyle` V.bold)
        , (syntaxTypeAttr, V.defAttr `V.withStyle` V.bold)
        , (syntaxFunctionAttr, V.defAttr `V.withStyle` V.bold)
        , (syntaxVariableAttr, V.defAttr)
        , (syntaxStringAttr, V.defAttr)
        , (syntaxNumberAttr, V.defAttr)
        , (syntaxCommentAttr, V.defAttr `V.withStyle` V.italic)
        , (syntaxOperatorAttr, V.defAttr)
        , (syntaxAnnotationAttr, V.defAttr `V.withStyle` V.bold)
        , (syntaxPreprocessorAttr, V.defAttr `V.withStyle` V.bold)
        , (syntaxWarningAttr, V.defAttr `V.withStyle` V.bold)
        , (syntaxErrorAttr, V.defAttr
            `V.withStyle` (V.bold .|. V.reverseVideo))
        ]

palette :: V.Color -> V.Attr
palette = V.withForeColor V.defAttr

-- | Grok-style user-prompt panel: a full-width wash one step off the page
-- background. Bright black is the ANSI gray slot, so Ghostty light/dark
-- palettes keep the card readable without fixing an RGB background.
userPanelAttr :: V.Attr
userPanelAttr = V.withBackColor V.defAttr V.brightBlack
