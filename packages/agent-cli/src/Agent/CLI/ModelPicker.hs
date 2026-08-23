-- | Interactive TTY model picker for bare @/model@.
module Agent.CLI.ModelPicker
    ( pickModel
    , formatCatalogListing
    , renderPickerFrame
    , decodePickerKey
    ) where

import Agent.CLI.Models
import Agent.CLI.ModelConfig (ModelCatalog)
import Agent.CLI.Picker (PickerKey(..), runOverlay)
import qualified Agent.CLI.Picker as Picker
import Agent.Dialect (DialectId, dialectSlug)
import Agent.CLI.Style
    ( glyphOk
    , roleMuted
    , rolePrompt
    , roleSuccess
    , roleWarn
    )
import Agent.Provider (Provider)
import Control.Monad (join)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import System.IO (hFlush, hIsTerminalDevice, stderr, stdin)

-- | Decode one keypress (including CSI arrow sequences) into a picker event.
decodePickerKey :: String -> Maybe PickerEvent
decodePickerKey raw = toEvent <$> Picker.decodePickerKey raw

toEvent :: PickerKey -> PickerEvent
toEvent = \case
    PickerKeyUp -> PickerUp
    PickerKeyDown -> PickerDown
    PickerKeyConfirm -> PickerConfirm
    PickerKeyCancel -> PickerCancel
    PickerKeyBackspace -> PickerBackspace
    PickerKeyChar c -> PickerType c

-- | Open the picker when stdin is a TTY; otherwise print the catalog.
-- Returns the provider/model choice on confirm and @Nothing@ on cancel, EOF,
-- or non-TTY input.
pickModel
    :: ModelCatalog
    -> Bool
    -> Text
    -> Provider
    -> Text
    -> DialectId
    -> IO (Maybe ModelOption)
pickModel catalog color connectionId provider current currentDialect = do
    isTty <- hIsTerminalDevice stdin
    state0 <-
        initialPickerStateResolved
            catalog connectionId provider current currentDialect
    if not isTty
        then do
            Text.hPutStrLn stderr
                (formatCatalogListingState
                    color current currentDialect state0)
            hFlush stderr
            pure Nothing
        else do
            result <- runOverlay (renderPickerFrame color) step state0
            pure (join result)
  where
    step key state = applyPickerEvent (toEvent key) state

formatCatalogListing
    :: ModelCatalog
    -> Bool
    -> Text
    -> Provider
    -> Text
    -> DialectId
    -> IO Text
formatCatalogListing
        catalog color connectionId provider current currentDialect = do
    state <-
        initialPickerStateResolved
            catalog connectionId provider current currentDialect
    pure (formatCatalogListingState
        color current currentDialect state)

formatCatalogListingState
    :: Bool
    -> Text
    -> DialectId
    -> PickerState
    -> Text
formatCatalogListingState color current currentDialect state =
    let header =
            roleMuted color
                (glyphSessionLike
                    <> "model: "
                    <> state.pickerConnectionId
                    <> "/"
                    <> current
                    <> " · all providers")
        rows =
            map
                (\opt ->
                    let mark
                            | isCurrent
                                state.pickerConnectionId
                                current
                                currentDialect
                                opt =
                                roleSuccess color
                                    (glyphOk <> formatOptionName opt)
                            | otherwise =
                                roleMuted color ("  " <> formatOptionName opt)
                        label = case opt.modelLabel of
                            Nothing -> ""
                            Just l -> roleMuted color ("  " <> l)
                    in mark <> label)
                state.pickerAll
    in Text.intercalate "\n" (header : rows)

-- | Pure frame used by the interactive loop (and tests).
renderPickerFrame :: Bool -> PickerState -> Text
renderPickerFrame color state =
    let visible = visibleOptions state
        n = length visible
        idx = if n == 0 then 0 else min state.pickerIndex (n - 1)
        header =
            rolePrompt color "model"
                <> roleMuted color
                    (" · all providers · current "
                        <> state.pickerConnectionId
                        <> "/"
                        <> state.pickerCurrent)
        filterLine
            | Text.null state.pickerFilter =
                roleMuted color "filter: (type to narrow)"
            | otherwise =
                roleMuted color "filter: "
                    <> roleWarn color state.pickerFilter
        body = case visible of
            [] -> [roleMuted color "(no matches)"]
            opts ->
                zipWith
                    (\i opt -> renderRow color (i == idx)
                        state.pickerConnectionId
                        state.pickerCurrent
                        state.pickerCurrentDialect
                        opt)
                    [0 ..]
                    opts
        footer =
            roleMuted color
                "↑↓/jk or scroll · click/enter · esc/q · type to filter"
    in Text.intercalate "\n" (header : filterLine : body <> [footer])

renderRow
    :: Bool
    -> Bool
    -> Text
    -> Text
    -> DialectId
    -> ModelOption
    -> Text
renderRow color selected currentConnection current currentDialect opt =
    let cursor = if selected then roleWarn color "› " else "  "
        name
            | selected = roleSuccess color (formatOptionName opt)
            | otherwise = roleMuted color (formatOptionName opt)
        currentMark
            | isCurrent currentConnection current currentDialect opt =
                roleSuccess color " ✓"
            | otherwise = ""
        label = case opt.modelLabel of
            Nothing -> ""
            Just l -> roleMuted color ("  " <> l)
    in cursor <> name <> currentMark <> label

-- Matches Style.glyphSession without pulling unicode env probing here.
glyphSessionLike :: Text
glyphSessionLike = "⧉ "

formatOptionName :: ModelOption -> Text
formatOptionName opt =
    opt.modelTarget.targetConnectionId
        <> " · "
        <> opt.modelTarget.targetModelId
        <> " · "
        <> dialectSlug opt.modelTarget.targetDialect

isCurrent :: Text -> Text -> DialectId -> ModelOption -> Bool
isCurrent connectionId current dialect opt =
    opt.modelTarget.targetConnectionId == connectionId
        && opt.modelTarget.targetModelId == current
        && opt.modelTarget.targetDialect == dialect
