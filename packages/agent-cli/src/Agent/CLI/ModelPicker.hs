-- | Interactive TTY model picker for bare @/model@.
module Agent.CLI.ModelPicker
    ( pickModel
    , formatCatalogListing
    , renderPickerFrame
    , decodePickerKey
    ) where

import Agent.CLI.Models
import Agent.CLI.Picker (PickerKey(..), runOverlay)
import qualified Agent.CLI.Picker as Picker
import Agent.CLI.Style
    ( glyphOk
    , roleMuted
    , rolePrompt
    , roleSuccess
    , roleWarn
    )
import Agent.Provider (Provider, providerSlug)
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
-- Returns @Just name@ on confirm, @Nothing@ on cancel / EOF / non-TTY.
pickModel :: Bool -> Provider -> Text -> IO (Maybe Text)
pickModel color provider current = do
    isTty <- hIsTerminalDevice stdin
    if not isTty
        then do
            Text.hPutStrLn stderr (formatCatalogListing color provider current)
            hFlush stderr
            pure Nothing
        else do
            let state0 = initialPickerState provider current
            result <- runOverlay (renderPickerFrame color) step state0
            pure (join result)
  where
    step key state = applyPickerEvent (toEvent key) state

formatCatalogListing :: Bool -> Provider -> Text -> Text
formatCatalogListing color provider current =
    let state = initialPickerState provider current
        header =
            roleMuted color
                (glyphSessionLike
                    <> "model: "
                    <> current
                    <> " · "
                    <> providerSlug provider)
        rows =
            map
                (\opt ->
                    let mark
                            | opt.modelId == current =
                                roleSuccess color (glyphOk <> opt.modelId)
                            | otherwise = roleMuted color ("  " <> opt.modelId)
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
                    (" · "
                        <> providerSlug state.pickerProvider
                        <> " · current "
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
                    (\i opt -> renderRow color (i == idx) state.pickerCurrent opt)
                    [0 ..]
                    opts
        footer =
            roleMuted color "↑↓/jk · enter · esc/q · type to filter"
    in Text.intercalate "\n" (header : filterLine : body <> [footer])

renderRow :: Bool -> Bool -> Text -> ModelOption -> Text
renderRow color selected current opt =
    let cursor = if selected then roleWarn color "› " else "  "
        name
            | selected = roleSuccess color opt.modelId
            | otherwise = roleMuted color opt.modelId
        currentMark
            | opt.modelId == current = roleSuccess color " ✓"
            | otherwise = ""
        label = case opt.modelLabel of
            Nothing -> ""
            Just l -> roleMuted color ("  " <> l)
    in cursor <> name <> currentMark <> label

-- Matches Style.glyphSession without pulling unicode env probing here.
glyphSessionLike :: Text
glyphSessionLike = "⧉ "
