-- | Byte-level enhanced keyboard support. Parse CSI-u before Vty's finite
-- terminfo table: an unlisted Unicode key must never become printable CSI text.
module Agent.CLI.TUI.Keyboard
    ( mkKeyboardVty
    , classifyKeyboard
    , decodeKeyboardBody
    , runKeyboardInput
    ) where

import Agent.CLI.Input.KeyDecoder
    (parseKittyKey, withoutKittyLockModifiers, splitFields, listAt, readDecimal)
import Agent.CLI.Input.Types (KittyKey(..))
import Control.Concurrent (threadWaitRead)
import qualified Control.Concurrent.Async as Async
import Control.Concurrent.STM
import Control.Exception.Safe (mask, onException, finally, throwIO)
import Control.Monad (unless, when, void)
import Data.Bits (testBit, (.&.))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Char (chr, toUpper)
import Foreign (allocaBytes, castPtr)
import qualified Graphics.Vty as V
import Graphics.Vty.Input (Input(..), InternalEvent(..))
import Graphics.Vty.Platform.Unix.Input (attributeControl)
import Graphics.Vty.Platform.Unix.Input.Classify (classify)
import Graphics.Vty.Platform.Unix.Input.Classify.Types
import Graphics.Vty.Platform.Unix.Input.Terminfo (classifyMapForTerm)
import Graphics.Vty.Platform.Unix.Output (buildOutput)
import Graphics.Vty.Platform.Unix.Settings
import qualified System.Console.Terminfo as Terminfo
import System.Posix.IO (fdReadBuf)
import System.Posix.Signals.Exts
import System.Posix.Terminal
import System.Posix.Types (Fd)
import System.Timeout (timeout)

-- An internal no-op, filtered before reaching Brick. Vty has no key-release
-- event, and unsupported functional keys must not be inserted as PUA text.
ignoredKey :: V.Event
ignoredKey = V.EvKey (V.KFun 0) []

decodeKeyboardBody :: String -> Maybe V.Event
decodeKeyboardBody body = do
    KittyKey{kittyCodepoint, kittyModifiers, kittyEvent} <- parseKittyKey body
    let modifiers = withoutKittyLockModifiers kittyModifiers
        shiftedCode = listAt 0 (splitFields ';' body)
            >>= listAt 1 . splitFields ':'
            >>= readDecimal
        vtyModifiers =
            [modifier | (bit, modifier) <-
                [(0, V.MShift), (1, V.MAlt), (2, V.MCtrl), (3, V.MMeta)]
            , testBit modifiers bit]
        character = chr kittyCodepoint
        key = case kittyCodepoint of
            9 | testBit modifiers 0 -> V.KBackTab
            9 -> V.KChar '\t'
            13 -> V.KEnter
            27 -> V.KEsc
            127 -> V.KBS
            45 | testBit modifiers 2
                && (testBit modifiers 0 || shiftedCode == Just 95) -> V.KChar '_'
            _ -> V.KChar $
                if modifiers == 1
                    then maybe (toUpper character) chr shiftedCode
                    else character
        eventModifiers = case key of
            V.KEsc -> []
            V.KBackTab -> []
            V.KChar '_' | modifiers `elem` [4, 5] -> [V.MCtrl]
            V.KChar _ | modifiers == 1 && kittyCodepoint >= 32 -> []
            _ -> vtyModifiers
    pure $
        if kittyEvent `notElem` [1, 2]
            || (key /= V.KEsc && modifiers .&. 48 /= 0)
            || kittyCodepoint < 0 || kittyCodepoint > 0x10ffff
            || kittyCodepoint >= 0xd800 && kittyCodepoint <= 0xdfff
            || kittyCodepoint >= 57344 && kittyCodepoint <= 63743
        then ignoredKey
        else V.EvKey key eventModifiers

-- | Nothing delegates to the regular Vty parser (mouse, paste, legacy keys).
-- Prefix retains fragmented CSI-u packets until their final byte arrives.
classifyKeyboard :: BS.ByteString -> Maybe KClass
classifyKeyboard bytes
    | not ("\ESC[" `BS.isPrefixOf` bytes) = Nothing
    | otherwise =
        let (parameters, rest) = BS8.span (\c -> c >= '0' && c <= '9' || c == ';' || c == ':')
                (BS.drop 2 bytes)
        in case BS8.uncons rest of
            Nothing | not (BS.null parameters) -> Just Prefix
            Just ('u', remaining) ->
                Just (Valid
                    (maybe ignoredKey id (decodeKeyboardBody (BS8.unpack parameters <> "u")))
                    remaining)
            _ -> Nothing

-- | Own the input worker through Vty's shutdown lifecycle, including suspended
-- and rebuilt Brick sessions. All other terminal behavior remains Vty's.
mkKeyboardVty :: V.VtyUserConfig -> IO V.Vty
mkKeyboardVty config = mask \restore -> do
    settings <- defaultSettings
    when (V.configAllowCustomUnicodeWidthTables config /= Just False) $
        V.installCustomWidthTable (V.configDebugLog config)
            (Just (settingTermName settings)) (V.configTermWidthMaps config)
    input <- buildKeyboardInput config settings
    output <- restore (buildOutput config settings)
        `onException` shutdownInput input
    restore (V.mkVtyFromPair input output) `onException`
        (shutdownInput input `finally`
            (V.releaseDisplay output `finally` V.releaseTerminal output))

buildKeyboardInput :: V.VtyUserConfig -> UnixSettings -> IO Input
buildKeyboardInput config settings = mask \restore -> do
    let fd = settingInputFd settings
        name = settingTermName settings
    terminal <- Terminfo.setupTerm name
    let table = classifyMapForTerm name terminal <>
            [(bytes, event) | (term, bytes, event) <- V.configInputMap config
                , term == Nothing || term == Just name]
    (setRaw, restoreTerminal) <- attributeControl fd
    let configure = do
            setRaw
            attrs <- getTerminalAttributes fd
            setTerminalAttributes fd (withMinInput (withTime attrs 0) 1) Immediately
    configure `onException` restoreTerminal
    channel <- newTChanIO
    -- The handle is retained below and cancelled/joined by shutdownInput.
    worker <- Async.async (restore (keyboardLoop fd table channel))
        `onException` restoreTerminal
    let stop = Async.cancel worker `finally` restoreTerminal
        resume = Catch (configure >> atomically (writeTChan channel ResumeAfterInterrupt))
    oldResize <- installHandler windowChange resume Nothing `onException` stop
    oldContinue <- installHandler continueProcess resume Nothing
        `onException` (void (installHandler windowChange oldResize Nothing) `finally` stop)
    Async.link worker
    pure Input
        { eventChannel = channel
        , shutdownInput =
            stop `finally` do
                void (installHandler windowChange oldResize Nothing)
                void (installHandler continueProcess oldContinue Nothing)
        , restoreInputState = restoreTerminal
        , inputLogMsg = const (pure ())
        }

keyboardLoop :: Fd -> V.ClassifyMap -> TChan InternalEvent -> IO ()
keyboardLoop fd table channel =
    runKeyboardInput table readBytes
        (\event -> atomically (writeTChan channel (InputEvent event)))
  where
    readBytes = do
        threadWaitRead fd
        allocaBytes 4096 \buffer -> do
            count <- fdReadBuf fd buffer 4096
            BS.packCStringLen (castPtr buffer, fromIntegral count)

-- | The production byte loop with injectable transport, so packet boundaries,
-- timeout handling and paste isolation are exercised without a fake decoder.
-- An empty read denotes EOF and is propagated to the owner of the input worker.
runKeyboardInput :: V.ClassifyMap -> IO BS.ByteString -> (V.Event -> IO ()) -> IO ()
runKeyboardInput table readTransport emit = readMore ClassifierStart BS.empty
  where
    standard = classify table
    readBytes = do
        bytes <- readTransport
        if BS.null bytes
            then throwIO (userError "Terminal input closed")
            else pure bytes
    readMore state buffered = do
        bytes <- readBytes
        process state (buffered <> bytes)
    process state bytes
        | BS.null bytes = readMore state BS.empty
        | otherwise =
            case classifyInput state bytes of
                Valid event remaining -> do
                    unless (event == ignoredKey) (emit event)
                    process ClassifierStart remaining
                Prefix | BS.length bytes > 1024 && "\ESC[" `BS.isPrefixOf` bytes ->
                    discardCsi (BS.drop 2 bytes)
                Prefix -> do
                    continuation <- timeout 100000 readBytes
                    case continuation of
                        Just more | not (BS.null more) -> process state (bytes <> more)
                        _ -> do
                            when (bytes == "\ESC") $
                                emit (V.EvKey V.KEsc [])
                            readMore ClassifierStart BS.empty
                Chunk -> readPaste [] (BS.drop 6 bytes)
                Invalid -> readMore ClassifierStart BS.empty
    classifyInput ClassifierStart bytes =
        if bytes == "\ESC" || bytes == "\ESC["
            || ("\ESC[<" `BS.isPrefixOf` bytes
                && not (BS8.any (\c -> c == 'M' || c == 'm') bytes))
            || ("\ESC[M" `BS.isPrefixOf` bytes && BS.length bytes < 6)
            then Prefix
            else maybe (standard ClassifierStart bytes) id (classifyKeyboard bytes)
    classifyInput state bytes = standard state bytes
    -- Keep only the possible delimiter suffix between reads. Paste contents
    -- never enter the keyboard decoder, even if they contain literal CSI-u.
    readPaste chunks bytes =
        let (content, ending) = BS.breakSubstring "\ESC[201~" bytes
        in if not (BS.null ending)
            then do
                emit (V.EvPaste (BS.concat (reverse (content : chunks))))
                process ClassifierStart (BS.drop 6 ending)
            else do
                let keep = min 5 (BS.length bytes)
                    (complete, suffix) = BS.splitAt (BS.length bytes - keep) bytes
                more <- readBytes
                readPaste (complete : chunks) (suffix <> more)
    -- Drop an oversized parameter run without retaining it or leaking the
    -- remaining payload as text. Resume normally after its final byte.
    discardCsi bytes =
        let rest = BS8.dropWhile (\c -> c >= '0' && c <= '9' || c == ';' || c == ':') bytes
        in case BS8.uncons rest of
            Just (final, remaining) | final >= '@' && final <= '~' ->
                process ClassifierStart remaining
            Just _ -> process ClassifierStart rest
            Nothing -> do
                next <- timeout 100000 readBytes
                maybe (readMore ClassifierStart BS.empty) discardCsi next
