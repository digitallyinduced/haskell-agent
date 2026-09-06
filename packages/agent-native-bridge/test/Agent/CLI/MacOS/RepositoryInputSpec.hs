module Agent.CLI.MacOS.RepositoryInputSpec (spec) where

import Agent.CLI.MacOS.RepositoryInput
import qualified Data.ByteString as BS
import Foreign (castPtr, nullPtr)
import Test.Hspec

spec :: Spec
spec = describe "repository bridge input" do
    it "rejects null pointers and empty required text" do
        copyRequiredText nullPtr 1 `shouldReturn` Left ()
        BS.useAsCStringLen "x" \(pointer, _) ->
            copyRequiredText (castPtr pointer) 0 `shouldReturn` Left ()

    it "rejects oversized individual fields before reading their buffers" do
        BS.useAsCStringLen "x" \(pointer, _) ->
            copyRequiredText (castPtr pointer) (8 * 1024 * 1024 + 1)
                `shouldReturn` Left ()

    it "rejects oversized aggregate input before reading any buffer" do
        BS.useAsCStringLen "x" \(pointer, _) ->
            copyRequiredTexts
                [ (castPtr pointer, 8 * 1024 * 1024)
                , (castPtr pointer, 8 * 1024 * 1024)
                , (castPtr pointer, 1)
                ] `shouldReturn` Left ()

    it "rejects invalid UTF-8" do
        BS.useAsCStringLen (BS.pack [0xff]) \(pointer, size) ->
            copyRequiredText (castPtr pointer) (fromIntegral size)
                `shouldReturn` Left ()

    it "copies ordered required fields into owned text" do
        copied <- BS.useAsCStringLen "first" \(first, firstSize) ->
            BS.useAsCStringLen "second" \(second, secondSize) ->
                copyRequiredTexts
                    [ (castPtr first, fromIntegral firstSize)
                    , (castPtr second, fromIntegral secondSize)
                    ]
        copied `shouldBe` Right ["first", "second"]
