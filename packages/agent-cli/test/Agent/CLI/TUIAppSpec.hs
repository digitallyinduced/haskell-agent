module Agent.CLI.TUIAppSpec (spec) where

import Agent.CLI.TUI.App (fullscreenVtyConfig)
import qualified Graphics.Vty as V
import Test.Hspec

spec :: Spec
spec =
    describe "fullscreenVtyConfig" do
        it "maps enhanced-keyboard Shift+Enter sequences before Vty decodes them" do
            V.configInputMap fullscreenVtyConfig
                `shouldMatchList`
                    [ ( Nothing
                      , "\ESC[27;2;13~"
                      , V.EvKey V.KEnter [V.MShift]
                      )
                    , ( Nothing
                      , "\ESC[13;2u"
                      , V.EvKey V.KEnter [V.MShift]
                      )
                    ]
