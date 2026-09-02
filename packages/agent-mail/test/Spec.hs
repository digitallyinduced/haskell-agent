module Main (main) where

import Agent.Mail.Contract
import Agent.Mail.Types
import Data.Aeson (Value(..), object, (.=))
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Text (Text)
import Test.Hspec

main :: IO ()
main = hspec do
    describe "email MCP contract" do
        it "keeps the canonical model-facing tool names" do
            map (.mailMcpToolName) mailMcpTools `shouldBe`
                [ "email_list_accounts"
                , "email_list_mailboxes"
                , "email_search"
                , "email_get"
                , "email_download_attachment"
                , "email_create_draft"
                , "email_update_draft"
                , "email_reply_draft"
                ]

        it "marks only draft mutations as requiring fresh approval" do
            map (.mailMcpToolName)
                (filter (.mailMcpToolRequiresFreshApproval) mailMcpTools)
                `shouldBe` mailDraftMutationToolNames

        it "round trips structured results only for the exact contract" do
            let accounts =
                    [ MailAccountSummary
                        "account-ref"
                        "gmail"
                        "person@example.com"
                        Nothing
                        True
                        True
                    ]
            decodeMailMcpResult (mailMcpSuccess accounts)
                `shouldBe` Right accounts
            let incompatible = object
                    [ "structuredContent" .= object
                        [ "contract" .= ("other" :: Text)
                        , "version" .= mailContractVersion
                        , "data" .= accounts
                        ]
                    , "isError" .= False
                    ]
            (decodeMailMcpResult incompatible
                :: Either Text [MailAccountSummary])
                `shouldBe` Left "Error in $: incompatible email MCP contract"

        it "publishes closed object schemas" do
            mailMcpToolDefinitions `shouldSatisfy` all closedSchema

closedSchema :: Value -> Bool
closedSchema (Object tool) =
    case KeyMap.lookup "inputSchema" tool of
        Just (Object input) ->
            KeyMap.lookup "additionalProperties" input == Just (Bool False)
        _ -> False
closedSchema _ = False
