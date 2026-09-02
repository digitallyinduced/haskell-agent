module Agent.CLI.MailGatewaySpec (spec) where

import Agent.CLI.Mail.Gateway
import Agent.Mail.Contract
import Agent.Mail.Types
import Agent.OsPath (fromText)
import Agent.Tools.Types
    ( AppTool (..)
    , ApprovalRule (..)
    , defaultToolEnv
    )
import Data.Aeson (Value, object, (.=))
import Data.Text (Text)
import Test.Hspec

spec :: Spec
spec = describe "gateway-backed mail tools" do
    it "accepts the exact first-party contract and retains canonical names" do
        tools <- gatewayTools compatibleGateway
        fmap (fmap (.appToolName)) tools `shouldBe`
            Right
                [ "email_list_accounts"
                , "email_list_mailboxes"
                , "email_search"
                , "email_get"
                , "email_download_attachment"
                , "email_create_draft"
                , "email_update_draft"
                , "email_reply_draft"
                ]

    it "fails closed when the gateway changes the contract" do
        let incompatible request
                | request.gatewayMailRequestTool == "$email/tools/list" =
                    pure $ Right $ object
                        [ "tools" .= take 7 mailMcpToolDefinitions ]
                | otherwise = pure $ Left "unexpected request"
        tools <- gatewayTools incompatible
        expectLeft
            "The organization gateway does not provide a compatible email service."
            tools

    it "fails closed when gateway discovery or account loading fails" do
        unavailable <- gatewayTools (\_ -> pure (Left "gateway unavailable"))
        expectLeft "gateway unavailable" unavailable
        accountFailure <- gatewayTools \request ->
            if request.gatewayMailRequestTool == "$email/tools/list"
                then compatibleGateway request
                else pure (Left "account list unavailable")
        expectLeft "account list unavailable" accountFailure

    it "keeps all gateway draft writes at AlwaysConfirm" do
        tools <- gatewayTools compatibleGateway
        case tools of
            Left err -> expectationFailure (show err)
            Right registered -> do
                fmap (isAlwaysConfirm . (.appToolApproval)) (drop 5 registered)
                    `shouldBe` replicate 3 True

gatewayTools
    :: (GatewayMailRequest -> IO (Either Text Value))
    -> IO (Either Text [AppTool])
gatewayTools call = do
    toolEnv <- defaultToolEnv (fromText "/tmp")
    gatewayMailToolsWith toolEnv call unusedDownload

compatibleGateway :: GatewayMailRequest -> IO (Either Text Value)
compatibleGateway request
    | request.gatewayMailRequestTool == "$email/tools/list" =
        pure $ Right $ object ["tools" .= mailMcpToolDefinitions]
    | request.gatewayMailRequestTool == mailListAccountsToolName =
        pure $ Right $ mailMcpSuccess [connectedAccount]
    | otherwise = pure $ Right $ mailMcpFailure "not exercised"

connectedAccount :: MailAccountSummary
connectedAccount = MailAccountSummary
    { mailAccountId = "gateway-account-1"
    , mailAccountProvider = "gmail"
    , mailAccountEmail = "person@example.com"
    , mailAccountLabel = Nothing
    , mailAccountEnabled = True
    , mailAccountVerified = True
    }

unusedDownload
    :: MailAttachmentDownload
    -> Int
    -> IO (Either Text MailAttachmentContent)
unusedDownload _ _ = pure (Left "not exercised")

expectLeft :: Text -> Either Text value -> Expectation
expectLeft expected = \case
    Left actual -> actual `shouldBe` expected
    Right _ -> expectationFailure "expected gateway tool construction to fail"

isAlwaysConfirm :: ApprovalRule -> Bool
isAlwaysConfirm = \case
    AlwaysConfirm -> True
    _ -> False
