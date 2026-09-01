module Agent.CLI.MailToolsSpec (spec) where

import Agent.CLI.Mail.Tools
import Agent.OsPath (fromText)
import Agent.ToolDispatch
    ( ToolCallResult(..)
    , ToolDispatchConfig(..)
    , dispatchToolCall
    , functionToolCall
    )
import Agent.Tools.Types
    ( AppTool(..)
    , ApprovalRule(..)
    , appToolHandlers
    , defaultToolEnv
    )
import qualified Data.ByteString as BS
import Data.Either (isLeft)
import Data.IORef (newIORef, readIORef, writeIORef)
import qualified Data.Text as Text
import Test.Hspec

spec :: Spec
spec = describe "mail tools" do
    it "registers only for an enabled verified account" do
        toolEnv <- defaultToolEnv (fromText "/tmp")
        let env accounts = MailToolsEnv
                { mailToolsToolEnv = toolEnv
                , mailToolsLimits = defaultMailToolLimits
                , mailToolsListAccounts = pure (Right accounts)
                , mailToolsListMailboxes = \_ _ -> pure (Right [])
                , mailToolsSearch = \_ -> pure (Right [])
                , mailToolsGetMessage = \_ _ ->
                    pure (Left "not exercised")
                , mailToolsDownloadAttachment = \_ _ ->
                    pure (Left "not exercised")
                , mailToolsCreateDraft = \_ -> pure (Left "not exercised")
                , mailToolsUpdateDraft = \_ -> pure (Left "not exercised")
                , mailToolsReplyDraft = \_ -> pure (Left "not exercised")
                }
            connected = MailAccountSummary
                { mailAccountId = "mail-1"
                , mailAccountProvider = "gmail"
                , mailAccountEmail = "person@example.com"
                , mailAccountLabel = Nothing
                , mailAccountEnabled = True
                , mailAccountVerified = True
                }
        map (.appToolName) <$> mailTools (env [])
            `shouldReturn` []
        tools <- mailTools (env [connected])
        map (.appToolName) tools `shouldBe`
            [ "email_list_accounts"
            , "email_list_mailboxes"
            , "email_search"
            , "email_get"
            , "email_download_attachment"
            , "email_create_draft"
            , "email_update_draft"
            , "email_reply_draft"
            ]
        all (isAlwaysReadOnly . (.appToolApproval)) (take 4 tools)
            `shouldBe` True
        isAlwaysPrompt (tools !! 4).appToolApproval `shouldBe` True
        all (isAlwaysConfirm . (.appToolApproval)) (drop 5 tools)
            `shouldBe` True

    it "rejects invalid date ranges and oversized references" do
        let request = MailSearchRequest
                { mailSearchAccountId = "mail-1"
                , mailSearchMailboxId = Nothing
                , mailSearchQuery = Nothing
                , mailSearchFrom = Nothing
                , mailSearchTo = Nothing
                , mailSearchSubject = Nothing
                , mailSearchAfter = Just "2026-09-02"
                , mailSearchBefore = Just "2026-09-01"
                , mailSearchHasAttachments = Nothing
                , mailSearchLimit = 20
                }
        validateMailSearchRequest defaultMailToolLimits request
            `shouldSatisfy` isLeft
        validateOpaqueMailReference "message_id" (mconcat (replicate 8193 "x"))
            `shouldSatisfy` isLeft

    it "binds opaque mail references to their account and capability kind" do
        let key = BS.replicate 32 0x5a
            sealed = sealMailReference key DraftReference "account-a"
                ["imap-draft:collision"]
        case sealed of
            Left err -> expectationFailure (Text.unpack err)
            Right reference -> do
                openMailReference key DraftReference "account-a" reference
                    `shouldBe` Right ["imap-draft:collision"]
                openMailReference key DraftReference "account-b" reference
                    `shouldSatisfy` isLeft
                openMailReference key MessageReference "account-a" reference
                    `shouldSatisfy` isLeft
                openMailReference key DraftReference "account-a"
                    (reference <> "A")
                    `shouldSatisfy` isLeft

    it "decodes search arguments and applies the default result limit" do
        seen <- newIORef Nothing
        toolEnv <- defaultToolEnv (fromText "/tmp")
        let connected = MailAccountSummary
                { mailAccountId = "mail-1"
                , mailAccountProvider = "gmail"
                , mailAccountEmail = "person@example.com"
                , mailAccountLabel = Nothing
                , mailAccountEnabled = True
                , mailAccountVerified = True
                }
            env = MailToolsEnv
                { mailToolsToolEnv = toolEnv
                , mailToolsLimits = defaultMailToolLimits
                , mailToolsListAccounts = pure (Right [connected])
                , mailToolsListMailboxes = \_ _ -> pure (Right [])
                , mailToolsSearch = \request -> do
                    writeIORef seen (Just request)
                    pure (Right [])
                , mailToolsGetMessage = \_ _ -> pure (Left "not exercised")
                , mailToolsDownloadAttachment = \_ _ ->
                    pure (Left "not exercised")
                , mailToolsCreateDraft = \_ -> pure (Left "not exercised")
                , mailToolsUpdateDraft = \_ -> pure (Left "not exercised")
                , mailToolsReplyDraft = \_ -> pure (Left "not exercised")
                }
        tools <- mailTools env
        _ <- dispatchToolCall dispatchConfig
            (appToolHandlers tools)
            (functionToolCall "call-1" "email_search"
                "{\"account_id\":\"mail-1\",\"query\":\"invoice\"}")
        readIORef seen `shouldReturn` Just MailSearchRequest
            { mailSearchAccountId = "mail-1"
            , mailSearchMailboxId = Nothing
            , mailSearchQuery = Just "invoice"
            , mailSearchFrom = Nothing
            , mailSearchTo = Nothing
            , mailSearchSubject = Nothing
            , mailSearchAfter = Nothing
            , mailSearchBefore = Nothing
            , mailSearchHasAttachments = Nothing
            , mailSearchLimit = 20
            }

    it "validates recipients and bounds draft content before a mailbox write" do
        let valid = MailDraftContent
                { mailDraftTo = ["person@example.com"]
                , mailDraftCc = []
                , mailDraftBcc = []
                , mailDraftSubject = "Hello"
                , mailDraftBody = "Draft body"
                }
        validateMailDraftContent defaultMailToolLimits valid
            `shouldBe` Right valid
        validateMailDraftContent defaultMailToolLimits valid
            { mailDraftTo = ["person@example.com\r\nBcc: victim@example.com"] }
            `shouldSatisfy` isLeft
        validateMailDraftContent defaultMailToolLimits valid
            { mailDraftSubject = "Hello\r\nBcc: victim@example.com" }
            `shouldSatisfy` isLeft

    it "saves a draft only after decoding a bounded request" do
        seen <- newIORef Nothing
        seenUpdate <- newIORef Nothing
        seenReply <- newIORef Nothing
        toolEnv <- defaultToolEnv (fromText "/tmp")
        let connected = MailAccountSummary
                { mailAccountId = "mail-1"
                , mailAccountProvider = "gmail"
                , mailAccountEmail = "person@example.com"
                , mailAccountLabel = Nothing
                , mailAccountEnabled = True
                , mailAccountVerified = True
                }
            env = MailToolsEnv
                { mailToolsToolEnv = toolEnv
                , mailToolsLimits = defaultMailToolLimits
                , mailToolsListAccounts = pure (Right [connected])
                , mailToolsListMailboxes = \_ _ -> pure (Right [])
                , mailToolsSearch = \_ -> pure (Right [])
                , mailToolsGetMessage = \_ _ -> pure (Left "not exercised")
                , mailToolsDownloadAttachment = \_ _ ->
                    pure (Left "not exercised")
                , mailToolsCreateDraft = \request -> do
                    writeIORef seen (Just request)
                    pure (Right MailDraft
                        { mailDraftId = "draft-1"
                        , mailDraftMessageId = Nothing
                        , mailDraftThreadId = Nothing
                        , mailDraftWarning = Nothing
                        })
                , mailToolsUpdateDraft = \request -> do
                    writeIORef seenUpdate (Just request)
                    pure (Right MailDraft
                        { mailDraftId = request.mailUpdateDraftId
                        , mailDraftMessageId = Nothing
                        , mailDraftThreadId = Nothing
                        , mailDraftWarning = Nothing
                        })
                , mailToolsReplyDraft = \request -> do
                    writeIORef seenReply (Just request)
                    pure (Right MailDraft
                        { mailDraftId = "draft-reply"
                        , mailDraftMessageId = Nothing
                        , mailDraftThreadId = Nothing
                        , mailDraftWarning = Nothing
                        })
                }
        tools <- mailTools env
        created <- dispatchToolCall dispatchConfig
            (appToolHandlers tools)
            (functionToolCall "call-1" "email_create_draft"
                "{\"account_id\":\"mail-1\",\"to\":[\"person@example.com\"],\
                \\"subject\":\"Hello\",\"body\":\"Draft body\"}")
        readIORef seen `shouldReturn` Just MailCreateDraftRequest
            { mailCreateDraftAccountId = "mail-1"
            , mailCreateDraftContent = MailDraftContent
                { mailDraftTo = ["person@example.com"]
                , mailDraftCc = []
                , mailDraftBcc = []
                , mailDraftSubject = "Hello"
                , mailDraftBody = "Draft body"
                }
            }
        created.output `shouldSatisfy` Text.isInfixOf "\"sent\":false"
        _ <- dispatchToolCall dispatchConfig
            (appToolHandlers tools)
            (functionToolCall "call-update" "email_update_draft"
                "{\"account_id\":\"mail-1\",\"draft_id\":\"draft-1\",\
                \\"to\":[\"changed@example.com\"],\"subject\":\"Changed\",\
                \\"body\":\"Updated body\"}")
        readIORef seenUpdate `shouldReturn` Just MailUpdateDraftRequest
            { mailUpdateDraftAccountId = "mail-1"
            , mailUpdateDraftId = "draft-1"
            , mailUpdateDraftContent = MailDraftContent
                { mailDraftTo = ["changed@example.com"]
                , mailDraftCc = []
                , mailDraftBcc = []
                , mailDraftSubject = "Changed"
                , mailDraftBody = "Updated body"
                }
            }
        _ <- dispatchToolCall dispatchConfig
            (appToolHandlers tools)
            (functionToolCall "call-2" "email_reply_draft"
                "{\"account_id\":\"mail-1\",\"message_id\":\"message-1\",\
                \\"to\":[\"sender@example.com\"],\"body\":\"Reply body\"}")
        readIORef seenReply `shouldReturn` Just MailReplyDraftRequest
            { mailReplyDraftAccountId = "mail-1"
            , mailReplyDraftMessageId = "message-1"
            , mailReplyDraftTo = ["sender@example.com"]
            , mailReplyDraftBody = "Reply body"
            }

isAlwaysReadOnly :: ApprovalRule -> Bool
isAlwaysReadOnly = \case
    AlwaysReadOnly -> True
    _ -> False

isAlwaysPrompt :: ApprovalRule -> Bool
isAlwaysPrompt = \case
    AlwaysPrompt -> True
    _ -> False

isAlwaysConfirm :: ApprovalRule -> Bool
isAlwaysConfirm = \case
    AlwaysConfirm -> True
    _ -> False

dispatchConfig :: ToolDispatchConfig
dispatchConfig = ToolDispatchConfig
    { toolDispatchUnknownTool = \name -> "unknown: " <> name
    , toolDispatchFormatResult = either ("error: " <>) id
    , toolDispatchFormatException = \name _ -> name <> ": exception"
    , toolDispatchOnException = \_ _ -> pure ()
    , toolDispatchOnOutput = \_ _ -> pure ()
    , toolDispatchFinalizeOutput = \_call output -> pure output
    }
