module Agent.HistorySpec (spec) where

import Agent.History
import Data.Text (Text)
import Test.Hspec

data Item
    = Checkpoint Int
    | Activation Int
    | Opaque Text
    | Call Bool Int
    | Output Bool Int
    deriving (Eq, Show)

spec :: Spec
spec = do
    describe "native transcript replay" $ do
        it "preserves opaque items when there is no checkpoint" $
            replayFromLatestCheckpoint isCheckpoint [Opaque "encrypted"] `shouldBe` [Opaque "encrypted"]
        it "retains only the latest checkpoint and its suffix" $
            replayFromLatestCheckpoint isCheckpoint
                [Opaque "old", Checkpoint 1, Activation 1, Checkpoint 2, Opaque "encrypted"]
                `shouldBe` [Checkpoint 2, Opaque "encrypted"]
        it "handles empty history" $
            replayFromLatestCheckpoint isCheckpoint [] `shouldBe` []
    describe "tool pair repair" $ do
        it "removes stale outputs and inserts interrupted outputs adjacent to calls" $
            repair [Output False 0, Call False 1, Opaque "retained"]
                `shouldBe` [Call False 1, Output False 1, Opaque "retained"]
        it "preserves completed pairs without moving their output" $
            repair [Call False 1, Opaque "retained", Output False 1]
                `shouldBe` [Call False 1, Opaque "retained", Output False 1]
        it "separates call kinds that share the same identifier" $
            repair [Call False 1, Output True 1]
                `shouldBe` [Call False 1, Output False 1]
        it "repairs after the checkpoint cut, not against discarded calls" $
            (repair . replayFromLatestCheckpoint isCheckpoint)
                [Call False 1, Checkpoint 1, Output False 1]
                `shouldBe` [Checkpoint 1]
        it "is idempotent" $
            let items = [Output True 2, Call False 1, Opaque "retained"]
            in repair (repair items) `shouldBe` repair items
    describe "protected context trimming" $ do
        it "restores the latest activation when suffix trimming would drop it" $
            trimActivation 2 [Activation 1, Activation 2, Opaque "a", Opaque "b"]
                `shouldBe` [Activation 2, Opaque "b"]
        it "does not duplicate an activation already retained" $
            trimActivation 2 [Activation 1, Opaque "a"]
                `shouldBe` [Activation 1, Opaque "a"]
        it "uses ordinary trimming when there is no activation" $
            trimActivation 1 [Opaque "a", Opaque "b"] `shouldBe` [Opaque "b"]
        it "reserves checkpoint space before preserving the latest activation" $
            trimCheckpoint 2 [Opaque "old", Checkpoint 1, Activation 1, Opaque "a", Opaque "b"]
                `shouldBe` [Checkpoint 1, Activation 1]
        it "retains checkpoint and activation even when their size exceeds the budget" $
            trimCheckpoint 0 [Checkpoint 1, Activation 1, Opaque "a"]
                `shouldBe` [Checkpoint 1, Activation 1]

isCheckpoint :: Item -> Bool
isCheckpoint (Checkpoint _) = True
isCheckpoint _ = False

isActivation :: Item -> Bool
isActivation (Activation _) = True
isActivation _ = False

repair :: [Item] -> [Item]
repair = repairToolPairs callReference outputReference (uncurry Output)
  where
    callReference (Call kind identifier) = Just (kind, identifier)
    callReference _ = Nothing
    outputReference (Output kind identifier) = Just (kind, identifier)
    outputReference _ = Nothing

trimSuffix :: Int -> [Item] -> [Item]
trimSuffix budget items = drop (max 0 (length items - max 0 budget)) items

trimActivation :: Int -> [Item] -> [Item]
trimActivation = trimPreservingLatestActivation id isActivation (const 1) trimSuffix

trimCheckpoint :: Int -> [Item] -> [Item]
trimCheckpoint = trimPreservingLatestCheckpoint id isCheckpoint (const 1) trimActivation
