-- | Small compatibility helpers for constructing Hasql statements.
module Agent.Store.Postgres.Hasql
    ( mkStatement
    ) where

import Data.Text (Text)
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Encoders as Encoders
import Hasql.Statement (Statement)
import qualified Hasql.Statement as Statement

mkStatement
    :: Text
    -> Encoders.Params params
    -> Decoders.Result result
    -> Bool
    -> Statement params result
mkStatement sql encoder decoder shouldPrepare
    | shouldPrepare = Statement.preparable sql encoder decoder
    | otherwise = Statement.unpreparable sql encoder decoder
