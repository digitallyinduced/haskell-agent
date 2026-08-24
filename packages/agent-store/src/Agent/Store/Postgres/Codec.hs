module Agent.Store.Postgres.Codec
    ( textParam
    , nullableTextParam
    , int32Param
    , int64Param
    , nullableInt64Param
    , boolParam
    , timeParam
    , textColumn
    , nullableTextColumn
    , int32Column
    , int64Column
    , nullableInt64Column
    , boolColumn
    , timeColumn
    , textSingleResult
    , boolResult
    ) where

import Data.Int (Int32, Int64)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Encoders as Encoders

textParam :: Encoders.Params Text
textParam = Encoders.param (Encoders.nonNullable Encoders.text)

nullableTextParam :: Encoders.Params (Maybe Text)
nullableTextParam = Encoders.param (Encoders.nullable Encoders.text)

int32Param :: Encoders.Params Int32
int32Param = Encoders.param (Encoders.nonNullable Encoders.int4)

int64Param :: Encoders.Params Int64
int64Param = Encoders.param (Encoders.nonNullable Encoders.int8)

nullableInt64Param :: Encoders.Params (Maybe Int64)
nullableInt64Param = Encoders.param (Encoders.nullable Encoders.int8)

boolParam :: Encoders.Params Bool
boolParam = Encoders.param (Encoders.nonNullable Encoders.bool)

timeParam :: Encoders.Params UTCTime
timeParam = Encoders.param (Encoders.nonNullable Encoders.timestamptz)

textColumn :: Decoders.Row Text
textColumn = Decoders.column (Decoders.nonNullable Decoders.text)

nullableTextColumn :: Decoders.Row (Maybe Text)
nullableTextColumn = Decoders.column (Decoders.nullable Decoders.text)

int32Column :: Decoders.Row Int32
int32Column = Decoders.column (Decoders.nonNullable Decoders.int4)

int64Column :: Decoders.Row Int64
int64Column = Decoders.column (Decoders.nonNullable Decoders.int8)

nullableInt64Column :: Decoders.Row (Maybe Int64)
nullableInt64Column = Decoders.column (Decoders.nullable Decoders.int8)

boolColumn :: Decoders.Row Bool
boolColumn = Decoders.column (Decoders.nonNullable Decoders.bool)

timeColumn :: Decoders.Row UTCTime
timeColumn = Decoders.column (Decoders.nonNullable Decoders.timestamptz)

textSingleResult :: Decoders.Result Text
textSingleResult = Decoders.singleRow textColumn

boolResult :: Decoders.Result Bool
boolResult = Decoders.singleRow boolColumn
