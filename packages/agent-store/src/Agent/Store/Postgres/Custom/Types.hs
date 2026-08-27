{-# LANGUAGE NoFieldSelectors #-}

module Agent.Store.Postgres.Custom.Types
    ( QueryLimits(..)
    , defaultQueryLimits
    , CatalogObject(..)
    , CatalogDefinition(..)
    , CatalogColumn(..)
    , CatalogConstraint(..)
    , CatalogIndex(..)
    , CustomAuditContext(..)
    , CustomExecutionResult(..)
    ) where

import Data.Int (Int64)
import Data.Text (Text)

data QueryLimits = QueryLimits
    { queryMaxRows :: !Int64
    , queryMaxOutputBytes :: !Int
    , queryStatementTimeoutMs :: !Int
    , queryLockTimeoutMs :: !Int
    }
    deriving (Eq, Show)

defaultQueryLimits :: QueryLimits
defaultQueryLimits = QueryLimits
    { queryMaxRows = 500
    , queryMaxOutputBytes = 100000
    , queryStatementTimeoutMs = 30000
    , queryLockTimeoutMs = 5000
    }

data CatalogObject = CatalogObject
    { catalogObjectKind :: !Text
    , catalogObjectName :: !Text
    , catalogObjectDefinition :: !CatalogDefinition
    }
    deriving (Eq, Show)

data CatalogDefinition = CatalogDefinition
    { definitionOwner :: !(Maybe Text)
    , definitionComment :: !(Maybe Text)
    , definitionView :: !(Maybe Text)
    , definitionColumns :: ![CatalogColumn]
    , definitionConstraints :: ![CatalogConstraint]
    , definitionIndexes :: ![CatalogIndex]
    }
    deriving (Eq, Show)

data CatalogColumn = CatalogColumn
    { columnName :: !Text
    , columnType :: !Text
    , columnNullable :: !Bool
    , columnDefault :: !(Maybe Text)
    , columnIdentity :: !(Maybe Text)
    , columnGenerated :: !(Maybe Text)
    , columnComment :: !(Maybe Text)
    }
    deriving (Eq, Show)

data CatalogConstraint = CatalogConstraint
    { constraintName :: !Text
    , constraintType :: !Text
    , constraintDefinition :: !Text
    }
    deriving (Eq, Show)

data CatalogIndex = CatalogIndex
    { indexName :: !Text
    , indexDefinition :: !Text
    }
    deriving (Eq, Show)

data CustomAuditContext = CustomAuditContext
    { customAuditSessionId :: !(Maybe Text)
    , customAuditAgentId :: !(Maybe Text)
    }
    deriving (Eq, Show)

data CustomExecutionResult = CustomExecutionResult
    { customExecutionAuditId :: !Text
    , customExecutionCatalogBefore :: ![CatalogObject]
    , customExecutionCatalogAfter :: ![CatalogObject]
    , customExecutionWarning :: !(Maybe Text)
    }
    deriving (Eq, Show)
