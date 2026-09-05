module Agent.Store.Postgres.Skill.Mapping.Statements.SearchSkills
    ( searchSkillsStatement
    ) where

import Data.Functor.Contravariant ((>$<))
import Data.Text (Text)
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Encoders as Encoders
import Hasql.Statement (Statement)

import Agent.Store.Postgres.Hasql (mkStatement)
import Agent.Store.Postgres.Skill.Mapping.Codec
    ( skillRowDecoder
    , skillColumns
    , applicableScopesEncoder
    )
import Agent.Store.Postgres.Skill.Mapping.Types

searchSkillsStatement
    :: Statement SkillSearchParams [(SkillRow, Double)]
searchSkillsStatement = mkStatement
    ("WITH query AS (SELECT websearch_to_tsquery('english', $4) AS value) "
        <> skillSelectSqlWithPrefix
        <> ", ts_rank_cd(s.search_vector, query.value)::float8"
        <> " FROM harness.skills s CROSS JOIN query"
        <> applicableWhereSqlWithPrefix
        <> " AND s.status = 'active' AND (\
           \ s.search_vector @@ query.value\
           \ OR s.slug ILIKE '%' || $4 || '%'\
           \ OR s.title ILIKE '%' || $4 || '%'\
           \ OR s.description ILIKE '%' || $4 || '%'\
           \ OR s.applies_when ILIKE '%' || $4 || '%'\
           \ OR s.instructions_text ILIKE '%' || $4 || '%'\
           \ )\
           \ ORDER BY ts_rank_cd(s.search_vector, query.value) DESC,\
           \ s.priority DESC, s.updated_at DESC\
           \ LIMIT $5")
    ( ((.skillSearchScopes) >$< applicableScopesEncoder)
        <> ((.skillSearchQuery) >$< Encoders.param (Encoders.nonNullable Encoders.text))
        <> ((.skillSearchLimit) >$< Encoders.param (Encoders.nonNullable Encoders.int8))
    )
    (Decoders.rowList ((,) <$> skillRowDecoder <*> Decoders.column (Decoders.nonNullable Decoders.float8)))
    True

skillSelectSqlWithPrefix :: Text
skillSelectSqlWithPrefix = "SELECT " <> skillColumns "s."

applicableWhereSqlWithPrefix :: Text
applicableWhereSqlWithPrefix =
    " WHERE (\
    \ (s.scope_kind = 'user' AND s.scope_id = $1::uuid)\
    \ OR (s.scope_kind = 'repository' AND s.scope_id = $2::uuid)\
    \ OR (s.scope_kind = 'checkout' AND s.scope_id = $3::uuid)\
    \ )"
