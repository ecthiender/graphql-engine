module Hasura.GraphQL.Schema.Default where

import           Hasura.GraphQL.Context
import           Hasura.GraphQL.Resolve.Types
import           Hasura.Prelude
import           Hasura.RQL.Types.Column
import           Hasura.RQL.Types.Error
import           Hasura.RQL.Types.Function
import           Hasura.RQL.Types.Permission
import           Hasura.RQL.Types.SchemaCache
import           Hasura.SQL.Types

import qualified Data.HashMap.Strict          as Map

class Monad m => DefaultRolesSchema m where
  -- return a hashmap with the defaults roles and their schema
  generateDefaultRolesSchema
    :: MonadError QErr m
    => TableInfo PGColumnInfo
    -> TableCache PGColumnInfo
    -> [PGColumnInfo]
    -- ^ cols
    -> [PGColumnInfo]
    -- ^ primary key cols info
    -> [ConstraintName]
    -> [FunctionInfo]
    -> m (Map.HashMap RoleName (TyAgg, RootFields, Map.HashMap QualifiedTable InsCtx))
