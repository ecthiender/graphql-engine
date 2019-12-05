module Hasura.GraphQL.Context where

import           Hasura.Prelude

import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Aeson.TH
import           Data.Has
import           Language.Haskell.TH.Syntax    (Lift)

import qualified Data.HashMap.Strict           as Map
import qualified Data.HashSet                  as Set
import qualified Data.Text                     as T
import qualified Language.GraphQL.Draft.Syntax as G

import           Hasura.GraphQL.Resolve.Types
import           Hasura.GraphQL.Validate.Types
import           Hasura.RQL.Types.Permission
import           Hasura.Server.Utils           (duplicates)
import           Hasura.SQL.Types

-- | A /GraphQL context/, aka the final output of GraphQL schema generation. Used to both validate
-- incoming queries and respond to introspection queries.
--
-- Combines information from 'TyAgg', 'RootFields', and 'InsCtxMap' datatypes and adds a bit more on
-- top. Constructed via the 'mkGCtx' smart constructor.
data GCtx
  = GCtx
  -- GraphQL type information
  { _gTypes          :: !TypeMap
  , _gFields         :: !FieldMap
  , _gQueryRoot      :: !ObjTyInfo
  , _gMutRoot        :: !(Maybe ObjTyInfo)
  , _gSubRoot        :: !(Maybe ObjTyInfo)
  -- Postgres type information
  , _gOrdByCtx       :: !OrdByCtx
  , _gQueryCtxMap    :: !QueryCtxMap
  , _gMutationCtxMap :: !MutationCtxMap
  , _gInsCtxMap      :: !InsCtxMap
  } deriving (Show, Eq)

data RemoteGCtx
  = RemoteGCtx
  { _rgTypes            :: !TypeMap
  , _rgQueryRoot        :: !ObjTyInfo
  , _rgMutationRoot     :: !(Maybe ObjTyInfo)
  , _rgSubscriptionRoot :: !(Maybe ObjTyInfo)
  } deriving (Show, Eq)

instance Has TypeMap GCtx where
  getter = _gTypes
  modifier f ctx = ctx { _gTypes = f $ _gTypes ctx }

instance ToJSON GCtx where
  toJSON _ = String "ToJSON for GCtx is not implemented"

type GCtxMap = Map.HashMap RoleName GCtx

-- | A /types aggregate/, which holds role-specific information about visible GraphQL types.
-- Importantly, it holds more than just the information needed by GraphQL: it also includes how the
-- GraphQL types relate to Postgres types, which is used to validate literals provided for
-- Postgres-specific scalars.
data TyAgg
  = TyAgg
  { _taTypes   :: !TypeMap
  , _taFields  :: !FieldMap
  , _taScalars :: !(Set.HashSet PGScalarType)
  , _taOrdBy   :: !OrdByCtx
  } deriving (Show, Eq)

instance Semigroup TyAgg where
  (TyAgg t1 f1 s1 o1) <> (TyAgg t2 f2 s2 o2) =
    TyAgg (Map.union t1 t2) (Map.union f1 f2)
          (Set.union s1 s2) (Map.union o1 o2)

instance Monoid TyAgg where
  mempty = TyAgg Map.empty Map.empty Set.empty Map.empty

-- | A role-specific mapping from root field names to allowed operations.
data RootFields
  = RootFields
  { rootQueryFields    :: !(Map.HashMap G.Name (QueryCtx, ObjFldInfo))
  , rootMutationFields :: !(Map.HashMap G.Name (MutationCtx, ObjFldInfo))
  } deriving (Show, Eq)

instance Semigroup RootFields where
  RootFields a1 b1 <> RootFields a2 b2
    = RootFields (a1 <> a2) (b1 <> b2)

instance Monoid RootFields where
  mempty = RootFields Map.empty Map.empty


mkQueryRootTyInfo :: [ObjFldInfo] -> ObjTyInfo
mkQueryRootTyInfo flds =
  mkHsraObjTyInfo (Just "query root")
  (G.NamedType "query_root") Set.empty $
  mapFromL _fiName $ schemaFld:typeFld:flds
  where
    schemaFld = mkHsraObjFldInfo Nothing "__schema" Map.empty $
                G.toGT $ G.toNT $ G.NamedType "__Schema"
    typeFld = mkHsraObjFldInfo Nothing "__type" typeFldArgs $
              G.toGT $ G.NamedType "__Type"
    typeFldArgs = mapFromL _iviName $ pure $
      InpValInfo (Just "name of the type") "name" Nothing
      $ G.toGT $ G.toNT $ G.NamedType "String"

defaultTypes :: [TypeInfo]
defaultTypes = $(fromSchemaDocQ defaultSchema TLHasuraType)

emptyGCtx :: GCtx
emptyGCtx =
  let queryRoot = mkQueryRootTyInfo []
      allTys    = mkTyInfoMap $ TIObj queryRoot:defaultTypes
  -- for now subscription root is query root
  in GCtx allTys mempty queryRoot Nothing Nothing mempty mempty mempty mempty

data TableCustomRootFields
  = TableCustomRootFields
  { _tcrfSelect          :: !(Maybe G.Name)
  , _tcrfSelectByPk      :: !(Maybe G.Name)
  , _tcrfSelectAggregate :: !(Maybe G.Name)
  , _tcrfInsert          :: !(Maybe G.Name)
  , _tcrfUpdate          :: !(Maybe G.Name)
  , _tcrfDelete          :: !(Maybe G.Name)
  } deriving (Show, Eq, Lift)
$(deriveToJSON (aesonDrop 5 snakeCase) ''TableCustomRootFields)

instance FromJSON TableCustomRootFields where
  parseJSON = withObject "Object" $ \obj -> do
    select <- obj .:? "select"
    selectByPk <- obj .:? "select_by_pk"
    selectAggregate <- obj .:? "select_aggregate"
    insert <- obj .:? "insert"
    update <- obj .:? "update"
    delete <- obj .:? "delete"

    let duplicateRootFields = duplicates $
                              catMaybes [ select, selectByPk, selectAggregate
                                        , insert, update, delete
                                        ]
    when (not $ null duplicateRootFields) $ fail $ T.unpack $
      "the following custom root field names are duplicated: "
      <> showNames duplicateRootFields

    pure $ TableCustomRootFields select selectByPk selectAggregate
                                 insert update delete

emptyCustomRootFields :: TableCustomRootFields
emptyCustomRootFields =
  TableCustomRootFields
  { _tcrfSelect          = Nothing
  , _tcrfSelectByPk      = Nothing
  , _tcrfSelectAggregate = Nothing
  , _tcrfInsert          = Nothing
  , _tcrfUpdate          = Nothing
  , _tcrfDelete          = Nothing
  }
