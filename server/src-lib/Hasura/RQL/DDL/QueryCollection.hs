module Hasura.RQL.DDL.QueryCollection
  ( runCreateCollection
  , runDropCollection
  , addCollectionToCatalog
  , delCollectionFromCatalog
  , runAddQueryToCollection
  , runDropQueryFromCollection
  , runAddCollectionToAllowlist
  , runDropCollectionFromAllowlist
  , addCollectionToAllowlistCatalog
  , delCollectionFromAllowlistCatalog
  , fetchAllCollections
  , fetchAllowlist
  , module Hasura.RQL.Types.QueryCollection
  ) where

import           Control.Lens                     (( # ))

import           Hasura.EncJSON
import           Hasura.Prelude
import           Hasura.RQL.Types
import           Hasura.RQL.Types.QueryCollection
import           Hasura.Server.Utils              (duplicates)
import           Hasura.SQL.Types

import qualified Data.Text                        as T
import qualified Data.Text.Extended               as T
import qualified Database.PG.Query                as Q

addCollectionP2
  :: (QErrM m code, AsCodeHasura code)
  => CollectionDef -> m ()
addCollectionP2 (CollectionDef queryList) =
  withPathK "queries" $
    unless (null duplicateNames) $ throw400 (_NotSupported # ()) $
      "found duplicate query names "
      <> T.intercalate ", " (map (T.dquote . unNonEmptyText . unQueryName) duplicateNames)
  where
    duplicateNames = duplicates $ map _lqName queryList

runCreateCollection
  :: (QErrM m code, MonadTx code m, AsCodeHasura code, HasSystemDefined m)
  => CreateCollection -> m EncJSON
runCreateCollection cc = do
  collDetM <- getCollectionDefM collName
  withPathK "name" $
    onJust collDetM $ const $ throw400 (_AlreadyExists # ()) $
      "query collection with name " <> collName <<> " already exists"
  withPathK "definition" $ addCollectionP2 def
  systemDefined <- askSystemDefined
  liftTx $ addCollectionToCatalog cc systemDefined
  return successMsg
  where
    CreateCollection collName def _ = cc

runAddQueryToCollection
  :: (CacheRWM m, MonadTx code m, AsCodeHasura code)
  => AddQueryToCollection -> m EncJSON
runAddQueryToCollection (AddQueryToCollection collName queryName query) = do
  CollectionDef qList <- getCollectionDef collName
  let queryExists = flip any qList $ \q -> _lqName q == queryName

  when queryExists $ throw400 (_AlreadyExists # ()) $ "query with name "
    <> queryName <<> " already exists in collection " <>> collName

  let collDef = CollectionDef $ qList <> pure listQ
  liftTx $ updateCollectionDefCatalog collName collDef
  withNewInconsistentObjsCheck buildSchemaCache
  return successMsg
  where
    listQ = ListedQuery queryName query

runDropCollection
  :: (MonadTx code m, AsCodeHasura code, CacheRWM m)
  => DropCollection -> m EncJSON
runDropCollection (DropCollection collName cascade) = do
  withPathK "collection" $ do
    -- check for query collection
    void $ getCollectionDef collName

    allowlist <- liftTx fetchAllowlist
    when (collName `elem` allowlist) $
      if cascade then do
        -- drop collection in allowlist
        liftTx $ delCollectionFromAllowlistCatalog collName
        withNewInconsistentObjsCheck buildSchemaCache
      else throw400 (_DependencyError # ()) $ "query collection with name "
           <> collName <<> " is present in allowlist; cannot proceed to drop"
  liftTx $ delCollectionFromCatalog collName
  return successMsg

runDropQueryFromCollection
  :: (CacheRWM m, MonadTx code m, AsCodeHasura code)
  => DropQueryFromCollection -> m EncJSON
runDropQueryFromCollection (DropQueryFromCollection collName queryName) = do
  CollectionDef qList <- getCollectionDef collName
  let queryExists = flip any qList $ \q -> _lqName q == queryName
  when (not queryExists) $ throw400 (_NotFound # ()) $ "query with name "
    <> queryName <<> " not found in collection " <>> collName
  let collDef = CollectionDef $ flip filter qList $
                                \q -> _lqName q /= queryName
  liftTx $ updateCollectionDefCatalog collName collDef
  withNewInconsistentObjsCheck buildSchemaCache
  return successMsg

runAddCollectionToAllowlist
  :: (MonadTx code m, AsCodeHasura code, CacheRWM m)
  => CollectionReq -> m EncJSON
runAddCollectionToAllowlist (CollectionReq collName) = do
  void $ withPathK "collection" $ getCollectionDef collName
  liftTx $ addCollectionToAllowlistCatalog collName
  withNewInconsistentObjsCheck buildSchemaCache
  return successMsg

runDropCollectionFromAllowlist
  :: (UserInfoM m, MonadTx code m, AsCodeHasura code, CacheRWM m)
  => CollectionReq -> m EncJSON
runDropCollectionFromAllowlist (CollectionReq collName) = do
  void $ withPathK "collection" $ getCollectionDef collName
  liftTx $ delCollectionFromAllowlistCatalog collName
  withNewInconsistentObjsCheck buildSchemaCache
  return successMsg

getCollectionDef
  :: (QErrM m code, MonadTx code m, AsCodeHasura code)
  => CollectionName -> m CollectionDef
getCollectionDef collName = do
  detM <- getCollectionDefM collName
  onNothing detM $ throw400 (_NotExists # ()) $
    "query collection with name " <> collName <<> " does not exists"

getCollectionDefM
  :: (QErrM m code, MonadTx code m, AsCodeHasura code)
  => CollectionName -> m (Maybe CollectionDef)
getCollectionDefM collName = do
  allCollections <- liftTx fetchAllCollections
  let filteredList = flip filter allCollections $ \c -> collName == _ccName c
  case filteredList of
    []  -> return Nothing
    [a] -> return $ Just $ _ccDefinition a
    _   -> throw500 "more than one row returned for query collections"

-- Database functions
fetchAllCollections :: AsCodeHasura code => Q.TxE (QErr code) [CreateCollection]
fetchAllCollections = do
  r <- Q.listQE defaultTxErrorHandler [Q.sql|
           SELECT collection_name, collection_defn::json, comment
             FROM hdb_catalog.hdb_query_collection
          |] () False
  return $ flip map r $ \(name, Q.AltJ defn, mComment)
                        -> CreateCollection name defn mComment

fetchAllowlist :: AsCodeHasura code => Q.TxE (QErr code) [CollectionName]
fetchAllowlist = map runIdentity <$>
  Q.listQE defaultTxErrorHandler [Q.sql|
      SELECT collection_name
        FROM hdb_catalog.hdb_allowlist
     |] () True

addCollectionToCatalog :: AsCodeHasura code => CreateCollection -> SystemDefined -> Q.TxE (QErr code) ()
addCollectionToCatalog (CreateCollection name defn mComment) systemDefined =
  Q.unitQE defaultTxErrorHandler [Q.sql|
    INSERT INTO hdb_catalog.hdb_query_collection
      (collection_name, collection_defn, comment, is_system_defined)
    VALUES ($1, $2, $3, $4)
  |] (name, Q.AltJ defn, mComment, systemDefined) True

delCollectionFromCatalog :: AsCodeHasura code => CollectionName -> Q.TxE (QErr code) ()
delCollectionFromCatalog name =
  Q.unitQE defaultTxErrorHandler [Q.sql|
     DELETE FROM hdb_catalog.hdb_query_collection
     WHERE collection_name = $1
  |] (Identity name) True

updateCollectionDefCatalog
  :: AsCodeHasura code => CollectionName -> CollectionDef -> Q.TxE (QErr code) ()
updateCollectionDefCatalog collName def = do
  -- Update definition
  Q.unitQE defaultTxErrorHandler [Q.sql|
    UPDATE hdb_catalog.hdb_query_collection
       SET collection_defn = $1
     WHERE collection_name = $2
  |] (Q.AltJ def, collName) True

addCollectionToAllowlistCatalog :: AsCodeHasura code => CollectionName -> Q.TxE (QErr code) ()
addCollectionToAllowlistCatalog collName =
  Q.unitQE defaultTxErrorHandler [Q.sql|
      INSERT INTO hdb_catalog.hdb_allowlist
                   (collection_name)
            VALUES ($1)
      |] (Identity collName) True

delCollectionFromAllowlistCatalog :: AsCodeHasura code => CollectionName -> Q.TxE (QErr code) ()
delCollectionFromAllowlistCatalog collName =
  Q.unitQE defaultTxErrorHandler [Q.sql|
      DELETE FROM hdb_catalog.hdb_allowlist
         WHERE collection_name = $1
      |] (Identity collName) True
