{-# LANGUAGE UndecidableInstances #-}

-- A module for postgres execution related types and operations

module Hasura.Db
  ( MonadTx(..)
  , LazyTx

  , PGExecCtx(..)
  , runLazyTx
  , runLazyTx'
  , withUserInfo
  , sessionInfoJsonExp

  , RespTx
  , LazyRespTx
  , defaultTxErrorHandler
  , mkTxErrorHandler
  ) where

import           Control.Lens
import           Control.Monad.Trans.Control  (MonadBaseControl (..))
import           Control.Monad.Unique
import           Control.Monad.Validate

import qualified Data.Aeson.Extended          as J
import qualified Database.PG.Query            as Q
import qualified Database.PG.Query.Connection as Q

import           Hasura.EncJSON
import           Hasura.Prelude
import           Hasura.RQL.Types.Error
import           Hasura.RQL.Types.Permission
import           Hasura.SQL.Error
import           Hasura.SQL.Types

import qualified Hasura.SQL.DML               as S

data PGExecCtx
  = PGExecCtx
  { _pecPool        :: !Q.PGPool
  , _pecTxIsolation :: !Q.TxIsolation
  }

-- (forall code m. MonadError (QErr impl) m) => MonadTx code m
class (MonadError (QErr impl) m) => MonadTx code m where
  liftTx :: Q.TxE (QErr impl) a -> m a

instance (MonadTx code m) => MonadTx code (StateT s m) where
  liftTx = lift . liftTx
instance (MonadTx code m) => MonadTx code (ReaderT s m) where
  liftTx = lift . liftTx
instance (Monoid w, MonadTx code m) => MonadTx code (WriterT w m) where
  liftTx = lift . liftTx
instance (MonadTx code m) => MonadTx code (ValidateT e m) where
  liftTx = lift . liftTx
instance (MonadTx code m) => MonadTx code (ExceptT (QErr impl) m) where
  liftTx = lift . liftTx


-- | Like 'Q.TxE', but defers acquiring a Postgres connection until the first execution of 'liftTx'.
-- If no call to 'liftTx' is ever reached (i.e. a successful result is returned or an error is
-- raised before ever executing a query), no connection is ever acquired.
--
-- This is useful for certain code paths that only conditionally need database access. For example,
-- although most queries will eventually hit Postgres, introspection queries or queries that
-- exclusively use remote schemas never will; using 'LazyTx' keeps those branches from unnecessarily
-- allocating a connection.
data LazyTx e a
  = LTErr !e
  | LTNoTx !a
  | LTTx !(Q.TxE e a)

lazyTxToQTx :: LazyTx e a -> Q.TxE e a
lazyTxToQTx = \case
  LTErr e  -> throwError e
  LTNoTx r -> return r
  LTTx tx  -> tx

runLazyTx
  :: (MonadIO m, AsCodeHasura code)
  => PGExecCtx
  -> Q.TxAccess
  -> LazyTx (QErr impl) a
  -> ExceptT (QErr impl) m a
runLazyTx (PGExecCtx pgPool txIso) txAccess = \case
  LTErr e  -> throwError e
  LTNoTx a -> return a
  LTTx tx  -> ExceptT <$> liftIO $ runExceptT $ Q.runTx pgPool (txIso, Just txAccess) tx

runLazyTx'
  :: (MonadIO m, AsCodeHasura code) => PGExecCtx -> LazyTx (QErr impl) a -> ExceptT (QErr impl) m a
runLazyTx' (PGExecCtx pgPool _) = \case
  LTErr e  -> throwError e
  LTNoTx a -> return a
  LTTx tx  -> ExceptT <$> liftIO $ runExceptT $ Q.runTx' pgPool tx

type RespTx code = Q.TxE (QErr impl) EncJSON
type LazyRespTx code = LazyTx (QErr impl) EncJSON

setHeadersTx :: AsCodeHasura code => UserVars -> Q.TxE (QErr impl) ()
setHeadersTx uVars =
  Q.unitQE defaultTxErrorHandler setSess () False
  where
    setSess = Q.fromText $
      "SET LOCAL \"hasura.user\" = " <> toSQLTxt (sessionInfoJsonExp uVars)

sessionInfoJsonExp :: UserVars -> S.SQLExp
sessionInfoJsonExp = S.SELit . J.encodeToStrictText

defaultTxErrorHandler :: AsCodeHasura code => Q.PGTxErr -> QErr code
defaultTxErrorHandler = mkTxErrorHandler (const False)

-- | Constructs a transaction error handler given a predicate that determines which errors are
-- expected and should be reported to the user. All other errors are considered internal errors.
mkTxErrorHandler :: AsCodeHasura code => (PGErrorType -> Bool) -> Q.PGTxErr -> QErr code
mkTxErrorHandler isExpectedError txe = fromMaybe unexpectedError expectedError
  where
    unexpectedError = (internalError "postgres query error") { qeInternal = Just $ J.toJSON txe }
    expectedError = uncurry err400 <$> do
      errorDetail <- Q.getPGStmtErr txe
      message <- Q.edMessage errorDetail
      errorType <- pgErrorType errorDetail
      guard $ isExpectedError errorType
      pure $ case errorType of
        PGIntegrityConstraintViolation code ->
          let cv = (_ConstraintViolation # (),)
              customMessage = (code ^? _Just._PGErrorSpecific) <&> \case
                PGRestrictViolation -> cv "Can not delete or update due to data being referred. "
                PGNotNullViolation -> cv "Not-NULL violation. "
                PGForeignKeyViolation -> cv "Foreign key violation. "
                PGUniqueViolation -> cv "Uniqueness violation. "
                PGCheckViolation -> (_PermissionError # (), "Check constraint violation. ")
                PGExclusionViolation -> cv "Exclusion violation. "
          in maybe (_ConstraintViolation # (), message) (fmap (<> message)) customMessage
        PGDataException code -> case code of
          Just (PGErrorSpecific PGInvalidEscapeSequence) -> (_BadRequest # (), message)
          _                                              -> (_DataException # (), message)

        PGSyntaxErrorOrAccessRuleViolation code -> (_ConstraintError # (),) $ case code of
          Just (PGErrorSpecific PGInvalidColumnReference) ->
            "there is no unique or exclusion constraint on target column(s)"
          _ -> message

withUserInfo :: AsCodeHasura code => UserInfo -> LazyTx (QErr code) a -> LazyTx (QErr code) a
withUserInfo uInfo = \case
  LTErr e  -> LTErr e
  LTNoTx a -> LTNoTx a
  LTTx tx  -> LTTx $ setHeadersTx (userVars uInfo) >> tx

instance Functor (LazyTx e) where
  fmap f = \case
    LTErr e  -> LTErr e
    LTNoTx a -> LTNoTx $ f a
    LTTx tx  -> LTTx $ fmap f tx

instance Applicative (LazyTx e) where
  pure = LTNoTx

  LTErr e   <*> _         = LTErr e
  LTNoTx f  <*> r         = fmap f r
  LTTx _    <*> LTErr e   = LTErr e
  LTTx txf  <*> LTNoTx a  = LTTx $ txf <*> pure a
  LTTx txf  <*> LTTx tx   = LTTx $ txf <*> tx

instance Monad (LazyTx e) where
  LTErr e >>= _  = LTErr e
  LTNoTx a >>= f = f a
  LTTx txa >>= f =
    LTTx $ txa >>= lazyTxToQTx . f

instance MonadError e (LazyTx e) where
  throwError = LTErr
  LTErr e  `catchError` f = f e
  LTNoTx a `catchError` _ = LTNoTx a
  LTTx txe `catchError` f =
    LTTx $ txe `catchError` (lazyTxToQTx . f)

instance MonadTx code (LazyTx (QErr code)) where
  liftTx = LTTx

instance MonadTx code (Q.TxE (QErr code)) where
  liftTx = id

instance MonadIO (LazyTx e) where
  liftIO = LTTx . liftIO

instance MonadBase IO (LazyTx e) where
  liftBase = liftIO

instance MonadBaseControl IO (LazyTx e) where
  type StM (LazyTx e) a = StM (Q.TxE e) a
  liftBaseWith f = LTTx $ liftBaseWith \run -> f (run . lazyTxToQTx)
  restoreM = LTTx . restoreM

instance MonadUnique (LazyTx e) where
  newUnique = liftIO newUnique
