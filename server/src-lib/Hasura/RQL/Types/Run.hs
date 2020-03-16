{-# LANGUAGE UndecidableInstances #-}

module Hasura.RQL.Types.Run
  ( Run(..)
  , RunCtx(..)
  , peelRun
  ) where

import           Hasura.Prelude

import qualified Database.PG.Query           as Q
import qualified Network.HTTP.Client         as HTTP

import           Control.Monad.Trans.Control (MonadBaseControl)
import           Control.Monad.Unique

import           Hasura.RQL.Types

data RunCtx
  = RunCtx
  { _rcUserInfo  :: !UserInfo
  , _rcHttpMgr   :: !HTTP.Manager
  , _rcSqlGenCtx :: !SQLGenCtx
  }

newtype Run code a
  = Run { unRun :: ReaderT RunCtx (LazyTx (QErr code)) a }
  deriving ( Functor, Applicative, Monad
           , MonadError (QErr code)
           , MonadReader RunCtx
           -- , MonadTx code
           , MonadIO
           , MonadBase IO
           , MonadBaseControl IO
           , MonadUnique
           )

deriving instance MonadTx code (LazyTx (QErr code)) => MonadTx code (Run code)

instance UserInfoM (Run code) where
  askUserInfo = asks _rcUserInfo

instance HasHttpManager (Run code) where
  askHttpManager = asks _rcHttpMgr

instance HasSQLGenCtx (Run code) where
  askSQLGenCtx = asks _rcSqlGenCtx

peelRun
  :: (MonadIO m, AsCodeHasura code)
  => RunCtx
  -> PGExecCtx
  -> Q.TxAccess
  -> Run code a
  -> ExceptT (QErr code) m a
peelRun runCtx@(RunCtx userInfo _ _) pgExecCtx txAccess (Run m) =
  runLazyTx pgExecCtx txAccess $ withUserInfo userInfo $ runReaderT m runCtx
