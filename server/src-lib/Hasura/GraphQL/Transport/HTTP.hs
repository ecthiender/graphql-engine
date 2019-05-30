module Hasura.GraphQL.Transport.HTTP
  ( runGQ
  ) where

import qualified Data.ByteString.Lazy                   as BL
import qualified Network.HTTP.Client                    as HTTP
import qualified Network.HTTP.Types                     as N

import           Hasura.EncJSON
import           Hasura.GraphQL.Logging
import           Hasura.GraphQL.Transport.HTTP.Protocol
import           Hasura.Prelude
import           Hasura.RQL.Types
import           Hasura.Server.Utils                    (RequestId)

import qualified Hasura.GraphQL.Execute                 as E
import qualified Hasura.Logging                         as L

runGQ
  :: (MonadIO m, MonadError QErr m)
  => PGExecCtx
  -> L.Logger
  -> L.VerboseLogging
  -> RequestId
  -> UserInfo
  -> SQLGenCtx
  -> Bool
  -> E.PlanCache
  -> SchemaCache
  -> SchemaCacheVer
  -> HTTP.Manager
  -> [N.Header]
  -> GQLReqUnparsed
  -> BL.ByteString -- this can be removed when we have a pretty-printer
  -> m EncJSON
runGQ pgExecCtx logger verbose reqId userInfo sqlGenCtx enableAL planCache
  sc scVer manager reqHdrs req rawReq = do
  execPlan <- E.getResolvedExecPlan pgExecCtx planCache
              userInfo sqlGenCtx enableAL sc scVer req
  case execPlan of
    E.GExPHasura resolvedOp ->
      runHasuraGQ pgExecCtx logger verbose reqId req userInfo resolvedOp
    E.GExPRemote rsi opDef  ->
      E.execRemoteGQ logger verbose manager reqId userInfo reqHdrs req rawReq
      rsi opDef

runHasuraGQ
  :: (MonadIO m, MonadError QErr m)
  => PGExecCtx
  -> L.Logger
  -> L.VerboseLogging
  -> RequestId
  -> GQLReqUnparsed
  -> UserInfo
  -> E.ExecOp
  -> m EncJSON
runHasuraGQ pgExecCtx logger verbose reqId query userInfo resolvedOp = do
  respE <- liftIO $ runExceptT $ case resolvedOp of
    E.ExOpQuery tx genSql  -> do
      -- log the generated SQL and the graphql query
      liftIO $ logGraphqlQuery logger verbose $ mkQueryLog reqId query genSql
      runLazyTx' pgExecCtx tx
    E.ExOpMutation tx -> do
      -- log the generated SQL and the graphql query
      liftIO $ logGraphqlQuery logger verbose $ mkQueryLog reqId query Nothing
      runLazyTx pgExecCtx $ withUserInfo userInfo tx
    E.ExOpSubs _ ->
      throw400 UnexpectedPayload
      "subscriptions are not supported over HTTP, use websockets instead"
  resp <- liftEither respE
  return $ encodeGQResp $ GQSuccess $ encJToLBS resp
