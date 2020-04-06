{-# LANGUAGE RecordWildCards #-}
module Hasura.GraphQL.Transport.HTTP
  ( runGQ
  , runGQBatched
  , GQLReq(..)
  , GQLReqUnparsed
  , GQLReqParsed
  , GQLExecDoc(..)
  , OperationName(..)
  , GQLQueryText(..)
  ) where

import           Control.Lens                           (( # ))

import qualified Database.PG.Query                      as Q
import qualified Language.GraphQL.Draft.Syntax          as G
import qualified Network.HTTP.Types                     as HTTP

import           Hasura.EncJSON
import           Hasura.GraphQL.Logging
import           Hasura.GraphQL.Transport.HTTP.Protocol
import           Hasura.Prelude
import           Hasura.RQL.Types
import           Hasura.Server.Context
import           Hasura.Server.Utils                    (RequestId)
import           Hasura.Server.Version                  (HasVersion)

import qualified Hasura.GraphQL.Execute                 as E
import qualified Hasura.Logging                         as L
import qualified Hasura.Server.Telemetry.Counters       as Telem

runGQ
  :: ( HasVersion
     , MonadIO m
     , MonadError (QErr code) m
     , AsCodeHasura code
     , MonadReader E.ExecutionCtx m
     )
  => RequestId
  -> UserInfo
  -> [HTTP.Header]
  -> (GQLReqUnparsed, GQLReqParsed)
  -> m (HttpResponse EncJSON)
-- <<<<<<< HEAD
-- runGQ reqId userInfo reqHdrs req@(reqUnparsed, _) = do
--   E.ExecutionCtx _ sqlGenCtx pgExecCtx planCache sc scVer _ enableAL <- ask
--   execPlan <- E.getResolvedExecPlan pgExecCtx planCache userInfo sqlGenCtx enableAL sc scVer req
--   case execPlan of
--     E.GExPHasura resolvedOp ->
--       flip HttpResponse Nothing <$> runHasuraGQ reqId reqUnparsed userInfo resolvedOp
--     E.GExPRemote rsi opDef  ->
--       E.execRemoteGQ reqId userInfo reqHdrs reqUnparsed rsi opDef
-- =======
runGQ reqId userInfo reqHdrs req@(reqUnparsed, _) = do
  -- The response and misc telemetry data:
  let telemTransport = Telem.HTTP
  (telemTimeTot_DT, (telemCacheHit, telemLocality, (telemTimeIO_DT, telemQueryType, !resp))) <- withElapsedTime $ do
    E.ExecutionCtx _ sqlGenCtx pgExecCtx planCache sc scVer httpManager enableAL <- ask
    (telemCacheHit, execPlan) <- E.getResolvedExecPlan pgExecCtx planCache
                                 userInfo sqlGenCtx enableAL sc scVer httpManager reqHdrs req
                -- userInfo sqlGenCtx enableAL sc scVer httpManager reqHdrs req
    case execPlan of
      E.GExPHasura resolvedOp -> do
        (telemTimeIO, telemQueryType, resp) <- runHasuraGQ reqId reqUnparsed userInfo resolvedOp
        return (telemCacheHit, Telem.Local, (telemTimeIO, telemQueryType, HttpResponse resp Nothing))
      E.GExPRemote rsi opDef  -> do
        let telemQueryType | G._todType opDef == G.OperationTypeMutation = Telem.Mutation
                            | otherwise = Telem.Query
        (telemTimeIO, resp) <- E.execRemoteGQ reqId userInfo reqHdrs reqUnparsed rsi opDef
        return (telemCacheHit, Telem.Remote, (telemTimeIO, telemQueryType, resp))
  let telemTimeIO = fromUnits telemTimeIO_DT
      telemTimeTot = fromUnits telemTimeTot_DT
  Telem.recordTimingMetric Telem.RequestDimensions{..} Telem.RequestTimings{..}
  return resp

runGQBatched
  :: ( HasVersion
     , MonadIO m
     , MonadError (QErr code) m
     , AsCodeHasura code
     , Show code
     , MonadReader E.ExecutionCtx m
     )
  => RequestId
  -> UserInfo
  -> [HTTP.Header]
  -> (GQLBatchedReqs GQLQueryText, GQLBatchedReqs GQLExecDoc)
  -> m (HttpResponse EncJSON)
runGQBatched reqId userInfo reqHdrs reqs =
  case reqs of
    (GQLSingleRequest req, GQLSingleRequest reqParsed) ->
      runGQ reqId userInfo reqHdrs (req, reqParsed)
    (GQLBatchedReqs batch, GQLBatchedReqs batchParsed) -> do
      -- It's unclear what we should do if we receive multiple
      -- responses with distinct headers, so just do the simplest thing
      -- in this case, and don't forward any.
      let removeHeaders = flip HttpResponse Nothing
                          . encJFromList
                          . map (either (encJFromJValue . encodeGQErr False) _hrBody)
          try = flip catchError (pure . Left) . fmap Right
      fmap removeHeaders $ traverse (try . runGQ reqId userInfo reqHdrs) $ zip batch batchParsed
    -- TODO: is this correct?
    _ -> throw500 "runGQBatched received different kinds of GQLBatchedReqs"

runHasuraGQ
  :: ( MonadIO m
     , MonadError (QErr code) m
     , AsCodeHasura code
     , MonadReader E.ExecutionCtx m
     )
  => RequestId
  -> GQLReqUnparsed
  -> UserInfo
  -> E.ExecOp code
  -> m (DiffTime, Telem.QueryType, EncJSON)
  -- ^ Also return 'Mutation' when the operation was a mutation, and the time
  -- spent in the PG query; for telemetry.
runHasuraGQ reqId query userInfo resolvedOp = do
  E.ExecutionCtx logger _ pgExecCtx _ _ _ _ _ <- ask
  (telemTimeIO, respE) <- withElapsedTime $ liftIO $ runExceptT $ case resolvedOp of
    E.ExOpQuery tx genSql  -> do
      -- log the generated SQL and the graphql query
      L.unLogger logger $ QueryLog query genSql reqId
      runLazyTx' pgExecCtx tx
    E.ExOpMutation tx -> do
      -- log the graphql query
      L.unLogger logger $ QueryLog query Nothing reqId
      runLazyTx pgExecCtx Q.ReadWrite $ withUserInfo userInfo tx
    E.ExOpSubs _ ->
      throw400 (_UnexpectedPayload # ())
      "subscriptions are not supported over HTTP, use websockets instead"
  resp <- liftEither respE
  let !json = encodeGQResp $ GQSuccess $ encJToLBS resp
      telemQueryType = case resolvedOp of E.ExOpMutation{} -> Telem.Mutation ; _ -> Telem.Query
  return (telemTimeIO, telemQueryType, json)
