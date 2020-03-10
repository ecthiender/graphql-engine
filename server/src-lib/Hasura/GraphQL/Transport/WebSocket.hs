{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE RecordWildCards #-}

module Hasura.GraphQL.Transport.WebSocket
  ( createWSServerApp
  , createWSServerEnv
  , stopWSServerApp
  , WSServerEnv
  ) where

import qualified Control.Concurrent.Async                    as A
import qualified Control.Concurrent.Async.Lifted.Safe        as LA
import qualified Control.Concurrent.STM                      as STM
import qualified Control.Monad.Trans.Control                 as MC
import qualified Data.Aeson                                  as J
import qualified Data.Aeson.Casing                           as J
import qualified Data.Aeson.TH                               as J
import qualified Data.ByteString.Lazy                        as BL
import qualified Data.CaseInsensitive                        as CI
import qualified Data.HashMap.Strict                         as Map
import qualified Data.Text                                   as T
import qualified Data.Text.Encoding                          as TE
import qualified Data.Time.Clock                             as TC
import qualified Database.PG.Query                           as Q
import qualified Language.GraphQL.Draft.Syntax               as G
import qualified Network.HTTP.Client                         as H
import qualified Network.HTTP.Types                          as H
import qualified Network.WebSockets                          as WS
import qualified StmContainers.Map                           as STMMap

import           Control.Concurrent.Extended                 (sleep)
import qualified ListT

import           Hasura.EncJSON
import           Hasura.GraphQL.Logging
import           Hasura.GraphQL.Transport.HTTP.Protocol
import           Hasura.GraphQL.Transport.WebSocket.Protocol
import           Hasura.Prelude
import           Hasura.RQL.Types
import           Hasura.RQL.Types.Error                      (Code (StartFailed))
import           Hasura.Server.Auth                          (AuthMode, UserAuthentication,
                                                              resolveUserInfo)
import           Hasura.Server.Context
import           Hasura.Server.Cors
import           Hasura.Server.Utils                         (IpAddress (..), RequestId,
                                                              getRequestId)
import           Hasura.Server.Version                       (HasVersion)

import qualified Hasura.GraphQL.Execute                      as E
import qualified Hasura.GraphQL.Execute.LiveQuery            as LQ
import qualified Hasura.GraphQL.Execute.LiveQuery.Poll       as LQ
import qualified Hasura.GraphQL.Transport.WebSocket.Server   as WS
import qualified Hasura.Logging                              as L
import qualified Hasura.Server.Telemetry.Counters            as Telem


type OperationMap
  = STMMap.Map OperationId (LQ.LiveQueryId, Maybe OperationName)

newtype WsHeaders
  = WsHeaders { unWsHeaders :: [H.Header] }
  deriving (Show, Eq)

data ErrRespType
  = ERTLegacy
  | ERTGraphqlCompliant
  deriving (Show)

data WsClientState
  = WsClientState
  { wscsUserInfo   :: !UserInfo
  -- ^ the 'UserInfo' required to execute the query and various other things
  , wscsJwtExpTime :: !(Maybe TC.UTCTime)
  -- ^ the JWT expiry time, if any
  , wscsReqHeaders :: ![H.Header]
  -- ^ headers from the client (in conn params) to forward to the remote schema
  , wscsIpAddress  :: !IpAddress
  -- ^ IP address required for 'GQLApiAuthorization'
  }

data WSConnState
  -- headers from the client for websockets
  = CSNotInitialised !WsHeaders !IpAddress
  | CSInitError Text
  -- headers from the client (in conn params) to forward to the remote schema
  -- and JWT expiry time if any
  | CSInitialised !WsClientState

data WSConnData
  = WSConnData
  -- the role and headers are set only on connection_init message
  { _wscUser      :: !(STM.TVar WSConnState)
  -- we only care about subscriptions,
  -- the other operations (query/mutations)
  -- are not tracked here
  , _wscOpMap     :: !OperationMap
  , _wscErrRespTy :: !ErrRespType
  }

type WSServer = WS.WSServer WSConnData

type WSConn = WS.WSConn WSConnData

sendMsg :: (MonadIO m) => WSConn -> ServerMsg -> m ()
sendMsg wsConn msg =
  liftIO $ WS.sendMsg wsConn $ WS.WSQueueResponse (encodeServerMsg msg) Nothing

sendMsgWithMetadata :: (MonadIO m) => WSConn -> ServerMsg -> LQ.LiveQueryMetadata -> m ()
sendMsgWithMetadata wsConn msg (LQ.LiveQueryMetadata execTime) =
  liftIO $ WS.sendMsg wsConn $ WS.WSQueueResponse bs wsInfo
  where
    bs = encodeServerMsg msg
    wsInfo = Just $ WS.WSEventInfo
      { WS._wseiQueryExecutionTime = Just $ realToFrac execTime
      , WS._wseiResponseSize = Just $ BL.length bs
      }

data OpDetail
  = ODStarted
  | ODProtoErr !Text
  | ODQueryErr !(QErr a)
  | ODCompleted
  | ODStopped
  deriving (Show, Eq)
$(J.deriveToJSON
  J.defaultOptions { J.constructorTagModifier = J.snakeCase . drop 2
                   , J.sumEncoding = J.TaggedObject "type" "detail"
                   }
  ''OpDetail)

data OperationDetails
  = OperationDetails
  { _odOperationId   :: !OperationId
  , _odRequestId     :: !(Maybe RequestId)
  , _odOperationName :: !(Maybe OperationName)
  , _odOperationType :: !OpDetail
  , _odQuery         :: !(Maybe GQLReqUnparsed)
  } deriving (Show, Eq)
$(J.deriveToJSON (J.aesonDrop 3 J.snakeCase) ''OperationDetails)

data WSEvent
  = EAccepted
  | ERejected !(QErr a)
  | EConnErr !ConnErrMsg
  | EOperation !OperationDetails
  | EClosed
  deriving (Show, Eq)
$(J.deriveToJSON
  J.defaultOptions { J.constructorTagModifier = J.snakeCase . drop 1
                   , J.sumEncoding = J.TaggedObject "type" "detail"
                   }
  ''WSEvent)

data WsConnInfo
  = WsConnInfo
  { _wsciWebsocketId :: !WS.WSId
  , _wsciJwtExpiry   :: !(Maybe TC.UTCTime)
  , _wsciMsg         :: !(Maybe Text)
  } deriving (Show, Eq)
$(J.deriveToJSON (J.aesonDrop 5 J.snakeCase) ''WsConnInfo)

data WSLogInfo
  = WSLogInfo
  { _wsliUserVars       :: !(Maybe UserVars)
  , _wsliConnectionInfo :: !WsConnInfo
  , _wsliEvent          :: !WSEvent
  } deriving (Show, Eq)
$(J.deriveToJSON (J.aesonDrop 5 J.snakeCase) ''WSLogInfo)

data WSLog
  = WSLog
  { _wslLogLevel :: !L.LogLevel
  , _wslInfo     :: !WSLogInfo
  }

instance L.ToEngineLog WSLog L.Hasura where
  toEngineLog (WSLog logLevel wsLog) =
    (logLevel, L.ELTWebsocketLog, J.toJSON wsLog)

mkWsInfoLog :: Maybe UserVars -> WsConnInfo -> WSEvent -> WSLog
mkWsInfoLog uv ci ev =
  WSLog L.LevelInfo $ WSLogInfo uv ci ev

mkWsErrorLog :: Maybe UserVars -> WsConnInfo -> WSEvent -> WSLog
mkWsErrorLog uv ci ev =
  WSLog L.LevelError $ WSLogInfo uv ci ev

data WSServerEnv
  = WSServerEnv
  { _wseLogger          :: !(L.Logger L.Hasura)
  , _wseRunTx           :: !PGExecCtx
  , _wseLiveQMap        :: !LQ.LiveQueriesState
  , _wseGCtxMap         :: !(IO (SchemaCache, SchemaCacheVer))
  -- ^ an action that always returns the latest version of the schema cache
  , _wseHManager        :: !H.Manager
  , _wseCorsPolicy      :: !CorsPolicy
  , _wseSQLCtx          :: !SQLGenCtx
  , _wseQueryCache      :: !E.PlanCache
  , _wseServer          :: !WSServer
  , _wseEnableAllowlist :: !Bool
  }

onConn :: (MonadIO m)
       => L.Logger L.Hasura -> CorsPolicy -> WS.OnConnH m WSConnData
onConn (L.Logger logger) corsPolicy wsId requestHead ipAddress = do
  res <- runExceptT $ do
    errType <- checkPath
    let reqHdrs = WS.requestHeaders requestHead
    headers <- maybe (return reqHdrs) (flip enforceCors reqHdrs . snd) getOrigin
    return (WsHeaders $ filterWsHeaders headers, errType)
  either reject (uncurry accept) res

  where
    keepAliveAction wsConn = liftIO $ forever $ do
      sendMsg wsConn SMConnKeepAlive
      sleep $ seconds 5

    jwtExpiryHandler wsConn = do
      expTime <- liftIO $ STM.atomically $ do
        connState <- STM.readTVar $ (_wscUser . WS.getData) wsConn
        case connState of
          CSNotInitialised _ _      -> STM.retry
          CSInitError _             -> STM.retry
          CSInitialised clientState -> maybe STM.retry return $ wscsJwtExpTime clientState
      currTime <- TC.getCurrentTime
      sleep $ fromUnits $ TC.diffUTCTime expTime currTime

    accept hdrs errType = do
      logger $ mkWsInfoLog Nothing (WsConnInfo wsId Nothing Nothing) EAccepted
      connData <- liftIO $ WSConnData
                  <$> STM.newTVarIO (CSNotInitialised hdrs ipAddress)
                  <*> STMMap.newIO
                  <*> pure errType
      let acceptRequest = WS.defaultAcceptRequest
                          { WS.acceptSubprotocol = Just "graphql-ws"}
      return $ Right $ WS.AcceptWith connData acceptRequest
                       (Just keepAliveAction) (Just jwtExpiryHandler)

    reject qErr = do
      logger $ mkWsErrorLog Nothing (WsConnInfo wsId Nothing Nothing) (ERejected qErr)
      return $ Left $ WS.RejectRequest
        (H.statusCode $ qeStatus qErr)
        (H.statusMessage $ qeStatus qErr) []
        (BL.toStrict $ J.encode $ encodeGQLErr False qErr)

    checkPath = case WS.requestPath requestHead of
      "/v1alpha1/graphql" -> return ERTLegacy
      "/v1/graphql"       -> return ERTGraphqlCompliant
      _                   ->
        throw404 "only '/v1/graphql', '/v1alpha1/graphql' are supported on websockets"

    getOrigin =
      find ((==) "Origin" . fst) (WS.requestHeaders requestHead)

    enforceCors origin reqHdrs = case cpConfig corsPolicy of
      CCAllowAll -> return reqHdrs
      CCDisabled readCookie ->
        if readCookie
        then return reqHdrs
        else do
          lift $ logger $ mkWsInfoLog Nothing (WsConnInfo wsId Nothing (Just corsNote)) EAccepted
          return $ filter (\h -> fst h /= "Cookie") reqHdrs
      CCAllowedOrigins ds
        -- if the origin is in our cors domains, no error
        | bsToTxt origin `elem` dmFqdns ds   -> return reqHdrs
        -- if current origin is part of wildcard domain list, no error
        | inWildcardList ds (bsToTxt origin) -> return reqHdrs
        -- otherwise error
        | otherwise                          -> corsErr

    filterWsHeaders hdrs = flip filter hdrs $ \(n, _) ->
      n `notElem` [ "sec-websocket-key"
                  , "sec-websocket-version"
                  , "upgrade"
                  , "connection"
                  ]

    corsErr = throw400 AccessDenied
              "received origin header does not match configured CORS domains"

    corsNote = "Cookie is not read when CORS is disabled, because it is a potential "
            <> "security issue. If you're already handling CORS before Hasura and enforcing "
            <> "CORS on websocket connections, then you can use the flag --ws-read-cookie or "
            <> "HASURA_GRAPHQL_WS_READ_COOKIE to force read cookie when CORS is disabled."


onStart :: forall m. (HasVersion, MonadIO m, E.GQLApiAuthorization m)
        => WSServerEnv -> WSConn -> StartMsg -> m ()
onStart serverEnv wsConn (StartMsg opId q) = catchAndIgnore $ do
  timerTot <- startTimer
  opM <- liftIO $ STM.atomically $ STMMap.lookup opId opMap

  when (isJust opM) $ withComplete $ sendStartErr $
    "an operation already exists with this id: " <> unOperationId opId

  userInfoM <- liftIO $ STM.readTVarIO userInfoR
  (userInfo, reqHdrs, ipAddress) <- case userInfoM of
    CSInitialised WsClientState{..} -> return (wscsUserInfo, wscsReqHeaders, wscsIpAddress)
    CSInitError initErr -> do
      let e = "cannot start as connection_init failed with : " <> initErr
      withComplete $ sendStartErr e
    CSNotInitialised _ _ -> do
      let e = "start received before the connection is initialised"
      withComplete $ sendStartErr e

  requestId <- getRequestId reqHdrs
  reqParsedE <- lift $ E.authorizeGQLApi userInfo (reqHdrs, ipAddress) q
  reqParsed <- either (withComplete . preExecErr requestId) return reqParsedE
  -- (sc, scVer) <- liftIO $ IORef.readIORef gCtxMapRef
  (sc, scVer) <- liftIO getSchemaCache
  execPlanE <- runExceptT $ E.getResolvedExecPlan pgExecCtx
               planCache userInfo sqlGenCtx enableAL sc scVer httpMgr reqHdrs (q, reqParsed)
               -- planCache userInfo sqlGenCtx enableAL sc scVer httpMgr reqHdrs q

  (telemCacheHit, execPlan) <- either (withComplete . preExecErr requestId) return execPlanE
  let execCtx = E.ExecutionCtx logger sqlGenCtx pgExecCtx planCache sc scVer httpMgr enableAL

  case execPlan of
    E.GExPHasura resolvedOp ->
      runHasuraGQ timerTot telemCacheHit requestId q userInfo resolvedOp
    E.GExPRemote rsi opDef  ->
      runRemoteGQ timerTot telemCacheHit execCtx requestId userInfo reqHdrs opDef rsi
  where
-- <<<<<<< HEAD
--     runHasuraGQ :: RequestId -> GQLReqUnparsed -> UserInfo -> E.ExecOp
--                 -> ExceptT () m ()
--     runHasuraGQ reqId query userInfo = \case
-- =======
    telemTransport = Telem.HTTP
    runHasuraGQ :: ExceptT () m DiffTime
                -> Telem.CacheHit -> RequestId -> GQLReqUnparsed -> UserInfo -> E.ExecOp
                -> ExceptT () m ()
    runHasuraGQ timerTot telemCacheHit reqId query userInfo = \case
      E.ExOpQuery opTx genSql ->
        execQueryOrMut Telem.Query genSql $ runLazyTx' pgExecCtx opTx
      E.ExOpMutation opTx ->
        execQueryOrMut Telem.Mutation Nothing $
          runLazyTx pgExecCtx Q.ReadWrite $ withUserInfo userInfo opTx
      E.ExOpSubs lqOp -> do
        -- log the graphql query
        L.unLogger logger $ QueryLog query Nothing reqId
        lqId <- liftIO $ LQ.addLiveQuery lqMap lqOp liveQOnChange
        liftIO $ STM.atomically $
          STMMap.insert (lqId, _grOperationName q) opId opMap
        logOpEv ODStarted (Just reqId)

-- <<<<<<< HEAD
    -- execQueryOrMut reqId query genSql action = do
    --   logOpEv ODStarted (Just reqId)
    --   -- log the generated SQL and the graphql query
    --   L.unLogger logger $ QueryLog query genSql reqId
    --   (dt, resp) <- withElapsedTime $ liftIO $ runExceptT action
    --   let lqMeta = LQ.LiveQueryMetadata dt
    --   either (postExecErr reqId) (`sendSuccResp` lqMeta) resp
    --   sendCompleted (Just reqId)

      where
        telemLocality = Telem.Local
        execQueryOrMut telemQueryType genSql action = do
          logOpEv ODStarted (Just reqId)
          -- log the generated SQL and the graphql query
          L.unLogger logger $ QueryLog query genSql reqId
          (withElapsedTime $ liftIO $ runExceptT action) >>= \case
            (_,      Left err) -> postExecErr reqId err
            (telemTimeIO_DT, Right encJson) -> do
              -- Telemetry. NOTE: don't time network IO:
              telemTimeTot <- Seconds <$> timerTot
              sendSuccResp encJson $ LQ.LiveQueryMetadata telemTimeIO_DT
              let telemTimeIO = fromUnits telemTimeIO_DT
              Telem.recordTimingMetric Telem.RequestDimensions{..} Telem.RequestTimings{..}

          sendCompleted (Just reqId)

    -- runRemoteGQ
    --   :: E.ExecutionCtx -> RequestId -> UserInfo -> [H.Header]
    --   -> G.TypedOperationDefinition -> RemoteSchemaInfo
    --   -> ExceptT () m ()
    -- runRemoteGQ execCtx reqId userInfo reqHdrs opDef rsi = do
    --   when (G._todType opDef == G.OperationTypeSubscription) $
    --     withComplete $ preExecErr reqId $
    --     err400 NotSupported "subscription to remote server is not supported"

    runRemoteGQ :: ExceptT () m DiffTime
                -> Telem.CacheHit -> E.ExecutionCtx -> RequestId -> UserInfo -> [H.Header]
                -> G.TypedOperationDefinition -> RemoteSchemaInfo
                -> ExceptT () m ()
    runRemoteGQ timerTot telemCacheHit execCtx reqId userInfo reqHdrs opDef rsi = do
      let telemLocality = Telem.Remote
      telemQueryType <- case G._todType opDef of
        G.OperationTypeSubscription ->
          withComplete $ preExecErr reqId $
          err400 NotSupported "subscription to remote server is not supported"
        G.OperationTypeMutation -> return Telem.Mutation
        G.OperationTypeQuery    -> return Telem.Query

      -- if it's not a subscription, use HTTP to execute the query on the remote
      (runExceptT $ flip runReaderT execCtx $
        E.execRemoteGQ reqId userInfo reqHdrs q rsi opDef) >>= \case
          Left  err           -> postExecErr reqId err
          Right (telemTimeIO_DT, !val) -> do
            -- Telemetry. NOTE: don't time network IO:
            telemTimeTot <- Seconds <$> timerTot
            sendRemoteResp reqId (_hrBody val) $ LQ.LiveQueryMetadata telemTimeIO_DT
            let telemTimeIO = fromUnits telemTimeIO_DT
            Telem.recordTimingMetric Telem.RequestDimensions{..} Telem.RequestTimings{..}

      sendCompleted (Just reqId)

    sendRemoteResp reqId resp meta =
      case J.eitherDecodeStrict (encJToBS resp) of
        Left e    -> postExecErr reqId $ invalidGqlErr $ T.pack e
        Right res -> sendMsgWithMetadata wsConn (SMData $ DataMsg opId $ GRRemote res) meta

    invalidGqlErr err = err500 Unexpected $
      "Failed parsing GraphQL response from remote: " <> err

    WSServerEnv logger pgExecCtx lqMap getSchemaCache httpMgr _ sqlGenCtx planCache
      _ enableAL = serverEnv

    WSConnData userInfoR opMap errRespTy = WS.getData wsConn

    logOpEv opTy reqId =
      logWSEvent logger wsConn $ EOperation opDet
      where
        opDet = OperationDetails opId reqId (_grOperationName q) opTy query
        -- log the query only in errors
        query = case opTy of
          ODQueryErr _ -> Just q
          _            -> Nothing

    getErrFn errTy =
      case errTy of
        ERTLegacy           -> encodeQErr
        ERTGraphqlCompliant -> encodeGQLErr

    sendStartErr e = do
      let errFn = getErrFn errRespTy
      sendMsg wsConn $
        SMErr $ ErrorMsg opId $ errFn False $ err400 StartFailed e
      logOpEv (ODProtoErr e) Nothing

    sendCompleted reqId = do
      liftIO $ sendMsg wsConn $ SMComplete $ CompletionMsg opId
      logOpEv ODCompleted reqId

    postExecErr reqId qErr = do
      let errFn = getErrFn errRespTy
      logOpEv (ODQueryErr qErr) (Just reqId)
      sendMsg wsConn $ SMData $
        DataMsg opId $ GRHasura $ GQExecError $ pure $ errFn False qErr

    -- why wouldn't pre exec error use graphql response?
    preExecErr reqId qErr = do
      let errFn = getErrFn errRespTy
      logOpEv (ODQueryErr qErr) (Just reqId)
      let err = case errRespTy of
            ERTLegacy           -> errFn False qErr
            ERTGraphqlCompliant -> J.object ["errors" J..= [errFn False qErr]]
      sendMsg wsConn (SMErr $ ErrorMsg opId err)

    sendSuccResp encJson =
      sendMsgWithMetadata wsConn
        (SMData $ DataMsg opId $ GRHasura $ GQSuccess $ encJToLBS encJson)

    withComplete :: ExceptT () m () -> ExceptT () m a
    withComplete action = do
      action
      sendCompleted Nothing
      throwError ()

    -- on change, send message on the websocket
    liveQOnChange :: LQ.OnChange
    liveQOnChange (GQSuccess (LQ.LiveQueryResponse bs dTime)) =
      sendMsgWithMetadata wsConn (SMData $ DataMsg opId $ GRHasura $ GQSuccess bs) $
        LQ.LiveQueryMetadata dTime
    liveQOnChange resp = sendMsg wsConn $ SMData $ DataMsg opId $ GRHasura $
      LQ._lqrPayload <$> resp

    catchAndIgnore :: ExceptT () m () -> m ()
    catchAndIgnore m = void $ runExceptT m

onMessage
  :: (HasVersion, MonadIO m, UserAuthentication m, E.GQLApiAuthorization m)
  => AuthMode
  -> WSServerEnv
  -> WSConn -> BL.ByteString -> m ()
onMessage authMode serverEnv wsConn msgRaw =
  case J.eitherDecode msgRaw of
    Left e    -> do
      let err = ConnErrMsg $ "parsing ClientMessage failed: " <> T.pack e
      logWSEvent logger wsConn $ EConnErr err
      sendMsg wsConn $ SMConnErr err

    Right msg -> case msg of
      CMConnInit params -> onConnInit (_wseLogger serverEnv)
                           (_wseHManager serverEnv)
                           wsConn authMode params
      CMStart startMsg  -> onStart serverEnv wsConn startMsg
      CMStop stopMsg    -> liftIO $ onStop serverEnv wsConn stopMsg
      CMConnTerm        -> liftIO $ WS.closeConn wsConn "GQL_CONNECTION_TERMINATE received"
  where
    logger = _wseLogger serverEnv

onStop :: WSServerEnv -> WSConn -> StopMsg -> IO ()
onStop serverEnv wsConn (StopMsg opId) = do
  -- probably wrap the whole thing in a single tx?
  opM <- liftIO $ STM.atomically $ STMMap.lookup opId opMap
  case opM of
    Just (lqId, opNameM) -> do
      logWSEvent logger wsConn $ EOperation $ opDet opNameM
      LQ.removeLiveQuery lqMap lqId
    Nothing    -> return ()
  STM.atomically $ STMMap.delete opId opMap
  where
    logger = _wseLogger serverEnv
    lqMap  = _wseLiveQMap serverEnv
    opMap  = _wscOpMap $ WS.getData wsConn
    opDet n = OperationDetails opId Nothing n ODStopped Nothing

logWSEvent
  :: (MonadIO m)
  => L.Logger L.Hasura -> WSConn -> WSEvent -> m ()
logWSEvent (L.Logger logger) wsConn wsEv = do
  userInfoME <- liftIO $ STM.readTVarIO userInfoR
  let (userVarsM, jwtExpM) = case userInfoME of
        CSInitialised WsClientState{..} -> ( Just $ userVars wscsUserInfo
                                           , wscsJwtExpTime
                                           )
        _                               -> (Nothing, Nothing)
  liftIO $ logger $ WSLog logLevel $ WSLogInfo userVarsM (WsConnInfo wsId jwtExpM Nothing) wsEv
  where
    WSConnData userInfoR _ _ = WS.getData wsConn
    wsId = WS.getWSId wsConn
    logLevel = bool L.LevelInfo L.LevelError isError
    isError = case wsEv of
      EAccepted   -> False
      ERejected _ -> True
      EConnErr _  -> True
      EClosed     -> False
      EOperation op -> case _odOperationType op of
        ODStarted    -> False
        ODProtoErr _ -> True
        ODQueryErr _ -> True
        ODCompleted  -> False
        ODStopped    -> False

onConnInit
  :: (HasVersion, MonadIO m, UserAuthentication m)
  => L.Logger L.Hasura -> H.Manager -> WSConn -> AuthMode -> Maybe ConnParams -> m ()
onConnInit logger manager wsConn authMode connParamsM = do
  connState <- liftIO (STM.readTVarIO (_wscUser $ WS.getData wsConn))
  case getIpAddress connState of
    Left err -> unexpectedInitError err
    Right ipAddress -> do
      let headers = mkHeaders connState
      res <- resolveUserInfo logger manager headers authMode
      case res of
        Left e  -> do
          liftIO $ STM.atomically $ STM.writeTVar (_wscUser $ WS.getData wsConn) $
            CSInitError $ qeError e
          let connErr = ConnErrMsg $ qeError e
          logWSEvent logger wsConn $ EConnErr connErr
          sendMsg wsConn $ SMConnErr connErr
        Right (userInfo, expTimeM) -> do
          liftIO $ STM.atomically $ STM.writeTVar (_wscUser $ WS.getData wsConn) $
            CSInitialised $ WsClientState userInfo expTimeM paramHeaders ipAddress
          sendMsg wsConn SMConnAck
          -- TODO: send it periodically? Why doesn't apollo's protocol use
          -- ping/pong frames of websocket spec?
          sendMsg wsConn SMConnKeepAlive
  where
    unexpectedInitError e = do
      let connErr = ConnErrMsg e
      logWSEvent logger wsConn $ EConnErr connErr
      sendMsg wsConn $ SMConnErr connErr

    mkHeaders connState =
      paramHeaders ++ getClientHeaders connState

    paramHeaders =
      [ (CI.mk $ TE.encodeUtf8 h, TE.encodeUtf8 v)
      | (h, v) <- maybe [] Map.toList $ connParamsM >>= _cpHeaders
      ]

    getClientHeaders = \case
      CSNotInitialised hdrs _ -> unWsHeaders hdrs
      _                       -> []

    getIpAddress = \case
      CSNotInitialised _ ip -> return ip
      CSInitialised WsClientState{..} -> return wscsIpAddress
      CSInitError e -> Left e

onClose
  :: MonadIO m
  => L.Logger L.Hasura
  -> LQ.LiveQueriesState
  -> WSConn
  -> m ()
onClose logger lqMap wsConn = do
  logWSEvent logger wsConn EClosed
  operations <- liftIO $ STM.atomically $ ListT.toList $ STMMap.listT opMap
  void $ liftIO $ A.forConcurrently operations $ \(_, (lqId, _)) ->
    LQ.removeLiveQuery lqMap lqId
  where
    opMap = _wscOpMap $ WS.getData wsConn

createWSServerEnv
  :: (MonadIO m)
  => L.Logger L.Hasura
  -> PGExecCtx
  -> LQ.LiveQueriesState
  -> IO (SchemaCache, SchemaCacheVer)
  -> H.Manager
  -> CorsPolicy
  -> SQLGenCtx
  -> Bool
  -> E.PlanCache
  -> m WSServerEnv
createWSServerEnv logger pgExecCtx lqState getSchemaCache httpManager
  corsPolicy sqlGenCtx enableAL planCache = do
  wsServer <- liftIO $ STM.atomically $ WS.createWSServer logger
  return $
    WSServerEnv logger pgExecCtx lqState getSchemaCache httpManager corsPolicy
    sqlGenCtx planCache wsServer enableAL

createWSServerApp
  :: ( HasVersion
     , MonadIO m
     , MC.MonadBaseControl IO m
     , LA.Forall (LA.Pure m)
     , UserAuthentication m
     , E.GQLApiAuthorization m
     )
  => AuthMode
  -> WSServerEnv
  -> WS.HasuraServerApp m
createWSServerApp authMode serverEnv =
  WS.createServerApp (_wseServer serverEnv) handlers
  where
    handlers =
      WS.WSHandlers
      (onConn (_wseLogger serverEnv) (_wseCorsPolicy serverEnv))
      (onMessage authMode serverEnv)
      (onClose (_wseLogger serverEnv) $ _wseLiveQMap serverEnv)

stopWSServerApp :: WSServerEnv -> IO ()
stopWSServerApp wsEnv = WS.shutdown (_wseServer wsEnv)
