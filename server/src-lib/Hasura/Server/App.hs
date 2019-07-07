{-# LANGUAGE CPP        #-}
{-# LANGUAGE DataKinds  #-}
{-# LANGUAGE RankNTypes #-}

module Hasura.Server.App where

import           Control.Arrow                          ((***))
import           Control.Concurrent.MVar
import           Control.Exception                      (IOException, try)
import           Data.Aeson                             hiding (json)
import           Data.IORef
import           Data.Time.Clock                        (UTCTime,
                                                         getCurrentTime)
import           Network.Mime                           (defaultMimeLookup)
import           Network.Wai                            (requestHeaders,
                                                         strictRequestBody)
import           System.Exit                            (exitFailure)

import           System.FilePath                        (joinPath, takeFileName)
import           Web.Spock.Core

import qualified Data.ByteString.Lazy                   as BL
import qualified Data.HashMap.Strict                    as M
import qualified Data.HashSet                           as S
import qualified Data.Text                              as T
import qualified Network.HTTP.Client                    as HTTP
import qualified Network.HTTP.Types                     as N
import qualified Network.Wai                            as Wai
import qualified Network.Wai.Handler.WebSockets         as WS
import qualified Network.WebSockets                     as WS
import qualified Text.Mustache                          as M
import qualified Text.Mustache.Compile                  as M

import qualified Database.PG.Query                      as Q
import qualified Hasura.GraphQL.Execute                 as E
import qualified Hasura.GraphQL.Execute.LiveQuery       as EL
import qualified Hasura.GraphQL.Explain                 as GE
import qualified Hasura.GraphQL.Schema                  as GS
import qualified Hasura.GraphQL.Transport.HTTP          as GH
import qualified Hasura.GraphQL.Transport.HTTP.Protocol as GH
import qualified Hasura.GraphQL.Transport.WebSocket     as WS
import qualified Hasura.Logging                         as L

import           Hasura.EncJSON
import           Hasura.Prelude                         hiding (get, put)
import           Hasura.RQL.DDL.RemoteSchema
import           Hasura.RQL.DDL.Schema.Table
import           Hasura.RQL.DML.QueryTemplate
import           Hasura.RQL.Types
import           Hasura.Server.Auth                     (AuthMode (..),
                                                         getUserInfo)
import           Hasura.Server.Config                   (runGetConfig)
import           Hasura.Server.Context
import           Hasura.Server.Cors
import           Hasura.Server.Init
import           Hasura.Server.Logging
import           Hasura.Server.Middleware               (corsMiddleware)
import qualified Hasura.Server.PGDump                   as PGD
import           Hasura.Server.Query
import           Hasura.Server.Utils
import           Hasura.Server.Version
import           Hasura.SQL.Types

consoleTmplt :: M.Template
consoleTmplt = $(M.embedSingleTemplate "src-rsr/console.html")

boolToText :: Bool -> T.Text
boolToText = bool "false" "true"

isAdminSecretSet :: AuthMode -> T.Text
isAdminSecretSet AMNoAuth = boolToText False
isAdminSecretSet _        = boolToText True

data SchemaCacheRef
  = SchemaCacheRef
  { _scrLock     :: MVar ()
  , _scrCache    :: IORef (SchemaCache, SchemaCacheVer)
  -- an action to run when schemacache changes
  , _scrOnChange :: IO ()
  }

getSCFromRef :: SchemaCacheRef -> IO SchemaCache
getSCFromRef scRef = fst <$> readIORef (_scrCache scRef)

logInconsObjs :: L.Logger -> [InconsistentMetadataObj] -> IO ()
logInconsObjs logger objs =
  unless (null objs) $ L.unLogger logger $ mkInconsMetadataLog objs

withSCUpdate
  :: (MonadIO m, MonadError e m)
  => SchemaCacheRef -> L.Logger -> m (a, SchemaCache) -> m a
withSCUpdate scr logger action = do
  acquireLock
  (res, newSC) <- action `catchError` onError
  liftIO $ do
    -- update schemacache in IO reference
    modifyIORef' cacheRef $
      \(_, prevVer) -> (newSC, incSchemaCacheVer prevVer)
    -- log any inconsistent objects
    logInconsObjs logger $ scInconsistentObjs newSC
    onChange
  releaseLock
  return res
  where
    SchemaCacheRef lk cacheRef onChange = scr
    onError e   = releaseLock >> throwError e
    acquireLock = liftIO $ takeMVar lk
    releaseLock = liftIO $ putMVar lk ()

data ServerCtx
  = ServerCtx
  { scPGExecCtx       :: !PGExecCtx
  , scConnInfo        :: !Q.ConnInfo
  , scLogger          :: !L.Logger
  , scCacheRef        :: !SchemaCacheRef
  , scAuthMode        :: !AuthMode
  , scManager         :: !HTTP.Manager
  , scSQLGenCtx       :: !SQLGenCtx
  , scEnabledAPIs     :: !(S.HashSet API)
  , scInstanceId      :: !InstanceId
  , scPlanCache       :: !E.PlanCache
  , scLQState         :: !EL.LiveQueriesState
  , scEnableAllowlist :: !Bool
  }

data HandlerCtx
  = HandlerCtx
  { hcServerCtx  :: !ServerCtx
  , hcReqBody    :: !BL.ByteString
  , hcUser       :: !UserInfo
  , hcReqHeaders :: ![N.Header]
  }

type Handler = ExceptT QErr (ReaderT HandlerCtx IO)

data APIResp
  = JSONResp !(HttpResponse EncJSON)
  | RawResp !(HttpResponse BL.ByteString) -- headers, body

apiRespToLBS :: APIResp -> BL.ByteString
apiRespToLBS = \case
  JSONResp (HttpResponse j _) -> encJToLBS j
  RawResp (HttpResponse b _)  -> b

mkAPIRespHandler :: Handler (HttpResponse EncJSON) -> Handler APIResp
mkAPIRespHandler = fmap JSONResp

isMetadataEnabled :: ServerCtx -> Bool
isMetadataEnabled sc = S.member METADATA $ scEnabledAPIs sc

isGraphQLEnabled :: ServerCtx -> Bool
isGraphQLEnabled sc = S.member GRAPHQL $ scEnabledAPIs sc

isPGDumpEnabled :: ServerCtx -> Bool
isPGDumpEnabled sc = S.member PGDUMP $ scEnabledAPIs sc

isConfigEnabled :: ServerCtx -> Bool
isConfigEnabled sc = S.member CONFIG $ scEnabledAPIs sc

isDeveloperAPIEnabled :: ServerCtx -> Bool
isDeveloperAPIEnabled sc = S.member DEVELOPER $ scEnabledAPIs sc

-- {-# SCC parseBody #-}
parseBody :: (FromJSON a) => Handler a
parseBody = do
  reqBody <- hcReqBody <$> ask
  case decode' reqBody of
    Just jVal -> decodeValue jVal
    Nothing   -> throw400 InvalidJSON "invalid json"

onlyAdmin :: Handler ()
onlyAdmin = do
  uRole <- asks (userRole . hcUser)
  when (uRole /= adminRole) $
    throw400 AccessDenied "You have to be an admin to access this endpoint"

buildQCtx ::  Handler QCtx
buildQCtx = do
  scRef    <- scCacheRef . hcServerCtx <$> ask
  userInfo <- asks hcUser
  cache <- fmap fst $ liftIO $ readIORef $ _scrCache scRef
  sqlGenCtx <- scSQLGenCtx . hcServerCtx <$> ask
  return $ QCtx userInfo cache sqlGenCtx

logResult
  :: (MonadIO m)
  => Maybe UserInfo -> Wai.Request -> BL.ByteString -> L.Logger
  -> Either QErr BL.ByteString -> Maybe (UTCTime, UTCTime)
  -> m ()
logResult userInfoM req reqBody logger res qTime =
  liftIO $ (L.unLogger logger) $ mkAccessLog userInfoM req (reqBody, res) qTime

logError
  :: MonadIO m
  => Maybe UserInfo -> Wai.Request
  -> BL.ByteString -> L.Logger -> QErr -> m ()
logError userInfoM req reqBody logger qErr =
  logResult userInfoM req reqBody logger (Left qErr) Nothing


mkSpockAction
  :: (MonadIO m)
  => (Bool -> QErr -> Value)
  -> (QErr -> QErr)
  -> ServerCtx
  -> Handler APIResp
  -> ActionT m ()
mkSpockAction qErrEncoder qErrModifier serverCtx handler = do
  req <- request
  reqBody <- liftIO $ strictRequestBody req
  let headers  = requestHeaders req
      authMode = scAuthMode serverCtx
      manager = scManager serverCtx

  userInfoE <- liftIO $ runExceptT $ getUserInfo logger manager headers authMode
  userInfo <- either (logAndThrow req reqBody False . qErrModifier) return userInfoE

  let handlerState = HandlerCtx serverCtx reqBody userInfo headers

  t1 <- liftIO getCurrentTime -- for measuring response time purposes
  result <- liftIO $ runReaderT (runExceptT handler) handlerState
  t2 <- liftIO getCurrentTime -- for measuring response time purposes

  -- apply the error modifier
  let modResult = fmapL qErrModifier result

  -- log result
  logResult (Just userInfo) req reqBody logger (apiRespToLBS <$> modResult) $
    Just (t1, t2)
  either (qErrToResp $ userRole userInfo == adminRole) resToResp modResult

  where
    logger = scLogger serverCtx
    -- encode error response
    qErrToResp :: (MonadIO m) => Bool -> QErr -> ActionCtxT ctx m b
    qErrToResp includeInternal qErr = do
      setStatus $ qeStatus qErr
      json $ qErrEncoder includeInternal qErr

    logAndThrow req reqBody includeInternal qErr = do
      logError Nothing req reqBody logger qErr
      qErrToResp includeInternal qErr

    resToResp = \case
      JSONResp (HttpResponse j h) -> do
        uncurry setHeader jsonHeader
        mapM_ (mapM_ (uncurry setHeader . unHeader)) h
        lazyBytes $ encJToLBS j
      RawResp (HttpResponse b h) -> do
        mapM_ (mapM_ (uncurry setHeader . unHeader)) h
        lazyBytes b

v1QueryHandler :: RQLQuery -> Handler (HttpResponse EncJSON)
v1QueryHandler query = do
  scRef <- scCacheRef . hcServerCtx <$> ask
  logger <- scLogger . hcServerCtx <$> ask
  res <- bool (fst <$> dbAction) (withSCUpdate scRef logger dbActionReload) $
         queryNeedsReload query
  return $ HttpResponse res Nothing
  where
    -- Hit postgres
    dbAction = do
      userInfo <- asks hcUser
      scRef <- scCacheRef . hcServerCtx <$> ask
      schemaCache <- fmap fst $ liftIO $ readIORef $ _scrCache scRef
      httpMgr <- scManager . hcServerCtx <$> ask
      sqlGenCtx <- scSQLGenCtx . hcServerCtx <$> ask
      pgExecCtx <- scPGExecCtx . hcServerCtx <$> ask
      instanceId <- scInstanceId . hcServerCtx <$> ask
      runQuery pgExecCtx instanceId userInfo schemaCache httpMgr sqlGenCtx query

    -- Also update the schema cache
    dbActionReload = do
      (resp, newSc) <- dbAction
      httpMgr <- scManager . hcServerCtx <$> ask
      --FIXME: should we be fetching the remote schema again? if not how do we get the remote schema?
      newSc' <- GS.updateSCWithGCtx newSc >>= flip resolveRemoteSchemas httpMgr
      return (resp, newSc')

v1Alpha1GQHandler :: GH.GQLReqUnparsed -> Handler (HttpResponse EncJSON)
v1Alpha1GQHandler query = do
  userInfo <- asks hcUser
  reqBody <- asks hcReqBody
  reqHeaders <- asks hcReqHeaders
  manager <- scManager . hcServerCtx <$> ask
  scRef <- scCacheRef . hcServerCtx <$> ask
  (sc, scVer) <- liftIO $ readIORef $ _scrCache scRef
  pgExecCtx <- scPGExecCtx . hcServerCtx <$> ask
  sqlGenCtx <- scSQLGenCtx . hcServerCtx <$> ask
  planCache <- scPlanCache . hcServerCtx <$> ask
  enableAL <- scEnableAllowlist . hcServerCtx <$> ask
  GH.runGQ pgExecCtx userInfo sqlGenCtx enableAL planCache
    sc scVer manager reqHeaders query reqBody

v1GQHandler :: GH.GQLReqUnparsed -> Handler (HttpResponse EncJSON)
v1GQHandler = v1Alpha1GQHandler

gqlExplainHandler :: GE.GQLExplain -> Handler (HttpResponse EncJSON)
gqlExplainHandler query = do
  onlyAdmin
  scRef <- scCacheRef . hcServerCtx <$> ask
  sc <- fmap fst $ liftIO $ readIORef $ _scrCache scRef
  pgExecCtx <- scPGExecCtx . hcServerCtx <$> ask
  sqlGenCtx <- scSQLGenCtx . hcServerCtx <$> ask
  enableAL <- scEnableAllowlist . hcServerCtx <$> ask
  res <- GE.explainGQLQuery pgExecCtx sc sqlGenCtx enableAL query
  return $ HttpResponse res Nothing

v1Alpha1PGDumpHandler :: PGD.PGDumpReqBody -> Handler APIResp
v1Alpha1PGDumpHandler b = do
  onlyAdmin
  ci <- scConnInfo . hcServerCtx <$> ask
  output <- PGD.execPGDump b ci
  return $ RawResp $ HttpResponse output (Just [Header sqlHeader])

consoleAssetsHandler :: L.Logger -> Text -> FilePath -> ActionT IO ()
consoleAssetsHandler logger dir path = do
  -- '..' in paths need not be handed as it is resolved in the url by
  -- spock's routing. we get the expanded path.
  eFileContents <- liftIO $ try $ BL.readFile $
    joinPath [T.unpack dir, path]
  either onError onSuccess eFileContents
  where
    onSuccess c = do
      mapM_ (uncurry setHeader) headers
      lazyBytes c
    onError :: IOException -> ActionT IO ()
    onError = raiseGenericApiError logger . err404 NotFound . T.pack . show
    fn = T.pack $ takeFileName path
    -- set gzip header if the filename ends with .gz
    (fileName, encHeader) = case T.stripSuffix ".gz" fn of
      Just v  -> (v, [gzipHeader])
      Nothing -> (fn, [])
    mimeType = bsToTxt $ defaultMimeLookup fileName
    headers = ("Content-Type", mimeType) : encHeader

mkConsoleHTML :: T.Text -> AuthMode -> Bool -> Maybe Text -> Either String T.Text
mkConsoleHTML path authMode enableTelemetry consoleAssetsDir = do
  bool (Left errMsg) (Right res) $ null errs
  where
    (errs, res) = M.checkedSubstitute consoleTmplt $
      -- variables required to render the template
      object [ "isAdminSecretSet" .= isAdminSecretSet authMode
             , "consolePath" .= consolePath
             , "enableTelemetry" .= boolToText enableTelemetry
             , "cdnAssets" .= boolToText (isNothing consoleAssetsDir)
             , "assetsVersion" .= consoleVersion
             , "serverVersion" .= currentVersion
             ]
    consolePath = case path of
      "" -> "/console"
      r  -> "/console/" <> r
    errMsg = "console template rendering failed: " ++ show errs

newtype QueryParser
  = QueryParser { getQueryParser :: QualifiedTable -> Handler RQLQuery }

queryParsers :: M.HashMap T.Text QueryParser
queryParsers =
  M.fromList
  [ ("select", mkQueryParser RQSelect)
  , ("insert", mkQueryParser RQInsert)
  , ("update", mkQueryParser RQUpdate)
  , ("delete", mkQueryParser RQDelete)
  , ("count", mkQueryParser RQCount)
  ]
  where
    mkQueryParser f =
      QueryParser $ \qt -> do
      obj <- parseBody
      let val = Object $ M.insert "table" (toJSON qt) obj
      q <- decodeValue val
      return $ f q

legacyQueryHandler :: TableName -> T.Text -> Handler (HttpResponse EncJSON)
legacyQueryHandler tn queryType =
  case M.lookup queryType queryParsers of
    Just queryParser -> getQueryParser queryParser qt >>= v1QueryHandler
    Nothing          -> throw404 "No such resource exists"
  where
    qt = QualifiedObject publicSchema tn

initErrExit :: QErr -> IO a
initErrExit e = do
  putStrLn $
    "failed to build schema-cache because of inconsistent metadata: "
    <> T.unpack (qeError e)
  exitFailure

mkWaiApp
  :: Q.TxIsolation
  -> L.LoggerCtx
  -> SQLGenCtx
  -> Bool
  -> Q.PGPool
  -> Q.ConnInfo
  -> HTTP.Manager
  -> AuthMode
  -> CorsConfig
  -> Bool
  -> Maybe Text
  -> Bool
  -> InstanceId
  -> S.HashSet API
  -> EL.LQOpts
  -> Maybe (RQLQuery -> Handler ())
  -> IO (Wai.Application, SchemaCacheRef, Maybe UTCTime)
mkWaiApp isoLevel loggerCtx sqlGenCtx enableAL pool ci httpManager mode corsCfg
         enableConsole consoleAssetsDir enableTelemetry instanceId apis
         lqOpts extraMiddleware = do
    let pgExecCtx = PGExecCtx pool isoLevel
        pgExecCtxSer = PGExecCtx pool Q.Serializable
    (cacheRef, cacheBuiltTime) <- do
      pgResp <- runExceptT $ peelRun emptySchemaCache adminUserInfo
                httpManager sqlGenCtx pgExecCtxSer $ do
                  buildSchemaCache
                  liftTx fetchLastUpdate
      (time, sc) <- either initErrExit return pgResp
      scRef <- newIORef (sc, initSchemaCacheVer)
      return (scRef, snd <$> time)

    cacheLock <- newMVar ()
    planCache <- E.initPlanCache

    let corsPolicy = mkDefaultCorsPolicy corsCfg
        logger = L.mkLogger loggerCtx

    lqState <- EL.initLiveQueriesState lqOpts pgExecCtx
    wsServerEnv <- WS.createWSServerEnv logger pgExecCtx lqState cacheRef
                   httpManager corsPolicy sqlGenCtx enableAL planCache

    let schemaCacheRef =
          SchemaCacheRef cacheLock cacheRef (E.clearPlanCache planCache)
        serverCtx = ServerCtx pgExecCtx ci logger
                    schemaCacheRef mode httpManager
                    sqlGenCtx apis instanceId planCache lqState enableAL

    spockApp <- spockAsApp $ spockT id $
                httpApp corsCfg serverCtx enableConsole
                  consoleAssetsDir enableTelemetry extraMiddleware

    let wsServerApp = WS.createWSServerApp mode wsServerEnv
    return ( WS.websocketsOr WS.defaultConnectionOptions wsServerApp spockApp
           , schemaCacheRef
           , cacheBuiltTime
           )

httpApp
  :: CorsConfig
  -> ServerCtx
  -> Bool
  -> Maybe Text
  -> Bool
  -> Maybe (RQLQuery -> Handler ())
  -> SpockT IO ()
httpApp corsCfg serverCtx enableConsole consoleAssetsDir enableTelemetry extraMiddleware = do
    -- cors middleware
    unless (isCorsDisabled corsCfg) $
      middleware $ corsMiddleware (mkDefaultCorsPolicy corsCfg)

    -- API Console and Root Dir
    when (enableConsole && enableMetadata) serveApiConsole

    -- Health check endpoint
    get "healthz" $ do
      sc <- liftIO $ getSCFromRef $ scCacheRef serverCtx
      if null $ scInconsistentObjs sc
        then setStatus N.status200 >> lazyBytes "OK"
        else setStatus N.status500 >> lazyBytes "ERROR"

    get "v1/version" $ do
      uncurry setHeader jsonHeader
      lazyBytes $ encode $ object [ "version" .= currentVersion ]

    when enableMetadata $ do
      get    ("v1/template" <//> var) tmpltGetOrDeleteH
      post   ("v1/template" <//> var) tmpltPutOrPostH
      put    ("v1/template" <//> var) tmpltPutOrPostH
      delete ("v1/template" <//> var) tmpltGetOrDeleteH

      post "v1/query" $ mkSpockAction encodeQErr id serverCtx $ mkAPIRespHandler $ do
        query <- parseBody
        -- run any given extra middleware
        maybe (return ()) (\fn -> fn query) extraMiddleware
        v1QueryHandler query

      post ("api/1/table" <//> var <//> var) $ \tableName queryType ->
        mkSpockAction encodeQErr id serverCtx $ mkAPIRespHandler $
        legacyQueryHandler (TableName tableName) queryType

    when enablePGDump $
      post "v1alpha1/pg_dump" $ mkSpockAction encodeQErr id serverCtx $ do
        query <- parseBody
        v1Alpha1PGDumpHandler query

    when enableConfig $
      get "v1alpha1/config" $ mkSpockAction encodeQErr id serverCtx $
        mkAPIRespHandler $ do
          onlyAdmin
          return $ HttpResponse
            (encJFromJValue $ runGetConfig (scAuthMode serverCtx))
            Nothing

    when enableGraphQL $ do
      post "v1alpha1/graphql/explain" gqlExplainAction

      post "v1alpha1/graphql" $ mkSpockAction GH.encodeGQErr id serverCtx $
        mkAPIRespHandler $ do
          query <- parseBody
          v1Alpha1GQHandler query

      post "v1/graphql/explain" gqlExplainAction

      post "v1/graphql" $ mkSpockAction GH.encodeGQErr allMod200 serverCtx $
        mkAPIRespHandler $ do
          query <- parseBody
          v1GQHandler query

    when (isDeveloperAPIEnabled serverCtx) $ do
      get "dev/plan_cache" $ mkSpockAction encodeQErr id serverCtx $
        mkAPIRespHandler $ do
          onlyAdmin
          respJ <- liftIO $ E.dumpPlanCache $ scPlanCache serverCtx
          return $ HttpResponse (encJFromJValue respJ) Nothing
      get "dev/subscriptions" $ mkSpockAction encodeQErr id serverCtx $
        mkAPIRespHandler $ do
          onlyAdmin
          respJ <- liftIO $ EL.dumpLiveQueriesState False $ scLQState serverCtx
          return $ HttpResponse (encJFromJValue respJ) Nothing
      get "dev/subscriptions/extended" $ mkSpockAction encodeQErr id serverCtx $
        mkAPIRespHandler $ do
          onlyAdmin
          respJ <- liftIO $ EL.dumpLiveQueriesState True $ scLQState serverCtx
          return $ HttpResponse (encJFromJValue respJ) Nothing

    forM_ [GET,POST] $ \m -> hookAny m $ \_ -> do
      let qErr = err404 NotFound "resource does not exist"
      raiseGenericApiError logger qErr

  where
    logger = scLogger serverCtx
    -- all graphql errors should be of type 200
    allMod200 qe = qe { qeStatus = N.status200 }

    gqlExplainAction =
      mkSpockAction encodeQErr id serverCtx $ mkAPIRespHandler $ do
        expQuery <- parseBody
        gqlExplainHandler expQuery

    enableGraphQL = isGraphQLEnabled serverCtx
    enableMetadata = isMetadataEnabled serverCtx
    enablePGDump = isPGDumpEnabled serverCtx
    enableConfig = isConfigEnabled serverCtx
    tmpltGetOrDeleteH tmpltName = do
      tmpltArgs <- tmpltArgsFromQueryParams
      mkSpockAction encodeQErr id serverCtx $ mkAPIRespHandler $
        mkQTemplateAction tmpltName tmpltArgs

    tmpltPutOrPostH tmpltName = do
      tmpltArgs <- tmpltArgsFromQueryParams
      mkSpockAction encodeQErr id serverCtx $ mkAPIRespHandler $ do
        bodyTmpltArgs <- parseBody
        mkQTemplateAction tmpltName $ M.union bodyTmpltArgs tmpltArgs

    tmpltArgsFromQueryParams = do
      qparams <- params
      return $ M.fromList $ flip map qparams $
        TemplateParam *** String

    mkQTemplateAction tmpltName tmpltArgs =
      v1QueryHandler $ RQExecuteQueryTemplate $
      ExecQueryTemplate (TQueryName tmpltName) tmpltArgs

    serveApiConsole = do
      -- redirect / to /console
      get root $ redirect "console"

      -- serve static files if consoleAssetsDir is set
      onJust consoleAssetsDir $ \dir ->
        get ("console/assets" <//> wildcard) $ \path ->
          consoleAssetsHandler logger dir (T.unpack path)

      -- serve console html
      get ("console" <//> wildcard) $ \path ->
        either (raiseGenericApiError logger . err500 Unexpected . T.pack) html $
        mkConsoleHTML path (scAuthMode serverCtx) enableTelemetry consoleAssetsDir

raiseGenericApiError :: L.Logger -> QErr -> ActionT IO ()
raiseGenericApiError logger qErr = do
  req <- request
  reqBody <- liftIO $ strictRequestBody req
  logError Nothing req reqBody logger qErr
  uncurry setHeader jsonHeader
  setStatus $ qeStatus qErr
  lazyBytes $ encode qErr
