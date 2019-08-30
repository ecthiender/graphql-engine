{-# LANGUAGE RecordWildCards #-}

module Hasura.App where

import           Control.Monad.STM          (atomically)
import           Data.Time.Clock            (getCurrentTime)
import           Options.Applicative
import           System.Environment         (getEnvironment, lookupEnv)
import           System.Exit                (exitFailure)


import qualified Control.Concurrent         as C
import qualified Data.Aeson                 as A
import qualified Data.ByteString.Char8      as BC
import qualified Data.ByteString.Lazy       as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.Text                  as T
import qualified Data.Time.Clock            as Clock
import qualified Data.Yaml                  as Y
import qualified Network.HTTP.Client        as HTTP
import qualified Network.HTTP.Client.TLS    as HTTP
import qualified Network.Wai.Handler.Warp   as Warp

import           Hasura.App.Migrate         (migrateCatalog)
import           Hasura.App.Ops
import           Hasura.Db
import           Hasura.Events.Lib
import           Hasura.Logging
import           Hasura.Prelude
import           Hasura.RQL.DDL.Metadata    (fetchMetadata)
import           Hasura.RQL.Types           (QErr, SQLGenCtx (..),
                                             SchemaCache (..), adminUserInfo,
                                             emptySchemaCache)
import           Hasura.Server.App          (ConsoleRenderer, HasuraMiddleware,
                                             SchemaCacheRef (..),
                                             UserInfoResolver (..),
                                             getSCFromRef, logInconsObjs,
                                             mkWaiApp)
import           Hasura.Server.Auth
import           Hasura.Server.CheckUpdates (checkForUpdates)
import           Hasura.Server.Init
import           Hasura.Server.Query        (RQLQuery, Run, peelRun)
import           Hasura.Server.SchemaUpdate
import           Hasura.Server.Telemetry
import           Hasura.Server.Version      (currentVersion)

import qualified Database.PG.Query          as Q


printErrExit :: forall a . String -> IO a
printErrExit = (>> exitFailure) . putStrLn

printErrJExit :: A.ToJSON a => forall b . a -> IO b
printErrJExit = (>> exitFailure) . printJSON

parseHGECommand :: Parser RawHGECommand
parseHGECommand =
  subparser
    ( command "serve" (info (helper <*> (HCServe <$> serveOpts))
          ( progDesc "Start the GraphQL Engine Server"
            <> footerDoc (Just serveCmdFooter)
          ))
        <> command "export" (info (pure  HCExport)
          ( progDesc "Export graphql-engine's metadata to stdout" ))
        <> command "clean" (info (pure  HCClean)
          ( progDesc "Clean graphql-engine's metadata to start afresh" ))
        <> command "execute" (info (pure  HCExecute)
          ( progDesc "Execute a query" ))
        <> command "version" (info (pure  HCVersion)
          (progDesc "Prints the version of GraphQL Engine"))
    )
  where
    serveOpts = RawServeOptions
                <$> parseServerPort
                <*> parseServerHost
                <*> parseConnParams
                <*> parseTxIsolation
                <*> (parseAdminSecret <|> parseAccessKey)
                <*> parseWebHook
                <*> parseJwtSecret
                <*> parseUnAuthRole
                <*> parseCorsConfig
                <*> parseEnableConsole
                <*> parseConsoleAssetsDir
                <*> parseEnableTelemetry
                <*> parseWsReadCookie
                <*> parseStringifyNum
                <*> parseEnabledAPIs
                <*> parseMxRefetchInt
                <*> parseMxBatchSize
                <*> parseFallbackRefetchInt
                <*> parseEnableAllowlist
                <*> parseEnabledLogs
                <*> parseLogLevel

parseArgs :: IO HGEOptions
parseArgs = do
  rawHGEOpts <- execParser opts
  env <- getEnvironment
  let eitherOpts = runWithEnv env $ mkHGEOptions rawHGEOpts
  either printErrExit return eitherOpts
  where
    opts = info (helper <*> hgeOpts)
           ( fullDesc <>
             header "Hasura GraphQL Engine: Realtime GraphQL API over Postgres with access control" <>
             footerDoc (Just mainCmdFooter)
           )
    hgeOpts = HGEOptionsG <$> parseRawConnInfo <*> parseHGECommand

printJSON :: (A.ToJSON a) => a -> IO ()
printJSON = BLC.putStrLn . A.encode

printYaml :: (A.ToJSON a) => a -> IO ()
printYaml = BC.putStrLn . Y.encode

-- | a separate function to create the initialization context because some of
-- these contexts might be used by external functions
mkInitContext :: (MakeLogger a) => HGECommand -> RawConnInfo -> a -> IO InitContext
mkInitContext hgeCmd rci loggingExtra = do
  httpManager <- HTTP.newManager HTTP.tlsManagerSettings
  instanceId <- generateInstanceId
  connInfo <- procConnInfo
  (loggers, pool) <- case hgeCmd of
    HCServe ServeOptions{..} -> do
      l@(Loggers _ logger pgLogger) <- mkLoggers soEnabledLogTypes soLogLevel
      let sqlGenCtx = SQLGenCtx soStringifyNum
      -- log postgres connection info
      unLogger logger $ connInfoToLog connInfo
      pool <- Q.initPGPool connInfo soConnParams pgLogger
      -- safe init catalog
      initialise pool sqlGenCtx logger
      return (l, pool)
    _ -> do
      l@(Loggers _ _ pgLogger) <- mkLoggers defaultEnabledLogTypes LevelInfo
      pool <- getMinimalPool pgLogger connInfo
      return (l, pool)

  -- get the unique db id
  eDbId <- runExceptT $ Q.runTx pool (Q.Serializable, Nothing) getDbId
  dbId <- either printErrJExit return eDbId

  return $ InitContext connInfo pool instanceId dbId httpManager loggers
  where
    initialise pool sqlGenCtx (Logger logger) = do
      currentTime <- getCurrentTime
      -- initialise the catalog
      initRes <- runAsAdmin pool sqlGenCtx $ initCatalogSafe currentTime
      either printErrJExit (logger . mkGenericStrLog LevelInfo "db_init") initRes

      -- migrate catalog if necessary
      migRes <- runAsAdmin pool sqlGenCtx $ migrateCatalog currentTime
      either printErrJExit (logger . mkGenericStrLog LevelInfo "db_migrate") migRes

    procConnInfo =
      either (printErrExit . connInfoErrModifier) return $ mkConnInfo rci

    getMinimalPool pgLogger ci = do
      let connParams = Q.defaultConnParams { Q.cpConns = 1 }
      Q.initPGPool ci connParams pgLogger

    mkLoggers enabledLogs logLevel = do
      loggerCtx <- mkLoggerCtx (defaultLoggerSettings True logLevel) enabledLogs
      let logger = makeLogger loggingExtra loggerCtx
          pgLogger = mkPGLogger logger
      return $ Loggers loggerCtx logger pgLogger

    mkPGLogger (Logger logger) (Q.PLERetryMsg msg) =
      logger $ PGLog LevelWarn msg


handleCommand
  :: (UserInfoResolver auth, ConsoleRenderer renderer)
  => HGECommand
  -> InitContext
  -> Maybe (HasuraMiddleware RQLQuery)
  -> auth
  -> renderer
  -> IO ()
handleCommand hgeCmd initCtx metadataMiddleware auth renderConsole =
  case hgeCmd of
    HCServe serveOptions ->
      runHGEServer serveOptions initCtx metadataMiddleware auth renderConsole
    HCExport -> do
      res <- runTx' fetchMetadata
      either printErrJExit printJSON res

    HCClean -> do
      res <- runTx' cleanCatalog
      either printErrJExit (const cleanSuccess) res

    HCExecute -> do
      queryBs <- BL.getContents
      let sqlGenCtx = SQLGenCtx False
      res <- runAsAdmin (_icPgPool initCtx) sqlGenCtx $ execQuery queryBs
      either printErrJExit BLC.putStrLn res

    HCVersion -> putStrLn $ "Hasura GraphQL Engine: " ++ T.unpack currentVersion
  where
    runTx' :: Q.TxE QErr a -> IO (Either QErr a)
    runTx' tx =
      runExceptT $ Q.runTx (_icPgPool initCtx) (Q.Serializable, Nothing) tx

    cleanSuccess =
      putStrLn "successfully cleaned graphql-engine related data"


runHGEServer
  :: (UserInfoResolver auth, ConsoleRenderer renderer)
  => ServeOptions
  -> InitContext
  -> Maybe (HasuraMiddleware RQLQuery)
  -> auth
  -> renderer
  -> IO ()
runHGEServer serveOptions@ServeOptions{..} initCtx@InitContext{..} metadataMiddleware auth renderConsole = do
  let sqlGenCtx = SQLGenCtx soStringifyNum

  let Loggers loggerCtx logger _ = _icLoggers

  initTime <- Clock.getCurrentTime
  -- log serve options
  unLogger logger $ serveOptsToLog serveOptions

  authModeRes <- runExceptT $ flip runReaderT (logger, _icHttpManager) $
                 mkAuthMode soAdminSecret soAuthHook soJwtSecret soUnAuthRole

  authMode <- either (printErrExit . T.unpack) return authModeRes

  (app, cacheRef, cacheInitTime) <- mkWaiApp logger
                                             sqlGenCtx
                                             initCtx
                                             serveOptions
                                             authMode
                                             metadataMiddleware
                                             auth
                                             renderConsole

  -- log inconsistent schema objects
  inconsObjs <- scInconsistentObjs <$> getSCFromRef cacheRef
  logInconsObjs logger inconsObjs

  -- start a background thread for schema sync
  startSchemaSync sqlGenCtx _icPgPool logger _icHttpManager cacheRef
    _icInstanceId cacheInitTime

  let warpSettings = Warp.setPort soPort $ Warp.setHost soHost Warp.defaultSettings

  maxEvThrds <- getFromEnv defaultMaxEventThreads "HASURA_GRAPHQL_EVENTS_HTTP_POOL_SIZE"
  evFetchMilliSec <- getFromEnv defaultFetchIntervalMilliSec "HASURA_GRAPHQL_EVENTS_FETCH_INTERVAL"
  logEnvHeaders <- getFromEnv False "LOG_HEADERS_FROM_ENV"

  -- prepare event triggers data
  prepareEvents _icPgPool logger
  eventEngineCtx <- atomically $ initEventEngineCtx maxEvThrds evFetchMilliSec
  let scRef = _scrCache cacheRef
  unLogger logger $ mkGenericStrLog LevelInfo "event_triggers" "starting workers"
  void $ C.forkIO $ processEventQueue logger logEnvHeaders _icHttpManager _icPgPool
                    scRef eventEngineCtx

  -- start a background thread to check for updates
  void $ C.forkIO $ checkForUpdates loggerCtx _icHttpManager

  -- start a background thread for telemetry
  when soEnableTelemetry $ do
    unLogger logger $ mkGenericStrLog LevelInfo "telemetry" telemetryNotice
    void $ C.forkIO $ runTelemetry logger _icHttpManager scRef _icDbUid _icInstanceId

  finishTime <- Clock.getCurrentTime
  let apiInitTime = realToFrac $ Clock.diffUTCTime finishTime initTime
  unLogger logger $ mkGenericLog LevelInfo "server" $
    StartupTimeInfo "starting API server" apiInitTime
  Warp.runSettings warpSettings app

  where
    prepareEvents pool (Logger logger) = do
      logger $ mkGenericStrLog LevelInfo "event_triggers" "preparing data"
      res <- runTx pool unlockAllEvents
      either printErrJExit return res

    getFromEnv :: (Read a) => a -> String -> IO a
    getFromEnv defaults env = do
      mEnv <- lookupEnv env
      let mRes = case mEnv of
            Nothing  -> Just defaults
            Just val -> readMaybe val
          eRes = maybe (Left $ "Wrong expected type for environment variable: " <> env) Right mRes
      either printErrExit return eRes

    runTx pool tx =
      runExceptT $ Q.runTx pool (Q.Serializable, Nothing) tx


runAsAdmin :: Q.PGPool -> SQLGenCtx -> Run a -> IO (Either QErr a)
runAsAdmin pool sqlGenCtx m = do
  httpManager <- HTTP.newManager HTTP.tlsManagerSettings
  res  <- runExceptT $ peelRun emptySchemaCache adminUserInfo
          httpManager sqlGenCtx (PGExecCtx pool Q.Serializable) m
  return $ fmap fst res


telemetryNotice :: String
telemetryNotice =
  "Help us improve Hasura! The graphql-engine server collects anonymized "
  <> "usage stats which allows us to keep improving Hasura at warp speed. "
  <> "To read more or opt-out, visit https://docs.hasura.io/1.0/graphql/manual/guides/telemetry.html"
