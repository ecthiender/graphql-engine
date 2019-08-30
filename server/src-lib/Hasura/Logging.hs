{-# LANGUAGE RankNTypes #-}

module Hasura.Logging
  ( LoggerSettings(..)
  , defaultLoggerSettings
  , LogLevel(..)
  , LoggerCtx(..)
  , mkLoggerCtx
  , cleanLoggerCtx

  , EngineLog(..)
  , ToEngineLog(..)

  , Logger (..)
  , HGELogging(..)

  , EngineLogType(..)
  , defaultEnabledLogTypes
  , userAllowedLogTypes
  , isLogTypeEnabled
  , eventTriggerLogType

  , debugT
  , debugBS
  , debugLBS
  , MakeLogger(..)

  , PGLog(..)
  ) where

import           Hasura.Prelude

import qualified Control.AutoUpdate    as Auto
import qualified Data.Aeson            as J
import qualified Data.Aeson.Casing     as J
import qualified Data.Aeson.TH         as J
import qualified Data.ByteString       as B
import qualified Data.ByteString.Lazy  as BL
import qualified Data.HashSet          as Set
import qualified Data.TByteString      as TBS
import qualified Data.Text             as T
import qualified Data.Time.Clock       as Time
import qualified Data.Time.Format      as Format
import qualified Data.Time.LocalTime   as Time
import qualified System.Log.FastLogger as FL

newtype FormattedTime
  = FormattedTime { _unFormattedTime :: Text }
  deriving (Show, Eq, J.ToJSON)

data EngineLogType
  = ELTHttpLog
  | ELTWebsocketLog
  | ELTWebhookLog
  | ELTQueryLog
  | ELTStartup
  -- internal log types
  | ELTInternal !Text
  deriving (Show, Eq, Generic)

instance Hashable EngineLogType

instance J.ToJSON EngineLogType where
  toJSON = \case
    ELTHttpLog      -> "http-log"
    ELTWebsocketLog -> "websocket-log"
    ELTWebhookLog   -> "webhook-log"
    ELTQueryLog     -> "query-log"
    ELTStartup      -> "startup"
    ELTInternal t   -> J.String t

-- the default enabled log-types
defaultEnabledLogTypes :: Set.HashSet EngineLogType
defaultEnabledLogTypes =
  Set.fromList [ELTStartup, ELTHttpLog, ELTWebhookLog, ELTWebsocketLog]

isLogTypeEnabled :: Set.HashSet EngineLogType -> EngineLogType -> Bool
isLogTypeEnabled enabledTypes logTy = case logTy of
  ELTInternal _ -> True
  _             -> logTy `Set.member` enabledTypes

-- log types that can be set by the user
userAllowedLogTypes :: [EngineLogType]
userAllowedLogTypes =
  [ ELTStartup
  , ELTHttpLog
  , ELTWebhookLog
  , ELTWebsocketLog
  , ELTQueryLog
  ]

eventTriggerLogType :: EngineLogType
eventTriggerLogType = ELTInternal "event-trigger"

data LogLevel
  = LevelDebug
  | LevelInfo
  | LevelWarn
  | LevelError
  | LevelOther Text
  deriving (Show, Eq, Ord)

instance J.ToJSON LogLevel where
  toJSON = J.toJSON . \case
    LevelDebug   -> "debug"
    LevelInfo    -> "info"
    LevelWarn    -> "warn"
    LevelError   -> "error"
    LevelOther t -> t

data EngineLog
  = EngineLog
  { _elTimestamp :: !FormattedTime
  , _elLevel     :: !LogLevel
  , _elType      :: !EngineLogType
  , _elDetail    :: !J.Value
  } deriving (Show, Eq)
$(J.deriveToJSON (J.aesonDrop 3 J.snakeCase) ''EngineLog)


class ToEngineLog a where
  toEngineLog :: a -> (LogLevel, EngineLogType, J.Value)

data LoggerCtx a
  = LoggerCtx
  { _lcLoggerSet       :: !FL.LoggerSet
  , _lcLogLevel        :: !LogLevel
  , _lcTimeGetter      :: !(IO FormattedTime)
  , _lcEnabledLogTypes :: !(Set.HashSet EngineLogType)
  , _lcArbitrary       :: !a
  -- ^ Any arbitrary value to have so this can be parameterized to have
  -- different implementations of the logging
  }

data LoggerSettings
  = LoggerSettings
  -- should current time be cached (refreshed every sec)
  { _lsCachedTimestamp :: !Bool
  , _lsTimeZone        :: !(Maybe Time.TimeZone)
  , _lsLevel           :: !LogLevel
  } deriving (Show, Eq)

class MakeLogger a where
  -- | Typeclass which is parametrized over the arbitrary value in LoggerCtx, which
  -- can be used to have different implementations of the actual function in @Logger@
  makeLogger :: LoggerCtx a -> Logger

newtype Logger =
  Logger { unLogger :: forall a. (ToEngineLog a) => a -> IO () }

defaultLoggerSettings :: Bool -> LogLevel -> LoggerSettings
defaultLoggerSettings isCached =
  LoggerSettings isCached Nothing

getFormattedTime :: Maybe Time.TimeZone -> IO FormattedTime
getFormattedTime tzM = do
  tz <- maybe Time.getCurrentTimeZone return tzM
  t  <- Time.getCurrentTime
  let zt = Time.utcToZonedTime tz t
  return $ FormattedTime $ T.pack $ formatTime zt
  where
    formatTime = Format.formatTime Format.defaultTimeLocale format
    format = "%FT%H:%M:%S%3Q%z"
    -- format = Format.iso8601DateFormat (Just "%H:%M:%S")

mkLoggerCtx
  :: LoggerSettings
  -> Set.HashSet EngineLogType
  -> a
  -> IO (LoggerCtx a)
mkLoggerCtx (LoggerSettings cacheTime tzM logLevel) enabledLogs extra = do
  loggerSet <- FL.newStdoutLoggerSet FL.defaultBufSize
  timeGetter <- bool (return $ getFormattedTime tzM) cachedTimeGetter cacheTime
  return $ LoggerCtx loggerSet logLevel timeGetter enabledLogs extra
  where
    cachedTimeGetter =
      Auto.mkAutoUpdate Auto.defaultUpdateSettings {
        Auto.updateAction = getFormattedTime tzM
      }

cleanLoggerCtx :: LoggerCtx a -> IO ()
cleanLoggerCtx =
  FL.rmLoggerSet . _lcLoggerSet

-- | type with no extra information, because in this implementation of logging
-- we don't need any extra information
data HGELogging = HGELogging

instance MakeLogger HGELogging where
  makeLogger (LoggerCtx loggerSet serverLogLevel timeGetter enabledLogTypes _) = Logger $ \l -> do
    localTime <- timeGetter
    let (logLevel, logTy, logDet) = toEngineLog l
    when (logLevel >= serverLogLevel && isLogTypeEnabled enabledLogTypes logTy) $
      FL.pushLogStrLn loggerSet $ FL.toLogStr $ J.encode $ EngineLog localTime logLevel logTy logDet


newtype UnstructuredLog
  = UnstructuredLog { _unUnstructuredLog :: TBS.TByteString }
  deriving (Show, Eq)

debugT :: Text -> UnstructuredLog
debugT = UnstructuredLog . TBS.fromText

debugBS :: B.ByteString -> UnstructuredLog
debugBS = UnstructuredLog . TBS.fromBS

debugLBS :: BL.ByteString -> UnstructuredLog
debugLBS = UnstructuredLog . TBS.fromLBS

instance ToEngineLog UnstructuredLog where
  toEngineLog (UnstructuredLog t) =
    (LevelDebug, ELTInternal "unstructured", J.toJSON t)

data PGLog
  = PGLog
  { plLogLevel :: !LogLevel
  , plMessage  :: !T.Text
  } deriving (Show, Eq)

instance J.ToJSON PGLog where
  toJSON (PGLog _ msg) =
    J.object ["message" J..= msg]

instance ToEngineLog PGLog where
  toEngineLog pgLog =
    (plLogLevel pgLog, ELTInternal "pg-client", J.toJSON pgLog)
