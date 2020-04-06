-- This is taken from wai-logger and customised for our use

module Hasura.Server.Logging
  ( StartupLog(..)
  , PGLog(..)
  , mkInconsMetadataLog
  , mkHttpAccessLogContext
  , mkHttpErrorLogContext
  , mkHttpLog
  , HttpInfoLog(..)
  , OperationLog(..)
  , HttpLogContext(..)
  , WebHookLog(..)
  , HttpException
  -- , getSourceFromFallback
  -- , getSource
  , HttpLog (..)
  ) where

import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Aeson.TH
import           Data.Int                  (Int64)
import           Data.Time.Clock

import qualified Data.ByteString.Lazy      as BL
import qualified Data.Text                 as T
import qualified Network.HTTP.Types        as HTTP
import qualified Network.Wai               as Wai

import           Hasura.HTTP
import           Hasura.Logging
import           Hasura.Prelude
import           Hasura.RQL.Types
import           Hasura.Server.Compression
import           Hasura.Server.Utils

data StartupLog
  = StartupLog
  { slLogLevel :: !LogLevel
  , slKind     :: !T.Text
  , slInfo     :: !Value
  } deriving (Show, Eq)

instance ToJSON StartupLog where
  toJSON (StartupLog _ k info) =
    object [ "kind" .= k
           , "info" .= info
           ]

instance ToEngineLog StartupLog Hasura where
  toEngineLog startupLog =
    (slLogLevel startupLog, ELTStartup, toJSON startupLog)

data PGLog
  = PGLog
  { plLogLevel :: !LogLevel
  , plMessage  :: !T.Text
  } deriving (Show, Eq)

instance ToJSON PGLog where
  toJSON (PGLog _ msg) =
    object ["message" .= msg]

instance ToEngineLog PGLog Hasura where
  toEngineLog pgLog =
    (plLogLevel pgLog, ELTInternal ILTPgClient, toJSON pgLog)

data MetadataLog
  = MetadataLog
  { mlLogLevel :: !LogLevel
  , mlMessage  :: !T.Text
  , mlInfo     :: !Value
  } deriving (Show, Eq)

instance ToJSON MetadataLog where
  toJSON (MetadataLog _ msg infoVal) =
    object [ "message" .= msg
           , "info" .= infoVal
           ]

instance ToEngineLog MetadataLog Hasura where
  toEngineLog ml =
    (mlLogLevel ml, ELTInternal ILTMetadata, toJSON ml)

mkInconsMetadataLog :: [InconsistentMetadata] -> MetadataLog
mkInconsMetadataLog objs =
  MetadataLog LevelWarn "Inconsistent Metadata!" $
    object [ "objects" .= objs]

data WebHookLog
  = WebHookLog
  { whlLogLevel   :: !LogLevel
  , whlStatusCode :: !(Maybe HTTP.Status)
  , whlUrl        :: !T.Text
  , whlMethod     :: !HTTP.StdMethod
  , whlError      :: !(Maybe HttpException)
  , whlResponse   :: !(Maybe T.Text)
  } deriving (Show)

instance ToEngineLog WebHookLog Hasura where
  toEngineLog webHookLog =
    (whlLogLevel webHookLog, ELTWebhookLog, toJSON webHookLog)

instance ToJSON WebHookLog where
  toJSON whl =
    object [ "status_code" .= (HTTP.statusCode <$> whlStatusCode whl)
           , "url" .= whlUrl whl
           , "method" .= show (whlMethod whl)
           , "http_error" .= whlError whl
           , "response" .= whlResponse whl
           ]


class (Monad m) => HttpLog m where
  logHttpError
    :: Show code
    => Logger Hasura
    -- ^ the logger
    -> Maybe UserInfo
    -- ^ user info may or may not be present (error can happen during user resolution)
    -> RequestId
    -- ^ request id of the request
    -> Wai.Request
    -- ^ the Wai.Request object
    -> Either BL.ByteString Value
    -- ^ the actual request body (bytestring if unparsed, Aeson value if parsed)
    -> QErr code
    -- ^ the error
    -> [HTTP.Header]
    -- ^ list of request headers
    -> m ()

  logHttpSuccess
    :: Logger Hasura
    -- ^ the logger
    -> Maybe UserInfo
    -- ^ user info may or may not be present (error can happen during user resolution)
    -> RequestId
    -- ^ request id of the request
    -> Wai.Request
    -- ^ the Wai.Request object
    -> Maybe Value
    -- ^ the actual request body, if present
    -> BL.ByteString
    -- ^ the response bytes
    -> BL.ByteString
    -- ^ the compressed response bytes
    -- ^ TODO: make the above two type represented
    -> Maybe (DiffTime, DiffTime)
    -- ^ IO/network wait time and service time (respectively) for this request, if available.
    -> Maybe CompressionType
    -- ^ possible compression type
    -> [HTTP.Header]
    -- ^ list of request headers
    -> m ()


-- | Log information about the HTTP request
data HttpInfoLog
  = HttpInfoLog
  { hlStatus      :: !HTTP.Status
  , hlMethod      :: !T.Text
  , hlSource      :: !IpAddress
  , hlPath        :: !T.Text
  , hlHttpVersion :: !HTTP.HttpVersion
  , hlCompression :: !(Maybe CompressionType)
  , hlHeaders     :: ![HTTP.Header]
  -- ^ all the request headers
  } deriving (Show, Eq)

instance ToJSON HttpInfoLog where
  toJSON (HttpInfoLog st met src path hv compressTypeM _) =
    object [ "status" .= HTTP.statusCode st
           , "method" .= met
           , "ip" .= bsToTxt (unIpAddress src)
           , "url" .= path
           , "http_version" .= show hv
           , "content_encoding" .= (compressionTypeToTxt <$> compressTypeM)
           ]

-- | Information about a GraphQL/Hasura metadata operation over HTTP
data OperationLog code
  = OperationLog
  { olRequestId          :: !RequestId
  , olUserVars           :: !(Maybe UserVars)
  , olResponseSize       :: !(Maybe Int64)
  , olRequestReadTime    :: !(Maybe Seconds)
  -- ^ Request IO wait time, i.e. time spent reading the full request from the socket.
  , olQueryExecutionTime :: !(Maybe Seconds)
  -- ^ Service time, not including request IO wait time.
  , olQuery              :: !(Maybe Value)
  , olRawQuery           :: !(Maybe Text)
  , olError              :: !(Maybe (QErr code))
  } deriving (Show, Eq)

-- empty splice to bring all the above definitions in scope
$(pure [])

instance Show code => ToJSON (OperationLog code) where
  toJSON = $(mkToJSON (aesonDrop 2 snakeCase) { omitNothingFields = True} ''OperationLog)

data HttpLogContext code
  = HttpLogContext
  { hlcHttpInfo  :: !HttpInfoLog
  , hlcOperation :: !(OperationLog code)
  } deriving (Show, Eq)

-- empty splice to bring all the above definitions in scope
$(pure [])

instance Show code => ToJSON (HttpLogContext code) where
  toJSON = $(mkToJSON (aesonDrop 3 snakeCase) ''HttpLogContext)

mkHttpAccessLogContext
  :: Maybe UserInfo
  -- ^ Maybe because it may not have been resolved
  -> RequestId
  -> Wai.Request
  -> BL.ByteString
  -> Maybe (DiffTime, DiffTime)
  -> Maybe CompressionType
  -> [HTTP.Header]
  -> HttpLogContext code
mkHttpAccessLogContext userInfoM reqId req res mTiming compressTypeM headers =
  let http = HttpInfoLog
             { hlStatus      = status
             , hlMethod      = bsToTxt $ Wai.requestMethod req
             , hlSource      = getSourceFromFallback req
             , hlPath        = bsToTxt $ Wai.rawPathInfo req
             , hlHttpVersion = Wai.httpVersion req
             , hlCompression  = compressTypeM
             , hlHeaders     = headers
             }
      op = OperationLog
           { olRequestId    = reqId
           , olUserVars     = userVars <$> userInfoM
           , olResponseSize = respSize
           , olRequestReadTime    = Seconds . fst <$> mTiming
           , olQueryExecutionTime = Seconds . snd <$> mTiming
           , olQuery = Nothing
           , olRawQuery = Nothing
           , olError = Nothing
           }
  in HttpLogContext http op
  where
    status = HTTP.status200
    respSize = Just $ BL.length res

mkHttpErrorLogContext
  :: Show code
  => Maybe UserInfo
  -- ^ Maybe because it may not have been resolved
  -> RequestId
  -> Wai.Request
  -> QErr code
  -> Either BL.ByteString Value
  -> Maybe (DiffTime, DiffTime)
  -> Maybe CompressionType
  -> [HTTP.Header]
  -> HttpLogContext code
mkHttpErrorLogContext userInfoM reqId req err query mTiming compressTypeM headers =
  let http = HttpInfoLog
             { hlStatus      = qeStatus err
             , hlMethod      = bsToTxt $ Wai.requestMethod req
             , hlSource      = getSourceFromFallback req
             , hlPath        = bsToTxt $ Wai.rawPathInfo req
             , hlHttpVersion = Wai.httpVersion req
             , hlCompression  = compressTypeM
             , hlHeaders     = headers
             }
      op = OperationLog
           { olRequestId          = reqId
           , olUserVars           = userVars <$> userInfoM
           , olResponseSize       = Just $ BL.length $ encode err
           , olRequestReadTime    = Seconds . fst <$> mTiming
           , olQueryExecutionTime = Seconds . snd <$> mTiming
           , olQuery              = either (const Nothing) Just query
           , olRawQuery           = either (Just . bsToTxt . BL.toStrict) (const Nothing) query
           , olError              = Just err
           }
  in HttpLogContext http op

data HttpLogLine code
  = HttpLogLine
  { _hlLogLevel :: !LogLevel
  , _hlLogLine  :: !(HttpLogContext code)
  }

instance Show code => ToEngineLog (HttpLogLine code) Hasura where
  toEngineLog (HttpLogLine logLevel logLine) =
    (logLevel, ELTHttpLog, toJSON logLine)

mkHttpLog :: HttpLogContext code -> HttpLogLine code
mkHttpLog httpLogCtx =
  let isError = isJust $ olError $ hlcOperation httpLogCtx
      logLevel = bool LevelInfo LevelError isError
  in HttpLogLine logLevel httpLogCtx

-- computeTimeDiff :: Maybe (UTCTime, UTCTime) -> Maybe Double
-- computeTimeDiff = fmap (realToFrac . uncurry (flip diffUTCTime))
