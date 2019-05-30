-- This is taken from wai-logger and customised for our use
{-# LANGUAGE OverloadedStrings #-}

module Hasura.Server.Logging
  ( StartupLog(..)
  , PGLog(..)
  , mkInconsMetadataLog
  , mkAccessLog
  , WebHookLog(..)
  , WebHookLogger
  , HttpException
  , ApiMetrics(..)
  ) where

import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Aeson.TH
import           Data.Bits             (shift, (.&.))
import           Data.ByteString.Char8 (ByteString)
import           Data.Int              (Int64)
import           Data.List             (find)
import           Data.Time.Clock
import           Data.Word             (Word32)
import           Network.Socket        (SockAddr (..))
import           System.ByteOrder      (ByteOrder (..), byteOrder)
import           Text.Printf           (printf)

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy  as BL
import qualified Data.Text             as T
import qualified Network.HTTP.Types    as N
import qualified Network.Wai           as Wai

import           Hasura.HTTP
import           Hasura.Prelude
import           Hasura.RQL.Types
import           Hasura.Server.Utils

import qualified Hasura.Logging        as L

data StartupLog
  = StartupLog
  { slLogLevel :: !L.LogLevel
  , slKind     :: !T.Text
  , slInfo     :: !Value
  } deriving (Show, Eq)

instance ToJSON StartupLog where
  toJSON (StartupLog _ k info) =
    object [ "kind" .= k
           , "info" .= info
           ]

instance L.ToEngineLog StartupLog where
  toEngineLog startupLog =
    (slLogLevel startupLog, "startup", toJSON startupLog)

data PGLog
  = PGLog
  { plLogLevel :: !L.LogLevel
  , plMessage  :: !T.Text
  } deriving (Show, Eq)

instance ToJSON PGLog where
  toJSON (PGLog _ msg) =
    object ["message" .= msg]

instance L.ToEngineLog PGLog where
  toEngineLog pgLog =
    (plLogLevel pgLog, "pg-client", toJSON pgLog)

data MetadataLog
  = MetadataLog
  { mlLogLevel :: !L.LogLevel
  , mlMessage  :: !T.Text
  , mlInfo     :: !Value
  } deriving (Show, Eq)

instance ToJSON MetadataLog where
  toJSON (MetadataLog _ msg infoVal) =
    object [ "message" .= msg
           , "info" .= infoVal
           ]

instance L.ToEngineLog MetadataLog where
  toEngineLog ml =
    (mlLogLevel ml, "metadata", toJSON ml)

mkInconsMetadataLog :: [InconsistentMetadataObj] -> MetadataLog
mkInconsMetadataLog objs =
  MetadataLog L.LevelWarn "Inconsistent Metadata!" $
    object [ "objects" .= objs]

data WebHookLog
  = WebHookLog
  { whlLogLevel   :: !L.LogLevel
  , whlStatusCode :: !(Maybe N.Status)
  , whlUrl        :: !T.Text
  , whlMethod     :: !N.StdMethod
  , whlError      :: !(Maybe HttpException)
  , whlResponse   :: !(Maybe T.Text)
  } deriving (Show)

instance L.ToEngineLog WebHookLog where
  toEngineLog webHookLog =
    (whlLogLevel webHookLog, "webhook-log", toJSON webHookLog)

instance ToJSON WebHookLog where
  toJSON whl =
    object [ "status_code" .= (N.statusCode <$> whlStatusCode whl)
           , "url" .= whlUrl whl
           , "method" .= show (whlMethod whl)
           , "http_error" .= whlError whl
           , "response" .= whlResponse whl
           ]

type WebHookLogger = WebHookLog -> IO ()

data HttpLog
  = HttpLog
  { hlStatus      :: !N.Status
  , hlMethod      :: !T.Text
  , hlSource      :: !T.Text
  , hlPath        :: !T.Text
  , hlHttpVersion :: !N.HttpVersion
  } deriving (Show, Eq)

instance ToJSON HttpLog where
  toJSON (HttpLog st met src path hv) =
    object [ "status" .= N.statusCode st
           , "method" .= met
           , "ip" .= src
           , "url" .= path
           , "http_version" .= show hv
           ]

data OperationLog
  = OperationLog
  { olRequestId          :: !RequestId
  , olUserVars           :: !(Maybe UserVars)
  , olResponseSize       :: !(Maybe Int64)
  , olQueryExecutionTime :: !(Maybe Double)
  , olQuery              :: !(Maybe Value)
  , olError              :: !(Maybe Value)
  } deriving (Show, Eq)
$(deriveToJSON (aesonDrop 2 snakeCase) ''OperationLog)

data AccessLog
  = AccessLog
  { alHttpInfo  :: !HttpLog
  , alOperation :: !OperationLog
  } deriving (Show, Eq)
$(deriveToJSON (aesonDrop 2 snakeCase) ''AccessLog)

instance L.ToEngineLog AccessLog where
  toEngineLog accessLog =
    (L.LevelInfo, "http-log", toJSON accessLog)

mkAccessLog
  :: (ToJSON a)
  => L.VerboseLogging
  -> Maybe UserInfo -- may not have been resolved
  -> RequestId
  -> Wai.Request
  -> Either QErr BL.ByteString
  -> Maybe (ApiMetrics a)
  -> Maybe (UTCTime, UTCTime)
  -> AccessLog
mkAccessLog _ userInfoM reqId req res extraInfo mTimeT =
  let http = HttpLog
             { hlStatus       = status
             , hlMethod       = bsToTxt $ Wai.requestMethod req
             , hlSource       = bsToTxt $ getSourceFromFallback req
             , hlPath         = bsToTxt $ Wai.rawPathInfo req
             , hlHttpVersion  = Wai.httpVersion req
             }
      op = OperationLog
           { olRequestId    = reqId
           , olUserVars     = userVars <$> userInfoM
           , olResponseSize = respSize
           , olQueryExecutionTime = respTime
           , olQuery = toJSON <$> query
           , olError = toJSON <$> err
           }
  in AccessLog http op
  where
    (query, status, err, respTime, respSize) = ravenLogGen res extraInfo mTimeT

ravenLogGen
  :: Either QErr BL.ByteString
  -> Maybe (ApiMetrics a)
  -> Maybe (UTCTime , UTCTime)
  -> (Maybe a, N.Status, Maybe QErr, Maybe Double, Maybe Int64)
ravenLogGen res extraInfo mTimeT =
  (query, status, err, diffTime, Just size)
  where
    status = either qeStatus (const N.status200) res
    size = BL.length $ either encode id res
    err = either Just (const Nothing) res
    diffTime = fmap (realToFrac . uncurry (flip diffUTCTime)) mTimeT
    q = amQuery <$> extraInfo
    query = either (const q) (const Nothing) res

getSourceFromSocket :: Wai.Request -> ByteString
getSourceFromSocket = BS.pack . showSockAddr . Wai.remoteHost

getSourceFromFallback :: Wai.Request -> ByteString
getSourceFromFallback req = fromMaybe (getSourceFromSocket req) $ getSource req

getSource :: Wai.Request -> Maybe ByteString
getSource req = addr
  where
    maddr = find (\x -> fst x `elem` ["x-real-ip", "x-forwarded-for"]) hdrs
    addr = fmap snd maddr
    hdrs = Wai.requestHeaders req

-- |  A type for IP address in numeric string representation.
type NumericAddress = String

showIPv4 :: Word32 -> Bool -> NumericAddress
showIPv4 w32 little
    | little    = show b1 ++ "." ++ show b2 ++ "." ++ show b3 ++ "." ++ show b4
    | otherwise = show b4 ++ "." ++ show b3 ++ "." ++ show b2 ++ "." ++ show b1
  where
    t1 = w32
    t2 = shift t1 (-8)
    t3 = shift t2 (-8)
    t4 = shift t3 (-8)
    b1 = t1 .&. 0x000000ff
    b2 = t2 .&. 0x000000ff
    b3 = t3 .&. 0x000000ff
    b4 = t4 .&. 0x000000ff

showIPv6 :: (Word32,Word32,Word32,Word32) -> String
showIPv6 (w1,w2,w3,w4) =
    printf "%x:%x:%x:%x:%x:%x:%x:%x" s1 s2 s3 s4 s5 s6 s7 s8
  where
    (s1,s2) = split16 w1
    (s3,s4) = split16 w2
    (s5,s6) = split16 w3
    (s7,s8) = split16 w4
    split16 w = (h1,h2)
      where
        h1 = shift w (-16) .&. 0x0000ffff
        h2 = w .&. 0x0000ffff

-- | Convert 'SockAddr' to 'NumericAddress'. If the address is
--   IPv4-embedded IPv6 address, the IPv4 is extracted.
showSockAddr :: SockAddr -> NumericAddress
-- HostAddr is network byte order.
showSockAddr (SockAddrInet _ addr4)                       = showIPv4 addr4 (byteOrder == LittleEndian)
-- HostAddr6 is host byte order.
showSockAddr (SockAddrInet6 _ _ (0,0,0x0000ffff,addr4) _) = showIPv4 addr4 False
showSockAddr (SockAddrInet6 _ _ (0,0,0,1) _)              = "::1"
showSockAddr (SockAddrInet6 _ _ addr6 _)                  = showIPv6 addr6
showSockAddr _                                            = "unknownSocket"
