{-# LANGUAGE TypeApplications #-}
module Hasura.Server.Utils where

import           Data.Aeson
import           Data.Bits                  (shift, (.&.))
import           Data.ByteString.Char8      (ByteString)
import           Data.Char
import           Data.List                  (find)
import           Data.Word                  (Word32)
import           Language.Haskell.TH.Syntax (Lift)
import           Network.Socket             (SockAddr (..))
import           System.ByteOrder           (ByteOrder (..), byteOrder)
import           System.Environment
import           System.Exit
import           System.Process
import           Text.Printf                (printf)

import qualified Data.ByteString            as B
import qualified Data.ByteString.Char8      as BS
import qualified Data.CaseInsensitive       as CI
import qualified Data.HashSet               as Set
import qualified Data.List.NonEmpty         as NE
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as TE
import qualified Data.Text.IO               as TI
import qualified Data.UUID                  as UUID
import qualified Data.UUID.V4               as UUID
import qualified Language.Haskell.TH.Syntax as TH
import qualified Network.HTTP.Client        as HC
import qualified Network.HTTP.Types         as HTTP
import qualified Network.Wai                as Wai
import qualified Text.Regex.TDFA            as TDFA
import qualified Text.Regex.TDFA.ByteString as TDFA

import           Hasura.Prelude

newtype RequestId
  = RequestId { unRequestId :: Text }
  deriving (Show, Eq, ToJSON, FromJSON)

jsonHeader :: (T.Text, T.Text)
jsonHeader = ("Content-Type", "application/json; charset=utf-8")

sqlHeader :: (T.Text, T.Text)
sqlHeader = ("Content-Type", "application/sql; charset=utf-8")

htmlHeader :: (T.Text, T.Text)
htmlHeader = ("Content-Type", "text/html; charset=utf-8")

gzipHeader :: (T.Text, T.Text)
gzipHeader = ("Content-Encoding", "gzip")

brHeader :: (T.Text, T.Text)
brHeader = ("Content-Encoding", "br")

userRoleHeader :: T.Text
userRoleHeader = "x-hasura-role"

deprecatedAccessKeyHeader :: T.Text
deprecatedAccessKeyHeader = "x-hasura-access-key"

adminSecretHeader :: T.Text
adminSecretHeader = "x-hasura-admin-secret"

userIdHeader :: T.Text
userIdHeader = "x-hasura-user-id"

requestIdHeader :: T.Text
requestIdHeader = "x-request-id"

getRequestHeader :: B.ByteString -> [HTTP.Header] -> Maybe B.ByteString
getRequestHeader hdrName hdrs = snd <$> mHeader
  where
    mHeader = find (\h -> fst h == CI.mk hdrName) hdrs

getRequestId :: (MonadIO m) => [HTTP.Header] -> m RequestId
getRequestId headers =
  -- generate a request id for every request if the client has not sent it
  case getRequestHeader (txtToBs requestIdHeader) headers  of
    Nothing    -> RequestId <$> liftIO generateFingerprint
    Just reqId -> return $ RequestId $ bsToTxt reqId

-- Get an env var during compile time
getValFromEnvOrScript :: String -> String -> TH.Q (TH.TExp String)
getValFromEnvOrScript n s = do
  maybeVal <- TH.runIO $ lookupEnv n
  case maybeVal of
    Just val -> [|| val ||]
    Nothing  -> runScript s

-- Run a shell script during compile time
runScript :: FilePath -> TH.Q (TH.TExp String)
runScript fp = do
  TH.addDependentFile fp
  fileContent <- TH.runIO $ TI.readFile fp
  (exitCode, stdOut, stdErr) <- TH.runIO $
    readProcessWithExitCode "/bin/sh" [] $ T.unpack fileContent
  when (exitCode /= ExitSuccess) $ fail $
    "Running shell script " ++ fp ++ " failed with exit code : "
    ++ show exitCode ++ " and with error : " ++ stdErr
  [|| stdOut ||]

-- find duplicates
duplicates :: Ord a => [a] -> [a]
duplicates = mapMaybe greaterThanOne . group . sort
  where
    greaterThanOne l = bool Nothing (Just $ head l) $ length l > 1

-- regex related
matchRegex :: B.ByteString -> Bool -> T.Text -> Either String Bool
matchRegex regex caseSensitive src =
  fmap (`TDFA.match` TE.encodeUtf8 src) compiledRegexE
  where
    compOpt = TDFA.defaultCompOpt
      { TDFA.caseSensitive = caseSensitive
      , TDFA.multiline = True
      , TDFA.lastStarGreedy = True
      }
    execOption = TDFA.defaultExecOpt {TDFA.captureGroups = False}
    compiledRegexE = TDFA.compile compOpt execOption regex


fmapL :: (a -> a') -> Either a b -> Either a' b
fmapL fn (Left e) = Left (fn e)
fmapL _ (Right x) = pure x

generateFingerprint :: IO Text
generateFingerprint = UUID.toText <$> UUID.nextRandom

-- json representation of HTTP exception
httpExceptToJSON :: HC.HttpException -> Value
httpExceptToJSON e = case e of
  HC.HttpExceptionRequest x c ->
      let reqObj = object
            [ "host" .= bsToTxt (HC.host x)
            , "port" .= show (HC.port x)
            , "secure" .= HC.secure x
            , "path" .= bsToTxt (HC.path x)
            , "method" .= bsToTxt (HC.method x)
            , "proxy" .= (showProxy <$> HC.proxy x)
            , "redirectCount" .= show (HC.redirectCount x)
            , "responseTimeout" .= show (HC.responseTimeout x)
            , "requestVersion" .= show (HC.requestVersion x)
            ]
          msg = show c
      in object ["request" .= reqObj, "message" .= msg]
  _        -> toJSON $ show e
  where
    showProxy (HC.Proxy h p) =
      "host: " <> bsToTxt h <> " port: " <> T.pack (show p)

-- ignore the following request headers from the client
commonClientHeadersIgnored :: (IsString a) => [a]
commonClientHeadersIgnored =
  [ "Content-Length", "Content-MD5", "User-Agent", "Host"
  , "Origin", "Referer" , "Accept", "Accept-Encoding"
  , "Accept-Language", "Accept-Datetime"
  , "Cache-Control", "Connection", "DNT", "Content-Type"
  ]

commonResponseHeadersIgnored :: (IsString a) => [a]
commonResponseHeadersIgnored =
  [ "Server", "Transfer-Encoding", "Cache-Control"
  , "Access-Control-Allow-Credentials"
  , "Access-Control-Allow-Methods"
  , "Access-Control-Allow-Origin"
  , "Content-Type", "Content-Length"
  ]

isUserVar :: Text -> Bool
isUserVar = T.isPrefixOf "x-hasura-" . T.toLower

mkClientHeadersForward :: [HTTP.Header] -> [HTTP.Header]
mkClientHeadersForward reqHeaders =
  xForwardedHeaders <> (filterUserVars . filterRequestHeaders) reqHeaders
  where
    filterUserVars = filter (\(k, _) -> not $ isUserVar $ bsToTxt $ CI.original k)
    xForwardedHeaders = flip mapMaybe reqHeaders $ \(hdrName, hdrValue) ->
      case hdrName of
        "Host"       -> Just ("X-Forwarded-Host", hdrValue)
        "User-Agent" -> Just ("X-Forwarded-User-Agent", hdrValue)
        _            -> Nothing

filterRequestHeaders :: [HTTP.Header] -> [HTTP.Header]
filterRequestHeaders =
  filterHeaders $ Set.fromList commonClientHeadersIgnored

-- ignore the following response headers from remote
filterResponseHeaders :: [HTTP.Header] -> [HTTP.Header]
filterResponseHeaders =
  filterHeaders $ Set.fromList commonResponseHeadersIgnored

filterHeaders :: Set.HashSet HTTP.HeaderName -> [HTTP.Header] -> [HTTP.Header]
filterHeaders list = filter (\(n, _) -> not $ n `Set.member` list)

hyphenate :: String -> String
hyphenate = u . applyFirst toLower
    where u []                 = []
          u (x:xs) | isUpper x = '-' : toLower x : hyphenate xs
                   | otherwise = x : u xs

applyFirst :: (Char -> Char) -> String -> String
applyFirst _ []     = []
applyFirst f [x]    = [f x]
applyFirst f (x:xs) = f x: xs

-- | The version integer
data APIVersion
  = VIVersion1
  | VIVersion2
  deriving (Show, Eq, Lift)

instance ToJSON APIVersion where
  toJSON VIVersion1 = toJSON @Int 1
  toJSON VIVersion2 = toJSON @Int 2

instance FromJSON APIVersion where
  parseJSON v = do
    verInt :: Int <- parseJSON v
    case verInt of
      1 -> return VIVersion1
      2 -> return VIVersion2
      i -> fail $ "expected 1 or 2, encountered " ++ show i

englishList :: NonEmpty Text -> Text
englishList = \case
  one :| []    -> one
  one :| [two] -> one <> " and " <> two
  several      ->
    let final :| initials = NE.reverse several
    in T.intercalate ", " (reverse initials) <> ", and " <> final

makeReasonMessage :: [a] -> (a -> Text) -> Text
makeReasonMessage errors showError =
  case errors of
    [singleError] -> "because " <> showError singleError
    _ -> "for the following reasons:\n" <> T.unlines
         (map (("  • " <>) . showError) errors)


-- | IP Address related code

newtype IpAddress
  = IpAddress { unIpAddress :: ByteString }
  deriving (Show, Eq)

getSourceFromSocket :: Wai.Request -> IpAddress
getSourceFromSocket = IpAddress . BS.pack . showSockAddr . Wai.remoteHost

getSourceFromFallback :: Wai.Request -> IpAddress
getSourceFromFallback req = fromMaybe (getSourceFromSocket req) $ getSource req

getSource :: Wai.Request -> Maybe IpAddress
getSource req = IpAddress <$> addr
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

-- withElapsedTime :: MonadIO m => m a -> m (NominalDiffTime, a)
-- withElapsedTime ma = do
--   t1 <- liftIO getCurrentTime
--   a <- ma
--   t2 <- liftIO getCurrentTime
--   return (diffUTCTime t2 t1, a)
