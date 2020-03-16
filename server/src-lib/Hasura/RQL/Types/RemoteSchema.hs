module Hasura.RQL.Types.RemoteSchema where

import           Control.Lens
import           Hasura.Prelude
import           Hasura.RQL.Types.Common    (NonEmptyText)
import           Language.Haskell.TH.Syntax (Lift)
import           System.Environment         (lookupEnv)

import qualified Data.Aeson                 as J
import qualified Data.Aeson.Casing          as J
import qualified Data.Aeson.TH              as J
import qualified Data.Text                  as T
import qualified Database.PG.Query          as Q
import qualified Network.URI.Extended       as N

import           Hasura.Incremental         (Cacheable)
import           Hasura.RQL.DDL.Headers     (HeaderConf (..))
import           Hasura.RQL.Types.Error
import           Hasura.SQL.Types           (DQuote)

type UrlFromEnv = Text

newtype RemoteSchemaName
  = RemoteSchemaName
  { unRemoteSchemaName :: NonEmptyText }
  deriving ( Show, Eq, Ord, Lift, Hashable, J.ToJSON, J.ToJSONKey
           , J.FromJSON, Q.ToPrepArg, Q.FromCol, DQuote, NFData
           , Generic, Cacheable, Arbitrary
           )

data RemoteSchemaInfo
  = RemoteSchemaInfo
  { rsUrl              :: !N.URI
  , rsHeaders          :: ![HeaderConf]
  , rsFwdClientHeaders :: !Bool
  , rsTimeoutSeconds   :: !Int
  } deriving (Show, Eq, Lift, Generic)

instance Hashable RemoteSchemaInfo

$(J.deriveJSON (J.aesonDrop 2 J.snakeCase) ''RemoteSchemaInfo)

data RemoteSchemaDef
  = RemoteSchemaDef
  { _rsdUrl                  :: !(Maybe N.URI)
  , _rsdUrlFromEnv           :: !(Maybe UrlFromEnv)
  , _rsdHeaders              :: !(Maybe [HeaderConf])
  , _rsdForwardClientHeaders :: !Bool
  , _rsdTimeoutSeconds       :: !(Maybe Int)
  } deriving (Show, Eq, Lift, Generic)
instance NFData RemoteSchemaDef
instance Cacheable RemoteSchemaDef
$(J.deriveToJSON (J.aesonDrop 4 J.snakeCase){J.omitNothingFields=True} ''RemoteSchemaDef)

instance J.FromJSON RemoteSchemaDef where
  parseJSON = J.withObject "Object" $ \o ->
    RemoteSchemaDef
      <$> o J..:? "url"
      <*> o J..:? "url_from_env"
      <*> o J..:? "headers"
      <*> o J..:? "forward_client_headers" J..!= False
      <*> o J..:? "timeout_seconds"

data AddRemoteSchemaQuery
  = AddRemoteSchemaQuery
  { _arsqName       :: !RemoteSchemaName
  , _arsqDefinition :: !RemoteSchemaDef
  , _arsqComment    :: !(Maybe Text)
  } deriving (Show, Eq, Lift, Generic)
instance NFData AddRemoteSchemaQuery
instance Cacheable AddRemoteSchemaQuery
$(J.deriveJSON (J.aesonDrop 5 J.snakeCase) ''AddRemoteSchemaQuery)

newtype RemoteSchemaNameQuery
  = RemoteSchemaNameQuery
  { _rsnqName    :: RemoteSchemaName
  } deriving (Show, Eq, Lift)

$(J.deriveJSON (J.aesonDrop 5 J.snakeCase) ''RemoteSchemaNameQuery)

getUrlFromEnv :: (MonadIO m, MonadError (QErr code) m, AsCodeHasura code) => Text -> m N.URI
getUrlFromEnv urlFromEnv = do
  mEnv <- liftIO . lookupEnv $ T.unpack urlFromEnv
  env  <- maybe (throw400 (review _InvalidParams ()) $ envNotFoundMsg urlFromEnv) return
          mEnv
  maybe (throw400 (review _InvalidParams ()) $ invalidUri env) return $ N.parseURI env
  where
    invalidUri uri = "not a valid URI: " <> T.pack uri
    envNotFoundMsg e =
      "environment variable '" <> e <> "' not set"

validateRemoteSchemaDef
  :: (MonadError (QErr code) m, AsCodeHasura code, MonadIO m)
  => RemoteSchemaDef
  -> m RemoteSchemaInfo
validateRemoteSchemaDef (RemoteSchemaDef mUrl mUrlEnv hdrC fwdHdrs mTimeout) =
  case (mUrl, mUrlEnv) of
    (Just url, Nothing)    ->
      return $ RemoteSchemaInfo url hdrs fwdHdrs timeout
    (Nothing, Just urlEnv) -> do
      url <- getUrlFromEnv urlEnv
      return $ RemoteSchemaInfo url hdrs fwdHdrs timeout
    (Nothing, Nothing)     ->
        throw400 (_InvalidParams # ()) "both `url` and `url_from_env` can't be empty"
    (Just _, Just _)       ->
        throw400 (review _InvalidParams ()) "both `url` and `url_from_env` can't be present"
  where
    hdrs = fromMaybe [] hdrC

    timeout = fromMaybe 60 mTimeout
