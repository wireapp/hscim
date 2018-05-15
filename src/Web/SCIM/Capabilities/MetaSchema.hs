{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE ConstraintKinds   #-}

{-|
Module:      Web.SCIM.Capabilities.MetaSchema
Description: endpoints for querying supported SCIM features

This module implements the config endpoints, which let SCIM clients
query the server for supported features, see 'Web.SCIM.Server.app' for
examples of usage.

-}

module Web.SCIM.Capabilities.MetaSchema (
  ConfigAPI
  , configServer
  , BulkConfig (..)
  , FilterConfig (..)
  , AuthenticationScheme (..)
  , Configuration (..)
  ) where

import           Web.SCIM.Capabilities.MetaSchema.User
import           Web.SCIM.Capabilities.MetaSchema.SPConfig
import           Web.SCIM.Capabilities.MetaSchema.Group
import           Web.SCIM.Capabilities.MetaSchema.Schema
import           Web.SCIM.Capabilities.MetaSchema.ResourceType
import           Web.SCIM.Schema.Common (URI)
import           Control.Monad.Except
import           Control.Error.Util (note)
import           Data.Aeson
import           Data.Text (Text)
import           GHC.Generics (Generic)
import           Servant hiding (URI)
import           Servant.Generic

import           Prelude hiding (filter)

{-| Configuration of bulk requests, as described in
    https://tools.ietf.org/html/rfc7644#section-3.7 -}
data BulkConfig = BulkConfig
  { bulkSupported :: Bool
  , maxOperations :: Int
  , maxPayloadSize :: Int
  } deriving (Show, Eq)

instance ToJSON BulkConfig where
  toJSON (BulkConfig supported maxOps maxSize) =
    object [ "supported" .= supported
           , "maxOperations" .= maxOps
           , "maxPayloadSize" .= maxSize
           ]

instance FromJSON BulkConfig where
  parseJSON = undefined

{-| Configuration of query filtering, as described in
    https://tools.ietf.org/html/rfc7644#section-3.4.2.2 -}
data FilterConfig = FilterConfig
  { filterSupported :: Bool
  , maxResults :: Int
  } deriving (Show, Eq)

instance ToJSON FilterConfig where
  toJSON (FilterConfig supported maxRes) =
    object [ "supported" .= supported
           , "maxResults" .= maxRes
           ]

instance FromJSON FilterConfig where
  parseJSON = undefined

{-| Configuration of supported authentication schemes. As an example,
  OAuth Bearer Tokens can be described as:

  @ AuthenticationScheme
  { authType = "oauthbearertoken"
  , name = "OAuth Bearer Token"
  , description = "Authentication scheme using the OAuth Bearer Token Standard"
  , specUri = "http://www.rfc-editor.org/info/rfc6750"
  , primary = True
  }
  @
-}
data AuthenticationScheme = AuthenticationScheme
  { authType :: Text
  , name :: Text
  , description :: Text
  , specUri :: URI
  , documentation :: URI
  , primary :: Bool
  } deriving (Show, Eq)

instance ToJSON AuthenticationScheme where
  toJSON (AuthenticationScheme typ' name' desc spec doc pri) =
    object [ "type" .= typ'
           , "name" .= name'
           , "description" .= desc
           , "specification" .= spec
           , "documentationUri" .= doc
           , "primary" .= pri
           ]

instance FromJSON AuthenticationScheme where
  parseJSON = undefined

{-| The server configuration. This will be served at the @/ServiceProviderConfig@
  endpoint, as described in https://tools.ietf.org/html/rfc7644#section-4 and
  https://tools.ietf.org/html/rfc7643#section-5.
-}
data Configuration = Configuration
  { documentationUri :: Maybe URI
    {-^ URL pointing to the service provider's human-consumable help
         documentation -}
  , patch :: Bool
  -- ^ server supports HTTP PATCH
  , bulk :: BulkConfig
  , filter :: FilterConfig
  , changePassword :: Bool
  -- ^ clients are allowed to change passwords
  , sort :: Bool
  -- ^ server supports sorting
  , etag :: Bool
  -- ^ server supports HTTP Etag
  , authenticationSchemes :: [AuthenticationScheme]
  -- ^ supported authentication schemes
  } deriving (Show, Eq)

instance ToJSON Configuration where
  toJSON (Configuration uri patch' bulk' filter' pw sort' etag' schemes) =
    object [ "documentationUri" .= uri
           , "patch" .= mkSupported patch'
           , "bulk" .= toJSON bulk'
           , "filter" .= toJSON filter'
           , "changePassword" .= mkSupported pw
           , "sort" .= mkSupported sort'
           , "etag" .= mkSupported etag'
           , "authenticationSchemes" .= (toJSON <$> schemes)
           ]

instance FromJSON Configuration where
  parseJSON = undefined

mkSupported :: Bool -> Value
mkSupported b = object [ "supported" .= b ]

{-| A servant server for the configuration endpoints described in
  https://tools.ietf.org/html/rfc7644#section-4.
-}
configServer :: MonadError ServantErr m =>
                Configuration -> ConfigAPI (AsServerT m)
configServer config = ConfigAPI
  { spConfig = pure config
  , schemas = pure [ userSchema
                   , spConfigSchema
                   , groupSchema
                   , metaSchema
                   , resourceSchema
                   ]
  , schema = liftEither . note err404 . getSchema
  , resourceTypes = pure ""
  }

getSchema :: Text -> Maybe Value
getSchema "urn:ietf:params:scim:schemas:core:2.0:ServiceProviderConfig" =
  pure spConfigSchema
getSchema "urn:ietf:params:scim:schemas:core:2.0:User" =
  pure userSchema
getSchema "urn:ietf:params:scim:schemas:core:2.0:Group" =
  pure groupSchema
getSchema "urn:ietf:params:scim:schemas:core:2.0:Schema" =
  pure metaSchema
getSchema "urn:ietf:params:scim:schemas:core:2.0:ResourceType" =
  pure resourceSchema
getSchema _ = Nothing 

data ConfigAPI route = ConfigAPI
  { spConfig :: route :- "ServiceProviderConfig" :> Get '[JSON] Configuration
  , schemas :: route :- "Schemas" :> Get '[JSON] [Value]
  , schema :: route :- "Schemas" :> Capture "id" Text :> Get '[JSON] Value
  , resourceTypes :: route :- "ResourceTypes" :> Get '[JSON] Text
  } deriving (Generic)

