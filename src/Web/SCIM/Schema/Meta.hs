
module Web.SCIM.Schema.Meta where

import Prelude hiding (map)

import Data.Text (Text)
import Data.Aeson
import Web.SCIM.Schema.Common
import GHC.Generics (Generic)
-- TODO: move to Meta.hs
import qualified Data.HashMap.Lazy as HML
import Data.Time.Clock
import Data.Monoid ((<>))

-- TODO: Move to Common/Meta
data WithId a = WithId
  { id :: Text
  , value :: a
  } deriving (Eq, Show)

instance (ToJSON a) => ToJSON (WithId a) where
  toJSON (WithId i v) = case toJSON v of
    (Object o) -> Object (HML.insert "id" (String i) o)
    other      -> other

instance (FromJSON a) => FromJSON (WithId a) where
  parseJSON = withObject "WithId" $ \o -> WithId
    <$> o .: "id"
    <*> (parseJSON $ Object o)

data ResourceType = UserResource
                  | GroupResource
  deriving (Eq, Show)

instance ToJSON ResourceType where
  toJSON UserResource = "User"
  toJSON GroupResource = "Group"

instance FromJSON ResourceType where
  parseJSON "User" = pure UserResource
  parseJSON "Group" = pure GroupResource
  parseJSON t = fail $ "Unknown resource type: " <> (show t)

data ETag = Weak Text | Strong Text
  deriving (Eq, Show)

instance ToJSON ETag where
  toJSON (Weak tag) = String $ "W/\"" `mappend` tag `mappend` "\""
  toJSON (Strong tag) = String $ "\"" `mappend` tag `mappend` "\""

instance FromJSON ETag where
  parseJSON = undefined

data Meta = Meta
  { resourceType :: ResourceType
  , created :: UTCTime
  , lastModified :: UTCTime
  , version :: ETag
  , location :: URI
  } deriving (Eq, Show, Generic)

instance ToJSON Meta
instance FromJSON Meta

data WithMeta a = WithMeta
  { meta :: Meta
  , thing :: a
  } deriving (Eq, Show, Generic)

instance (ToJSON a) => ToJSON (WithMeta a) where
  toJSON (WithMeta m v) = case toJSON v of
    (Object o) -> Object (HML.insert "meta" (toJSON m) o)
    other      -> other

instance (FromJSON a) => FromJSON (WithMeta a) where
  parseJSON = withObject "WithMeta" $ \o -> WithMeta
    <$> o .: "meta"
    <*> (parseJSON $ Object o)
