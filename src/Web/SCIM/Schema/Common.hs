module Web.SCIM.Schema.Common where

import Control.Applicative
import Data.Text hiding (dropWhile)
import Data.Aeson
import qualified Data.Char as Char
import qualified Data.HashMap.Strict as HM
import Data.String (IsString)
import qualified Network.URI as Network
import GHC.Generics (Generic)


newtype URI = URI { unURI :: Network.URI }
  deriving (Show, Eq)

instance FromJSON URI where
  parseJSON = withText "URI" $ \uri -> case Network.parseURI (unpack uri) of
    Nothing -> fail "Invalid URI"
    Just som -> pure $ URI som

instance ToJSON URI where
  toJSON (URI uri) = String $ pack $ show uri

toKeyword :: (IsString p, Eq p) => p -> p
toKeyword "typ" = "type"
toKeyword "ref" = "$ref"
toKeyword other = other

serializeOptions :: Options
serializeOptions = defaultOptions
  { omitNothingFields = True
  , fieldLabelModifier = toKeyword
  }

-- | Turn all keys in a JSON object to lowercase.
jsonLower :: Value -> Value
jsonLower (Object o) = Object . HM.fromList . fmap lowerPair . HM.toList $ o
  where lowerPair (key, val) = (fromKeyword . toLower $ key, val)
jsonLower x = x

fromKeyword :: (IsString p, Eq p) => p -> p
fromKeyword "type" = "typ"
fromKeyword "$ref" = "ref"
fromKeyword other = other

parseOptions :: Options
parseOptions = defaultOptions
  { fieldLabelModifier = fmap Char.toLower
  }

newtype Unsettable a = Maybe (Maybe a)
  deriving (Show, Eq, Generic, Functor, Applicative, Alternative)

instance (ToJSON a) => ToJSON (Unsettable a)
instance (FromJSON a) => FromJSON (Unsettable a)
