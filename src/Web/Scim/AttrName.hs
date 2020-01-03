-- | Scim attribute names. these are case-insensitive
module Web.Scim.AttrName where

import Prelude hiding (takeWhile)
import Data.Text (Text, toCaseFold, cons)
import Data.Text.Encoding (decodeUtf8)
import Data.String (IsString, fromString)
import Data.Aeson.Types (ToJSON, FromJSON, ToJSONKey, FromJSONKey, toJSON, parseJSON)
import Data.Attoparsec.ByteString.Char8
import Data.Hashable 
-- | An attribute (e.g. username).
--
-- ATTRNAME  = ALPHA *(nameChar)
newtype AttrName
  = AttrName Text deriving (Show)

instance Eq AttrName where
  AttrName a == AttrName b = toCaseFold a == toCaseFold b

instance Ord AttrName where
  compare (AttrName a) (AttrName b) = compare (toCaseFold a) (toCaseFold b)

instance Hashable AttrName where
  hashWithSalt x (AttrName a) = hashWithSalt x (toCaseFold a)

instance IsString AttrName where
  fromString = AttrName . fromString

instance ToJSON AttrName where
  toJSON (AttrName a) = toJSON a

instance FromJSON AttrName where
  parseJSON x = AttrName <$> parseJSON x

instance ToJSONKey AttrName where

instance FromJSONKey AttrName where

-- | Attribute name parser.
pAttrName :: Parser AttrName
pAttrName =
  (\c str -> AttrName (cons c (decodeUtf8 str))) 
    <$> letter_ascii 
    <*> takeWhile (\x -> isDigit x || isAlpha_ascii x || x == '-' || x == '_')

-- | Attribute name renderer.
rAttrName :: AttrName -> Text
rAttrName (AttrName x) = x
