-- | A query might specify a filter that should be applied to the results
-- before returning them. This module implements a very limited subset of
-- the specification: <https://tools.ietf.org/html/rfc7644#section-3.4.2.2>.
--
-- Supported:
--
-- * All comparison operators (@eq@, @le@, etc)
-- * The @userName@ attribute
--
-- Not supported:
--
-- * The @pr@ operator
-- * Boolean operators
-- * Combined filters
-- * Fully qualified attribute names (schema prefixes, attribute paths)

module Web.Scim.Filter
  (
  -- * Filter type
    Filter(..)
  , parseFilter
  , renderFilter

  -- * Filtering logic
  , filterUser

  -- * Constructing filters
  , CompValue(..)
  , CompareOp(..)
  , Attribute(..)
  , Path(Path)
  ) where

import Control.Applicative((<|>), optional)
import Data.Scientific
import Data.Text
import Data.Text.Encoding
import Data.Text.Lazy (toStrict)
import Data.Attoparsec.ByteString.Char8
import Data.Aeson.Parser as Aeson
import Data.Aeson.Text as Aeson
import Data.Aeson as Aeson
import Data.List.NonEmpty (NonEmpty)
import Web.Scim.Schema.Schema (Schema)
import Lens.Micro
import Web.HttpApiData

import Web.Scim.Schema.User
import Web.Scim.Schema.Schema (fromSchemaUri, pSchema)

import qualified Data.List.NonEmpty as NonEmpty

----------------------------------------------------------------------------
-- Types

-- NB: when extending these types, don't forget to update Test.FilterSpec

-- | A value type. Attributes are compared against literal values.
data CompValue
  = ValNull
  | ValBool Bool
  | ValNumber Scientific
  | ValString Text
  deriving (Eq, Ord, Show)

-- | A comparison operator.
data CompareOp
  = OpEq            -- ^ Equal
  | OpNe            -- ^ Not equal
  | OpCo            -- ^ Contains
  | OpSw            -- ^ Starts with
  | OpEw            -- ^ Ends with
  | OpGt            -- ^ Greater than
  | OpGe            -- ^ Greater than or equal to
  | OpLt            -- ^ Less than
  | OpLe            -- ^ Less than or equal to
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | An attribute (e.g. username).
--
-- Only usernames are supported as attributes. Paths are not supported.
data Attribute
  = AttrUserName
  | AttrDisplayName
  | AttrExternalId
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | A filter.
--
-- Our representation of filters is lax and doesn't attempt to ensure
-- validity on the type level. If a filter does something silly (e.g. tries
-- to compare a username with a boolean), it will be caught during filtering
-- and an appropriate error message will be thrown (see 'filterUser').
data Filter
  -- | Compare the attribute value with a literal
  = FilterAttrCompare Attribute CompareOp CompValue
  deriving (Eq, Show)


-- These are all WIP, to support more complex filters; not currently exported

-- | valuePath = attrPath "[" valFilter "]"
data ValuePath  = ValuePath AttrPath Filter
  deriving Show

-- | ATTRNAME  = ALPHA *(nameChar)
data AttrName = AttrName Text
  deriving Show

-- | subAttr   = "." ATTRNAME
data SubAttr = SubAttr AttrName
  deriving Show

-- | attrPath  = [URI ":"] ATTRNAME *1subAtt
data AttrPath = AttrPath (Maybe Schema) AttrName (NonEmpty SubAttr)
  deriving Show

-- | PATH = attrPath / valuePath [subAttr]
--
-- Currently we don't support matching on lists in paths as
-- we currently don't support filtering on arbitrary attributes yet
-- e.g. 
-- @
-- "path":"members[value eq
--            \"2819c223-7f76-453a-919d-413861904646\"].displayName"
-- @
-- is not supported
data Path
  = Path AttrPath 
  | SubAttrPath ValuePath (Maybe SubAttr)
  deriving Show

----------------------------------------------------------------------------
-- Parsing

-- | ATTRNAME  = ALPHA *(nameChar)
--
-- TODO: We simply support only a few whitelisted names now
pAttrName :: Parser AttrName
pAttrName = AttrName . decodeUtf8 <$> (stringCI "userName" <|> stringCI "displayName" <|> stringCI "externalId")

-- | An honest version of many1
many1'' :: Parser a -> Parser (NonEmpty a)
many1'' a = NonEmpty.fromList <$> many1 a

-- | attrPath  = [URI ":"] ATTRNAME *1subAtt
pAttrPath :: Parser AttrPath
pAttrPath =
  AttrPath 
    <$> (optional pSchema <* char ':')
    <*> pAttrName
    <*> many1'' pSubAttr


-- | subAttr   = "." ATTRNAME
pSubAttr :: Parser SubAttr
pSubAttr = char '.' *> (SubAttr <$> pAttrName)

-- | valuePath = attrPath "[" valFilter "]"
pValuePath :: Parser ValuePath
pValuePath =
  ValuePath <$> pAttrPath <*> (char '[' *> pFilter <* char ']')

-- | PATH = attrPath / valuePath [subAttr]
pPath :: Parser Path
pPath = 
  Path <$> pAttrPath <|> SubAttrPath <$> pValuePath <*> optional pSubAttr

-- Note: this parser is written with Attoparsec because I don't know how to
-- lift an Attoparsec parser (from Aeson) to Megaparsec

-- | Parse a filter. Spaces surrounding the filter will be stripped.
--
-- If parsing fails, returns a 'Left' with an error description.
parseFilter :: Text -> Either Text Filter
parseFilter =
  over _Left pack .
  parseOnly (skipSpace *> pFilter <* skipSpace <* endOfInput) .
  encodeUtf8

-- parser pieces

-- | Value literal parser.
pCompValue :: Parser CompValue
pCompValue = choice
  [ ValNull       <$  string "null"
  , ValBool True  <$  string "true"
  , ValBool False <$  string "false"
  , ValNumber     <$> Aeson.scientific
  , ValString     <$> Aeson.jstring
  ]

-- | Comparison operator parser.
--
-- TODO: Replace with asciiCI
pCompareOp :: Parser CompareOp
pCompareOp = choice
  [ OpEq <$ stringCI "eq"
  , OpNe <$ stringCI "ne"
  , OpCo <$ stringCI "co"
  , OpSw <$ stringCI "sw"
  , OpEw <$ stringCI "ew"
  , OpGt <$ stringCI "gt"
  , OpGe <$ stringCI "ge"
  , OpLt <$ stringCI "lt"
  , OpLe <$ stringCI "le"
  ]

-- | Attribute name parser.
pAttribute :: Parser Attribute
pAttribute = choice
  [ AttrUserName <$ stringCI "userName"
  , AttrDisplayName <$ stringCI "displayName"
  , AttrExternalId <$ stringCI "externalId"
  ]

-- | Filter parser.
pFilter :: Parser Filter
pFilter = choice
  [ FilterAttrCompare
      <$> pAttribute
      <*> (skipSpace1 *> pCompareOp)
      <*> (skipSpace1 *> pCompValue)
  ]

-- | Utility parser for skipping one or more spaces.
skipSpace1 :: Parser ()
skipSpace1 = space *> skipSpace

----------------------------------------------------------------------------
-- Rendering

-- | Render a filter according to the SCIM spec.
renderFilter :: Filter -> Text
renderFilter filter_ = case filter_ of
  FilterAttrCompare attr op val ->
    rAttribute attr <> " " <> rCompareOp op <> " " <> rCompValue val

-- | Value literal renderer.
rCompValue :: CompValue -> Text
rCompValue = \case
  ValNull       -> "null"
  ValBool True  -> "true"
  ValBool False -> "false"
  ValNumber n   -> toStrict $ Aeson.encodeToLazyText (Aeson.Number n)
  ValString s   -> toStrict $ Aeson.encodeToLazyText (Aeson.String s)

-- | Comparison operator renderer.
rCompareOp :: CompareOp -> Text
rCompareOp = \case
  OpEq -> "eq"
  OpNe -> "ne"
  OpCo -> "co"
  OpSw -> "sw"
  OpEw -> "ew"
  OpGt -> "gt"
  OpGe -> "ge"
  OpLt -> "lt"
  OpLe -> "le"

-- | Attribute name renderer.
rAttribute :: Attribute -> Text
rAttribute = \case
  AttrUserName -> "userName"
  AttrDisplayName -> "displayName"
  AttrExternalId -> "externalId"

----------------------------------------------------------------------------
-- Applying

-- | Check whether a user satisfies the filter.
--
-- Returns 'Left' if the filter is constructed incorrectly (e.g. tries to
-- compare a username with a boolean).
filterUser :: Filter -> User extra -> Either Text Bool
filterUser filter_ user = case filter_ of
  FilterAttrCompare AttrUserName op (ValString str) ->
    -- Comparing usernames has to be case-insensitive; look at the
    -- 'caseExact' parameter of the user schema
    Right (compareStr op (toCaseFold (userName user)) (toCaseFold str))
  FilterAttrCompare AttrUserName _ _ ->
    Left "usernames can only be compared with strings"

-- | Execute a comparison operator.
compareStr :: CompareOp -> Text -> Text -> Bool
compareStr = \case
  OpEq -> (==)                -- equal
  OpNe -> (/=)                -- not equal
  OpCo -> flip isInfixOf      -- A contains B
  OpSw -> flip isPrefixOf     -- A starts with B
  OpEw -> flip isSuffixOf     -- A ends with B
  OpGt -> (>)                 -- greater than
  OpGe -> (>=)                -- greater than or equal to
  OpLt -> (<)                 -- less than
  OpLe -> (<=)                -- less than or equal to

----------------------------------------------------------------------------
-- Instances

instance FromHttpApiData Filter where
  parseUrlPiece = parseFilter

instance ToHttpApiData Filter where
  toUrlPiece = renderFilter
