module Web.Scim.Schema.PatchOp where

import Control.Applicative
import Control.Monad (guard)
import Web.Scim.Schema.Schema (Schema(PatchOp20))
import Data.Aeson.Types (Value, withText, FromJSON(parseJSON), withObject, (.:), Value)
import qualified Data.HashMap.Strict as HashMap
import Data.Text (toLower, Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Bifunctor (first)
import Data.Attoparsec.ByteString (Parser, parseOnly)
import Web.Scim.Filter (AttrPath(..), ValuePath(..), SubAttr(..), pAttrPath, pValuePath, pSubAttr, rAttrPath, rSubAttr, rValuePath)
import Data.Maybe (fromMaybe)

newtype PatchOp = PatchOp
  { getOperations :: [Operation] }
  deriving Show
    
-- TODO(arianvp):  When value is an array, it needs special handling.
-- e.g. primary fields need to be negated and whatnot. 
-- We currently do not do that :)
data Operation = Operation
  { op :: Op 
  , path :: Maybe Path
  , value :: Maybe Value
  } deriving (Show)

-- The "path" attribute value is a String containing an attribute path
-- describing the target of the operation.  The "path" attribute is OPTIONAL
-- for "add" and "replace" and is REQUIRED for "remove operations.  See
-- relevant operation sections below for details.
data Op
  = Add
  | Replace 
  | Remove
  deriving Show

instance FromJSON Op where
  parseJSON = withText "Op" $ \op' ->
    case toCaseFold op' of
      "add" -> pure Add
      "replace" -> pure Replace
      "remove" -> pure Remove
      _ -> fail "unknown operation"

-- | PATH = attrPath / valuePath [subAttr]
data Path
  = NormalPath AttrPath 
  | IntoValuePath ValuePath (Maybe SubAttr)
  deriving (Eq, Show)

instance  FromJSON Path where
  parseJSON = withText "Path" $ \v -> case parseOnly pPath (encodeUtf8 v) of
    Left x -> fail x
    Right x -> pure x

-- | PATH = attrPath / valuePath [subAttr]
pPath :: Parser Path
pPath = 
  IntoValuePath <$> pValuePath <*> optional pSubAttr <|>
  NormalPath <$> pAttrPath

rPath :: Path -> Text
rPath (NormalPath attrPath) = rAttrPath attrPath
rPath (IntoValuePath valuePath subAttr) = rValuePath valuePath <> fromMaybe "" (rSubAttr <$> subAttr)


instance FromJSON PatchOp where
  parseJSON = withObject "PatchOp" $ \v -> do
    let o = HashMap.fromList . map (first toLower) . HashMap.toList $ v
    schemas :: [Schema] <- o .: "schemas"
    guard $ PatchOp20 `elem` schemas
    operations <- o .: "operations"
    pure $ PatchOp operations


-- NOTE: Azure wants us to be case-insensitive on _values_ as well here
instance FromJSON Operation where
  parseJSON = withObject "Operation" $ \v -> do
    let o = HashMap.fromList . map (first toLower) . HashMap.toList $ v
    Operation <$> (o .: "op") <*> (o .: "path") <*> (o .: "value")

    


