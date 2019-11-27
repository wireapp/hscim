module Web.Scim.Schema.PatchOp where

import Control.Applicative
import Control.Monad (guard)
import Web.Scim.Schema.Schema (Schema(PatchOp20), getSchemaUri,fromSchemaUri)
import Data.Aeson.Types (Value, withText, FromJSON(parseJSON), ToJSON, withObject, (.:), Value)
import qualified Data.HashMap.Strict as HashMap
import Data.Text (toLower, Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Bifunctor (first)
import Data.Attoparsec.ByteString (Parser, parseOnly)
import Web.Scim.Filter (AttrPath(..), ValuePath(..), SubAttr(..), pAttrPath, pValuePath, pSubAttr, rAttrPath, rSubAttr, rValuePath)
import Data.Maybe (fromMaybe)

newtype PatchOp = PatchOp
  { patchOperations :: [Operation] }
  deriving Show
    

data Operation = Operation
  { op :: Op 
  , value :: OperationValue
  } deriving (Show)

-- The "path" attribute value is a String containing an attribute path
-- describing the target of the operation.  The "path" attribute is OPTIONAL
-- for "add" and "replace" and is REQUIRED for "remove operations.  See
-- relevant operation sections below for details.
data Op
  = Add (Maybe Path)
  | Replace (Maybe Path)
  | Remove Path
  deriving Show



data OperationValue 
  = Singular Value
  | MultiValue Value 
  deriving Show
  
-- | PATH = attrPath / valuePath [subAttr]
data Path
  = NormalPath AttrPath 
  | IntoValuePath ValuePath (Maybe SubAttr)
  deriving (Eq, Show)

instance  FromJSON Path where
  parseJSON = withText "Path" $ \path -> case parseOnly pPath (encodeUtf8 path) of
    Left x -> fail x
    Right x -> pure x

instance FromJSON OperationValue where
  parseJSON = undefined

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
    op' <- o .: "op" >>= \x -> case toLower x of
      "add" -> Add <$> optional (o .: "path")
      "remove" -> Remove <$> (o .: "path")
      "replace" -> Replace <$> optional (o .: "path")
      _ -> fail $ "Unsupported patch operation"
    val <- o .: "value"
    pure $ Operation op' val

    


