module Web.Scim.Schema.PatchOp where

import Control.Applicative
import Control.Monad (guard)
import Web.Scim.Schema.Schema (Schema(PatchOp20), getSchemaUri,fromSchemaUri)
import Data.Aeson.Types (Value, FromJSON(parseJSON), ToJSON, withObject, (.:), Value)
import qualified Data.HashMap.Strict as HashMap
import Data.Text (toLower, Text)
import Data.Bifunctor (first)
import Data.Attoparsec.Text (Parser)
import Web.Scim.Filter (Path)

newtype PatchOp = PatchOp
  { patchOperations :: [Operation] }
  deriving Show

instance FromJSON PatchOp where
  parseJSON = withObject "PatchOp" $ \v -> do
    let o = HashMap.fromList . map (first toLower) . HashMap.toList $ v
    schemas :: [Schema] <- o .: "schemas"
    guard $ PatchOp20 `elem` schemas
    operations <- o .: "operations"
    pure $ PatchOp operations
    

data Operation = Operation
  { op :: Op
  , path :: Path
  , value :: OperationValue
  } deriving Show

-- NOTE: Azure wants us to be case-insensitive on _values_ as well here
instance FromJSON Operation where
  parseJSON = undefined

data Op
  = Add AddOp
  | Remove RemoveOp
  | Replace ReplaceOp
  deriving Show


-- The "path" attribute value is a String containing an attribute path
-- describing the target of the operation.  The "path" attribute is OPTIONAL
-- for "add" and "replace" and is REQUIRED for "remove operations.  See
-- relevant operation sections below for details.
data AddOp = AddOp (Maybe Path)
  deriving Show

data ReplaceOp = ReplaceOp (Maybe Path)
  deriving Show

data RemoveOp = RemoveOp Path
  deriving Show



data OperationValue 
  = Singular Value
  | MultiValue Value 
  deriving Show
