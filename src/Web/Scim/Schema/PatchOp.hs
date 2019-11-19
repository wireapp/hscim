module Web.Scim.Schema.PatchOp where

newtype Patch = Patch
    { patchOperations :: [Operation] }

data Op
  = Add AddOp
  | Remove RemoveOp
  | Replace ReplaceOp

-- The "path" attribute value is a String containing an attribute path
-- describing the target of the operation.  The "path" attribute is OPTIONAL
-- for "add" and "replace" and is REQUIRED for "remove operations.  See
-- relevant operation sections below for details.
data AddOp = AddOp (Maybe Path)

data ReplaceOp = ReplaceOp (Maybe Path)

data RemoveOp = RemoveOp Path

data Path  = AttrPath | ValuePath (Maybe SubAttr)

data SubAttr
data OperationValue

data Operation = Operation
  { op :: Op
  , path :: Path
  , value :: OperationValue
  }


