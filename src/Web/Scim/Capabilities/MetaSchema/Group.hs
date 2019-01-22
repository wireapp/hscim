{-# LANGUAGE QuasiQuotes #-}

module Web.Scim.Capabilities.MetaSchema.Group (groupSchema) where

import Data.Aeson (Value)
import Data.Aeson.QQ

-- NB: errata for the SCIM RFC says that 'displayName' should have
-- 'required: true'. This makes sense but we're not going to change this for
-- now because nobody seems to care about these schemas anyway.
-- See https://www.rfc-editor.org/errata_search.php?rfc=7643
groupSchema :: Value
groupSchema = [aesonQQ|
{
  "id": "urn:ietf:params:scim:schemas:core:2.0:Group",
  "name": "Group",
  "description": "Group",
  "attributes": [
    {
      "name": "displayName",
      "type": "string",
      "multiValued": false,
      "description": "A human-readable name for the Group. REQUIRED.",
      "required": false,
      "caseExact": false,
      "mutability": "readWrite",
      "returned": "default",
      "uniqueness": "none"
    },
    {
      "name": "members",
      "type": "complex",
      "multiValued": true,
      "description": "A list of members of the Group.",
      "required": false,
      "subAttributes": [
        {
          "name": "value",
          "type": "string",
          "multiValued": false,
          "description": "Identifier of the member of this Group.",
          "required": false,
          "caseExact": false,
          "mutability": "immutable",
          "returned": "default",
          "uniqueness": "none"
        },
        {
          "name": "$ref",
          "type": "reference",
          "referenceTypes": [
            "User",
            "Group"
          ],
          "multiValued": false,
          "description": "The URI corresponding to a SCIM resource that is a member of this Group.",
          "required": false,
          "caseExact": false,
          "mutability": "immutable",
          "returned": "default",
          "uniqueness": "none"
        },
        {
          "name": "type",
          "type": "string",
          "multiValued": false,
          "description": "A label indicating the type of resource, e.g., 'User' or 'Group'.",
          "required": false,
          "caseExact": false,
          "canonicalValues": [
            "User",
            "Group"
          ],
          "mutability": "immutable",
          "returned": "default",
          "uniqueness": "none"
        }
      ],
      "mutability": "readWrite",
      "returned": "default"
    }
  ],
  "meta": {
    "resourceType": "Schema",
    "location": "/v2/Schemas/urn:ietf:params:scim:schemas:core:2.0:Group"
  }
}
|]