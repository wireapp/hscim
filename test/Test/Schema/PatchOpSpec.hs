module Test.Schema.PatchOpSpec (spec) where

import Data.Foldable (for_)
import Test.Hspec (Spec, describe, it, pending, shouldBe)
import Web.Scim.Test.Util (scim, post, put, patch)

spec :: Spec
spec = do
  -- NB: The hscim library itself does not _implement_ or force an implementation of PATCH spec for you.
  -- Instead, we have a set of acceptance tests that are parameterized
  describe "PATCH" $ do
    describe "urn:ietf:params:scim:schemas:core:2.0:ServiceProviderConfig" $ do
      it "Service provider support for HTTP PATCH may be discovered by querying the service provider configuration (see Section 4)." $
        -- NB: Actually Section 5 https://tools.ietf.org/html/rfc7643#section-5
        pending
    describe "urn:ietf:params:scim:api:messages:2.0:PatchOp" $ do
      it "The body of each request MUST contain the \"schemas\" attribute with the URI value of \"urn:ietf:params:scim:api:messages:2.0:PatchOp\"." $
        pending
      describe "Examples from https://tools.ietf.org/html/rfc7644#section-3.5.2 Figure 8" $ do
        let
          examples =
            [ ("members"
              , undefined
              )
            , ("name.familyName"
              , undefined
              )
            , ("addresses[type eq \"work\"]"
              , undefined
              )
            , ("members[value eq \"2819c223-7f76-453a-919d-413861904646\"]"
              , undefined
              )
            , ("members[value eq \"2819c223-7f76-453a-919d-413861904646\"].displayName"
              , undefined
              )
            ]
        for_ examples $ \(path, expected) -> it ("parses " ++ show path) $ pending -- parse path `shouldBe` expected
      describe "Each operation against an attribute MUST be compatible with the attribute's mutability and schema as defined in Sections 2.2 and 2.3 of [RFC7643]." $ do
        it "For example, a client MUST NOT modify an attribute that has mutability \"readOnly\" or \"immutable\"." $ pending
        it "However, a client MAY \"add\" a value to an \"immutable\" attribute if the attribute had no previous value." $ pending
        it "An operation that is not compatible with an attribute's mutability or schema SHALL return the appropriate HTTP response status code and a JSON detail error response as defined in Section 3.12." $ pending
      it "For multi-valued attributes, a PATCH operation that sets a  value's \"primary\" sub-attribute to \"true\" SHALL cause the server to automatically set \"primary\" to \"false\" for any other values in the array." $ pending
      it "A PATCH request, regardless of the number of operations, SHALL be treated as atomic.  If a single operation encounters an error condition, the original SCIM resource MUST be restored, and a failure status SHALL be returned." $ pending

      it "If a request fails, the server SHALL return an HTTP response status code and a JSON detail error response as defined in Section 3.12." $ pending
      it "On successful completion, the server either MUST return a 200 OK response code and the entire resource within the response body, subject to the \"attributes\" query parameter (see Section 3.9)," $ pending
      it "The server MUST return a 200 OK if the \"attributes\" parameter is specified in the request." $ pending

      describe "Operations" $ do
        it "Operations are case-insensitive" $ pending
        describe "Add" $
          describe "The result of the add operation depends upon what the target location indicated by \"path\" references:" $ do
            it "If omitted, the target location is assumed to be the resource itself.  The \"value\" parameter contains a set of attributes to be added to the resource." $ pending
            it "If the target location does not exist, the attribute and value are added." $ pending
            it "If the target location specifies a complex attribute, a set of sub-attributes SHALL be specified in the \"value\" parameter." $ pending
            it "If the target location specifies a multi-valued attribute, a new value is added to the attribute." $ pending
            it "If the target location specifies a single-valued attribute, the existing value is replaced." $ pending
            it "If the target location specifies an attribute that does not exist (has no value), the attribute is added with the new value." $ pending
            it "If the target location exists, the value is replaced." $ pending
            it "If the target location already contains the value specified, no changes SHOULD be made to the resource, and a success response SHOULD be returned.  Unless other operations change the resource, this operation SHALL NOT change the modify timestamp of the resource." $ pending
        -- describe "Remove" $ _
        -- describe "Modify" $ _
    
