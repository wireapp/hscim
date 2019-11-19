{-# LANGUAGE QuasiQuotes #-}
-- | A bunch of hspec acceptance tests that assert that your SCIM
-- implementation is compatible with popular SCIM 2.0 providers
module Web.Scim.Test.Acceptance where

import Web.Scim.Test.Util (scim, post,  put, patch, get', post', put', patch')
import Test.Hspec (Spec, it, beforeAll, pending, describe, shouldBe)
import Test.Hspec.Wai (shouldRespondWith, delete, get, matchStatus)
import Network.Wai (Application)


-- https://docs.microsoft.com/en-us/azure/active-directory/manage-apps/use-scim-to-provision-users-and-groups#step-2-understand-the-azure-ad-scim-implementation
microsoftAzure :: IO Application -> Spec
microsoftAzure app = do
  describe "Within the SCIM 2.0 protocol specification, your application must meet these requirements:" $ do
    it "Supports creating users, and optionally also groups, as per section 3.3 of the SCIM protocol." $ pending
    it "Supports modifying users or groups with PATCH requests, as per section 3.5.2 of the SCIM protocol." $ pending
    it "Supports retrieving a known resource for a user or group created earlier, as per section 3.4.1 of the SCIM protocol." $ pending
    it "Supports querying users or groups, as per section 3.4.2 of the SCIM protocol. By default, users are retrieved by their id and queried by their username and externalid, and groups are queried by displayName." $ pending
    it "Supports querying user by ID and by manager, as per section 3.4.2 of the SCIM protocol." $ pending
    it "Supports querying groups by ID and by member, as per section 3.4.2 of the SCIM protocol." $ pending
    it "Accepts a single bearer token for authentication and authorization of Azure AD to your application." $ pending

  describe "Follow these general guidelines when implementing a SCIM endpoint to ensure compatibility with Azure AD:" $ do
    it "id is a required property for all the resources. Every response that returns a resource should ensure each resource has this property, except for ListResponse with zero members." $ pending
    it "Response to a query/filter request should always be a ListResponse." $ pending
    it "Groups are optional, but only supported if the SCIM implementation supports PATCH requests." $ pending
    it "Don't require a case-sensitive match on structural elements in SCIM, in particular PATCH op operation values, as defined in https://tools.ietf.org/html/rfc7644#section-3.5.2. Azure AD emits the values of 'op' as Add, " $ pending
    it "Microsoft Azure AD only uses the following operators: eq and" $ pending

  beforeAll app $ do
    describe "User Operations" $ do
      it "POST /Users" $ do
        let user = [scim|
          {
            "schemas": [
                "urn:ietf:params:scim:schemas:core:2.0:User",
                "urn:ietf:params:scim:schemas:extension:enterprise:2.0:User"],
            "externalId": "0a21f0f2-8d2a-4f8e-bf98-7363c4aed4ef",
            "userName": "Test_User_ab6490ee-1e48-479e-a20b-2d77186b5dd1",
            "active": true,
            "emails": [{
                    "primary": true,
                    "type": "work",
                    "value": "Test_User_fd0ea19b-0777-472c-9f96-4f70d2226f2e@testuser.com"
            }],
            "meta": {
                    "resourceType": "User"
            },
            "name": {
                    "formatted": "givenName familyName",
                    "familyName": "familyName",
                    "givenName": "givenName"
            },
            "roles": []
          }
        |]
        -- TODO Slightly modify
        post' "/Users" user `shouldRespondWith` [scim|
          {
            "schemas": ["urn:ietf:params:scim:schemas:core:2.0:User"],
            "id": "48af03ac28ad4fb88478",
            "externalId": "0a21f0f2-8d2a-4f8e-bf98-7363c4aed4ef",
            "meta": {
                    "resourceType": "User",
                    "created": "2018-03-27T19:59:26.000Z",
                    "lastModified": "2018-03-27T19:59:26.000Z"
            },
            "userName": "Test_User_ab6490ee-1e48-479e-a20b-2d77186b5dd1",
            "name": {
                    "formatted": "givenName familyName",
                    "familyName": "familyName",
                    "givenName": "givenName"
            },
            "active": true,
            "emails": [{
                    "value": "Test_User_fd0ea19b-0777-472c-9f96-4f70d2226f2e@testuser.com",
                    "type": "work",
                    "primary": true
            }]
          }
        |] { matchStatus = 201 }
      it "Get user by query, zero results" $ do
        get' "/Users?filter=userName eq \"non-existent user\"" `shouldRespondWith` [scim|
          {
            "schemas": ["urn:ietf:params:scim:api:messages:2.0:ListResponse"],
            "totalResults": 0,
            "Resources": [],
            "startIndex": 1,
            "itemsPerPage": 20
          }
        |] { matchStatus = 200 }

