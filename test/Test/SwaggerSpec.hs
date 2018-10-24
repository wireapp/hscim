module Test.SwaggerSpec (spec) where

import Data.Proxy
import Data.Typeable
import Data.Aeson
import Data.Swagger
import Test.QuickCheck
import Test.Hspec
import Servant.API (NoContent)

import Servant.Swagger.Internal.Test as Swagger
import Servant.Swagger.Internal.TypeLevel as Swagger

import Web.SCIM.Server (SiteAPI)
import Web.SCIM.ContentType (SCIM)

spec :: Spec
spec = do
    validateEveryToSCIM (Proxy @SiteAPI)

----------------------------------------------------------------------------
-- Utils

-- | Like 'validateEveryToJSON', but looks for the 'SCIM' content type
-- instead of 'JSON' content type.
validateEveryToSCIM
    :: forall proxy api.
       TMap (Every [Typeable, Show, Arbitrary, ToJSON, ToSchema])
            (BodyTypes SCIM api)
    => proxy api
    -> Spec
validateEveryToSCIM _ = Swagger.props
  (Proxy :: Proxy [ToJSON, ToSchema])
  (null . validateToJSON)
  (Proxy :: Proxy (BodyTypes SCIM api))

----------------------------------------------------------------------------
-- Orphan instances

instance ToJSON NoContent where
    toJSON _ = toJSON ("NoContent" :: String)
