{-# LANGUAGE QuasiQuotes #-}
module Test.Schema.PatchOpSpec where
import Data.Foldable (for_)
import Test.Hspec (Spec, describe, it, shouldSatisfy)
import Data.Text.Encoding (encodeUtf8)
import Web.Scim.Test.Util (scim)
import Web.Scim.Schema.PatchOp
import Data.Aeson.Types (fromJSON, Result(Success, Error))
import Data.Attoparsec.ByteString (parseOnly)
import HaskellWorks.Hspec.Hedgehog (require)
import Hedgehog (Gen, tripping, forAll, Property, property)
import qualified Hedgehog.Gen as Gen
import Test.FilterSpec (genValuePath, genAttrPath, genSubAttr)
import Data.Either (isRight)

isSuccess :: Result a -> Bool
isSuccess (Success _) = True
isSuccess (Error _) = False

genPath :: Gen Path
genPath = Gen.choice
  [ IntoValuePath <$> genValuePath <*> Gen.maybe genSubAttr
  , NormalPath <$> genAttrPath
  ]

--TODO(arianvp): We don't support arbitrary path names (yet)
prop_roundtrip :: Property
prop_roundtrip = property $ do
  x <- forAll genPath
  tripping x (encodeUtf8 . rPath) (parseOnly pPath)
  
spec :: Spec
spec = do
  describe "urn:ietf:params:scim:api:messages:2.0:PatchOp" $ do
    describe "The body of each request MUST contain the \"schemas\" attribute with the URI value of \"urn:ietf:params:scim:api:messages:2.0:PatchOp\"." $ do
      it "rejects an empty schemas list" $ do
        fromJSON @PatchOp [scim| { 
          "schemas": [],
          "operations": []
        }|] `shouldSatisfy`  (not . isSuccess)
    --TODO(arianvp): We don't support arbitrary path names (yet)
    it "roundtrips" $ require prop_roundtrip
    describe "Examples from https://tools.ietf.org/html/rfc7644#section-3.5.2 Figure 8" $ do
      let
        examples =
          [ "members"
          , "name.familyName"
          , "addresses[type eq \"work\"]"
          , "members[value eq \"2819c223-7f76-453a-919d-413861904646\"]"
          , "members[value eq \"2819c223-7f76-453a-919d-413861904646\"].displayName"
          ]
      for_ examples $ \p -> it ("parses " ++ show p) $ (rPath <$> parseOnly pPath p) `shouldSatisfy` isRight

  
