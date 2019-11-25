{-# LANGUAGE QuasiQuotes #-}

module Test.FilterSpec (spec) where

import           Web.Scim.Filter
import           Web.Scim.Schema.Schema (Schema(User20, Group20))

import           Test.Hspec
import           HaskellWorks.Hspec.Hedgehog
import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

spec :: Spec
spec = do
  describe "Filter" $ do
    it "parse . render === id" $ require $ property $ do
      filter_ <- forAll genFilter
      parseFilter (renderFilter filter_) === Right filter_

----------------------------------------------------------------------------
-- Generators

genCompValue :: Gen CompValue
genCompValue = Gen.choice
  [ pure ValNull
  , ValBool <$> Gen.bool
  , ValNumber <$> Gen.choice
      [ Gen.realFrac_ (Range.constantFrom 0 (-100) 100)
      , fromInteger <$> Gen.integral (Range.constantFrom 0 (-100) 100)
      ]
  , ValString <$> Gen.text (Range.constant 0 1000) Gen.unicode
  ]

genCompareOp :: Gen CompareOp
genCompareOp = Gen.element
  [ minBound .. ]

genSubAttr :: Gen SubAttr
genSubAttr = SubAttr <$> genAttrName

-- TODO(arianvp): We only support the core schema so far, enterprise and custom schemas
-- will come later
genSchema :: Gen Schema
genSchema = Gen.element [ User20, Group20 ]
 

genAttrPath :: Gen AttrPath
genAttrPath = AttrPath <$> Gen.maybe genSchema <*> genAttrName  <*> Gen.maybe genSubAttr

genAttrName :: Gen AttrName
genAttrName = Gen.element
  [ minBound .. ]

genFilter :: Gen Filter
genFilter = Gen.choice
  [ FilterAttrCompare <$> genAttrPath <*> genCompareOp <*> genCompValue
  ]
