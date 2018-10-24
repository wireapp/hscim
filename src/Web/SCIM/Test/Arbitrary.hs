{-# OPTIONS_GHC -Wno-orphans #-}  -- for Arbitrary instances

-- | Arbitrary instances for types in @hscim@, as well as Hedgehog
-- generators for those.
module Web.SCIM.Test.Arbitrary where

import           Test.QuickCheck
import           Test.QuickCheck.Hedgehog
import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import           Web.SCIM.Filter

----------------------------------------------------------------------------
-- Web.SCIM.Filter

genCompValue :: Hedgehog.Gen CompValue
genCompValue = Gen.choice
  [ pure ValNull
  , ValBool <$> Gen.bool
  , ValNumber <$> Gen.choice
      [ Gen.realFrac_ (Range.constantFrom 0 (-100) 100)
      , fromInteger <$> Gen.integral (Range.constantFrom 0 (-100) 100)
      ]
  , ValString <$> Gen.text (Range.constant 0 1000) Gen.unicode
  ]

genCompareOp :: Hedgehog.Gen CompareOp
genCompareOp = Gen.element
  [ OpEq, OpNe, OpCo, OpSw, OpEw, OpGt, OpGe, OpLt, OpLe ]

genAttribute :: Hedgehog.Gen Attribute
genAttribute = Gen.element
  [ AttrUserName ]

genFilter :: Hedgehog.Gen Filter
genFilter = Gen.choice
  [ FilterAttrCompare <$> genAttribute <*> genCompareOp <*> genCompValue
  ]

instance Arbitrary CompValue where
  arbitrary = hedgehog genCompValue
instance Arbitrary CompareOp where
  arbitrary = hedgehog genCompareOp
instance Arbitrary Attribute where
  arbitrary = hedgehog genAttribute
instance Arbitrary Filter where
  arbitrary = hedgehog genFilter

----------------------------------------------------------------------------
-- Web.SCIM.Schema.Common

genWithId :: Hedgehog.Gen a -> Hedgehog.Gen (WithId a)
genWithId gen =
    WithId
        <$> Gen.text (Range.constant 0 100) Gen.ascii
        <*> gen
