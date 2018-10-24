{-# LANGUAGE QuasiQuotes #-}

module Test.FilterSpec (spec) where

import           Web.SCIM.Filter
import           Web.SCIM.Test.Arbitrary (genFilter)

import           Test.Hspec
import           HaskellWorks.Hspec.Hedgehog
import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

spec :: Spec
spec = do
  context "parsing:" $ do
    it "parse . render === id" $ require $ property $ do
      filter_ <- forAll genFilter
      parseFilter (renderFilter filter_) === Right filter_
