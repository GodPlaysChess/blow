{-# LANGUAGE FlexibleInstances #-}

module Data.BayesReprSpec (spec) where

import           Data.BayesRepr      ()
import           Data.Classification (Classification (..))
import           Data.Hashable       (Hashable)
import qualified Data.HashMap.Lazy   as M (HashMap, fromList, singleton)
import qualified Data.Set            as S (singleton)
import           Data.Text.Lazy      (pack)
import           NLP.Hext.NaiveBayes (BayesModel (..), Labeled (..))
import           Test.Hspec

import           Control.Monad       (liftM3)
import           Data.ByteString     (empty)
import           Data.Either         (Either (Right))
import           Data.Serialize      (decode, encode, get, put)
import           Data.Serialize.Get  (runGet)
import           Data.Serialize.Put  (runPut)

spec :: Spec
spec = do
  describe "Serialization of BayesModel" $ do
    it "should work properly" $ do
      (decode . encode) basicBayesModel `shouldBe` Right basicBayesModel

    -- it "deconding encoded instance should give back same instance" $
    --     property $ \x -> (decode . encode) x == Right (x :: BayesModel Class)

    -- it "throws an exception if used with an empty list" $ do
      -- evaluate (head []) `shouldThrow` anyException



-- instance (Hashable k, Eq k, Arbitrary k, Arbitrary v) => Arbitrary (M.HashMap k v) where
--   arbitrary = fmap M.fromList arbitrary
--   shrink = genericShrink

-- instance Arbitrary (Labeled Class) where
--   arbitrary = genericArbitrary
--   shrink = genericShrink


-- instance Arbitrary (BayesModel Class) where
--   arbitrary = genericArbitrary
--   shrink = genericShrink


basicBayesModel :: BayesModel Classification
basicBayesModel = BayesModel
                   (S.singleton Positive)
                   (M.singleton (pack "Hello") 5)
                   [Labeled (M.singleton (pack "hello") 1) Negative]
