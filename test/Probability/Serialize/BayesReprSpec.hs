module Probability.Serialize.BayesReprSpec (spec) where

import qualified Data.HashMap.Lazy               as M (singleton)
import qualified Data.Set                        as S (singleton)
import           Data.Text.Lazy                  (pack)
import           NLP.Hext.NaiveBayes             (Labeled (..))
import           Probability.Classifier          (Class (..))
import           Probability.Serialize.BayesRepr (BayesRepr (..))
import           Test.Hspec

import           Data.Either                     (Either (Right))
import           Data.Serialize                  (decode, encode, get, put)
import           Data.Serialize.Get              (runGet)
import           Data.Serialize.Put              (runPut)


spec :: Spec
spec = do
  describe "Prelude.head" $ do
    it "returns the first element of a list" $ do
      head [23 ..] `shouldBe` (23 :: Int)

  describe "Bayes Representation" $ do
    it "should be serialized properly" $ do
       (decode (encode  basicBayes)) `shouldBe` (Right basicBayes)

    -- it "returns the first element of an *arbitrary* list" $
      -- property $ \x xs -> head (x:xs) == (x :: Int)

    -- it "throws an exception if used with an empty list" $ do
      -- evaluate (head []) `shouldThrow` anyException



basicBayes :: BayesRepr Class
basicBayes = BayesRepr
                (S.singleton Positive)
                (M.singleton (pack "Hello") 5)
                [Labeled (M.singleton (pack "hello") 1) Negative]
