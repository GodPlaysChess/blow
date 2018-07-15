-- {-# OPTIONS_GHC -F -pgmF hspec-discover #-}

import           Test.Hspec

import qualified Controller.BayesControllerSpec
import qualified Data.BayesReprSpec
import qualified Route.TextClassificationResourceSpec


main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "BayesController"   Controller.BayesControllerSpec.spec
  describe "BayesRepr"         Data.BayesReprSpec.spec
  describe "Routes"            Route.TextClassificationResourceSpec.spec
