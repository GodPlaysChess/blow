module Route.TextClassificationResourceSpec(spec)
 where

import           Test.Hspec




spec :: Spec
spec = do
  describe "/hello" $ do
    it "initialize the model and return 200" $ do
      pending

    it "should give a note, if training set is empty" $ do
      pending

  describe "/conf" $ do
    it "should show the config" $ do
        pending

  describe "/classify" $ do
    it "should classify the model accordingly" $ do
        pending

  describe "/train" $ do
    it "should update the model with the file provided" $ do
        pending
