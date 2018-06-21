module BayesControllerSpec(spec) where

import           Test.Hspec

import           BayesController


spec :: Spec
spec = do
    describe "Controller" $ do
        it "initialize model should create a file" $
            pending
            -- initializeModel "./test/resources/initial_training_set" >>
            --  readFile "./dist/resources/modelstorage/bayesmodel" `shouldBe` liftIO

        it "should update model" $
            pending


        it "classify model" $
            pending
