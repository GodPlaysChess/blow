{-# LANGUAGE OverloadedStrings #-}

module BayesControllerSpec(spec) where

import           BayesController            (classifyModelT, initializeModelT,
                                             readModelT, refineModelT)
import           Control.Exception.Base     (bracket_)
import           Control.Monad.Trans.Reader (runReaderT)
import           Data.Bitraversable         (bitraverse)
import           Data.Either                (isRight)
import           Probability.Classifier
import           System.Directory           (removeFile)
import           Test.Hspec


spec :: Spec
spec = around_ withModelInitialised $ do
    describe "Controller" $ do
        it "initialize model should create a file with parsable model" $
                         runReaderT readModelT testModelStorage  >>= (`shouldSatisfy` isRight)

        it "after refinement the model shall be classified correcly" $
                     runReaderT (do
                        refineModelT "Love story" Positive
                        refineModelT "Horror story" Negative
                        bitraverse classifyModelT classifyModelT ("Love poem" , "horror poem"))
                        testModelStorage >>= (`shouldBe` (Positive, Negative))


withModelInitialised :: IO () -> IO ()
withModelInitialised        = bracket_ beforeTestInitialise
                                       afterTestDelete

testModelStorage :: FilePath
testModelStorage = "./test/resources/modelstorage"

beforeTestInitialise :: IO ()
beforeTestInitialise = runReaderT
                            (initializeModelT "./test/resources/initial_training_set")
                            testModelStorage

afterTestDelete :: IO ()
afterTestDelete = removeFile testModelStorage
