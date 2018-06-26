{-# LANGUAGE OverloadedStrings #-}

module BayesControllerSpec(spec) where

import           BayesController               (initializeModelT, readModelT,
                                                refineModelT, classifyModelT)
import           Control.Exception.Base        (bracket_, finally)
import           Control.Monad.Trans.Reader    (ReaderT, ask, local, runReaderT)
import           Data.Either                   (Either (Right), isRight)
import           System.Directory              (removeFile)
import           System.IO.Unsafe              (unsafePerformIO)
import           Test.Hspec
import           Test.Hspec.Expectations.Match (shouldMatch)
import          Probability.Classifier


spec :: Spec
spec = do
    describe "Controller" $ do
        it "initialize model should create a file with parsable model" $
                bracket_ beforeTestInitialise
                         afterTestDelete
                         (runReaderT readModelT testModelStorage)  >>= (`shouldSatisfy` isRight)


        it "after refinement the model shall be classified correcly" $
            bracket_ beforeTestInitialise
                     afterTestDelete
                     (runReaderT (do 
                        _ <- refineModelT "Love story" Positive
                        classifyModelT "Love story")
                        testModelStorage) >>= (`shouldBe` Positive)


        -- it "classify model" $
        --     pending

testModelStorage :: FilePath
testModelStorage = "./test/resources/modelstorage"

beforeTestInitialise :: IO ()
beforeTestInitialise = runReaderT
                            (initializeModelT "./test/resources/initial_training_set")
                            testModelStorage


afterTestDelete :: IO ()
afterTestDelete = removeFile testModelStorage
