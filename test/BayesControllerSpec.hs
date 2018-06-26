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
import          Probability.Classifier
import           Data.Bitraversable(bitraverse)


spec :: Spec
spec = around_ withModelInitialised $ do
    describe "Controller" $ do
        it "initialize model should create a file with parsable model" $
                         (runReaderT readModelT testModelStorage)  >>= (`shouldSatisfy` isRight)

        it "after refinement the model shall be classified correcly" $
                     (runReaderT (do 
                        _ <- refineModelT "Love story" Positive
                        _ <- refineModelT "Horror story" Negative
                        bitraverse classifyModelT classifyModelT ("Love poem" , "horror poem"))
                        testModelStorage) >>= (`shouldBe` (Positive, Negative))

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
