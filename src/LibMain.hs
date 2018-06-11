{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module LibMain where

import           Web.Scotty                      as Rest (get, html, scotty,
                                                          text)                                                      

import Control.Monad.IO.Class(liftIO)                     
import Control.Monad((>>))       
import Probability.Classifier(Class(..))
import BayesController
import Data.Text.Lazy                  as T (pack)

main :: IO ()
main = scottyMain

scottyMain :: IO ()
scottyMain = scotty 3000 $ do
  Rest.get "/" $ html "hello world"
  Rest.get "/hello" $ text "nothing to look up here"
  Rest.get "/init" $ 
    liftIO (initializeModel initialTrainingPath) >> text "Initializing the model"
  Rest.get "/classify" $ do     
    cl <- liftIO $ classifyFile controlDoc
    let txt = T.pack ("classifies the given input in the" ++ controlDoc ++ ".\n The result model is " ++ cl)
    text txt
  Rest.get "/train" $ 
    liftIO (refineModel trainPath Positive) >>
      text "refined the model, given the additional classified input. to be implemented"



-- path for the model to be classified
controlDoc :: FilePath
controlDoc = "./dist/resources/to_classify"

trainPath :: FilePath
trainPath = "./dist/resources/to_train"

initialTrainingPath :: FilePath
initialTrainingPath = "./dist/resources/initial_training_set"