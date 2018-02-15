{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Text.Lazy         as T (pack)
import           NLP.Hext.NaiveBayes    (emptyModel, runBayes, teach)
import           Probability.Classifier (classifiedDocs)
import           Web.Scotty



main :: IO ()
main = learningMain

scottyMain :: IO ()
scottyMain = scotty 3000 $ do
  get "/" $ html "hello world"
  get "/hello" $ text "nothing to look up here"


learningMain :: IO ()
learningMain = do
  let material = foldl (\m (sample, cl) -> teach (T.pack sample) cl m) emptyModel
  let review = "I loved the great acting"
  let result = runBayes (material classifiedDocs) review
  putStrLn $ "The review '" ++ review ++ "' is " ++ show result
