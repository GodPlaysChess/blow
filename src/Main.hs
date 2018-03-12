{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Text.Lazy         as T (Text, pack)
import           NLP.Hext.NaiveBayes    (BayesModel, emptyModel, runBayes, teach)
import           Probability.Classifier (Class, classifiedDocs)
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
  review <- readFile "./dist/resources/review1.txt"
  let result = runBayes (material classifiedDocs) review
  putStrLn $ "The review '" ++ review ++ "' is " ++ show result


storagePath :: FilePath
storagePath = "./dist/resources/modelstorage/bayesmodel"

readModel :: IO (BayesModel Class)
readModel = undefined

writeModel :: BayesModel Class -> IO ()
writeModel = undefined -- writeFile $ encode 

-- delete them in the end. They are here just to memoize faster what's going on.
updateModel :: T.Text -> Class -> BayesModel Class -> BayesModel Class
updateModel = teach

classify :: BayesModel Class -> String -> Class 
classify = runBayes 
