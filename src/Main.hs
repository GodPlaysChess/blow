{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           NLP.Hext.NaiveBayes             (BayesModel, emptyModel,
                                                  runBayes, teach)
-- import           Control.Monad.Trans    (liftIO)
--import           Control.Trans          ((~>))
import           Data.ByteString                 as BS (readFile, writeFile)
import           Data.Serialize                  as S (decode, encode)
import           Data.Text.Lazy                  as T (Text, pack)
import           Probability.Classifier          (Class, classifiedDocs)
import           Probability.Serialize.BayesRepr
import           System.Directory                (listDirectory)
import           Web.Scotty                      as Rest (get, html, scotty,
                                                          text)
import Data.Either(fromRight)                                                          

main :: IO ()
main = learningMain

scottyMain :: IO ()
scottyMain = scotty 3000 $ do
  Rest.get "/" $ html "hello world"
  Rest.get "/hello" $ text "nothing to look up here"
  Rest.get "/classify" $ text "classifies the given input. To be implemented"
  Rest.get "/train" $ text "refine the model, given the additional classified input. to be implemented"

--  read list of files from dir A, updates the model which is stored in some file
learningMain :: IO ()
learningMain = do
  initialModel <- fromRight emptyModel <$> readModel
  let material = foldl (\m (sample, cl) -> teach (T.pack sample) cl m) initialModel
  files <- listDirectory  ".dist/resources/learning"
  review <- pure "" -- BS.readFile "./dist/resources/review1.txt"
  let result = runBayes (material classifiedDocs) review
  putStrLn $ "The review '" ++ review ++ "' is " ++ show result

-- path for serialized BayesModel
storagePath :: FilePath
storagePath = "./dist/resources/modelstorage/bayesmodel"

readModel :: IO (Either String (BayesModel Class))
readModel = do 
              file <- BS.readFile storagePath
              let bayesRepr = S.decode file
              let bayesModel = toModel <$> bayesRepr
              return bayesModel

writeModel :: BayesModel Class -> IO ()
writeModel = BS.writeFile storagePath . S.encode . fromModel 

-- updates the model given additional information
updateModel :: BayesModel Class -> [(T.Text, Class)] -> BayesModel Class
updateModel = foldl (\m (sample, cl) -> teach sample cl m)

classify :: BayesModel Class -> String -> Class
classify = runBayes