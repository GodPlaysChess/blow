{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}

module Main where

import           NLP.Hext.NaiveBayes    (BayesModel, emptyModel, runBayes,
                                         teach)
-- import           Control.Monad.Trans    (liftIO)
import           Data.ByteString        as BS (readFile, writeFile)
import           Data.Serialize         as S (Serialize, decode, encode, get,
                                              put)
import           Data.Serialize.Get     (runGet)
import           Data.Serialize.Put     (Put, runPut)
import           Data.Text.Lazy         as T (Text, pack)
import           GHC.Generics           (Generic)
import           Probability.Classifier (Class, classifiedDocs)
import           System.Directory       (listDirectory)
import           Web.Scotty             as Rest (get, html, scotty, text)

instance Serialize (BayesModel Class)

main :: IO ()
main = learningMain

scottyMain :: IO ()
scottyMain = scotty 3000 $ do
  Rest.get "/" $ html "hello world"
  Rest.get "/hello" $ text "nothing to look up here"
 -- post "/train" $ undefined () --run classifier and return the result (as a feature, confirm if helpful -> adds the result to the training set)

--  read list of files from dir A, updates the model which is stored in some file
learningMain :: IO ()
learningMain = do
  let material = foldl (\m (sample, cl) -> teach (T.pack sample) cl m) emptyModel
  files <- listDirectory  ".dist/resources/learning"
  review <- pure "" -- BS.readFile "./dist/resources/review1.txt"
  let result = runBayes (material classifiedDocs) review
  putStrLn $ "The review '" ++ review ++ "' is " ++ show result

-- path for serialized BayesModel
storagePath :: FilePath
storagePath = "./dist/resources/modelstorage/bayesmodel"

readModel :: IO (Either String (BayesModel Class))
readModel =  decode <$> BS.readFile storagePath

writeModel :: BayesModel Class -> IO ()
writeModel model = BS.writeFile storagePath (runPut $ put model)

-- delete them in the end. They are here just to memoize faster what's going on.
updateModel :: T.Text -> Class -> BayesModel Class -> BayesModel Class
updateModel = teach

classify :: BayesModel Class -> String -> Class
classify = runBayes
-- reads
--classify :: String -> [(String, Class)] -> Class
--classify text model = runBayes model text
