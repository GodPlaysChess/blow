{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module LibMain where

import           Web.Scotty                 as Rest (get, html, scotty, text)
import           Web.Scotty.Trans           as RestT (ScottyT, get, scottyT,
                                                      text)

import           BayesController
import           Control.Monad              ((>>))
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Trans.Class  (lift)
import           Control.Monad.Trans.Reader (ReaderT, ask, local, runReaderT)
import           Data.Text.Lazy             as T (pack)
import           Data.Text.Lazy             (Text)
import           Probability.Classifier     (Class (..))

main :: IO ()
main = scottyMain

scottyMain :: IO ()
scottyMain = scotty 3000 $ do
  Rest.get "/" $ html "hello world"
  Rest.get "/hello" $ Rest.text "nothing to look up here"
  Rest.get "/init" $
    liftIO (initializeModel initialTrainingPath) >> Rest.text "Initializing the model"
  Rest.get "/classify" $ do
    cl <- liftIO $ classifyFile controlDoc
    let txt = T.pack ("classifies the given input in the" ++ controlDoc ++ ".\n The result model is " ++ cl)
    Rest.text txt
  Rest.get "/train" $
    liftIO (refineModel trainPath Positive) >>
    Rest.text "refined the model, given the additional classified input. to be implemented"

-- path for the model to be classified
controlDoc :: FilePath
controlDoc = "./dist/resources/to_classify"

trainPath :: FilePath
trainPath = "./dist/resources/to_train"

initialTrainingPath :: FilePath
initialTrainingPath = "./dist/resources/initial_training_set"

-- data AppConfig = AppConfig {
--   trainPath             :: FilePath
--   , initialTrainingPath :: FilePath
--   , modelStoragePath    :: FilePath
--   , controlDoc          :: FilePath
-- }



appRoutes :: ScottyT Text (ReaderT FilePath IO) ()
appRoutes = do
  getInitialize
  getConfig

rootPath :: FilePath
rootPath = "lol"

mainT :: IO ()
mainT = scottyT 3001 (`runReaderT` rootPath) getInitialize

-- change to put
getInitialize :: ScottyT Text (ReaderT FilePath IO) ()
getInitialize = RestT.get "/hello" $
             lift (initializeModelT initialTrainingPath) >> RestT.text "Initializing the model"

getConfig :: ScottyT Text (ReaderT FilePath IO) ()
getConfig = RestT.get "/conf" $ lift ask >>= RestT.text . pack
