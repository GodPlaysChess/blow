{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Route.TextClassificationResource(appRoutes) where

import           Control.Monad              ((>>))
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Trans.Class  (lift)
import           Control.Monad.Trans.Reader (ReaderT, ask, local, runReaderT)
import           Controller.BayesController
import           Data.ByteString.Lazy       (toStrict)
import           Data.Classification        (Classification (..))
import           Data.Text.Lazy             as T (pack)
import           Data.Text.Lazy             (Text)
import           Web.Scotty.Trans           as RestT (ScottyT, body, file, get,
                                                      post, scottyT, text)

appRoutes :: ScottyT Text (ReaderT FilePath IO) ()
appRoutes = do
  getInitialize
  getConfig
  postClassify
  -- postTrain

getInitialize :: ScottyT Text (ReaderT FilePath IO) ()
getInitialize = RestT.get "/hello" $
             lift (initializeModelT initialTrainingPath) >> RestT.text "Initializing the model"

getConfig :: ScottyT Text (ReaderT FilePath IO) ()
getConfig = RestT.get "/conf" $ lift ask >>= RestT.text . pack

postClassify :: ScottyT Text (ReaderT FilePath IO) ()
postClassify = RestT.post "/classify" $ do
              model <- body
              classification <- lift $ classifyModelT (toStrict model)
              RestT.text $ pack $ "classified as: " ++ show classification

-- postTrain :: ScottyT Text (ReaderT FilePath IO) ()
-- postTrain = RestT.post "/train" $ do
--               payload <- body
--               model <- file
--               lift $ refineModelT model (parseClass cl)
--               RestT.text $ pack $ show classification

initialTrainingPath :: FilePath
initialTrainingPath = "./dist/resources/initial_training_set"
