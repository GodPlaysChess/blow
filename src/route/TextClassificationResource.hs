{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Route.TextClassificationResource(appRoutes) where

import           Control.Applicative          (liftA2)
import           Control.Monad                ((>>))
import           Control.Monad.Trans.Class    (lift)
import           Control.Monad.Trans.Reader   (ReaderT, ask)
import           Controller.BayesController
import           Data.ByteString.Lazy         (ByteString, toStrict)
import           Data.Classification          (fromByteString)
import           Data.Foldable                (fold)
import           Data.Maybe                   (listToMaybe, maybe)
import           Data.Text.Lazy               (Text, pack)
import           Data.Text.Lazy.Encoding      (decodeUtf8)
import           Network.Wai.Parse            (fileContent)
import           System.IO.Streams.ByteString (fromLazyByteString)
import           Util.PdfParsing              (getContent)
import           Web.Scotty.Trans             as RestT (ScottyT, body, files,
                                                        get, post, text)


appRoutes :: ScottyT Text (ReaderT FilePath IO) ()
appRoutes = do
  getInitialize
  getConfig
  postClassify
  testFileUpload
  postRefineModel

getInitialize :: ScottyT Text (ReaderT FilePath IO) ()
getInitialize = RestT.get "/hello" $
             lift (initializeModelT initialTrainingPath) >> RestT.text "Initializing the model"

getConfig :: ScottyT Text (ReaderT FilePath IO) ()
getConfig = RestT.get "/conf" $ lift ask >>= RestT.text . pack

postClassify :: ScottyT Text (ReaderT FilePath IO) ()
postClassify = RestT.post "/classify" $ do
              (meta, bytes) <- head <$> files -- not safe. change headoption, and cata to bad response
              classification <- lift . classifyModelT . toStrict . fileContent $ bytes
              RestT.text $ pack $ "classified as: " ++ show classification

postRefineModel :: ScottyT Text (ReaderT FilePath IO) ()
postRefineModel = RestT.post "/refine" $ do
                  mbFile <- listToMaybe <$> files
                  let mbTxt = (decodeUtf8 . fileContent . snd) <$> mbFile
                  mbClass <- fromByteString <$> body
                  maybe
                    (RestT.text "Model was not updated since could not read uploaded file or classification")
                    (\_ -> RestT.text "successfully updated the model")
                    (liftA2 refineModelT mbTxt mbClass)



testFileUpload :: ScottyT Text (ReaderT FilePath IO) ()
testFileUpload = RestT.post "/upload" $ do
              fs <- files
              (meta, bytes) <- head <$> files
              RestT.text $ (getContent . fileContent) bytes
              --RestT.text $ fold $ (decodeUtf8 . fileContent . snd) <$> fs


-- readPdf :: InputStream ByteString -> Text
-- readPdf = error

-- postTrain :: ScottyT Text (ReaderT FilePath IO) ()
-- postTrain = RestT.post "/train" $ do
--               payload <- body
--               model <- file
--               lift $ refineModelT model (parseClass cl)
--               RestT.text $ pack $ show classification

initialTrainingPath :: FilePath
initialTrainingPath = "./dist/resources/initial_training_set"
