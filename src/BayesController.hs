{-# LANGUAGE OverloadedStrings #-}

module BayesController(
    initializeModelT
    , classifyFileT
    , refineModelT
    , readModelT
) where

import           NLP.Hext.NaiveBayes             (BayesModel, emptyModel,
                                                  runBayes, teach)

import           Control.Monad                   (forM)
import           Control.Monad.Trans.Class       (lift)
import           Control.Monad.Trans.Reader      (ReaderT, ask)
import           Data.Bitraversable              (bitraverse)
import           Data.ByteString                 as BS (ByteString, readFile,
                                                        writeFile)
import qualified Data.ByteString.Char8           as C8 (unpack)
import           Data.Either                     (fromRight)
import           Data.Maybe                      (maybeToList)
import           Data.Serialize                  as S (decode, encode)
import           Data.Text.Lazy                  as T (Text, lines, pack, span,
                                                       strip, unpack)
import qualified Data.Text.Lazy.IO               as T (readFile)
import           Probability.Classifier          (Class)
import           Probability.Serialize.BayesRepr ()
import qualified System.IO                       (readFile)
import           Text.Read                       (readMaybe)

-- updates the model given additional information
updateModel :: BayesModel Class -> [(T.Text, Class)] -> BayesModel Class
updateModel = foldl (\m (sample, cl) -> teach sample cl m)

-- writes model to the path, specified in the environment
persistModelT :: BayesModel Class -> ReaderT FilePath IO ()
persistModelT bm = do
        storagePathR <- ask
        lift $ (BS.writeFile storagePathR . S.encode) bm

--reads model from the file, specified in the environment
readModelT :: ReaderT FilePath IO (Either String (BayesModel Class))
readModelT = do
                storagePath <- ask
                file <- lift $ BS.readFile storagePath
                return $ S.decode file

--classifies the model from the file
classifyFileT :: FilePath -> ReaderT FilePath IO String
classifyFileT f = classifyModelT =<< lift (BS.readFile f)

--  initialize the model based on models, stored in ./initial_training_set and classified accordingly to /initial_training_set/classification
initializeModelT :: FilePath -> ReaderT FilePath IO ()
initializeModelT path = do
    classificationFile <- lift $ T.readFile (path ++ "/classification")
    let classification = readClassificationT classificationFile
    classifiedDocs <- lift $ forM classification $ bitraverse (T.readFile . (path ++) . unpack) pure
    persistModelT (updateModel emptyModel classifiedDocs) where
        readClassificationT :: Text -> [(Text, Class)]
        readClassificationT file = do
            line <- T.lines file
            let (text, cltxt) = T.span (== '|') line
            cl <- maybeToList . readMaybe . unpack . strip $ cltxt
            return (text, cl)


-- updates the current model, given the new model in the FilePath asserted to a certain Class
refineModelT :: Text -> Class -> ReaderT FilePath IO ()
refineModelT additionalModel cl = do
                            initialModel <- fromRight emptyModel <$> readModelT
                            let newModel = updateModel initialModel [(additionalModel, cl)]
                            persistModelT newModel

classifyModelT :: ByteString -> ReaderT FilePath IO String
classifyModelT modelToClassify = do
                            model <- fromRight emptyModel <$> readModelT
                            let result = runBayes model (C8.unpack modelToClassify)
                            return $ "The review is " ++ show result
