{-# LANGUAGE OverloadedStrings #-}

module BayesController(
      refineModel
    , classifyFile
    , initializeModel
    , initializeModelT
) where

import           NLP.Hext.NaiveBayes             (BayesModel, emptyModel,
                                                  runBayes, teach)

import           Control.Monad                   (forM)
import           Control.Monad.Trans.Class       (lift)
import           Control.Monad.Trans.Reader      (ReaderT, ask)
import           Data.Bitraversable              (bitraverse)
import           Data.ByteString                 as BS (readFile, writeFile)
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

--reads model from the file
readModel :: IO (Either String (BayesModel Class))
readModel = do
                file <- BS.readFile storagePath
                return $ S.decode file

-- writes model to the file
persistModel :: BayesModel Class -> IO ()
persistModel = BS.writeFile storagePath . S.encode

-- updates the model given additional information
updateModel :: BayesModel Class -> [(T.Text, Class)] -> BayesModel Class
updateModel = foldl (\m (sample, cl) -> teach sample cl m)

-- updates the current model, given the new model in the FilePath asserted to a certain Class
refineModel :: FilePath -> Class -> IO ()
refineModel filePath cl = do
                            initialModel <- fromRight emptyModel <$> readModel
                            additionalModel <- System.IO.readFile filePath
                            let newModel = updateModel initialModel [(T.pack additionalModel, cl)]
                            persistModel newModel

-- TODO in addition, need to move file from *to_classify* to *classified_models* folder after classification
-- classifes the certain File
classifyFile :: FilePath -> IO String
classifyFile filePath = do
                            model <- fromRight emptyModel <$> readModel
                            modelToClassify <- BS.readFile filePath
                            let result = runBayes model (C8.unpack modelToClassify)
                            return $ "The review is " ++ show result

--  initialize the model based on models, stored in ./initial_training_set and classified accordingly to /initial_training_set/classification
initializeModel :: FilePath -> IO ()
initializeModel path = do
    classificationFile <- T.readFile (path ++ "/classification")
    let classification = readClassification classificationFile
    classifiedDocs <- forM classification $ bitraverse (T.readFile . (path ++) . unpack) pure
    persistModel (updateModel emptyModel classifiedDocs) where
        readClassification :: Text -> [(Text, Class)]
        readClassification file = do
            line <- T.lines file
            let (text, cltxt) = T.span (== '|') line
            cl <- maybeToList . readMaybe . unpack . strip $ cltxt
            return (text, cl)

-- path for serialized BayesModel
storagePath :: FilePath
storagePath = "./dist/resources/modelstorage/bayesmodel"

-- mapTuple = join (***)

{- same inside the ReaderT -}

-- writes model to the path, specified in the environment
persistModelT :: BayesModel Class -> ReaderT FilePath IO ()
persistModelT bm = do
        storagePathR <- ask
        lift $ (BS.writeFile storagePathR . S.encode) bm

--reads model from the file, specified in the environment
readModelT :: ReaderT FilePath IO (Either String (BayesModel Class))
readModelT = do
                storagePathR <- ask
                file <- lift $ BS.readFile storagePath
                return $ S.decode file

classifyFileT :: FilePath -> ReaderT FilePath IO String
classifyFileT filePath = do
                            model <- fromRight emptyModel <$> readModelT
                            modelToClassify <- lift $ BS.readFile filePath
                            let result = runBayes model (C8.unpack modelToClassify)
                            return $ "The review is " ++ show result

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
