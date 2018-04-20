{-# LANGUAGE OverloadedStrings #-}

module BayesController(
      refineModel
    , classifyFile
    , initializeModel
) where

import           NLP.Hext.NaiveBayes             (BayesModel, emptyModel,
                                                  runBayes, teach)
                                                  
import           Data.ByteString                 as BS (readFile, writeFile)
import qualified System.IO (readFile)
import qualified Data.ByteString.Char8 as C8(unpack) 
import           Data.Serialize                  as S (decode, encode)
import           Data.Text.Lazy                  as T (Text, pack, unpack, lines, span, strip)
import           Data.Either(fromRight) 
import           Probability.Classifier          (Class)
import           Probability.Serialize.BayesRepr (fromModel, toModel)
import           Control.Monad(forM)
import Data.Bitraversable(bitraverse)
import Data.Maybe(maybeToList)
import Text.Read(readMaybe)
import qualified Data.Text.Lazy.IO as T(readFile)



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

refineModel :: FilePath -> Class -> IO ()
refineModel filePath cl = do 
                            initialModel <- fromRight emptyModel <$> readModel
                            additionalModel <- System.IO.readFile filePath
                            let newModel = updateModel initialModel [(T.pack additionalModel, cl)]
                            writeModel newModel                          

-- TODO move file from *to_classify* to *classified_models*
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
    classifiedDocs <- forM classification (bitraverse (\t -> T.readFile (path ++ unpack t)) pure)
    writeModel (updateModel emptyModel classifiedDocs)

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