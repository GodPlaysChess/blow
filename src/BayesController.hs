module BayesController(
      refineModel
    , classifyFile
    , initializeModel
) where

import           NLP.Hext.NaiveBayes             (BayesModel, emptyModel,
                                                  runBayes, teach)
                                                  
import           Data.ByteString                 as BS (readFile, writeFile, ByteString)
import qualified System.IO (readFile)
import Data.ByteString.Char8 (unpack)
import           Data.Serialize                  as S (decode, encode)
import           Data.Text.Lazy                  as T (Text, pack)
import           Data.Either(fromRight) 
import           Probability.Classifier          (Class, classifiedDocs)
import           Probability.Serialize.BayesRepr (fromModel, toModel)
import           System.Directory                (listDirectory)
import           Data.List(isInfixOf)
import           Control.Monad(forM)



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
                            let result = runBayes model (unpack modelToClassify)
                            return $ "The review is " ++ show result

--  initialize the model based on models, stored in ./initial_training_set and classified accordingly to /initial_training_set/classification
initializeModel :: FilePath -> IO ()
initializeModel path = do  
    files <- filter (not . isInfixOf "classification") <$> (listDirectory path)
    classificationFile <- BS.readFile (path ++ "/classification")
    let classification = readClassification classificationFile
    classifiedDocs <- forM classification (\(t, c) -> (BS.readFile (path ++ unpack t), c))
    let result = runBayes (updateModel classifiedDocs) review
    writeModel result

readClassification :: ByteString -> [(ByteString, Class)]
readClassification = undefined

-- path for serialized BayesModel
storagePath :: FilePath
storagePath = "./dist/resources/modelstorage/bayesmodel"