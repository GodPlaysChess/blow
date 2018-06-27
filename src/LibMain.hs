module LibMain(main) where

import           Control.Monad.Trans.Reader       (runReaderT)
import           Route.TextClassificationResource (appRoutes)
import           Web.Scotty.Trans                 as RestT (scottyT)

main :: IO ()
main = scottyT 3001 (`runReaderT` rootPath) appRoutes

-- scottyMain :: IO ()
-- scottyMain = scotty 3000 $ do
--   Rest.get "/" $ html "hello world"
--   Rest.get "/hello" $ Rest.text "nothing to look up here"
--   Rest.get "/init" $
--     liftIO (initializeModel initialTrainingPath) >> Rest.text "Initializing the model"
--   Rest.get "/classify" $ do
--     cl <- liftIO $ classifyFile controlDoc
--     let txt = T.pack ("classifies the given input in the" ++ controlDoc ++ ".\n The result model is " ++ cl)
--     Rest.text txt
--   Rest.get "/train" $
--     liftIO (refineModel trainPath Positive) >>
--     Rest.text "refined the model, given the additional classified input. to be implemented"

-- path for the model to be classified
controlDoc :: FilePath
controlDoc = "./dist/resources/to_classify"

trainPath :: FilePath
trainPath = "./dist/resources/to_train"


rootPath :: FilePath
rootPath = "./dist/resources/modelstorage/bayesmodel"

-- data AppConfig = AppConfig {
--   trainPath             :: FilePath
--   , initialTrainingPath :: FilePath
--   , modelStoragePath    :: FilePath
--   , controlDoc          :: FilePath
-- }
