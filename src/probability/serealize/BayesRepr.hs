module BayesRepr where

import           Control.Natural
import           NLP.Hext.NaiveBayes (BayesModel)

    -- Let's try to serialize this!
data BayesRepr a = BayesRepr {
     cls :: Set a
   , voc :: HashMap Int a
   , mat :: [(HashMap Text Int, a)]
} deriving (Show, Eq, Serialize)


instance Transformation BayesRepr BayesModel _ where
    BayesRepr a # BayesModel a = undefined


