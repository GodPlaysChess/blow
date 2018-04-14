{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE ExplicitForAll    #-}
{-# LANGUAGE FlexibleInstances #-}

module Probability.Serialize.BayesRepr where

import           Control.Arrow           ((<<^))
import           Control.Natural
import qualified Data.Foldable           as F (fold, foldl)
import qualified Data.HashMap.Lazy       as HM (HashMap (..), fromList, toList)
import qualified Data.Map.Lazy           as M (Map (..), fromList, mapKeys,
                                               toList)
import           Data.Set                (Set)

import           Data.Text.Lazy.Encoding (decodeUtf8, encodeUtf8)
--import           Data.Text.Show         (unpack)
import           Data.Hashable           (Hashable)
import           Data.Serialize          (Serialize, get, put)
import           Data.Serialize.Put      (putTwoOf)
import           GHC.Generics            (Generic)

import           NLP.Hext.NaiveBayes     (BayesModel (..), FrequencyList (..),
                                          Labeled (..))
import           Probability.Classifier  (Class, classifiedDocs)

import           Control.Arrow           ((&&&))
import           Data.Text.Lazy          (Text (..), pack)


    -- Let's try to serialize this!
data BayesRepr a = BayesRepr {
     cls :: Set a
   , voc :: HM.HashMap Text Int
   , mat :: [(HM.HashMap Text Int, a)]
} deriving (Show, Eq, Generic)

instance Serialize (BayesRepr Class)

-- instance Generic (BayesModel Class)
-- instance Serialize (BayesModel Class)
-- instance Serialize (Labeled Class) where
--       put (Labeled h l) = putTwoOf put put (h, l)
--       get = get
-- instance Generic (Labeled Class)
instance Serialize Text where
    put txt = put $ encodeUtf8 txt
    get     = fmap decodeUtf8 get

instance (Hashable k, Eq k, Serialize k, Serialize v) => Serialize (HM.HashMap k v) where
    get = fmap HM.fromList get
    put = put . HM.toList


-- inline this:
m2m :: Ord k => HM.HashMap k v -> M.Map k v
m2m = M.fromList . HM.toList

m2m' :: (Hashable k, Ord k) => M.Map k v -> HM.HashMap k v
m2m' = HM.fromList . M.toList

fromModel :: BayesModel a -> BayesRepr a
fromModel (BayesModel c v m) = BayesRepr
    c
    v
    (map (hash Control.Arrow.&&& label) m
    )

toModel :: BayesRepr a -> BayesModel a
toModel (BayesRepr cls voc mat) = BayesModel
                                    cls
                                    voc
                                    (map (uncurry Labeled) mat)




-- instance Transformation BayesRepr BayesModel _ where
    -- BayesRepr a # BayesModel a = undefined



-- TODO
-- implement conversions >>
-- tests for conversions >>
-- redifine them in terms of natural transformations >>
-- check what is written in the file >>
-- *(may be try to avoid conversions as well and derive serializer directly)


