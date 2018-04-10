{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE ExplicitForAll    #-}
{-# LANGUAGE FlexibleInstances #-}

module Probability.Serialize.BayesRepr where

import           Control.Arrow           ((<<^))
import           Control.Natural
import qualified Data.Foldable           as F (fold, foldl)
import qualified Data.HashMap.Lazy       as HM (HashMap (..), fromList, toList)
import           Data.Map.Lazy           (Map (..), fromList, mapKeys, toList)
import           Data.Set                (Set)
--import           Data.Text              (Text (..))
import           Data.Text.Lazy.Encoding (decodeUtf8, encodeUtf8)
--import           Data.Text.Show         (unpack)
import           Data.Hashable           (Hashable)
import           Data.Serialize          (Serialize, get, put)
import           Data.Serialize.Put      (putTwoOf)
import           GHC.Generics            (Generic)

import           NLP.Hext.NaiveBayes     (BayesModel (..), FrequencyList (..),
                                          Labeled (..))
import           Probability.Classifier  (Class, classifiedDocs)

import           Data.Text.Lazy          (Text (..))


    -- Let's try to serialize this!
data BayesRepr a = BayesRepr {
     cls :: Set a
   , voc :: Map String Int
   , mat :: [(Map String Int, a)]
} deriving (Show, Eq, Generic)

instance Serialize (BayesModel Class)
instance Serialize (Labeled Class) where
      put (Labeled h l) = putTwoOf put put (h, l)
      get = get
instance Generic (Labeled Class)

instance Serialize (BayesRepr Class)

instance Serialize Text where
    put txt = put $ encodeUtf8 txt
    get     = fmap decodeUtf8 get

instance (Hashable k, Eq k, Serialize k, Serialize v) => Serialize (HM.HashMap k v) where
    get = fmap HM.fromList get
    put = put . HM.toList


-- inline this:
m2m ::Ord k => HM.HashMap k v -> Map k v
m2m = fromList . HM.toList

m2m' ::Ord k => HM.HashMap k v -> Map k v
m2m' = undefined -- F.fold

fromModel :: BayesModel a -> BayesRepr a
fromModel (BayesModel c v m) = BayesRepr
    c
    (mapKeys show $ m2m v)
    (map (\l ->
            (mapKeys show (m2m (hash l)), label l)) m
    )

toModel :: BayesRepr a -> BayesModel a
toModel (BayesRepr cls voc mat) = BayesModel
                                    cls
                                    undefined -- (mapKeys fromString (HM.fromList . toList voc))
                                    undefined -- (map (\l ->
                                        -- (mapKeys fromString (HM.fromList . toList (hash l)), label l)))




-- instance Transformation BayesRepr BayesModel _ where
    -- BayesRepr a # BayesModel a = undefined



-- TODO
-- implement conversions >>
-- tests for conversions >>
-- redifine them in terms of natural transformations >>
-- check what is written in the file >>
-- *(may be try to avoid conversions as well and derive serializer directly)


