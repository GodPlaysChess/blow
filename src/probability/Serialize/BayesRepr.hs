{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE ExplicitForAll     #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE StandaloneDeriving #-}

module Probability.Serialize.BayesRepr (
    fromModel
  , toModel
  , BayesRepr(..)
  ) where

import           Control.Natural         ()
import qualified Data.HashMap.Lazy       as HM (HashMap, fromList, toList)
import           Data.Set                (Set)

import           Data.Hashable           (Hashable)
import           Data.Serialize          (Serialize, get, put)
import           Data.Serialize.Get      (getTwoOf)
import           Data.Serialize.Put      (putTwoOf)
import           Data.Text.Lazy.Encoding (decodeUtf8, encodeUtf8)
import           GHC.Generics            (Generic)

import           NLP.Hext.NaiveBayes     (BayesModel (..), FrequencyList,
                                          Labeled (..))
import           Probability.Classifier  (Class)

import           Control.Monad           (liftM2)
import           Data.Text.Lazy          (Text)


data BayesRepr a = BayesRepr (Set a) FrequencyList [Labeled a] deriving (Generic, Show, Eq)

deriving instance Show a => Show (Labeled a)
deriving instance Eq a => Eq (Labeled a)
deriving instance Generic a => Generic (Labeled a)

instance Serialize (BayesRepr Class)

instance Serialize (Labeled Class) where
      --put (Labeled h l) = putTwoOf put put (h, l)

instance Serialize Text where
    put txt = put $ encodeUtf8 txt
    get     = fmap decodeUtf8 get

instance (Hashable k, Eq k, Serialize k, Serialize v) => Serialize (HM.HashMap k v) where
    get = fmap HM.fromList get
    put = put . HM.toList

fromModel :: BayesModel a -> BayesRepr a
fromModel (BayesModel a b c) = BayesRepr a b c

toModel :: BayesRepr a -> BayesModel a
toModel (BayesRepr a b c) = BayesModel a b c

-- instance Transformation BayesRepr BayesModel _ where
    -- BayesRepr a # BayesModel a = undefined



-- TODO
-- implement conversions >>
-- tests for conversions >>
-- redifine them in terms of natural transformations >>
-- check what is written in the file >>
-- *(may be try to avoid conversions as well and derive serializer directly)


