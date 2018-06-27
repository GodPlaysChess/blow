{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE ExplicitForAll     #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE StandaloneDeriving #-}

module Data.BayesRepr (
    fromModel,
    toModel
  ) where

import           Data.Classification     (Classification)
import           Data.Hashable           (Hashable)
import qualified Data.HashMap.Lazy       as HM (HashMap, fromList, toList)
import           Data.Serialize          (Serialize, get, put)
import           Data.Set                (Set)
import           Data.Text.Lazy          (Text)
import           Data.Text.Lazy.Encoding (decodeUtf8, encodeUtf8)
import           GHC.Generics            (Generic)
import           NLP.Hext.NaiveBayes     (BayesModel (..), FrequencyList,
                                          Labeled (..))

--newtype BayesRepr1 a = BayesRepr1 (BayesModel a) deriving (Generic, Show, Eq)
data BayesRepr a = BayesRepr (Set a) FrequencyList [Labeled a] deriving (Generic, Show, Eq)

deriving instance Show a => Show (Labeled a)
deriving instance Eq a => Eq (Labeled a)
deriving instance Generic a => Generic (Labeled a)
deriving instance Eq a => Eq (BayesModel a)

deriving instance Generic (BayesModel Classification)

instance Serialize (BayesRepr Classification)
instance Serialize (BayesModel Classification) where
    put = put . fromModel
    get = fmap toModel get

instance Serialize (Labeled Classification)
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


