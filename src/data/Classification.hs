{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Classification (
        Classification(..)
)

where

import           Data.Serialize (Serialize)
import           GHC.Generics   (Generic)

data Classification = Positive | Negative deriving (Eq, Show, Ord, Generic, Read)

instance Serialize Classification
