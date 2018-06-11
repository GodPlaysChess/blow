{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}

module Probability.Classifier (        
        Class(..)    
)

where

import           Data.Serialize (Serialize)
import           GHC.Generics   (Generic)

data Class = Positive | Negative deriving (Eq, Show, Ord, Generic, Read)

instance Serialize Class
