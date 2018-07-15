{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Classification (
        Classification(..),
        fromByteString
)

where

import           Data.ByteString.Lazy.Char8 (ByteString, unpack)
import           Data.Serialize             (Serialize)
import           GHC.Generics               (Generic)
import           Text.Read                  (readMaybe)


data Classification = Positive | Negative deriving (Eq, Show, Ord, Generic, Read)

instance Serialize Classification

fromByteString :: ByteString -> Maybe Classification
fromByteString = readMaybe . unpack
