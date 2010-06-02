{-# LANGUAGE DeriveDataTypeable #-}

module Web.Zenfolio.Types.Search (
    SortOrder(..),
    PhotoResult(..)
) where

import Data.Data (Data, Typeable)
import Web.Zenfolio.Types.Base (Photo)

data SortOrder = Date
               | Popularity
               | Rank
    deriving (Eq, Ord, Enum, Bounded, Read, Show, Data, Typeable)

data PhotoResult = PhotoResult {
        prPhotos     :: [Photo],
        prTotalCount :: Int
    }
    deriving (Eq, Show)
