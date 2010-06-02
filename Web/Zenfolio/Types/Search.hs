{-# LANGUAGE DeriveDataTypeable #-}

module Web.Zenfolio.Types.Search (
    SortOrder(..),
    PhotoResult(..),
    PhotoSetResult(..)
) where

import Control.Applicative ((<$>), (<*>))
import Data.Data (Data, Typeable)
import Text.JSON (JSON(..), JSValue(..), makeObj)
import Text.JSON.Generic (toJSON, fromJSON)

import Web.Zenfolio.Types.Base (Photo, PhotoSet)
import Web.Zenfolio.Utils

data SortOrder = Date
               | Popularity
               | Rank
    deriving (Eq, Ord, Enum, Bounded, Read, Show, Data, Typeable)

data PhotoResult = PhotoResult {
        prPhotos     :: [Photo],
        prTotalCount :: Int
    }
    deriving (Eq, Show)

data PhotoSetResult = PhotoSetResult {
        psrPhotoSets  :: [PhotoSet],
        psrTotalCount :: Int
    }
    deriving (Eq, Show)

instance JSON SortOrder where
    showJSON = toJSON
    readJSON = fromJSON

instance JSON PhotoResult where
    showJSON pr = makeObj 
        [ ("$type", showJSON $ "PhotoResult")
        , ("Photos", showJSON $ prPhotos pr)
        , ("TotalCount", showJSON $ prTotalCount pr)
        ]

    readJSON (JSObject obj) =
        PhotoResult <$> field "Photos" obj
                    <*> field "TotalCount" obj

    readJSON json = fail $ "Unexpected JSON for PhotoResult: " ++ show json

instance JSON PhotoSetResult where
    showJSON psr = makeObj 
        [ ("$type", showJSON $ "PhotoSetResult")
        , ("PhotoSets", showJSON $ psrPhotoSets psr)
        , ("TotalCount", showJSON $ psrTotalCount psr)
        ]

    readJSON (JSObject obj) =
        PhotoSetResult <$> field "PhotoSets" obj
                       <*> field "TotalCount" obj

    readJSON json = fail $ "Unexpected JSON for PhotoSetResult: " ++ show json
