module Web.Zenfolio.Photos (
    deletePhoto,
    getPopularPhotos,
    getRecentPhotos,
    loadPhoto,
    movePhoto,
    replacePhoto,
    rotatePhoto,
    searchPhotoByCategory,
    searchPhotoByText,
    updatePhoto,
    updatePhotoAccess,

    newUpdater
) where

import Web.Zenfolio.Auth (updatePhotoAccess)
import Web.Zenfolio.Monad (ZM)
import Web.Zenfolio.RPC (zfRemote)
import Web.Zenfolio.Types (Void, Photo, PhotoID, PhotoRotation, PhotoUpdater(..),
                           PhotoSetID, GroupIndex, SortOrder, CategoryID,
                           SearchID, PhotoResult)

deletePhoto :: PhotoID -> ZM Void
deletePhoto = zfRemote "DeletePhoto"

getPopularPhotos :: Int -> Int -> ZM [Photo]
getPopularPhotos = zfRemote "GetPopularPhotos"

getRecentPhotos :: Int -> Int -> ZM [Photo]
getRecentPhotos = zfRemote "GetRecentPhotos"

loadPhoto :: PhotoID -> ZM Photo
loadPhoto = zfRemote "LoadPhoto"

movePhoto :: PhotoSetID -> PhotoID -> PhotoSetID -> GroupIndex -> ZM Void
movePhoto = zfRemote "MovePhoto"

replacePhoto :: PhotoID -> PhotoID -> ZM Void
replacePhoto = zfRemote "ReplacePhoto"

rotatePhoto :: PhotoID -> PhotoRotation -> ZM Photo
rotatePhoto = zfRemote "RotatePhoto"

searchPhotoByCategory :: SearchID -> SortOrder -> CategoryID -> Int -> Int -> ZM PhotoResult
searchPhotoByCategory = zfRemote "SearchPhotoByCategory"

searchPhotoByText :: SearchID -> SortOrder -> String -> Int -> Int -> ZM PhotoResult
searchPhotoByText = zfRemote "SearchPhotoByText"

updatePhoto :: PhotoID -> PhotoUpdater -> ZM Photo
updatePhoto = zfRemote "UpdatePhoto"

newUpdater :: PhotoUpdater
newUpdater = PhotoUpdater {
                 puTitle = Nothing,
                 puCaption = Nothing,
                 puKeywords = Nothing,
                 puCategories = Nothing,
                 puCopyright = Nothing,
                 puFileName = Nothing
             }
