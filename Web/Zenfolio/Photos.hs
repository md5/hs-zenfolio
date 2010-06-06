module Web.Zenfolio.Photos (
    deletePhoto,
    loadPhoto,
    movePhoto,
    replacePhoto,
    rotatePhoto,
    updatePhoto,
    updatePhotoAccess,

    newUpdater
) where

import Web.Zenfolio.Auth (updatePhotoAccess)
import Web.Zenfolio.Monad (ZM)
import Web.Zenfolio.RPC (zfRemote)
import Web.Zenfolio.Types (Photo, PhotoID, PhotoRotation, PhotoUpdater(..),
                           PhotoSetID, GroupIndex)

deletePhoto :: PhotoID -> ZM ()
deletePhoto = zfRemote "DeletePhoto"

loadPhoto :: PhotoID -> ZM Photo
loadPhoto = zfRemote "LoadPhoto"

movePhoto :: PhotoSetID -> PhotoID -> PhotoSetID -> GroupIndex -> ZM ()
movePhoto = zfRemote "MovePhoto"

replacePhoto :: PhotoID -> PhotoID -> ZM ()
replacePhoto = zfRemote "ReplacePhoto"

rotatePhoto :: PhotoID -> PhotoRotation -> ZM Photo
rotatePhoto = zfRemote "RotatePhoto"

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
