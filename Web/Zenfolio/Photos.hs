module Web.Zenfolio.Photos (
    loadPhoto,
    updatePhoto,

    newUpdater
) where

import Web.Zenfolio.Monad (ZM)
import Web.Zenfolio.RPC (zfRemote)
import Web.Zenfolio.Types (PhotoID, Photo, PhotoUpdater(..))

loadPhoto :: PhotoID -> ZM Photo
loadPhoto = zfRemote "LoadPhoto"

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
