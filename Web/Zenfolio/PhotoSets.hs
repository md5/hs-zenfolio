module Web.Zenfolio.PhotoSets (
    createPhotoSet,
    loadPhotoSet
) where

import Web.Zenfolio.Monad (ZM)
import Web.Zenfolio.RPC (zfRemote)
import Web.Zenfolio.Types (GroupID, PhotoSet, PhotoSetID, PhotoSetType, PhotoSetUpdater)

createPhotoSet :: GroupID -> PhotoSetType -> PhotoSetUpdater -> ZM PhotoSet
createPhotoSet = zfRemote "CreatePhotoSet"

loadPhotoSet :: PhotoSetID -> ZM PhotoSet
loadPhotoSet = zfRemote "LoadPhotoSet"
