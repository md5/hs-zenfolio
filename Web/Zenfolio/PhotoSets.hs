module Web.Zenfolio.PhotoSets (
    createPhotoSet,
    loadPhotoSet
) where

import Web.Zenfolio.Monad (ZM)
import Web.Zenfolio.RPC (zfRemote)
import Web.Zenfolio.Types (GroupID, PhotoSet, PhotoSetID, PhotoSetType, PhotoSetUpdater)

createPhotoSet :: GroupID -> PhotoSetType -> PhotoSetUpdater -> ZM PhotoSet
createPhotoSet groupId setType updater = zfRemote "CreatePhotoSet" groupId setType updater

loadPhotoSet :: PhotoSetID -> ZM PhotoSet
loadPhotoSet photoSetId = zfRemote "LoadPhotoSet" photoSetId
