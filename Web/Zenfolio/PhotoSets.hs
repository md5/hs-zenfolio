module Web.Zenfolio.PhotoSets (
    createPhotoSet
) where

import Web.Zenfolio.Monad (ZM)
import Web.Zenfolio.RPC (zfRemote)
import Web.Zenfolio.Types (GroupID, PhotoSetType, PhotoSetUpdater, PhotoSet)

createPhotoSet :: GroupID -> PhotoSetType -> PhotoSetUpdater -> ZM PhotoSet
createPhotoSet groupId setType updater = zfRemote "CreatePhotoSet" groupId setType updater
