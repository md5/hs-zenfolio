module Web.Zenfolio.PhotoSets (
    createPhotoSet
) where

import Network.JsonRpc (RpcAction)
import Web.Zenfolio.RPC (zfRemote)
import Web.Zenfolio.Types (GroupID, PhotoSetType, PhotoSetUpdater, PhotoSet)

createPhotoSet :: GroupID -> PhotoSetType -> PhotoSetUpdater -> RpcAction IO PhotoSet
createPhotoSet groupId setType updater = zfRemote "CreatePhotoSet" groupId setType updater
