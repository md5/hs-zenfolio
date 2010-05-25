module Web.Zenfolio.PhotoSets (
    createPhotoSet
) where

import Control.Monad.Reader (ask)
import Network.JsonRpc (RpcAction, remote)
import Web.Zenfolio.Types (GroupID, PhotoSetType, PhotoSetUpdater, PhotoSet)
import Web.Zenfolio.Types.JSON ()

createPhotoSet :: GroupID -> PhotoSetType -> PhotoSetUpdater -> RpcAction IO PhotoSet
createPhotoSet groupId setType updater = do
    env <- ask
    remote env "CreatePhotoSet" groupId setType updater
