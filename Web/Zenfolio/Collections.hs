module Web.Zenfolio.Collections (
    collectionAddPhoto,
    collectionRemovePhoto
) where

import Web.Zenfolio.Monad (ZM)
import Web.Zenfolio.RPC (zfRemote)
import Web.Zenfolio.Types (PhotoID, PhotoSetID)

collectionAddPhoto :: PhotoSetID -> PhotoID -> ZM ()
collectionAddPhoto = zfRemote "CollectionAddPhoto"

collectionRemovePhoto :: PhotoSetID -> PhotoID -> ZM ()
collectionRemovePhoto = zfRemote "CollectionRemovePhoto"
