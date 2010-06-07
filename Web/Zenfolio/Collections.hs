module Web.Zenfolio.Collections (
    collectionAddPhoto,
    collectionRemovePhoto
) where

import Web.Zenfolio.Monad (ZM)
import Web.Zenfolio.RPC (zfRemote)
import Web.Zenfolio.Types (Void, PhotoID, PhotoSetID)

collectionAddPhoto :: PhotoSetID -> PhotoID -> ZM Void
collectionAddPhoto = zfRemote "CollectionAddPhoto"

collectionRemovePhoto :: PhotoSetID -> PhotoID -> ZM Void
collectionRemovePhoto = zfRemote "CollectionRemovePhoto"
