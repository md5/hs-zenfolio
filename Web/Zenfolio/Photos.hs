module Web.Zenfolio.Photos (
    loadPhoto
) where

import Web.Zenfolio.Monad (ZM)
import Web.Zenfolio.RPC (zfRemote)
import Web.Zenfolio.Types (PhotoID, Photo)

loadPhoto :: PhotoID -> ZM Photo
loadPhoto = zfRemote "LoadPhoto"
