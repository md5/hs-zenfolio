module Web.Zenfolio.Categories (
    getCategories
) where

import Web.Zenfolio.Monad (ZM)
import Web.Zenfolio.RPC (zfRemote)
import Web.Zenfolio.Types (CategoryID)

getCategories :: ZM [CategoryID]
getCategories = zfRemote "GetCategories"
