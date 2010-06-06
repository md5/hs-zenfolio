module Web.Zenfolio.Categories (
    getCategories
) where

import Web.Zenfolio.Monad (ZM)
import Web.Zenfolio.RPC (zfRemote)
import Web.Zenfolio.Types (CategoryInfo)

getCategories :: ZM [CategoryInfo]
getCategories = zfRemote "GetCategories"
