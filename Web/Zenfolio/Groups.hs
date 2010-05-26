module Web.Zenfolio.Groups (
    loadGroupHierarchy
) where

import Web.Zenfolio.Monad (ZM)
import Web.Zenfolio.RPC (zfRemote)
import Web.Zenfolio.Types (Group, LoginName)

loadGroupHierarchy :: LoginName -> ZM Group
loadGroupHierarchy login = zfRemote "LoadGroupHierarchy" login
