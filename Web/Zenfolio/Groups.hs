module Web.Zenfolio.Groups (
    loadGroupHierarchy
) where

import Network.JsonRpc (RpcAction)
import Web.Zenfolio.RPC (zfRemote)
import Web.Zenfolio.Types (Group, LoginName)

loadGroupHierarchy :: LoginName -> RpcAction IO Group
loadGroupHierarchy login = zfRemote "LoadGroupHierarchy" login
