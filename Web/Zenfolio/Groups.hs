module Web.Zenfolio.Groups (
    loadGroupHierarchy
) where

import Control.Monad.Reader (ask)
import Network.JsonRpc (RpcAction, remote)
import Web.Zenfolio.Types (Group, LoginName)

loadGroupHierarchy :: LoginName -> RpcAction IO Group
loadGroupHierarchy login = do
    env <- ask
    remote env "LoadGroupHierarchy" login
