module Web.Zenfolio.Groups (
    createGroup,
    deleteGroup,
    loadGroup,
    moveGroup,
    updateGroup
) where

import Web.Zenfolio.Monad (ZM)
import Web.Zenfolio.RPC (zfRemote)
import Web.Zenfolio.Types (Group, GroupID, GroupIndex, GroupUpdater)

createGroup :: GroupID -> GroupUpdater -> ZM Group
createGroup = zfRemote "CreateGroup"

deleteGroup :: GroupID -> ZM ()
deleteGroup = zfRemote "DeleteGroup"

loadGroup :: GroupID -> ZM Group
loadGroup = zfRemote "LoadGroup"

moveGroup :: GroupID -> GroupID -> GroupIndex -> ZM ()
moveGroup = zfRemote "MoveGroup"

updateGroup :: GroupID -> GroupUpdater -> ZM Group
updateGroup = zfRemote "UpdateGroup"
