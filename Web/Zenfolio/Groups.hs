module Web.Zenfolio.Groups (
    createGroup,
    deleteGroup,
    loadGroup,
    moveGroup,
    reorderGroup,
    setGroupTitlePhoto,
    updateGroup,
    updateGroupAccess,

    newUpdater
) where

import Web.Zenfolio.Auth (updateGroupAccess)
import Web.Zenfolio.Monad (ZM)
import Web.Zenfolio.RPC (zfRemote)
import Web.Zenfolio.Types (Void, Group, GroupID, GroupIndex, GroupShiftOrder,
                           GroupUpdater(..), PhotoID)

createGroup :: GroupID -> GroupUpdater -> ZM Group
createGroup = zfRemote "CreateGroup"

deleteGroup :: GroupID -> ZM Void
deleteGroup = zfRemote "DeleteGroup"

loadGroup :: GroupID -> ZM Group
loadGroup = zfRemote "LoadGroup"

moveGroup :: GroupID -> GroupID -> GroupIndex -> ZM Void
moveGroup = zfRemote "MoveGroup"

reorderGroup :: GroupID -> GroupShiftOrder -> ZM Void
reorderGroup = zfRemote "ReorderGroup"

setGroupTitlePhoto :: GroupID -> PhotoID -> ZM Void
setGroupTitlePhoto = zfRemote "SetGroupTitlePhoto"

updateGroup :: GroupID -> GroupUpdater -> ZM Group
updateGroup = zfRemote "UpdateGroup"

newUpdater :: GroupUpdater
newUpdater = GroupUpdater {
                 guTitle = Nothing,
                 guCaption = Nothing,
                 guCustomReference = Nothing
             }
