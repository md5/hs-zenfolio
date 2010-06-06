module Web.Zenfolio.PhotoSets (
    createPhotoSet,
    deletePhotoSet,
    loadPhotoSet,
    movePhotoSet,
    reorderPhotoSet,
    setPhotoSetTitlePhoto,
    setPhotoSetFeaturedIndex,
    updatePhotoSet,
    updatePhotoSetAccess,

    newUpdater
) where

import Web.Zenfolio.Auth (updatePhotoSetAccess)
import Web.Zenfolio.Monad (ZM)
import Web.Zenfolio.RPC (zfRemote)
import Web.Zenfolio.Types (GroupID, GroupIndex, PhotoSet, PhotoSetID,
                           PhotoSetType, PhotoSetUpdater(..), PhotoID,
                           ShiftOrder)

createPhotoSet :: GroupID -> PhotoSetType -> PhotoSetUpdater -> ZM PhotoSet
createPhotoSet = zfRemote "CreatePhotoSet"

deletePhotoSet :: PhotoSetID -> ZM ()
deletePhotoSet = zfRemote "DeletePhotoSet"

loadPhotoSet :: PhotoSetID -> ZM PhotoSet
loadPhotoSet = zfRemote "LoadPhotoSet"

movePhotoSet :: PhotoSetID -> GroupID -> GroupIndex -> ZM ()
movePhotoSet = zfRemote "MovePhotoSet"

reorderPhotoSet :: PhotoSetID -> ShiftOrder -> ZM ()
reorderPhotoSet = zfRemote "ReorderPhotoSet"

setPhotoSetFeaturedIndex :: PhotoSetID -> GroupIndex -> ZM ()
setPhotoSetFeaturedIndex = zfRemote "SetPhotoSetFeaturedIndex"

setPhotoSetTitlePhoto :: PhotoSetID -> PhotoID -> ZM ()
setPhotoSetTitlePhoto = zfRemote "SetPhotoSetTitlePhoto"

updatePhotoSet :: PhotoSetID -> PhotoSetUpdater -> ZM PhotoSet
updatePhotoSet = zfRemote "UpdatePhotoSet"

newUpdater :: PhotoSetUpdater
newUpdater = PhotoSetUpdater {
                 psuTitle = Nothing,
                 psuCaption = Nothing,
                 psuKeywords = Nothing,
                 psuCategories = Nothing,
                 psuCustomReference = Nothing
             }
