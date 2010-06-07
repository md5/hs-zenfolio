module Web.Zenfolio.PhotoSets (
    createPhotoSet,
    deletePhotoSet,
    getPopularSets,
    getRecentSets,
    loadPhotoSet,
    movePhotoSet,
    reorderPhotoSet,
    searchSetByCategory,
    searchSetByText,
    setPhotoSetTitlePhoto,
    setPhotoSetFeaturedIndex,
    updatePhotoSet,
    updatePhotoSetAccess,

    newUpdater
) where

import Web.Zenfolio.Auth (updatePhotoSetAccess)
import Web.Zenfolio.Monad (ZM)
import Web.Zenfolio.RPC (zfRemote)
import Web.Zenfolio.Types (Void, GroupID, GroupIndex, PhotoSet, PhotoSetID,
                           PhotoSetType, PhotoSetUpdater(..), PhotoID,
                           ShiftOrder, SearchID, PhotoSetResult, SortOrder, CategoryID)

createPhotoSet :: GroupID -> PhotoSetType -> PhotoSetUpdater -> ZM PhotoSet
createPhotoSet = zfRemote "CreatePhotoSet"

deletePhotoSet :: PhotoSetID -> ZM Void
deletePhotoSet = zfRemote "DeletePhotoSet"

getPopularSets :: PhotoSetType -> Int -> Int -> ZM [PhotoSet]
getPopularSets = zfRemote "GetPopularSets"

getRecentSets :: PhotoSetType -> Int -> Int -> ZM [PhotoSet]
getRecentSets = zfRemote "GetRecentSets"

loadPhotoSet :: PhotoSetID -> ZM PhotoSet
loadPhotoSet = zfRemote "LoadPhotoSet"

movePhotoSet :: PhotoSetID -> GroupID -> GroupIndex -> ZM Void
movePhotoSet = zfRemote "MovePhotoSet"

reorderPhotoSet :: PhotoSetID -> ShiftOrder -> ZM Void
reorderPhotoSet = zfRemote "ReorderPhotoSet"

searchSetByCategory :: SearchID -> PhotoSetType -> SortOrder -> CategoryID
                    -> Int -> Int -> ZM PhotoSetResult
searchSetByCategory = zfRemote "SearchSetByCategory"

searchSetByText :: SearchID -> PhotoSetType -> SortOrder -> String
                -> Int -> Int -> ZM PhotoSetResult
searchSetByText = zfRemote "SearchSetByText"

setPhotoSetFeaturedIndex :: PhotoSetID -> GroupIndex -> ZM Void
setPhotoSetFeaturedIndex = zfRemote "SetPhotoSetFeaturedIndex"

setPhotoSetTitlePhoto :: PhotoSetID -> PhotoID -> ZM Void
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
