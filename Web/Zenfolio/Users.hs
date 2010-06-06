module Web.Zenfolio.Users (
    loadGroupHierarchy,
    loadPrivateProfile,
    loadPublicProfile
) where

import Web.Zenfolio.Monad (ZM)
import Web.Zenfolio.RPC (zfRemote)
import Web.Zenfolio.Types (Group, LoginName, User)

loadGroupHierarchy :: LoginName -> ZM Group
loadGroupHierarchy = zfRemote "LoadGroupHierarchy"

loadPrivateProfile :: ZM User
loadPrivateProfile = zfRemote "LoadPrivateProfile"

loadPublicProfile :: LoginName -> ZM User
loadPublicProfile = zfRemote "LoadPublicProfile"
