module Web.Zenfolio.Users (
    loadPrivateProfile,
    loadPublicProfile
) where

import Web.Zenfolio.Monad (ZM)
import Web.Zenfolio.RPC (zfRemote)
import Web.Zenfolio.Types (User, LoginName)

loadPrivateProfile :: ZM User
loadPrivateProfile = zfRemote "LoadPrivateProfile"

loadPublicProfile :: LoginName -> ZM User
loadPublicProfile login = zfRemote "LoadPublicProfile" login
