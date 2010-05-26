module Web.Zenfolio.Users (
    loadPrivateProfile,
    loadPublicProfile
) where

import Network.JsonRpc (RpcAction)
import Web.Zenfolio.RPC (zfRemote)
import Web.Zenfolio.Types (User, LoginName)

loadPrivateProfile :: RpcAction IO User
loadPrivateProfile = zfRemote "LoadPrivateProfile"

loadPublicProfile :: LoginName -> RpcAction IO User
loadPublicProfile login = zfRemote "LoadPublicProfile" login
