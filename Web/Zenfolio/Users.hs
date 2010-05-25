module Web.Zenfolio.Users (
    loadPrivateProfile,
    loadPublicProfile
) where

import Control.Monad.Reader (ask)
import Network.JsonRpc (RpcAction, remote)
import Web.Zenfolio.Types (User, LoginName)

loadPrivateProfile :: RpcAction IO User
loadPrivateProfile = do
    env <- ask
    remote env "LoadPrivateProfile"

loadPublicProfile :: LoginName -> RpcAction IO User
loadPublicProfile login = do
    env <- ask
    remote env "LoadPublicProfile" login
