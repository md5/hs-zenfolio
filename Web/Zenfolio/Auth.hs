module Web.Zenfolio.Auth (
    authenticate,
    authenticatePlain,
    getChallenge,
    keyringAddKeyPlain,
    loadAccessRealm,
    updateGroupAccess,
    updatePhotoAccess,
    updatePhotoSetAccess,

    login,
    emptyKeyring
) where

import qualified Data.Digest.SHA256 as SHA256 (hash)
import Data.String.UTF8 (fromString, toRep)
import Web.Zenfolio.Monad (ZM)
import Web.Zenfolio.RPC (zfRemote, zfRemoteSsl)
import Web.Zenfolio.Types (LoginName, AuthChallenge(..), Password, AuthToken, RealmID,
                           AccessDescriptor, PhotoID, AccessUpdater, GroupID, PhotoSetID)
import Web.Zenfolio.Types.Access (Keyring(..))

getChallenge :: LoginName -> ZM AuthChallenge
getChallenge = zfRemote "GetChallenge"

authenticate :: AuthChallenge -> Password -> ZM AuthToken
authenticate challenge password = do
        zfRemote "Authenticate" challengeBytes roundTwoBytes
    where saltBytes      = acPasswordSalt challenge
          challengeBytes = acChallenge challenge
          passwordBytes  = toRep $ fromString password
          roundOneBytes  = SHA256.hash $ saltBytes      ++ passwordBytes
          roundTwoBytes  = SHA256.hash $ challengeBytes ++ roundOneBytes

authenticatePlain :: LoginName -> Password -> ZM AuthToken
authenticatePlain = zfRemoteSsl "AuthenticatePlain"

keyringAddKeyPlain :: Keyring -> RealmID -> Password -> ZM Keyring
keyringAddKeyPlain = zfRemoteSsl "KeyringAddKeyPlain"

loadAccessRealm :: RealmID -> ZM AccessDescriptor
loadAccessRealm = zfRemote "LoadAccessRealm"

updateGroupAccess :: GroupID -> AccessUpdater -> ZM ()
updateGroupAccess = zfRemote "UpdateGroupAccess"

updatePhotoAccess :: PhotoID -> AccessUpdater -> ZM ()
updatePhotoAccess = zfRemote "UpdatePhotoAccess"

updatePhotoSetAccess :: PhotoSetID -> AccessUpdater -> ZM ()
updatePhotoSetAccess = zfRemote "UpdatePhotoSetAccess"

-- Helper functions

login :: LoginName -> Password -> ZM AuthToken
login username password = do
    challenge <- getChallenge username
    authenticate challenge password

emptyKeyring :: Keyring
emptyKeyring = Keyring ""
