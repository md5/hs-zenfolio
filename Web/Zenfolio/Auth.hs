module Web.Zenfolio.Auth (
    getChallenge,
    authenticate,
    authenticatePlain,

    login
) where

import qualified Data.Digest.SHA256 as SHA256 (hash)
import Data.String.UTF8 (fromString, toRep)
import Web.Zenfolio.Monad (ZM)
import Web.Zenfolio.RPC (zfRemote, zfRemoteSsl)
import Web.Zenfolio.Types (LoginName, AuthChallenge(..), Password, AuthToken)

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

login :: LoginName -> Password -> ZM AuthToken
login username password = do
    challenge <- getChallenge username
    authenticate challenge password
