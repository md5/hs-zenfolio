module Web.Zenfolio.Auth (
    getChallenge,
    authenticate,
    login
) where

import qualified Data.Digest.SHA256 as SHA256 (hash)
import Data.String.UTF8 (fromString, toRep)
import Network.JsonRpc (RpcAction)
import Web.Zenfolio.RPC (zfRemote)
import Web.Zenfolio.Types (LoginName, AuthChallenge(..), Password, AuthToken)

getChallenge :: LoginName -> RpcAction IO AuthChallenge
getChallenge loginName = zfRemote "GetChallenge" loginName

authenticate :: AuthChallenge -> Password -> RpcAction IO AuthToken
authenticate challenge password = zfRemote "Authenticate" challengeBytes roundTwoBytes
    where saltBytes      = acPasswordSalt challenge
          challengeBytes = acChallenge challenge
          passwordBytes  = toRep $ fromString password
          roundOneBytes  = SHA256.hash $ saltBytes      ++ passwordBytes
          roundTwoBytes  = SHA256.hash $ challengeBytes ++ roundOneBytes

login :: LoginName -> Password -> RpcAction IO AuthToken
login username password = do
    challenge <- getChallenge username
    authenticate challenge password
