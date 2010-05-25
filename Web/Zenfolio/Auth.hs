module Web.Zenfolio.Auth (
    getChallenge,
    authenticate,

    zfLogin
) where

import Control.Monad.Reader (ask)
import qualified Data.Digest.SHA256 as SHA256 (hash)
import Data.String.UTF8 (fromString, toRep)
import Network.JsonRpc (RpcAction, remote)
import Web.Zenfolio.Types (LoginName, AuthChallenge(..), Password, AuthToken)

getChallenge :: LoginName -> RpcAction IO AuthChallenge
getChallenge login = do
    env <- ask
    remote env "GetChallenge" login

authenticate :: AuthChallenge -> Password -> RpcAction IO AuthToken
authenticate challenge password = do
    env <- ask
    remote env "Authenticate" (acChallenge challenge) roundTwo
    where roundOne = SHA256.hash $ acPasswordSalt challenge ++ toRep (fromString password)
          roundTwo = SHA256.hash $ acChallenge challenge    ++ roundOne

zfLogin :: LoginName -> Password -> RpcAction IO AuthToken
zfLogin username password = getChallenge username >>= flip authenticate password
