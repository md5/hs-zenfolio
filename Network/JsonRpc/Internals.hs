module Network.JsonRpc.Internals where

import Control.Exception
import Control.OldException (ioErrors) -- XXX
import Control.Monad.Error
import qualified Network.HTTP as H
import Network.URI
import Text.JSON

import Network.JsonRpc.Request
import Network.JsonRpc.Response

data RpcEnv = RpcEnv {
        rpcBaseUri   :: Maybe URI,
        rpcUserAgent :: Maybe String,
        rpcHeaders   :: [H.Header]
    }

nullEnv :: RpcEnv
nullEnv = RpcEnv {
    rpcBaseUri   = Nothing,
    rpcUserAgent = Nothing,
    rpcHeaders   = []
}

type Err m a = ErrorT String m a

jsonErrorToErr :: (JSON a, Monad m) => Result a -> Err m a
jsonErrorToErr = ErrorT . liftM resultToEither . return

-- | Catch IO errors in the error monad.
ioErrorToErr :: IO a -> Err IO a
ioErrorToErr = ErrorT . liftM (either (Left . show) Right) . tryJust ioErrors

-- | Handle errors from the error monad.
handleError :: Monad m => (String -> m a) -> Err m a -> m a
handleError h m = do 
		  Right x <- runErrorT (catchError m (lift . h))
		  return x

parseResponse :: Monad m => String -> Err m Response
parseResponse = jsonErrorToErr . decode

renderCall :: Request -> String
renderCall = encode . showJSON
