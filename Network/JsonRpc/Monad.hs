module Network.JsonRpc.Monad where

import Control.Monad.Reader
import qualified Network.HTTP as H
import Network.URI

import Network.JsonRpc.Internals

withEnv :: (RpcEnv -> RpcEnv) -> RpcM m a -> RpcM m a
withEnv fenv = withRpcM fenv

getEnv :: Monad m => RpcM m RpcEnv
getEnv = RpcM $ \env -> return env

withBaseUri :: URI -> RpcM m a -> RpcM m a
withBaseUri u = withEnv $ \env -> env { rpcBaseUri = Just u }

getBaseUri :: Monad m => RpcM m (Maybe URI)
getBaseUri = getEnv >>= return . rpcBaseUri

withUserAgent :: String -> RpcM m a -> RpcM m a
withUserAgent ua = withEnv $ \env -> env { rpcUserAgent = Just ua }

getUserAgent :: Monad m => RpcM m (Maybe String)
getUserAgent = getEnv >>= return . rpcUserAgent

withHeaders :: [H.Header] -> RpcM m a -> RpcM m a
withHeaders hs = withEnv $ \env -> env { rpcHeaders = hs }

withExtraHeaders :: [H.Header] -> RpcM m a -> RpcM m a
withExtraHeaders hs = withEnv $ \env -> env { rpcHeaders = rpcHeaders env ++ hs }

getHeaders :: Monad m => RpcM m [H.Header]
getHeaders = getEnv >>= return . rpcHeaders

newtype RpcM m a = RpcM { runRpcM :: RpcEnv -> Err m a }

instance Monad m => Monad (RpcM m) where
    return x = RpcM $ \_   -> return x
    m >>= k  = RpcM $ \env -> do
        v <- runRpcM m env
        runRpcM (k v) env

withRpcM :: (RpcEnv -> RpcEnv) -> RpcM m a -> RpcM m a
withRpcM f m = RpcM $ runRpcM m . f
