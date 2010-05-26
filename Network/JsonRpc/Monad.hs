{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Network.JsonRpc.Monad where

import Control.Exception
import Control.OldException (ioErrors) -- XXX
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.Trans
import qualified Network.HTTP as H
import Text.JSON

import Network.JsonRpc.Request
import Network.JsonRpc.Response

data RpcEnv = RpcEnv {
        rpcHeaders   :: [H.Header],
        rpcDebug     :: Bool
    }

rpcEnv :: RpcEnv
rpcEnv = RpcEnv {
    rpcHeaders   = [],
    rpcDebug     = False
}

newtype RpcAction m a = RpcA {
        unA :: ReaderT RpcEnv m a
    } deriving (Monad, MonadIO, MonadReader RpcEnv, MonadTrans)

rpcAction :: Monad m => (RpcEnv -> m a) -> RpcAction m a
rpcAction = RpcA . ReaderT

runRpc :: Monad m => RpcAction m a -> RpcEnv -> m a
runRpc = runReaderT . unA

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
parseResponse = jsonErrorToErr . decodeStrict

renderCall :: Request -> String
renderCall = encodeStrict . showJSON

withHeaders :: Monad m => [H.Header] -> RpcAction m a -> RpcAction m a
withHeaders hs = local $ \env -> env { rpcHeaders = hs }

withExtraHeaders :: Monad m => [H.Header] -> RpcAction m a -> RpcAction m a
withExtraHeaders hs = local $ \env -> env { rpcHeaders = rpcHeaders env ++ hs }

getHeaders :: Monad m => RpcAction m [H.Header]
getHeaders = asks rpcHeaders

withDebug :: Monad m => Bool -> RpcAction m a -> RpcAction m a
withDebug dbg = local $ \env -> env { rpcDebug = dbg }

getDebug :: Monad m => RpcAction m Bool
getDebug = asks rpcDebug

debug :: MonadIO m => String -> RpcAction m ()
debug msg = do
    dbg <- getDebug
    when dbg (liftIO $ putStrLn msg)
