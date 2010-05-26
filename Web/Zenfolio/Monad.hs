{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Web.Zenfolio.Monad (
    ZM,
    zenfolio,

    getToken,
    withToken,

    getDebug,
    withDebug,
    debug
) where

import Control.Monad (when)
import Control.Monad.Reader (MonadReader, ReaderT(..), asks, local)
import Control.Monad.Trans (MonadIO, liftIO)
import Network.JsonRpc (Remote(..), ioRemote_, JSON, Header(..), HeaderName(..),
                        RpcEnv(..), rpcEnv)

import Web.Zenfolio.Types (AuthToken)

data ZMEnv = ZMEnv {
        zmToken :: Maybe AuthToken,
        zmDebug :: Bool
    } deriving (Eq, Show)

newtype ZM a = ZM {
        unZM :: ReaderT ZMEnv IO a
    } deriving (Monad, MonadReader ZMEnv, MonadIO)

instance JSON a => Remote (ZM a) where
    remote_ h f = do
        token <- getToken
        dbg   <- getDebug
        liftIO $ do
            let env = rpcEnv {
                          rpcHeaders = headers token,
                          rpcDebug   = dbg
                      }
            ioRemote_ h f env
        where headers Nothing      = []
              headers (Just token) = [ Header (HdrCustom "X-Zenfolio-Token") token ]

zenfolio :: ZM a -> IO a
zenfolio zm = runReaderT (unZM zm) nullEnv

nullEnv :: ZMEnv
nullEnv = ZMEnv {
        zmToken = Nothing,
        zmDebug = False
    }

getToken :: ZM (Maybe AuthToken)
getToken = asks zmToken

withToken :: AuthToken -> ZM a -> ZM a
withToken token zm = local addToken zm
    where addToken env = env { zmToken = Just token }

getDebug :: ZM Bool
getDebug = asks zmDebug

withDebug :: Bool -> ZM a -> ZM a
withDebug dbg zm = local setDebug zm
    where setDebug env = env { zmDebug = dbg }

debug :: String -> ZM ()
debug msg = getDebug >>= \d -> when d (liftIO $ putStrLn msg)
