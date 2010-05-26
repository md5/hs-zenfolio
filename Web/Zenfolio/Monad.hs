{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Web.Zenfolio.Monad (
    ZM,
    zenfolio,

    withToken
) where

import Control.Monad.Reader (MonadReader, ReaderT(..), asks, local)
import Control.Monad.Trans (MonadIO, liftIO)
import Network.JsonRpc (Remote(..), ioRemote_, JSON, Header(..), HeaderName(..))

import Web.Zenfolio.Types (AuthToken)

data ZMEnv = ZMEnv {
        zmToken :: Maybe AuthToken
    } deriving (Eq, Show)

newtype ZM a = ZM {
        unZM :: ReaderT ZMEnv IO a
    } deriving (Monad, MonadReader ZMEnv, MonadIO)

instance JSON a => Remote (ZM a) where
    remote_ h f = do
        token <- getToken
        liftIO $ ioRemote_ h f (headers token)
        where headers Nothing      = []
              headers (Just token) = [ Header (HdrCustom "X-Zenfolio-Token") token ]

zenfolio :: ZM a -> IO a
zenfolio zm = runReaderT (unZM zm) nullEnv

nullEnv :: ZMEnv
nullEnv = ZMEnv {
        zmToken = Nothing
    }

getToken :: ZM (Maybe AuthToken)
getToken = asks zmToken

withToken :: AuthToken -> ZM a -> ZM a
withToken token zm = local addToken zm
    where addToken env = env { zmToken = Just token }
