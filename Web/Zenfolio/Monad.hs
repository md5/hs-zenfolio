module Web.Zenfolio.Monad where

data ZM a = ZM (ZMEnv -> IO a)

data ZMEnv = ZMEnv {
             }

instance Monad ZM where
    return = ZM . const . return

    ZM a >>= k = ZM $ \env -> do v <- a env
                                 case k v of
                                      ZM b -> b env
