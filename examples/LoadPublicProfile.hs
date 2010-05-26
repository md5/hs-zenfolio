module Main(main) where

import Data.Maybe (fromMaybe)
import System.Environment (getArgs, getProgName)

import Web.Zenfolio.API
import qualified Web.Zenfolio.Users as Users

rpcMain :: String -> RpcAction IO ()
rpcMain username = do
    user <- Users.loadPublicProfile username
    liftIO $ do
        let ln = uLoginName user
            dn = fromMaybe "<anonymous>" $ uDisplayName user
        putStrLn $ "User: " ++ dn ++ " (" ++ ln ++ ")"
        putStrLn $ "Public profile: " ++ show user

main :: IO ()
main = do
    ls <- getArgs
    case ls of
        [] -> do
            prg <- getProgName
            putStrLn ("Usage: " ++ prg ++ " user-name")
        (x:_) -> do
            runRpc (rpcMain x) rpcEnv
