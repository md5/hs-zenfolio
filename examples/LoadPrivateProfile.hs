module Main(main) where

import Control.Exception (bracket_)
import Data.Maybe (fromMaybe)
import System.Environment (getArgs, getProgName)
import System.IO (hSetEcho, hFlush, stdin, stdout)

import Web.Zenfolio.API
import qualified Web.Zenfolio.Users as Users

dumpProfile :: LoginName -> Password -> ZM ()
dumpProfile username password = do
    token <- login username password
    withToken token $ do
        user <- Users.loadPrivateProfile
        liftIO $ do
            let ln = uLoginName user
                dn = fromMaybe "<anonymous>" $ uDisplayName user
            putStrLn $ "User: " ++ dn ++ " (" ++ ln ++ ")"
            putStrLn $ "Private profile: " ++ show user

prompt :: String -> IO Password
prompt message = do
    putStr message >> hFlush stdout
    bracket_ (hSetEcho stdin False)
             (hSetEcho stdin True)
             (getLine)

main :: IO ()
main = do
    ls <- getArgs
    case ls of
        [] -> do
            prg <- getProgName
            putStrLn ("Usage: " ++ prg ++ " user-name")
        (username:_) -> do
            password <- prompt "Password: "
            zenfolio $ dumpProfile username password
