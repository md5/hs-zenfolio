module Main(main) where

import Control.Exception (bracket_)
import System.Environment (getArgs, getProgName)
import System.IO (hSetEcho, hFlush, stdin, stdout)

import Web.Zenfolio.API
import qualified Web.Zenfolio.PhotoSets as PhotoSets
import qualified Web.Zenfolio.Users as Users

prompt :: String -> IO Password
prompt message = do
    putStr message >> hFlush stdout
    bracket_ (hSetEcho stdin False)
             (hSetEcho stdin True >> putStrLn "")
             (getLine)

createCollection :: LoginName -> Password -> String -> ZM ()
createCollection username password title = do
    token <- login username password
    withToken token $ do
        rootGroup <- Users.loadGroupHierarchy username
        let updater = PhotoSets.newUpdater { psuTitle = Just title }
        collection <- PhotoSets.createPhotoSet (groupId rootGroup) Collection updater
        liftIO $ putStrLn ("Collection: " ++ show collection)

main :: IO ()
main = do
    ls <- getArgs
    case ls of
        (username:title:_) -> do
            password <- prompt "Password: "
            zenfolio $ createCollection username password title
        _ -> do
            prg <- getProgName
            putStrLn ("Usage: " ++ prg ++ " user-name title")
