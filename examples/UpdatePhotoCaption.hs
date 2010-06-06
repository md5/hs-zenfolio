module Main(main) where

import Control.Exception (bracket_)
import System.Environment (getArgs, getProgName)
import System.IO (hSetEcho, hFlush, stdin, stdout)

import Web.Zenfolio.API
import qualified Web.Zenfolio.Photos as Photos

updatePhoto :: LoginName -> Password -> PhotoID -> String -> ZM ()
updatePhoto username password pId caption = do
    token <- login username password
    withToken token $ withDebug True $ do
        let updater = Photos.newUpdater {
                         puCaption = Just caption
                      }
        photo <- Photos.updatePhoto pId updater
        liftIO $ do
            putStrLn $ "Photo: " ++ show photo

prompt :: String -> IO Password
prompt message = do
    putStr message >> hFlush stdout
    bracket_ (hSetEcho stdin False)
             (hSetEcho stdin True >> putStrLn "")
             (getLine)

main :: IO ()
main = do
    ls <- getArgs
    case ls of
        (username:photo:caption:_) -> do
            password <- prompt "Password: "
            zenfolio $ updatePhoto username password (read photo) caption
        _ -> do
            prg <- getProgName
            putStrLn ("Usage: " ++ prg ++ " user-name photo-id new-caption")
