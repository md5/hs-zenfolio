module Main(main) where

import Control.Exception (bracket_)
import System.Environment (getArgs, getProgName)
import System.IO (hSetEcho, hFlush, stdin, stdout)

import Web.Zenfolio.API
import qualified Web.Zenfolio.Photos as Photos

dumpPhoto :: LoginName -> Password -> PhotoID -> ZM ()
dumpPhoto username password pId = do
    token <- login username password
    withToken token $ withDebug True $ do
        photo <- Photos.loadPhoto pId
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
        (username:photo:_) -> do
            password <- prompt "Password: "
            zenfolio $ dumpPhoto username password (read photo)
        _ -> do
            prg <- getProgName
            putStrLn ("Usage: " ++ prg ++ " user-name photo-id")
