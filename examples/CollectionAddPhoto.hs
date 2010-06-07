module Main(main) where

import Control.Exception (bracket_)
import System.Environment (getArgs, getProgName)
import System.IO (hSetEcho, hFlush, stdin, stdout)

import Web.Zenfolio.API
import qualified Web.Zenfolio.Collections as Collections
import qualified Web.Zenfolio.PhotoSets as PhotoSets

prompt :: String -> IO Password
prompt message = do
    putStr message >> hFlush stdout
    bracket_ (hSetEcho stdin False)
             (hSetEcho stdin True >> putStrLn "")
             (getLine)

collectionAddPhoto :: LoginName -> Password -> PhotoSetID -> PhotoID -> ZM ()
collectionAddPhoto username password collId pId = do
    token <- login username password
    withToken token $ withDebug True $ do
        Collections.collectionAddPhoto collId pId
        collection <- PhotoSets.loadPhotoSet collId
        liftIO $ putStrLn ("Collection: " ++ show collection)

main :: IO ()
main = do
    ls <- getArgs
    case ls of
        (username:collection:photo:_) -> do
            password <- prompt "Password: "
            zenfolio $ collectionAddPhoto username password (read collection) (read photo)
        _ -> do
            prg <- getProgName
            putStrLn ("Usage: " ++ prg ++ " user-name collection-id photo-id")
