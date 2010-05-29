module Main(main) where

import Control.Exception (bracket_)
import System.Environment (getArgs, getProgName)
import System.IO (hSetEcho, hFlush, stdin, stdout)

import Web.Zenfolio.API
import qualified Web.Zenfolio.Groups as Groups
import qualified Web.Zenfolio.Photos.Upload as Upload

prompt :: String -> IO Password
prompt message = do
    putStr message >> hFlush stdout
    bracket_ (hSetEcho stdin False)
             (hSetEcho stdin True >> putStrLn "")
             (getLine)

findPhotoSet :: LoginName -> String -> ZM (Maybe PhotoSet)
findPhotoSet username title = do
    rootGroup <- Groups.loadGroupHierarchy username
    findPhotoSet' $ groupElements rootGroup
    where findPhotoSet' [] = return Nothing
          findPhotoSet' (GroupElementPhotoSet ps:ges) =
              if psTitle ps == Just title
                  then return $ Just ps
                  else findPhotoSet' ges
          findPhotoSet' (_:ges) = findPhotoSet' ges

uploadPhoto :: LoginName -> Password -> String -> FilePath -> ZM ()
uploadPhoto username password photoset filename = do
    token <- login username password
    withToken token $ do
        mps <- findPhotoSet username photoset
        case mps of
            Just ps -> do
                photoId <- Upload.uploadFile ps filename
                liftIO $ putStrLn ("Photo id: " ++ show photoId)
            Nothing -> fail $ "No photosets available to upload to"

main :: IO ()
main = do
    ls <- getArgs
    case ls of
        (username:photoset:filename:_) -> do
            password <- prompt "Password: "
            zenfolio $ uploadPhoto username password photoset filename
        _ -> do
            prg <- getProgName
            putStrLn ("Usage: " ++ prg ++ " user-name photo-set filename")
