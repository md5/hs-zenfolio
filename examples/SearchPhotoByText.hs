module Main(main) where

import Control.Exception (bracket_)
import System.Environment (getArgs, getProgName)
import System.IO (hSetEcho, hFlush, stdin, stdout)

import Web.Zenfolio.API
import qualified Web.Zenfolio.Photos as Photos
import qualified Web.Zenfolio.Types.Search as Search

searchPhoto :: LoginName -> Password -> String -> ZM ()
searchPhoto username password query = do
    token <- login username password
    withToken token $ withDebug True $ do
        result <- Photos.searchPhotoByText "" Search.Date query 0 100
        liftIO $ do
            putStrLn $ "Result: " ++ show result

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
        (username:query:_) -> do
            password <- prompt "Password: "
            zenfolio $ searchPhoto username password query
        _ -> do
            prg <- getProgName
            putStrLn ("Usage: " ++ prg ++ " user-name query")
