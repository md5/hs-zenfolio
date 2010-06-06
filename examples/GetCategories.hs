module Main(main) where

import System.Environment (getArgs, getProgName)

import Web.Zenfolio.API
import qualified Web.Zenfolio.Categories as Categories

dumpCategories :: ZM ()
dumpCategories = do
    categories <- Categories.getCategories
    liftIO $ putStrLn ("Categories: " ++ show categories)

main :: IO ()
main = do
    ls <- getArgs
    case ls of
        [] -> zenfolio $ dumpCategories
        _  -> do
            prg <- getProgName
            putStrLn ("Usage: " ++ prg)
