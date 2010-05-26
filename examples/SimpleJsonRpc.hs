module Main(main) where

import Text.JSON (JSValue, showJSValue)
import Network.JsonRpc (simpleRemote)
import System.Environment

remoteMethodOne :: String -> String -> String -> IO JSValue
remoteMethodOne url method arg = simpleRemote url method arg

remoteMethodTwo :: String -> String -> String -> String -> IO JSValue
remoteMethodTwo url method arg1 arg2 = simpleRemote url method arg1 arg2

main :: IO ()
main = do
    prog <- getProgName
    args <- getArgs
    case args of
        [url,method,arg]       -> remoteMethodOne url method arg       >>= putJSValue
        [url,method,arg1,arg2] -> remoteMethodTwo url method arg1 arg2 >>= putJSValue
        _                      -> putStrLn $ "Usage: " ++ prog ++ " url method arg [arg2]"
    where putJSValue js = putStrLn $ showJSValue js ""
