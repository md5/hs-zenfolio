{-# LANGUAGE FlexibleInstances #-}

module Network.JsonRpc (
    Error(..),
    Request(..),
    Response(..),
    Remote,

    module Network.JsonRpc.Monad,

    call,
    remote
) where

import Control.Monad (when)
import Control.Monad.Trans (lift)
import qualified Data.ByteString.UTF8 as U
import qualified Data.ByteString as BS
import Data.Char
import Data.List
import Data.Time.Clock.POSIX
import Data.Maybe
import Network.HTTP hiding (Request, Response, defaultUserAgent)
import qualified Network.HTTP as H
import Network.Stream
import Network.URI
import Text.JSON

import Network.JsonRpc.Error
import Network.JsonRpc.Monad
import Network.JsonRpc.Request
import Network.JsonRpc.Response

handleResponse :: Monad m => Response -> m JSValue
handleResponse (Rsp (Just v) _ _)              = return v
handleResponse (Rsp _ (Just (Err msg code)) _) = fail ("Error " ++ show msg ++ ": " ++ show code)
handleResponse (Rsp _ _ _)                     = fail "Unknown error"

type MethodName = String

doCall :: RpcEnv -> Request -> Err IO Response
doCall env req = do
    let uri    = rpcBaseUri env
        reqStr = renderCall req
    debug_ $ "URI:     " ++ show uri
    debug_ $ "Content: " ++ reqStr
    respStr <- ioErrorToErr (post uri (rpcHeaders env) reqStr)
    debug_ $ "Response: " ++ respStr
    parseResponse respStr
    where debug_ str = when (rpcDebug env) (lift $ putStrLn str)

call :: RpcEnv -> MethodName -> [JSValue] -> Err IO JSValue
call env method args = do
    requestId <- lift getRequestId
    rpcResp <- doCall env (Req method args requestId)
    handleResponse rpcResp
    where getRequestId = getPOSIXTime >>= return . floor

remote :: Remote a =>
          RpcEnv
       -> MethodName
       -> a
remote env m = remote_ (\e -> "Error calling " ++ m ++ ": " ++ e) (call env m)

class Remote a where
    remote_ :: (String -> String)
            -> ([JSValue] -> Err IO JSValue)
            -> a

instance JSON a => Remote (RpcAction IO a) where
    remote_ h f = rpcAction $ \_ ->
        handleError (fail . h) $ f [] >>= jsonErrorToErr . readJSON

instance (JSON a, Remote r) => Remote (a -> r) where
    remote_ h f x = remote_ h (\xs -> f (showJSON x : xs))

defaultUserAgent :: String
defaultUserAgent = "Haskell JsonRpcClient/0.1"

-- | Handle connection errors.
handleE :: Monad m => (ConnError -> m a) -> Either ConnError a -> m a
handleE h (Left e) = h e
handleE _ (Right v) = return v

post :: URI -> [H.Header] -> String -> IO String
post uri headers content = do
    eresp <- simpleHTTP (request uri headers (U.fromString content))
    resp <- handleE (fail . show) eresp
    case rspCode resp of
		      (2,0,0) -> return (U.toString (rspBody resp))
		      _ -> fail (httpError resp)
    where
    showRspCode (a,b,c) = map intToDigit [a,b,c]
    httpError resp = showRspCode (rspCode resp) ++ " " ++ rspReason resp

-- | Create an XML-RPC compliant HTTP request.
request :: URI -> [H.Header] -> BS.ByteString -> H.Request BS.ByteString
request uri headers content = H.Request {
    rqURI = uri, 
    rqMethod = POST, 
    rqHeaders = headers ++ filter (\h -> isNothing $ lookupHeader (hdrName h) headers) [
        Header HdrUserAgent defaultUserAgent,
        Header HdrContentType "application/json;charset=utf-8",
        Header HdrContentLength (show (BS.length content))
    ], 
    rqBody = content
}
