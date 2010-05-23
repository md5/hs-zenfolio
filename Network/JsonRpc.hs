module Network.JsonRpc (
    Error(..),
    Request(..),
    Response(..),
    Remote,

    module Network.JsonRpc.Monad,

    RpcEnv(..),
    nullEnv,

    call,
    remote
) where

import Control.Monad.Trans (liftIO)
import qualified Data.ByteString.UTF8 as U
import qualified Data.ByteString as BS
import Data.Char
import Data.Maybe
import Network.HTTP hiding (Request, Response)
import qualified Network.HTTP as H
import Network.Stream
import Network.URI
import Text.JSON

import Network.JsonRpc.Error
import Network.JsonRpc.Internals
import Network.JsonRpc.Monad
import Network.JsonRpc.Request
import Network.JsonRpc.Response

handleResponse :: Monad m => Response -> m JSValue
handleResponse (Rsp (Just v) _ _)   = return v
handleResponse (Rsp _ (Just (Err msg code)) _) = fail ("Error " ++ show msg ++ ": " ++ show code)

type MethodName = String

doCall :: String -> Request -> Err IO Response
doCall url req = do
    let reqStr = renderCall req
    liftIO (putStrLn reqStr)
    respStr <- ioErrorToErr (post url reqStr)
    parseResponse respStr

call :: String -> MethodName -> [JSValue] -> Err IO JSValue
call url method args = doCall url (Req method args 0) >>= handleResponse

remote :: Remote a =>
          String
       -> MethodName
       -> a
remote u m = remote_ (\e -> "Error calling " ++ m ++ ": " ++ e) (call u m)

class Remote a where
    remote_ :: (String -> String)
            -> ([JSValue] -> Err IO JSValue)
            -> a

instance JSON a => Remote (IO a) where
    remote_ h f = handleError (fail . h) $ f [] >>= jsonErrorToErr . readJSON

instance (JSON a, Remote r) => Remote (a -> r) where
    remote_ h f x = remote_ h (\xs -> f (showJSON x : xs))

defaultUserAgent :: String
defaultUserAgent = "Haskell JsonRpcClient/0.1"

-- | Handle connection errors.
handleE :: Monad m => (ConnError -> m a) -> Either ConnError a -> m a
handleE h (Left e) = h e
handleE _ (Right v) = return v

post :: String -> String -> IO String
post url content = do
    uri <- maybeFail ("Bad URI: '" ++ url ++ "'") (parseURI url)
    post_ uri content

post_ :: URI -> String -> IO String
post_ uri content = 
    do
    -- FIXME: remove
    --putStrLn (show (request uri content))
    --putStrLn content
    eresp <- simpleHTTP (request uri (U.fromString content))
    resp <- handleE (fail . show) eresp
    case rspCode resp of
		      (2,0,0) -> return (U.toString (rspBody resp))
		      _ -> fail (httpError resp)
    where
    showRspCode (a,b,c) = map intToDigit [a,b,c]
    httpError resp = showRspCode (rspCode resp) ++ " " ++ rspReason resp

-- | Create an XML-RPC compliant HTTP request.
request :: URI -> BS.ByteString -> H.Request BS.ByteString
request uri content = H.Request{ rqURI = uri, 
				    rqMethod = POST, 
				    rqHeaders = headers, 
				    rqBody = content }
    where
    -- the HTTP module adds a Host header based on the URI
    headers = [Header HdrUserAgent defaultUserAgent,
	       Header HdrContentType "application/json;charset=utf-8",
	       Header HdrContentLength (show (BS.length content))
	      ]

maybeFail :: Monad m => String -> Maybe a -> m a
maybeFail msg = maybe (fail msg) return
