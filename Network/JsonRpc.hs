{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}

module Network.JsonRpc (
    Error(..),
    Request(..),
    Response(..),
    Remote(),

    module Network.JsonRpc.Monad,

    MethodName,
    remote,
    simpleRemote
) where

import Control.Monad.Reader
import Control.Monad.Trans (lift)
import qualified Data.ByteString.UTF8 as U
import qualified Data.ByteString as BS
import Data.Char
import Data.List
import Data.Time.Clock.POSIX
import Data.Maybe
import Network.HTTP hiding (Request, Response, defaultUserAgent, getHeaders)
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

doCall :: URI -> [H.Header] -> Request -> Err IO Response
doCall uri headers req = do
    let reqStr = renderCall req
    respStr <- ioErrorToErr (post uri headers reqStr)
    parseResponse respStr

call :: URI -> [H.Header] -> MethodName -> ([H.Header] -> [JSValue] -> Err IO JSValue)
call uri initialHeaders method extraHeaders args = do
    requestId <- lift getRequestId
    rpcResp <- doCall uri (initialHeaders ++ extraHeaders) (Req method args requestId)
    handleResponse rpcResp
    where getRequestId = getPOSIXTime >>= return . floor

remote :: Remote a =>
          URI
       -> [H.Header]
       -> MethodName
       -> a
remote uri headers method = remote_ (\err -> "Error calling " ++ method ++ ": " ++ err)
                                    (call uri headers method)

simpleRemote :: Remote a =>
                String
             -> MethodName
             -> a
simpleRemote uriStr method = let uri = fromMaybe (error $ "Invalid URI: " ++ uriStr)
                                                 (parseURI uriStr)
                             in remote uri [] method

class Remote a where
    remote_ :: (String -> String)
            -> ([H.Header] -> [JSValue] -> Err IO JSValue)
            -> a

ioRemote_ :: JSON a =>
             (String -> String)
          -> ([H.Header] -> [JSValue] -> Err IO JSValue)
          -> [H.Header]
          -> IO a
ioRemote_ h f headers = handleError (fail . h) $ f headers [] >>= jsonErrorToErr . readJSON

instance JSON a => Remote (IO a) where
    remote_ h f = ioRemote_ h f []

instance JSON a => Remote (RpcAction IO a) where
    remote_ h f = getHeaders >>= liftIO . ioRemote_ h f

instance (JSON a, Remote r) => Remote (a -> r) where
    remote_ h f x = remote_ h (\hs xs -> f hs (showJSON x : xs))

defaultUserAgent :: String
defaultUserAgent = "Haskell JsonRpcClient/0.1"

-- | Handle connection errors.
handleE :: Monad m => (ConnError -> m a) -> Either ConnError a -> m a
handleE h (Left e) = h e
handleE _ (Right v) = return v

post :: URI -> [H.Header] -> String -> IO String
post uri headers content = do
    eresp <- simpleHTTP $ request uri headers (U.fromString content)
    resp <- handleE (fail . show) eresp
    case rspCode resp of
		      (2,0,0) -> return (U.toString (rspBody resp))
		      _ -> fail (httpError resp)
    where
    showRspCode (a,b,c) = map intToDigit [a,b,c]
    httpError resp = showRspCode (rspCode resp) ++ " " ++ rspReason resp

-- XXX: We should allow the user to pass a content type header matching known
-- JSON content types and possibly including an alternate charset; if the user
-- doesn't add their own content type header, fall back to this. Allowing an
-- alternate charset would involve moving from Data.ByteString.UTF8 to Text or
-- some other method supporting more encodings
--
-- CGI.parseContentType is able to parse these things headers, but I'd rather
-- not be adding a CGI dependency
jsonContentType :: String
jsonContentType = "application/json;charset=utf-8"

-- | Create an XML-RPC compliant HTTP request.
request :: URI -> [H.Header] -> BS.ByteString -> H.Request BS.ByteString
request uri headers content = pipeline req
    where req = H.Request {
                    rqURI     = uri,
                    rqMethod  = POST,
                    rqHeaders = headers,
                    rqBody    = content
                }

          pipeline :: H.Request BS.ByteString -> H.Request BS.ByteString
          pipeline = addDefaultUserAgent . setJsonRpcHeaders
  
          setJsonRpcHeaders :: H.Request BS.ByteString -> H.Request BS.ByteString
          setJsonRpcHeaders = ct . cl
              where ct = replaceHeader HdrContentType jsonContentType
                    cl = replaceHeader HdrContentLength (show (BS.length content))
  
          addDefaultUserAgent :: H.Request BS.ByteString -> H.Request BS.ByteString
          addDefaultUserAgent = insertHeaderIfMissing HdrUserAgent defaultUserAgent
