module Web.Zenfolio.RPC (
    zfRemote
) where

import Network.JsonRpc (MethodName, Remote, remote)
import qualified Network.HTTP as H (Header(..), HeaderName(..))
import Network.URI (URI, parseURI)

zfUserAgent :: String
zfUserAgent = "hs-zenfolio/0.1"

zfBaseUri :: URI
zfBaseUri = maybe (error $ "Invalid base URI: " ++ base) id (parseURI base)
    where base = "http://www.zenfolio.com/api/1.2/zfapi.asmx"

zfRemote :: Remote a => MethodName -> a
zfRemote = remote zfBaseUri agentHeaders
    where agentHeaders = [ H.Header H.HdrUserAgent zfUserAgent,
                           H.Header (H.HdrCustom "X-Zenfolio-User-Agent") zfUserAgent ]
