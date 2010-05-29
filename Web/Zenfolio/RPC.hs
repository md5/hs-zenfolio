module Web.Zenfolio.RPC (
    zfRemote,
    zfAgentHeaders,
    zfTokenHeader
) where

import Network.JsonRpc (MethodName, Remote, remote)
import qualified Network.HTTP as H (Header(..), HeaderName(..))
import Network.URI (URI, parseURI)

import Web.Zenfolio.Types (AuthToken)

zfUserAgent :: String
zfUserAgent = "hs-zenfolio/0.1"

zfAgentHeaders :: [H.Header]
zfAgentHeaders = [ H.Header H.HdrUserAgent zfUserAgent
                 , H.Header (H.HdrCustom "X-Zenfolio-User-Agent") zfUserAgent
                 ]

zfTokenHeader :: AuthToken -> H.Header
zfTokenHeader token = H.Header (H.HdrCustom "X-Zenfolio-Token") token

zfBaseUri :: URI
zfBaseUri = maybe (error $ "Invalid base URI: " ++ base) id (parseURI base)
    where base = "http://www.zenfolio.com/api/1.2/zfapi.asmx"

zfRemote :: Remote a => MethodName -> a
zfRemote = remote zfBaseUri zfAgentHeaders
