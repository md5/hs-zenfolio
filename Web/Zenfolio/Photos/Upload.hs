module Web.Zenfolio.Photos.Upload (
    uploadFile
) where

import Codec.MIME.Parse (parseMIMEType)
import Codec.MIME.Type (Type(..), MIMEType(..), showType)
import Control.Monad (when)
import Data.ByteString as BS (ByteString, readFile, length)
import Data.ByteString.UTF8 as U (toString)
import Data.Maybe (catMaybes)
import Data.Time (UTCTime, TimeZone(..), utcToZonedTime)
import Data.Time.Clock.POSIX (POSIXTime, posixSecondsToUTCTime)
import Data.Time.Format (formatTime)
import Magic (MagicFlag(..), magicOpen, magicLoadDefault, magicFile)
import Network.HTTP (urlEncodeVars, simpleHTTP, getResponseBody,
                     Header(..), HeaderName(..),
                     Request(..), RequestMethod(..))
import Network.URI (URI, parseURI, uriQuery)
import System.FilePath.Posix (takeFileName)
import System.Locale (defaultTimeLocale, rfc822DateFormat)
import System.Posix.Files (getFileStatus, modificationTime)

import Web.Zenfolio.Monad (ZM, liftIO, getToken)
import Web.Zenfolio.RPC (zfAgentHeaders, zfTokenHeader)
import Web.Zenfolio.Types (PhotoSet(..), PhotoID)

getFileType :: FilePath -> IO Type
getFileType fn = do
    magic <- magicOpen [MagicMime, MagicError]
    magicLoadDefault magic
    magicType <- magicFile magic fn
    maybe (fail $ "Unable to determine mime type for file: " ++ fn)
          return (parseMIMEType magicType)

getStrippedImageFileType :: FilePath -> IO Type
getStrippedImageFileType fn = do
    fullFileType <- getFileType fn
    when (not $ isImage fullFileType)
         (fail $ "File is not an image: file=" ++ fn ++ ", type=" ++ show fullFileType)
    return $ fullFileType { mimeParams = [] }

isImage :: Type -> Bool
isImage t = case mimeType t of
                 Image _ -> True
                 _       -> False

uploadFile :: PhotoSet -> FilePath -> ZM PhotoID
uploadFile ps fn = do
    fileType <- liftIO $ getStrippedImageFileType fn
    modified <- liftIO $ getModificationTime fn
    fileData <- liftIO $ BS.readFile fn
    let baseUriStr = maybe (fail $ "Unable to determine upload URL for set: " ++ show ps)
                           id (psUploadUrl ps)
        baseUri    = maybe (error $ "Unable to parse upload URL: " ++ baseUriStr)
                           id (parseURI baseUriStr)
        params     = [ ("file", takeFileName fn), ("modified", formatHttpTime modified) ]
        uploadUri  = baseUri { uriQuery = "?" ++ urlEncodeVars params }
    request <- createRequest uploadUri fileType fileData
    liftIO $ simpleHTTP request >>= getResponseBody >>= return . read . U.toString

createRequest :: URI -> Type -> BS.ByteString -> ZM (Request BS.ByteString)
createRequest uri ct bs = do
    token <- getToken
    let headers = zfAgentHeaders ++
                  catMaybes [ token >>= return . zfTokenHeader ] ++
                  [ Header HdrContentLength (show $ BS.length bs)
                  , Header HdrContentType (showType ct)
                  ]
    return $ Request {
                 rqURI     = uri,
                 rqMethod  = POST,
                 rqHeaders = headers,
                 rqBody    = bs
             }

getModificationTime :: FilePath -> IO UTCTime
getModificationTime fn = do
    stat <- getFileStatus fn
    let mtime = modificationTime stat
    return $ posixSecondsToUTCTime (realToFrac mtime :: POSIXTime)

formatHttpTime :: UTCTime -> String
formatHttpTime utcTime =
    let timeZone  = TimeZone 0 False "GMT"
        zonedTime = utcToZonedTime timeZone utcTime
    in formatTime defaultTimeLocale rfc822DateFormat zonedTime
