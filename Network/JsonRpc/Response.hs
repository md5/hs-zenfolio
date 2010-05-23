module Network.JsonRpc.Response (
    Response(Rsp)
) where

import Prelude hiding (error, id)
import Control.Applicative
import Text.JSON

import Network.JsonRpc.Error
import Network.JsonRpc.Utils

data Response = Rsp {
    --outcome :: Either Error a,
    result :: Maybe JSValue,
    error  :: Maybe Error,
    id     :: Int
} deriving (Eq, Show)

instance JSON Response where
    showJSON rsp = makeObj
        [ ("result", showJSON $ result rsp)
        , ("error", showJSON $ error rsp)
        , ("id", showJSON $ id rsp)
        ]

    readJSON (JSObject obj) = Rsp
            <$> mField "result" obj
            <*> mField "error" obj
            <*> field "id" obj

    readJSON v = fail $ "Unexpected JSON value for Response: " ++ show v
