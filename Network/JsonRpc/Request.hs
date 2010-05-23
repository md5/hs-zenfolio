module Network.JsonRpc.Request (
    Request(Req)
) where

import Control.Applicative
import Prelude hiding (id)
import Text.JSON

import Network.JsonRpc.Utils

data Request = Req {
    method :: String,
    params :: [JSValue],
    id     :: Int
} deriving (Eq, Show)

instance JSON Request where
    showJSON req = makeObj
        [ ("method", showJSON $ method req)
        , ("params", showJSON $ params req)
        , ("id", showJSON $ id req)
        ]

    readJSON (JSObject obj) = Req <$> field "method" obj <*> field "params" obj <*> field "id" obj

    readJSON _ = fail ""
