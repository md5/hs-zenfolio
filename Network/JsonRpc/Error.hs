module Network.JsonRpc.Error (
    Error(Err)
) where

import Prelude hiding (error)
import Control.Applicative
import Text.JSON

import Network.JsonRpc.Utils

data Error = Err {
    message :: String,
    error   :: Maybe String
} deriving (Eq, Show)


instance JSON Error where
    showJSON err = makeObj
        [ ("message", showJSON $ message err)
        , ("error", showJSON $ error err)
        ]

    readJSON (JSObject obj) = Err
            <$> field "message" obj
            <*> mField "error" obj

    readJSON v = fail $ "Unexpected JSON value for Error: " ++ show v
