{-# LANGUAGE DeriveDataTypeable #-}

module Web.Zenfolio.Types.Access (
    AccessType(..),
    AccessMask,
    AccessMaskFlag(..),
    AccessDescriptor(..),
    AccessUpdater(..),
    DownloadKey(..),
    Keyring(..),
    RealmID
) where

import Control.Applicative ((<$>), (<*>))
import Data.Data (Data, Typeable)
import Data.Maybe (catMaybes)
import Text.JSON (JSON(..), JSValue(..), fromJSString, toJSString, makeObj)
import Text.JSON.Generic (toJSON, fromJSON)

import Web.Zenfolio.Utils

type RealmID = Integer

data AccessType = Private
                | Public
                | UserList
                | Password
    deriving (Eq, Enum, Bounded, Read, Show, Typeable, Data)

newtype AccessMask = AccessMask [AccessMaskFlag]
    deriving (Eq, Read, Typeable, Data)

instance Show AccessMask where
    show (AccessMask flags) = show flags

data AccessMaskFlag = None
                    -- "Hide" flags
                    | HideDateCreated
                    | HideDateModified
                    | HideDateTaken
                    | HideMetaData
                    | HideUserStats
                    | HideVisits
                    -- "No" flags
                    | NoCollections
                    | NoPrivateSearch
                    | NoPublicSearch
                    | NoRecentList
                    -- "Protect" flags
                    | ProtectExif
                    | ProtectExtraLarge
                    | ProtectLarge
                    | ProtectMedium
                    | ProtectOriginals
                    -- Guestbook flags
                    | ProtectGuestBook
                    | NoPublicGuestbookPosts
                    | NoPrivateGuestbookPosts
                    | NoAnonymousGuestbookPosts
                    -- Comment flags
                    | ProtectComments
                    | NoPublicComments
                    | NoPrivateComments
                    | NoAnonymousComments
                    -- Other flags
                    | PasswordProtectOriginals
                    | ProtectAll
    deriving (Eq, Ord, Enum, Bounded, Read, Show, Typeable, Data)

data AccessDescriptor = AccessDescriptor {
    adRealmId         :: RealmID,
    adAccessType      :: AccessType,
    adIsDerived       :: Bool,
    adAccessMask      :: AccessMask,
    adViewers         :: [String],
    adPasswordHint    :: Maybe String,
    adSrcPasswordHint :: Maybe String
} deriving (Eq, Show, Typeable, Data)

data AccessUpdater = AccessUpdater {
    auAccessMask :: Maybe AccessMask,
    auAccessType :: Maybe AccessType,
    auViewers    :: Maybe [String],
    auPassword   :: Maybe String,
    auIsDerived  :: Maybe Bool
} deriving (Eq, Show, Typeable, Data)

newtype DownloadKey = DownloadKey String
    deriving (Eq, Show, Typeable, Data)

newtype Keyring = Keyring String
    deriving (Eq, Show, Typeable, Data)

instance JSON AccessDescriptor where
    showJSON descriptor = makeObj
        [ recTypeField descriptor
        , ("RealmId", showJSON $ adRealmId descriptor)
        , ("AccessType", showJSON $ adAccessType descriptor)
        , ("IsDerived", showJSON $ adIsDerived descriptor)
        , ("AccessMask", showJSON $ adAccessMask descriptor)
        , ("Viewers", showJSON $ adViewers descriptor)
        , ("PasswordHint", mShowJSON $ adPasswordHint descriptor)
        , ("SrcPasswordHint", mShowJSON $ adSrcPasswordHint descriptor)
        ]

    readJSON (JSObject obj) =
        AccessDescriptor <$> field "RealmId" obj
                         <*> field "AccessType" obj
                         <*> field "IsDerived" obj
                         <*> field "AccessMask" obj
                         <*> lField "Viewers" obj
                         <*> mField "PasswordHint" obj
                         <*> mField "SrcPasswordHint" obj

    readJSON json = fail $ "Unexpected JSON AccessDescriptor: " ++ show json

instance JSON AccessMask where
    showJSON (AccessMask flags) = JSString $ toJSString flagStr
        where flagStr = filter (`elem` ['[', ']']) $ show flags

    readJSON (JSString s) =
        AccessMask <$> return parseFlags
        where parseFlags :: [AccessMaskFlag]
              parseFlags = read $ "[" ++ fromJSString s ++ "]"

    readJSON json = fail $ "Unexpected JSON AccessMask: " ++ show json

instance JSON AccessType where
    showJSON = toJSON
    readJSON = fromJSON

instance JSON AccessUpdater where
    showJSON updater = makeObj $ catMaybes
        [ Just (recTypeField updater)
        , oJSONField "AccessMask" $ auAccessMask updater
        , oJSONField "AccessType" $ auAccessType updater
        , oJSONField "Viewers" $ auViewers updater
        , oJSONField "Password" $ auPassword updater
        , oJSONField "IsDerived" $ auIsDerived updater
        ]

    readJSON (JSObject obj) =
        AccessUpdater <$> mField "AccessMask" obj
                      <*> mField "AccessType" obj
                      <*> mField "Viewers" obj
                      <*> mField "Password" obj
                      <*> mField "IsDerived" obj

    readJSON json = fail $ "Unexpected JSON AccessUpdater: " ++ show json

instance JSON DownloadKey where
    showJSON (DownloadKey ring) = JSString $ toJSString ring

    readJSON (JSString s) = return $ DownloadKey (fromJSString s)

    readJSON json = fail $ "Unexpected JSON DownloadKey: " ++ show json

instance JSON Keyring where
    showJSON (Keyring ring) = JSString $ toJSString ring

    readJSON (JSString s) = return $ Keyring (fromJSString s)

    readJSON json = fail $ "Unexpected JSON Keyring: " ++ show json
