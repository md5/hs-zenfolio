module Web.Zenfolio.Types where

import Data.Word

type LoginName = String

type Password = String

type RealmID = Integer

type GroupID = Integer

type PhotoID = Integer

type PhotoSetID = Integer

type FileID = Integer

type CategoryID = Integer

type DateTime = Integer

data AuthChallenge = AuthChallenge {
    acPasswordSalt :: [Word8],
    acChallenge    :: [Word8]
} deriving (Eq, Show)

type AuthToken = String

data User = User {
    uLoginName         :: LoginName,
    uDisplayName       :: Maybe String,
    uFirstName         :: Maybe String,
    uLastName          :: Maybe String,
    uPrimaryEmail      :: Maybe String,
    uBioPhoto          :: Maybe File,
    uBio               :: Maybe String,
    uViews             :: Integer,
    uGalleryCount      :: Integer,
    uCollectionCount   :: Integer,
    uPhotoCount        :: Integer,
    uPhotoBytes        :: Integer,
    uUserSince         :: DateTime,
    uLastUpdated       :: DateTime,
    uPublicAddress     :: Address,
    uPersonalAddress   :: Address,
    uRecentPhotoSets   :: [ PhotoSet ],
    uFeaturedPhotoSets :: [ PhotoSet ],
    uRootGroup         :: Group,
    uReferralCode      :: String,
    uExpiresOn         :: Maybe DateTime,
    uBalance           :: Maybe Float,
    uDomainName        :: Maybe String,
    uStorageQuota      :: Integer,
    uPhotoBytesQuota   :: Integer
} deriving (Eq, Show)

data Address = Address {
} deriving (Eq, Show)

data File = File {
} deriving (Eq, Show)

data Group = Group {
} deriving (Eq, Show)

data PhotoSet = PhotoSet {
} deriving (Eq, Show)
