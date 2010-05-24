module Web.Zenfolio.Types where

import Data.Word
import Data.Time.LocalTime

type LoginName = String

type Password = String

type RealmID = Integer

type GroupID = Integer

type PhotoID = Integer

type PhotoSetID = Integer

type FileID = Integer

type CategoryID = Integer

type DateTime = LocalTime

type GroupIndex = Integer

data AuthChallenge = AuthChallenge {
    acPasswordSalt :: [Word8],
    acChallenge    :: [Word8]
} deriving (Eq, Show)

type AuthToken = String

type Owner = String

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
    uPublicAddress     :: Maybe Address,
    uPersonalAddress   :: Maybe Address,
    uRecentPhotoSets   :: [PhotoSet],
    uFeaturedPhotoSets :: [PhotoSet],
    uRootGroup         :: Maybe Group,
    uReferralCode      :: Maybe String,
    uExpiresOn         :: DateTime,
    uBalance           :: Maybe Float,
    uDomainName        :: Maybe String,
    uStorageQuota      :: Integer,
    uPhotoBytesQuota   :: Integer
} deriving (Eq, Show)

data Address = Address {
    addrFirstName   :: String,
    addrLastName    :: String,
    addrCompanyName :: String,
    addrStreet      :: String,
    addrStreet2     :: String,
    addrCity        :: String,
    addrZip         :: String,
    addrState       :: String,
    addrCountry     :: String,
    addrPhone       :: String,
    addrPhone2      :: String,
    addrFax         :: String,
    addrUrl         :: String,
    addrEmail       :: String,
    addrOther       :: String
} deriving (Eq, Show)

data File = File {
    fileId       :: FileID,
    fileWidth    :: Int,
    fileHeight   :: Int,
    fileSequence :: String,
    fileMimeType :: String,
    fileHash     :: Maybe [Word8],
    fileUrlCore  :: String
} deriving (Eq, Show)

data Group = Group {
    groupCaption          :: Maybe String,
    groupCreatedOn        :: DateTime,
    groupModifiedOn       :: DateTime,
    groupCollectionCount  :: Integer,
    groupSubGroupCount    :: Integer,
    groupGalleryCount     :: Integer,
    groupPhotoCount       :: Integer,
    groupParentGroups     :: [Group],
    groupElements         :: [GroupElement],
    groupPageUrl          :: Maybe String,
    groupTitlePhoto       :: Maybe File,
    groupId               :: GroupID,
    groupIndex            :: GroupIndex,
    groupTitle            :: String,
    groupAccessDescriptor :: Maybe AccessDescriptor,
    groupOwner            :: Maybe Owner,
    groupHideBranding     :: Bool
} deriving (Eq, Show)

data GroupElement =
    GroupElementGroup    Group |
    GroupElementPhotoSet PhotoSet
    deriving (Eq, Show)

type AccessType = String

type AccessMask = String

data AccessDescriptor = AccessDescriptor {
    adRealmId         :: RealmID,
    adAccessType      :: AccessType,
    adIsDerived       :: Bool,
    adAccessMask      :: AccessMask,
    adViewers         :: [String],
    adPasswordHint    :: Maybe String,
    adSrcPasswordHint :: Maybe String
} deriving (Eq, Show)

data PhotoSetType = Gallery | Collection deriving (Eq, Show)

type PhotoSetKeyword = String

data PhotoSetUpdater = PhotoSetUpdater {
    psuTitle           :: Maybe String,
    psuCaption         :: Maybe String,
    psuKeywords        :: Maybe [PhotoSetKeyword],
    psuCategories      :: Maybe [CategoryID],
    psuCustomReference :: Maybe String
} deriving (Eq, Show)

data PhotoSet = PhotoSet {
    psCaption            :: Maybe String,
    psCreatedOn          :: DateTime,
    psModifiedOn         :: DateTime,
    psPhotoCount         :: Integer,
    psPhotoBytes         :: Integer,
    psViews              :: Integer,
    psType               :: PhotoSetType,
    psFeaturedIndex      :: Maybe Integer,
    psTitlePhoto         :: Maybe File,
    psIsRandomTitlePhoto :: Bool,
    psParentGroups       :: [Group],
    psPhotos             :: [File],
    psKeywords           :: [PhotoSetKeyword],
    psCategories         :: [CategoryID],
    psUploadUrl          :: Maybe String,
    psPageUrl            :: Maybe String,
    psId                 :: PhotoSetID,
    psGroupIndex         :: GroupIndex,
    psTitle              :: Maybe String,
    psAccessDescriptor   :: Maybe AccessDescriptor,
    psOwner              :: Maybe Owner,
    psHideBranding       :: Bool
} deriving (Eq, Show)
