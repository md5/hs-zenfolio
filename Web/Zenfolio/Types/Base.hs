{-# LANGUAGE DeriveDataTypeable #-}

module Web.Zenfolio.Types.Base (
    LoginName,
    Password,
    GroupID,
    PhotoSetID,
    PhotoID,
    CategoryID,
    DateTime,
    GroupIndex,
    AuthToken,

    AuthChallenge(..),
    ErrorCode(..),
    User(..),
    Group(..),
    GroupElement(..),
    GroupUpdater(..),
    CategoryInfo(..),
    Photo(..),
    PhotoRotation(..),
    PhotoUpdater(..),
    PhotoSet(..),
    PhotoSetType(..),
    PhotoSetUpdater(..)
) where

import Control.Applicative ((<*>), (<$>))
import Data.Data (Data, Typeable)
import Data.Maybe (catMaybes)
import Data.Time.Format (formatTime, parseTime)
import Data.Word (Word8)
import Data.Time.LocalTime (LocalTime)
import System.Locale (defaultTimeLocale)

import Text.JSON (JSON(..), JSValue(..), makeObj)
import Text.JSON.Generic (fromJSON, toJSON)

import qualified Web.Zenfolio.Types.Access as Access
import Web.Zenfolio.Utils

type LoginName = String

type Password = String

type GroupID = Integer

type PhotoSetID = Integer

type PhotoID = Integer

type FileID = Integer

type CategoryID = Integer

type DateTime = LocalTime

type GroupIndex = Integer

type FileHash = [Word8]

type PasswordSalt = [Word8]

type Challenge = [Word8]

data AuthChallenge = AuthChallenge {
    acPasswordSalt :: PasswordSalt,
    acChallenge    :: Challenge
} deriving (Eq, Show)

type AuthToken = String

data ErrorCode = E__ACCOUNTLOCKED
               | E_CONNECTIONISNOTSECURE
               | E_DUPLICATEEMAIL
               | E_DUPLICATELOGINNAME
               | E_INVALIDCREDENTIALS
               | E_INVALIDPARAM
               | E_NOSUCHOBJECT
               | E_NOTAUTHENTICATED
               | E_NOTAUTHORIZED
               | E_UNSPECIFIEDERROR
    deriving (Eq, Ord, Enum, Bounded, Read, Show, Data, Typeable)

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
} deriving (Eq, Show, Typeable, Data)

data File = File {
    fileId       :: FileID,
    fileWidth    :: Int,
    fileHeight   :: Int,
    fileSequence :: String,
    fileMimeType :: String,
    fileHash     :: Maybe FileHash,
    fileUrlCore  :: String
} deriving (Eq, Show, Typeable, Data)

data Group = Group {
    groupCaption          :: Maybe String,
    groupCreatedOn        :: DateTime,
    groupModifiedOn       :: DateTime,
    groupCollectionCount  :: Integer,
    groupSubGroupCount    :: Integer,
    groupGalleryCount     :: Integer,
    groupPhotoCount       :: Integer,
    groupParentGroups     :: [GroupID],
    groupElements         :: [GroupElement],
    groupPageUrl          :: Maybe String,
    groupTitlePhoto       :: Maybe Photo,
    groupId               :: GroupID,
    groupIndex            :: GroupIndex,
    groupTitle            :: String,
    groupAccessDescriptor :: Maybe Access.AccessDescriptor,
    groupOwner            :: Maybe LoginName,
    groupHideBranding     :: Bool
} deriving (Eq, Show)

data GroupElement = GroupElementGroup    Group
                  | GroupElementPhotoSet PhotoSet
    deriving (Eq, Show)

data GroupUpdater = GroupUpdater {
    guTitle           :: Maybe String,
    guCaption         :: Maybe String,
    guCustomReference :: Maybe String
} deriving (Eq, Show, Typeable, Data)

data CategoryInfo = CategoryInfo {
    categoryCode        :: CategoryID,
    categoryDisplayName :: String
} deriving (Eq, Show)

type PricingKey = Integer

data PhotoUpdater = PhotoUpdater {
    puTitle      :: Maybe String,
    puCaption    :: Maybe String,
    puKeywords   :: Maybe [String],
    puCategories :: Maybe [CategoryID],
    puCopyright  :: Maybe String,
    puFileName   :: Maybe String
} deriving (Eq, Show, Typeable, Data)

data PhotoRotation = None
                   | Rotate90
                   | Rotate180
                   | Rotate270
                   | Flip
                   | Rotate90Flip
                   | Rotate180Flip
                   | Rotate270Flip
    deriving (Eq, Ord, Enum, Bounded, Read, Show, Typeable, Data)

data Photo = Photo {
    photoId               :: PhotoID,
    photoWidth            :: Int,
    photoHeight           :: Int,
    photoSequence         :: String,
    photoAccessDescriptor :: Access.AccessDescriptor,
    photoTitle            :: Maybe String,
    photoCaption          :: Maybe String,
    photoFileName         :: Maybe String,
    photoUploadedOn       :: Maybe DateTime,
    photoTakenOn          :: Maybe DateTime,
    photoOwner            :: Maybe String,
    photoGallery          :: PhotoSetID,
    photoViews            :: Integer,
    photoSize             :: Integer,
    photoKeywords         :: [String],
    photoCategories       :: [CategoryID],
    photoPricingKey       :: Maybe PricingKey,
    photoMimeType         :: String,
    photoOriginalUrl      :: Maybe String,
    photoUrlCore          :: String,
    photoCopyright        :: Maybe String,
    photoRotation         :: PhotoRotation,
    photoFileHash         :: Maybe FileHash,
    photoPageUrl          :: Maybe String
} deriving (Eq, Show)

data PhotoSetType = Gallery
                  | Collection
    deriving (Eq, Show, Typeable, Data)

type PhotoSetKeyword = String

data PhotoSetUpdater = PhotoSetUpdater {
    psuTitle           :: Maybe String,
    psuCaption         :: Maybe String,
    psuKeywords        :: Maybe [PhotoSetKeyword],
    psuCategories      :: Maybe [CategoryID],
    psuCustomReference :: Maybe String
} deriving (Eq, Show, Typeable, Data)

data PhotoSet = PhotoSet {
    psCaption            :: Maybe String,
    psCreatedOn          :: DateTime,
    psModifiedOn         :: DateTime,
    psPhotoCount         :: Integer,
    psPhotoBytes         :: Integer,
    psViews              :: Integer,
    psType               :: PhotoSetType,
    psFeaturedIndex      :: Maybe Integer,
    psTitlePhoto         :: Maybe Photo,
    psIsRandomTitlePhoto :: Bool,
    psParentGroups       :: [GroupID],
    psPhotos             :: [Photo],
    psKeywords           :: [PhotoSetKeyword],
    psCategories         :: [CategoryID],
    psUploadUrl          :: Maybe String,
    psPageUrl            :: Maybe String,
    psId                 :: PhotoSetID,
    psGroupIndex         :: GroupIndex,
    psTitle              :: Maybe String,
    psAccessDescriptor   :: Maybe Access.AccessDescriptor,
    psOwner              :: Maybe LoginName,
    psHideBranding       :: Bool
} deriving (Eq, Show)

--------------------------
-- JSON bindings
--------------------------

instance JSON AuthChallenge where
    showJSON ac = makeObj
        [ ("$type", showJSON $ "AuthChallenge")
        , ("PasswordSalt", showJSON $ acPasswordSalt ac)
        , ("Challenge", showJSON $ acChallenge ac)
        ]

    readJSON (JSObject obj) = AuthChallenge <$> field "PasswordSalt" obj
                                            <*> field "Challenge" obj

    readJSON json = fail $ "Unexpected JSON for AuthChallenge: " ++ show json

jsonRpcDateFormat :: String
jsonRpcDateFormat = "%Y-%m-%d %H:%M:%S"

instance JSON ErrorCode where
    showJSON = toJSON
    readJSON = fromJSON

instance JSON LocalTime where
    showJSON t = makeObj
        [ ("$type", showJSON $ "DateTime")
        , ("Value", showJSON $ formatTime defaultTimeLocale jsonRpcDateFormat t)
        ]

    readJSON (JSObject obj) = do
        value <- field "Value" obj
        case parseTime defaultTimeLocale jsonRpcDateFormat value of
            Just lt -> return lt
            Nothing -> fail $ "Error parsing date time: " ++ value

    readJSON json = fail $ "Unexpected JSON for DateTime: " ++ show json

instance JSON User where
    showJSON u = makeObj
        [ ("$type", showJSON $ "User")
        , ("LoginName", showJSON $ uLoginName u)
        , ("DisplayName", mShowJSON $ uDisplayName u)
        , ("FirstName", mShowJSON $ uFirstName u)
        , ("LastName", mShowJSON $ uLastName u)
        , ("PrimaryEmail", mShowJSON $ uPrimaryEmail u)
        , ("BioPhoto", mShowJSON $ uBioPhoto u)
        , ("Bio", mShowJSON $ uBio u)
        , ("Views", showJSON $ uViews u)
        , ("GalleryCount", showJSON $ uGalleryCount u)
        , ("CollectionCount", showJSON $ uCollectionCount u)
        , ("PhotoCount", showJSON $ uPhotoCount u)
        , ("PhotoBytes", showJSON $ uPhotoBytes u)
        , ("UserSince", showJSON $ uUserSince u)
        , ("LastUpdated", showJSON $ uLastUpdated u)
        , ("PublicAddress", mShowJSON $ uPublicAddress u)
        , ("PersonalAddress", mShowJSON $ uPersonalAddress u)
        , ("RecentPhotoSets", showJSON $ uRecentPhotoSets u)
        , ("FeaturedPhotoSets", showJSON $ uFeaturedPhotoSets u)
        , ("RootGroup", mShowJSON $ uRootGroup u)
        , ("ReferralCode", mShowJSON $ uReferralCode u)
        , ("ExpiresOn", showJSON $ uExpiresOn u)
        , ("Balance", mShowJSON $ uBalance u)
        , ("DomainName", mShowJSON $ uDomainName u)
        , ("StorageQuota", showJSON $ uStorageQuota u)
        , ("PhotoBytesQuota", showJSON $ uPhotoBytesQuota u)
        ]

    readJSON (JSObject obj) =
        User <$> field "LoginName" obj
             <*> mField "DisplayName" obj
             <*> mField "FirstName" obj
             <*> mField "LastName" obj
             <*> mField "PrimaryEmail" obj
             <*> mField "BioPhoto" obj
             <*> mField "Bio" obj
             <*> field "Views" obj
             <*> field "GalleryCount" obj
             <*> field "CollectionCount" obj
             <*> field "PhotoCount" obj
             <*> field "PhotoBytes" obj
             <*> field "UserSince" obj
             <*> field "LastUpdated" obj
             <*> mField "PublicAddress" obj
             <*> mField "PersonalAddress" obj
             <*> lField "RecentPhotoSets" obj
             <*> lField "FeaturedPhotoSets" obj
             <*> mField "RootGroup" obj
             <*> mField "ReferralCode" obj
             <*> field "ExpiresOn" obj
             <*> mField "Balance" obj
             <*> mField "DomainName" obj
             <*> field "StorageQuota" obj
             <*> field "PhotoBytesQuota" obj

    readJSON json = fail $ "Unexpected JSON for User: " ++ show json

instance JSON Address where
    showJSON addr = makeObj
        [ ("$type", showJSON $ "Address")
        , ("FirstName", showJSON $ addrFirstName addr)
        , ("LastName", showJSON $ addrLastName addr)
        , ("CompanyName", showJSON $ addrCompanyName addr)
        , ("Street", showJSON $ addrStreet addr)
        , ("Street2", showJSON $ addrStreet2 addr)
        , ("City", showJSON $ addrCity addr)
        , ("Zip", showJSON $ addrZip addr)
        , ("State", showJSON $ addrState addr)
        , ("Country", showJSON $ addrCountry addr)
        , ("Phone", showJSON $ addrPhone addr)
        , ("Phone2", showJSON $ addrPhone2 addr)
        , ("Fax", showJSON $ addrFax addr)
        , ("Url", showJSON $ addrUrl addr)
        , ("Email", showJSON $ addrEmail addr)
        , ("Other", showJSON $ addrOther addr)
        ]

    readJSON (JSObject obj) =
        Address <$> field "FirstName" obj
                <*> field "LastName" obj
                <*> field "CompanyName" obj
                <*> field "Street" obj
                <*> field "Street2" obj
                <*> field "City" obj
                <*> field "Zip" obj
                <*> field "State" obj
                <*> field "Country" obj
                <*> field "Phone" obj
                <*> field "Phone2" obj
                <*> field "Fax" obj
                <*> field "Url" obj
                <*> field "Email" obj
                <*> field "Other" obj

    readJSON json = fail $ "Unexpected JSON for Address: " ++ show json

instance JSON File where
    showJSON file = makeObj
        [ ("$type", showJSON $ "File")
        , ("Id", showJSON $ fileId file)
        , ("Width", showJSON $ fileWidth file)
        , ("Height", showJSON $ fileHeight file)
        , ("Sequence", showJSON $ fileSequence file)
        , ("MimeType", showJSON $ fileMimeType file)
        , ("FileHash", mShowJSON $ fileHash file)
        , ("UrlCore", showJSON $ fileUrlCore file)
        ]

    readJSON (JSObject obj) =
        File <$> field "Id" obj
             <*> field "Width" obj
             <*> field "Height" obj
             <*> field "Sequence" obj
             <*> field "MimeType" obj
             <*> oField "FileHash" obj
             <*> field "UrlCore" obj

    readJSON json = fail $ "Unexpected JSON for File: " ++ show json

instance JSON Group where
    showJSON group = makeObj
        [ ("$type", showJSON $ "Group")
        , ("Caption", mShowJSON $ groupCaption group)
        , ("CreatedOn", showJSON $ groupCreatedOn group)
        , ("ModifiedOn", showJSON $ groupModifiedOn group)
        , ("CollectionCount", showJSON $ groupCollectionCount group)
        , ("SubGroupCount", showJSON $ groupSubGroupCount group)
        , ("GalleryCount", showJSON $ groupGalleryCount group)
        , ("PhotoCount", showJSON $ groupPhotoCount group)
        , ("ParentGroups", showJSON $ groupParentGroups group)
        , ("Elements", showJSON $ groupElements group)
        , ("PageUrl", mShowJSON $ groupPageUrl group)
        , ("TitlePhoto", mShowJSON $ groupTitlePhoto group)
        , ("Id", showJSON $ groupId group)
        , ("GroupIndex", showJSON $ groupIndex group)
        , ("Title", showJSON $ groupTitle group)
        , ("AccessDescriptor", mShowJSON $ groupAccessDescriptor group)
        , ("Owner", mShowJSON $ groupOwner group)
        , ("HideBranding", showJSON $ groupHideBranding group)
        ]

    readJSON (JSObject obj) =
        Group <$> mField "Caption" obj
              <*> field "CreatedOn" obj
              <*> field "ModifiedOn" obj
              <*> field "CollectionCount" obj
              <*> field "SubGroupCount" obj
              <*> field "GalleryCount" obj
              <*> field "PhotoCount" obj
              <*> lField "ParentGroups" obj
              <*> lField "Elements" obj
              <*> mField "PageUrl" obj
              <*> mField "TitlePhoto" obj
              <*> field "Id" obj
              <*> field "GroupIndex" obj
              <*> field "Title" obj
              <*> mField "AccessDescriptor" obj
              <*> mField "Owner" obj
              <*> field "HideBranding" obj

    readJSON json = fail $ "Unexpected JSON for Group: " ++ show json

instance JSON GroupElement where
    showJSON (GroupElementGroup group)       = showJSON group
    showJSON (GroupElementPhotoSet photoSet) = showJSON photoSet

    readJSON v@(JSObject obj) = do
        objType <- field "$type" obj
        case objType of
            "Group"    -> GroupElementGroup    <$> readJSON v
            "PhotoSet" -> GroupElementPhotoSet <$> readJSON v
            _          -> fail $ "Unexepected GroupElement object type: " ++ show v

    readJSON json = fail $ "Unexpected JSON GroupElement: " ++ show json

instance JSON GroupUpdater where
    showJSON updater = makeObj $ catMaybes
        [ Just (recTypeField updater)
        , oJSONField "Title" $ guTitle updater
        , oJSONField "Caption" $ guCaption updater
        , oJSONField "CustomReference" $ guCustomReference updater
        ]

    readJSON (JSObject obj) =
        GroupUpdater <$> mField "Title" obj
                     <*> mField "Caption" obj
                     <*> mField "CustomReference" obj

    readJSON json = fail $ "Unexpected JSON GroupUpdater: " ++ show json

instance JSON CategoryInfo where
    showJSON cat = makeObj
        [ ("$type", showJSON $ "CategoryInfo")
        , ("Code", showJSON $ categoryCode cat)
        , ("DisplayName", showJSON $ categoryDisplayName cat)
        ]

    readJSON (JSObject obj) =
        CategoryInfo <$> field "Code" obj
                     <*> field "DisplayName" obj

    readJSON json = fail $ "Unexpected JSON for CategoryInfo: " ++ show json

instance JSON Photo where
    showJSON photo = makeObj 
        [ ("$type", showJSON $ "Photo")
        , ("Id", showJSON $ photoId photo)
        , ("Width", showJSON $ photoWidth photo)
        , ("Height", showJSON $ photoHeight photo)
        , ("Sequence", showJSON $ photoSequence photo)
        , ("AccessDescriptor", showJSON $ photoAccessDescriptor photo)
        , ("Title", mShowJSON $ photoTitle photo)
        , ("Caption", mShowJSON $ photoCaption photo)
        , ("FileName", mShowJSON $ photoFileName photo)
        , ("UploadedOn", mShowJSON $ photoUploadedOn photo)
        , ("TakenOn", mShowJSON $ photoTakenOn photo)
        , ("Owner", mShowJSON $ photoOwner photo)
        , ("Gallery", showJSON $ photoGallery photo)
        , ("Views", showJSON $ photoViews photo)
        , ("Size", showJSON $ photoSize photo)
        , ("Keywords", showJSON $ photoKeywords photo)
        , ("Categories", showJSON $ photoCategories photo)
        , ("PricingKey", mShowJSON $ photoPricingKey photo)
        , ("MimeType", showJSON $ photoMimeType photo)
        , ("OriginalUrl", mShowJSON $ photoOriginalUrl photo)
        , ("UrlCore", showJSON $ photoUrlCore photo)
        , ("Copyright", mShowJSON $ photoCopyright photo)
        , ("Rotation", showJSON $ photoRotation photo)
        , ("FileHash", mShowJSON $ photoFileHash photo)
        , ("PageUrl", mShowJSON $ photoPageUrl photo)
        ]

    readJSON (JSObject obj) =
        Photo <$> field "Id" obj
              <*> field "Width" obj
              <*> field "Height" obj
              <*> field "Sequence" obj
              <*> field "AccessDescriptor" obj
              <*> mField "Title" obj
              <*> mField "Caption" obj
              <*> mField "FileName" obj
              <*> mField "UploadedOn" obj
              <*> mField "TakenOn" obj
              <*> mField "Owner" obj
              <*> field "Gallery" obj
              <*> field "Views" obj
              <*> field "Size" obj
              <*> lField "Keywords" obj
              <*> lField "Categories" obj
              <*> mField "PricingKey" obj
              <*> field "MimeType" obj
              <*> mField "OriginalUrl" obj
              <*> field "UrlCore" obj
              <*> mField "Copyright" obj
              <*> field "Rotation" obj
              <*> mField "FileHash" obj
              <*> mField "PageUrl" obj

    readJSON json = fail $ "Unexpected JSON Photo: " ++ show json

instance JSON PhotoRotation where
    showJSON = toJSON
    readJSON = fromJSON

instance JSON PhotoSetType where
    showJSON = toJSON
    readJSON = fromJSON

instance JSON PhotoUpdater where
    showJSON updater = makeObj $ catMaybes
        [ Just (recTypeField updater)
        , oJSONField "Title" $ puTitle updater
        , oJSONField "Caption" $ puCaption updater
        , oJSONField "Keywords" $ puKeywords updater
        , oJSONField "Categories" $ puCategories updater
        , oJSONField "Copyright" $ puCopyright updater
        , oJSONField "FileName" $ puFileName updater
        ]

    readJSON (JSObject obj) =
        PhotoUpdater <$> mField "Title" obj
                        <*> mField "Caption" obj
                        <*> mField "Keywords" obj
                        <*> mField "Categories" obj
                        <*> mField "Copyright" obj
                        <*> mField "FileName" obj

    readJSON json = fail $ "Unexpected JSON PhotoUpdater: " ++ show json

instance JSON PhotoSetUpdater where
    showJSON updater = makeObj $ catMaybes
        [ Just (recTypeField updater)
        , oJSONField "Title" $ psuTitle updater
        , oJSONField "Caption" $ psuCaption updater
        , oJSONField "Keywords" $ psuKeywords updater
        , oJSONField "Categories" $ psuCategories updater
        , oJSONField "CustomReference" $ psuCustomReference updater
        ]

    readJSON (JSObject obj) =
        PhotoSetUpdater <$> mField "Title" obj
                        <*> mField "Caption" obj
                        <*> mField "Keywords" obj
                        <*> mField "Categories" obj
                        <*> mField "CustomReference" obj

    readJSON json = fail $ "Unexpected JSON PhotoSetUpdater: " ++ show json

instance JSON PhotoSet where
    showJSON photoSet = makeObj
        [ ("$type", showJSON $ "PhotoSet")
        , ("Caption", mShowJSON $ psCaption photoSet)
        , ("CreatedOn", showJSON $ psCreatedOn photoSet)
        , ("ModifiedOn", showJSON $ psModifiedOn photoSet)
        , ("PhotoCount", showJSON $ psPhotoCount photoSet)
        , ("PhotoBytes", showJSON $ psPhotoBytes photoSet)
        , ("Views", showJSON $ psViews photoSet)
        , ("Type", showJSON $ psType photoSet)
        , ("FeaturedIndex", mShowJSON $ psFeaturedIndex photoSet)
        , ("TitlePhoto", mShowJSON $ psTitlePhoto photoSet)
        , ("IsRandomTitlePhoto", showJSON $ psIsRandomTitlePhoto photoSet)
        , ("ParentGroups", showJSON $ psParentGroups photoSet)
        , ("Photos", showJSON $ psPhotos photoSet)
        , ("Keywords", showJSON $ psKeywords photoSet)
        , ("Categories", showJSON $ psCategories photoSet)
        , ("UploadUrl", mShowJSON $ psUploadUrl photoSet)
        , ("PageUrl", mShowJSON $ psPageUrl photoSet)
        , ("Id", showJSON $ psId photoSet)
        , ("GroupIndex", showJSON $ psGroupIndex photoSet)
        , ("Title", mShowJSON $ psTitle photoSet)
        , ("AccessDescriptor", mShowJSON $ psAccessDescriptor photoSet)
        , ("Owner", mShowJSON $ psOwner photoSet)
        , ("HideBranding", showJSON $ psHideBranding photoSet)
        ]

    readJSON (JSObject obj) =
        PhotoSet <$> mField "Caption" obj
                 <*> field "CreatedOn" obj
                 <*> field "ModifiedOn" obj
                 <*> field "PhotoCount" obj
                 <*> field "PhotoBytes" obj
                 <*> field "Views" obj
                 <*> field "Type" obj
                 <*> mField "FeaturedIndex" obj
                 <*> mField "TitlePhoto" obj
                 <*> field "IsRandomTitlePhoto" obj
                 <*> lField "ParentGroups" obj
                 <*> lField "Photos" obj
                 <*> lField "Keywords" obj
                 <*> lField "Categories" obj
                 <*> mField "UploadUrl" obj
                 <*> mField "PageUrl" obj
                 <*> field "Id" obj
                 <*> field "GroupIndex" obj
                 <*> mField "Title" obj
                 <*> mField "AccessDescriptor" obj
                 <*> mField "Owner" obj
                 <*> field "HideBranding" obj

    readJSON json = fail $ "Unexpected JSON PhotoSet: " ++ show json
