module Web.Zenfolio.Types.JSON where

import Control.Applicative
import Data.Time.Format
import Data.Time.LocalTime
import System.Locale
import Text.JSON
import Web.Zenfolio.Types
import Web.Zenfolio.Utils

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
             <*> mField "FileHash" obj
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

instance JSON AccessDescriptor where
    showJSON descriptor = makeObj
        [ ("$type", showJSON $ "AccessDescriptor")
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

instance JSON PhotoSetType where
    showJSON setType = makeObj
        [ ("$type", showJSON $ "PhotoSetType")
        , ("Value", showJSON $ case setType of {
                                   Gallery    -> "Gallery";
                                   Collection -> "Collection";
                               })
        ]

    showJSON Gallery = showJSON "Gallery"
    showJSON Collection = showJSON "Collection"

    readJSON (JSString str) =
        case fromJSString str of
            "Gallery"    -> return Gallery
            "Collection" -> return Collection
            _            -> fail $ "Unknown photo set type: " ++ show str

    readJSON json = fail $ "Unexpected JSON PhotoSetType: " ++ show json

mShowJSON :: JSON a => Maybe a -> JSValue
mShowJSON = maybe JSNull showJSON

instance JSON PhotoSetUpdater where
    showJSON updater = makeObj
        [ ("$type", showJSON $ "PhotoSetUpdater")
        , ("Title", mShowJSON $ psuTitle updater)
        , ("Caption", mShowJSON $ psuCaption updater)
        , ("Keywords", mShowJSON $ psuKeywords updater)
        , ("Categories", mShowJSON $ psuCategories updater)
        , ("CustomReference", mShowJSON $ psuCustomReference updater)
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
