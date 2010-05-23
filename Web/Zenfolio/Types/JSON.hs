module Web.Zenfolio.Types.JSON where

import Control.Applicative
import Text.JSON
import Web.Zenfolio.Types
import Web.Zenfolio.Utils

instance JSON AuthChallenge where
    showJSON ac = makeObj
        [ ("PasswordSalt", showJSON $ acPasswordSalt ac)
        , ("Challenge", showJSON $ acChallenge ac)
        ]

    readJSON (JSObject obj) = AuthChallenge <$> field "PasswordSalt" obj
                                            <*> field "Challenge" obj

    readJSON _ = fail ""

instance JSON User where
    showJSON u = makeObj
        [ ("LoginName", showJSON $ uLoginName u)
        , ("DisplayName", showJSON $ uDisplayName u)
        , ("FirstName", showJSON $ uFirstName u)
        , ("LastName", showJSON $ uLastName u)
        , ("PrimaryEmail", showJSON $ uPrimaryEmail u)
        , ("BioPhoto", showJSON $ uBioPhoto u)
        , ("Bio", showJSON $ uBio u)
        , ("Views", showJSON $ uViews u)
        , ("GalleryCount", showJSON $ uGalleryCount u)
        , ("CollectionCount", showJSON $ uCollectionCount u)
        , ("PhotoCount", showJSON $ uPhotoCount u)
        , ("PhotoBytes", showJSON $ uPhotoBytes u)
        , ("UserSince", showJSON $ uUserSince u)
        , ("LastUpdated", showJSON $ uLastUpdated u)
        , ("PublicAddress", showJSON $ uPublicAddress u)
        , ("PersonalAddress", showJSON $ uPersonalAddress u)
        , ("RecentPhotoSets", showJSON $ uRecentPhotoSets u)
        , ("FeaturedPhotoSets", showJSON $ uFeaturedPhotoSets u)
        , ("RootGroup", showJSON $ uRootGroup u)
        , ("ReferralCode", showJSON $ uReferralCode u)
        , ("ExpiresOn", showJSON $ uExpiresOn u)
        , ("Balance", showJSON $ uBalance u)
        , ("DomainName", showJSON $ uDomainName u)
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
             <*> field "PublicAddress" obj
             <*> field "PersonalAddress" obj
             <*> field "RecentPhotoSets" obj
             <*> field "FeaturedPhotoSets" obj
             <*> field "RootGroup" obj
             <*> field "ReferralCode" obj
             <*> mField "ExpiresOn" obj
             <*> mField "Balance" obj
             <*> mField "DomainName" obj
             <*> field "StorageQuota" obj
             <*> field "PhotoBytesQuota" obj

    readJSON _ = fail ""

instance JSON Address where
    showJSON = undefined
    readJSON = undefined

instance JSON File where
    showJSON = undefined
    readJSON = undefined

instance JSON Group where
    showJSON = undefined
    readJSON = undefined

instance JSON PhotoSet where
    showJSON = undefined
    readJSON = undefined
