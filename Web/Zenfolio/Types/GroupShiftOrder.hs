{-# LANGUAGE DeriveDataTypeable #-}

module Web.Zenfolio.Types.GroupShiftOrder (
    GroupShiftOrder(..)
) where

import Data.Data (Typeable, Data)
import Text.JSON (JSON(..))
import Text.JSON.Generic (toJSON, fromJSON)

data GroupShiftOrder = CreatedAsc
                     | CreatedDesc
                     | ModifiedAsc
                     | ModifiedDesc
                     | TitleAsc
                     | TitleDesc
                     | GroupsTop
                     | GroupsBottom
    deriving (Eq, Ord, Enum, Bounded, Read, Show, Typeable, Data)

instance JSON GroupShiftOrder where
    showJSON = toJSON
    readJSON = fromJSON
