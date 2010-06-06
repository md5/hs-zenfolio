{-# LANGUAGE DeriveDataTypeable #-}

module Web.Zenfolio.Types.ShiftOrder (
    ShiftOrder(..)
) where

import Data.Data (Typeable, Data)
import Text.JSON (JSON(..))
import Text.JSON.Generic (toJSON, fromJSON)

data ShiftOrder = CreatedAsc
                | CreatedDesc
                | TakenAsc
                | TakenDesc
                | TitleAsc
                | TitleDesc
                | SizeAsc
                | SizeDesc
                | FileNameAsc
                | FileNameDesc
    deriving (Eq, Ord, Enum, Bounded, Read, Show, Typeable, Data)

instance JSON ShiftOrder where
    showJSON = toJSON
    readJSON = fromJSON
