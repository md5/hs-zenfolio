module Network.JsonRpc.Utils (
    field,
    mField,
    lField,

    mShowJSON,
    oJSONField,
    recTypeField
) where

import Data.Data (Data, toConstr, showConstr)
import Text.JSON (JSON(..), JSObject(..), JSValue(..), Result)

-- Normal field, directly deserialized
field :: JSON a => String -> JSObject JSValue -> Result a
field k obj = maybe (fail $ "No such element: " ++ k)
                    readJSON
                    (lookup k (fromJSObject obj))

-- Nullable field with null translated to Nothing and value wrapped in Just
mField :: JSON a => String -> JSObject JSValue -> Result (Maybe a)
mField k obj = maybe (fail $ "No such element: " ++ k)
                     (\val -> case val of
                                   JSNull -> return Nothing
                                   _      -> readJSON val >>= return . Just)
                     (lookup k (fromJSObject obj))

-- Nullable list field with null translated to empty list
lField :: JSON a => String -> JSObject JSValue -> Result [a]
lField k obj = maybe (fail $ "No such element: " ++ k)
                     (\val -> case val of
                                   JSNull -> return []
                                   _      -> readJSON val)
                     (lookup k (fromJSObject obj))

mShowJSON :: JSON a => Maybe a -> JSValue
mShowJSON = maybe JSNull showJSON

oJSONField :: JSON a => String -> Maybe a -> Maybe (String, JSValue)
oJSONField f (Just value) = Just (f, showJSON value)
oJSONField _ Nothing      = Nothing

recTypeField :: Data a => a -> (String,JSValue)
recTypeField d = ("$type", showJSON $ showConstr (toConstr d))
