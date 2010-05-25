module Network.JsonRpc.Utils (
    field,
    mField,
    lField
) where

import Text.JSON (JSON(..), JSObject(..), JSValue(..), Result)

-- Normal field, directly deserialized
field :: JSON a => String -> JSObject JSValue -> Result a
field k obj = maybe (fail $ "No such element: " ++ k)
                    readJSON
                    (lookup k (fromJSObject obj))

-- Nullable field with null translated to Maybe and value wrapped in Just
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
