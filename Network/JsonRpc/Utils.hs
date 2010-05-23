module Network.JsonRpc.Utils (
    field,
    mField
) where

import Data.Data
import Text.JSON
import Text.JSON.Types

field :: JSON a => String -> JSObject JSValue -> Result a
field k obj = maybe (fail $ "No such element: " ++ k)
                    readJSON
                    (lookup k (fromJSObject obj))

mField :: JSON a => String -> JSObject JSValue -> Result (Maybe a)
mField k obj = maybe (fail $ "No such element: " ++ k)
                     (\val -> case val of
                                   JSNull -> return Nothing
                                   _      -> readJSON val >>= return . Just)
                     (lookup k (fromJSObject obj))

instance Data JSValue where
    gfoldl k z JSNull           = z JSNull
    gfoldl k z (JSBool a)       = z JSBool `k` a
    gfoldl k z (JSRational a b) = z JSRational `k` a `k` b
    gfoldl k z (JSString a)     = z JSString `k` a
    gfoldl k z (JSArray a)      = z JSArray `k` a
    gfoldl k z (JSObject a)     = z JSObject `k` a
    gunfold k z c               = case constrIndex c of
                                       1 -> z JSNull
                                       2 -> k (z JSBool)
                                       3 -> k (k (z JSRational))
                                       4 -> k (z JSString)
                                       5 -> k (z JSArray)
                                       6 -> k (z JSObject)
    toConstr JSNull             = val_con_C1
    toConstr (JSBool _)         = val_con_C2
    toConstr (JSRational _ _)   = val_con_C3
    toConstr (JSString _)       = val_con_C4
    toConstr (JSArray _)        = val_con_C5
    toConstr (JSObject _)       = val_con_C6
    dataTypeOf _                = val_ty_T

val_con_C1 = mkConstr val_ty_T "JSNull" [] Prefix
val_con_C2 = mkConstr val_ty_T "JSBool" [] Prefix
val_con_C3 = mkConstr val_ty_T "JSRational" [] Prefix
val_con_C4 = mkConstr val_ty_T "JSString" [] Prefix
val_con_C5 = mkConstr val_ty_T "JSArray" [] Prefix
val_con_C6 = mkConstr val_ty_T "JSObject" [] Prefix
val_ty_T   = mkDataType "Text.JSON.Types.JSValue" [val_con_C1, val_con_C2, val_con_C3, val_con_C4, val_con_C5, val_con_C6]

instance Data JSString where
    gfoldl k z (JSONString a) = z JSONString `k` a
    gunfold k z c             = case constrIndex c of
                                     1 -> k (z JSONString)
    toConstr (JSONString _)   = str_con_C1
    dataTypeOf _              = str_ty_T

str_con_C1 = mkConstr str_ty_T "JSONString" [] Prefix
str_ty_T   = mkDataType "Text.JSON.Types.JSString" [str_con_C1]

instance Data a => Data (JSObject a) where
    gfoldl k z (JSONObject a) = z JSONObject `k` a
    gunfold k z c             = case constrIndex c of
                                     1 -> k (z JSONObject)
    toConstr (JSONObject _)   = obj_con_C1
    dataTypeOf _              = obj_ty_T

obj_con_C1 = mkConstr obj_ty_T "JSONObject" [] Prefix
obj_ty_T   = mkDataType "Text.JSON.Types.JSObject" [obj_con_C1]
