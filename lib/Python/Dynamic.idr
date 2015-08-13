module Python.Dynamic

import Python
import Python.Objects
import Python.IO

%default total
%access public

class RTTI a where
  typeNameOk : a -> String -> Bool

instance RTTI Int where
  typeNameOk _ tn = (tn == "int")

instance RTTI Float where
  typeNameOk _ tn = (tn == "float")

instance RTTI String where
  typeNameOk _ tn = (tn == "str") || (tn == "unicode")

instance RTTI Integer where
  typeNameOk _ tn = (tn == "int")

instance RTTI Bool where
  typeNameOk _ tn = (tn == "bool")

instance RTTI Char where
  typeNameOk _ tn = (tn == "str") || (tn == "unicode")

{- TODO
instance RTTI a => RTTI (Maybe a) where
  typeNameOk _ tn = typeNameOk ( tn
-}

private
verify : RTTI a => a -> Maybe a
verify {a = a} x = unsafePerformIO $ do
  typeName <- foreign FFI_Py "_idris_typename" (Raw a -> PIO String) (MkRaw x)
  return $
    if typeNameOk x typeName
      then Just x
      else Nothing

abstract
cast : RTTI b => a -> Maybe b
cast = verify . believe_me
