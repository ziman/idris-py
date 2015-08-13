module Python.Dynamic

import Python
import Python.Objects
import Python.IO

%default total
%access public

class RTTI a where
  expectedTypeName : a -> String

abstract
verify : RTTI a => a -> Maybe a
verify {a = a} x = unsafePerformIO $ do
  typeName <- foreign FFI_Py "_idris_typename" (Raw a -> PIO String) (MkRaw x)
  return $
    if typeName == expectedTypeName x
      then Just x
      else Nothing

abstract
cast : RTTI b => a -> Maybe b
cast = verify . believe_me
