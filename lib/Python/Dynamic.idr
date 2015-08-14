module Python.Dynamic

import Python
import Python.Objects
import Python.IO

%default total
%access public

||| A dynamically typed Python reference
Ref : Type
Ref = Ptr

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

abstract
fromRef : RTTI a => Ref -> Maybe a
fromRef = cast

abstract
toRef : a -> Ref
toRef = believe_me

infixl 4 //.
abstract
(//.) : Ref -> String -> PIO Ref
(//.) ref f = foreign FFI_Py "getattr" (Ref -> String -> PIO Ref) ref f

infixl 4 //:
abstract
(//:) : PIO Ref -> String -> PIO Ref
(//:) ref f = ref >>= (//. f)

infixl 4 $$.
abstract
($$.) : Ref -> List Ref -> PIO Ref
($$.) ref args = foreign FFI_Py "_idris_call" (Ref -> List Ref -> PIO Ref) ref args

infixl 4 $$:
abstract
($$:) : PIO Ref -> List Ref -> PIO ref
($$.) ref args = ref >>= ($$. args)

{-

record Class (typeName : String) where

* Ref
* Class "int" / Class "numpy"

paper: comparison of approaches:
  - list-based signatures
  - datatype-based signatures (like edwin's ffi)
  - completely dynamic approach with Ref
  - string-based typenames (Class "ndarray")
  - nominal, 1st-class classes (ndarray <- Numpy.import_ /. "ndarray"; Class ndarray)

-}
