module Python

%default total

data PyTypes : Type -> Type where
  PyStr     : PyTypes String
  PyFloat   : PyTypes Float
  PyInt     : PyTypes Int
  PyInteger : PyTypes Integer
  PyChar    : PyTypes Char
  PyPtr     : PyTypes Ptr
  PyUnit    : PyTypes ()

FFI_Py : FFI
FFI_Py = MkFFI PyTypes String String

-- read "pie-oh"
PIO : Type -> Type
PIO = IO' FFI_Py

data Args : Type where
  Fixed : (as : List Type) -> Args

data FieldType : Type where
  Attr : FieldType
  Method : (args : Args) -> FieldType

record Object : (fields : String -> FieldType -> Type -> Type) -> Type where
  MkObject : (obj : Ptr) -> Object fs

data HList : List Type -> Type where
  Nil : HList []
  (::) : (x : a) -> HList as -> HList (a :: as)

fieldTy : FieldType -> Type -> Type
fieldTy Attr r = PIO r
fieldTy (Method $ Fixed as) r = HList as -> PIO r

getField : Object fs -> String -> PIO Ptr
getField (MkObject obj) f = foreign FFI_Py "idris_getfield" (Ptr -> String -> PIO Ptr) obj f

callField : Object fs -> String -> HList ts -> PIO Ptr
callField (MkObject obj) f args = ?rhs

infixl 3 ./
(./) : PIO (Object fields) -> (f : String) -> {auto pf : fields f fty a} -> fieldTy fty a
(./) {fty = Attr} obj f = do
  o <- obj
  x <- getField o f
  return $ believe_me x

(./) {fty = Method (Fixed as)} obj f = \args => do
  o <- obj
  x <- callField o f args
  return $ believe_me x

-- Example application: python requests

data Py_Response : String -> FieldType -> Type -> Type where
  Py_Response_text : Py_Response "text" Attr String

data Py_Session : String -> FieldType -> Type -> Type where
  Py_Session_get : Py_Session "get" (Method $ Fixed [String]) (Object Py_Response)

data Py_Requests : String -> FieldType -> Type -> Type where
  Py_Requests_Session : Py_Requests "Session" (Method $ Fixed []) (Object Py_Session)

import_ : String -> PIO Ptr
import_ n = foreign FFI_Py "idris_pymodule" (String -> PIO Ptr) n
