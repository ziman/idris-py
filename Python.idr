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
  PyAny     : PyTypes (Raw a)

FFI_Py : FFI
FFI_Py = MkFFI PyTypes String String

-- read "pie-oh"
PIO : Type -> Type
PIO = IO' FFI_Py

data Args : Type where
  Fixed : (as : List Type) -> Args

record Object : (fields : String -> Type -> Type) -> Type where
  MkObject : (obj : Ptr) -> Object fs

record Method : (args : Args) -> (ret : Type) -> Type where
  MkMethod : (meth : Ptr) -> Method args ret

data HList : List Type -> Type where
  Nil : HList []
  (::) : (x : a) -> HList as -> HList (a :: as)

unRaw : Raw a -> a
unRaw (MkRaw a) = a

infixl 3 ./
(./) : Object fields -> (f : String) -> {auto pf : fields f a} -> PIO a
(./) {a = a} obj f =
  unRaw <$>
    foreign FFI_Py "idris_getfield" (Ptr -> String -> PIO (Raw a)) obj f

methTy : Args -> Type -> Type
methTy (Fixed as) ret = HList as -> PIO ret

callFixedMethod : Method (Fixed as) ret -> HList as -> PIO ret
callFixedMethod {as = as} meth args = 
  unRaw <$>
    foreign FFI_Py "idris_call" (Ptr -> Raw (HList as) -> PIO (Raw ret)) meth args

infixl 1 .$
(.$) : PIO (Method margs ret) -> methTy args ret
(.$) {margs = Fixed as} meth =
  \args => do
    m <- meth
    callFixedMethod meth args

-- Example application: python requests

data Py_Response : String -> Type -> Type where
  Py_Response_text : Py_Response "text" String

data Py_Session : String -> Type -> Type where
  Py_Session_get : Py_Session "get" (Method (Fixed [String]) $ Object Py_Response)

data Py_Requests : String -> Type -> Type where
  Py_Requests_Session : Py_Requests "Session" (Method (Fixed []) $ Object Py_Session)

import_ : (ty : String -> Type -> Type) -> (name : String) -> PIO (Object ty)
import_ ty n = unRaw <$> foreign FFI_Py "idris_pymodule" (String -> PIO (Raw $ Object ty)) n
