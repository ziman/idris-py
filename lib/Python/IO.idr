module Python.IO

%default total
%access public

unRaw : FFI_C.Raw a -> a
unRaw (MkRaw x) = x

data HList : List Type -> Type where
  Nil : HList []
  (::) : (x : a) -> HList as -> HList (a :: as)

||| Supported Python foreign types.
data FFI_PyTypes : Type -> Type where

  -- Primitive types
  FFI_PyInt     : FFI_PyTypes Int
  FFI_PyNat     : FFI_PyTypes Nat
  FFI_PyInteger : FFI_PyTypes Integer
  FFI_PyFloat   : FFI_PyTypes Float
  FFI_PyBool    : FFI_PyTypes Bool
  FFI_PyChar    : FFI_PyTypes Char
  FFI_PyString  : FFI_PyTypes String

  -- Other types
  FFI_PyUnit  : FFI_PyTypes ()
  FFI_PyPair  : FFI_PyTypes a -> FFI_PyTypes b -> FFI_PyTypes (a, b)
  FFI_PyList  : FFI_PyTypes a -> FFI_PyTypes (List a)
  FFI_PyMaybe : FFI_PyTypes a -> FFI_PyTypes (Maybe a)
  FFI_PyFun   : FFI_PyTypes a -> FFI_PyTypes b -> FFI_PyTypes (a -> b)
  FFI_PyHList : FFI_PyTypes (HList as)

  ||| Python objects, opaque to Idris.
  FFI_PyPtr : FFI_PyTypes Ptr

  ||| Arbitrary Idris objects, opaque to Python.
  FFI_PyAny : FFI_PyTypes (FFI_C.Raw a)

FFI_Py : FFI
FFI_Py = MkFFI FFI_PyTypes String String

PIO : Type -> Type
PIO = IO' FFI_Py
