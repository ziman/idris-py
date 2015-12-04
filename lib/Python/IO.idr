module Python.IO

import Python.Objects

%default total
%access public

unRaw : FFI_C.Raw a -> a
unRaw (MkRaw x) = x

||| Supported Python foreign types.
data PyTypes : Type -> Type where

  -- Primitive types
  PyInt_io     : PyTypes Int
  PyNat_io     : PyTypes Nat
  PyInteger_io : PyTypes Integer
  PyDouble_io   : PyTypes Double
  PyBool_io    : PyTypes Bool
  PyChar_io    : PyTypes Char
  PyString_io  : PyTypes String

  -- Other types
  PyUnit_io  : PyTypes ()
  PyPair_io  : PyTypes a -> PyTypes b -> PyTypes (a, b)
  PyList_io  : PyTypes a -> PyTypes (List a)
  PyFun_io   : PyTypes a -> PyTypes b -> PyTypes (a -> b)
  PyMaybe_io : PyTypes a -> PyTypes (Maybe a)

  ||| Python objects, opaque to Idris.
  PyPtr_io       : PyTypes Ptr

  ||| Arbitrary Idris objects, opaque to Python.
  PyAny_io : PyTypes (FFI_C.Raw a)

  ||| Python objects with a signature known to Idris.
  PyObj_io : PyTypes (Obj sig)

FFI_Py : FFI
FFI_Py = MkFFI PyTypes String String

||| Python IO. Read "pie-oh".
PIO : Type -> Type
PIO = IO' FFI_Py
