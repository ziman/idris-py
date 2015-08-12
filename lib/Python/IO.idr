module Python.IO

import Python.Objects
import Python.Telescope

%default total
%access public

unRaw : FFI_C.Raw a -> a
unRaw (MkRaw x) = x

||| Supported Python foreign types.
data PyTypes : Type -> Type where

  -- Primitive types
  PyInt     : PyTypes Int
  PyNat     : PyTypes Nat
  PyInteger : PyTypes Integer
  PyFloat   : PyTypes Float
  PyBool    : PyTypes Bool
  PyChar    : PyTypes Char
  PyString  : PyTypes String

  -- Other types
  PyUnit  : PyTypes ()
  PyPair  : PyTypes a -> PyTypes b -> PyTypes (a, b)
  PyList  : PyTypes a -> PyTypes (List a)
  PyFun   : PyTypes a -> PyTypes b -> PyTypes (a -> b)
  PyTList : PyTypes (TList t args)
  PyMaybe : PyTypes a -> PyTypes (Maybe a)

  ||| Python objects, opaque to Idris.
  PyPtr       : PyTypes Ptr

  ||| Arbitrary Idris objects, opaque to Python.
  PyAny : PyTypes (FFI_C.Raw a)

  ||| Python objects with a signature known to Idris.
  PyObj : PyTypes (Obj sig)

FFI_Py : FFI
FFI_Py = MkFFI PyTypes String String

||| Python IO. Read "pie-oh".
PIO : Type -> Type
PIO = IO' FFI_Py
