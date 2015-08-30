module Python

import public Python.Telescope
import public Python.Objects
import public Python.IO
import public Python.Fields
import public Python.Functions
import public Python.Exceptions

%default total
%access public

||| Import a Python module. This is a low-level function
||| since the correctness of signatures cannot be checked.
|||
||| Libraries are encouraged to provide an ``import_`` function in their namespaces.
|||
||| @ sig     Signature of the returned object. Not checked.
||| @ modName Name of the module, as given to ``__import__``.
abstract
importModule : (modName : String) -> PIO (Obj sig)
importModule {sig = sig} modName =
  foreign FFI_Py "_idris_pymodule" (String -> PIO (Obj sig)) modName

||| Turn a PIO action into a Python function.
||| The function can then be used as a target for threading.Thread etc.
abstract
marshalPIO : PIO a -> [] ~> a
marshalPIO {a = a} action =
  unsafePerformIO $
    foreign FFI_Py "_idris_marshal_PIO" (Raw (PIO a) -> PIO ([] ~> a)) (MkRaw action)

abstract
getGlobal : (name : String) -> Obj sig
getGlobal {sig=sig} name =
  unsafePerformIO $
    foreign FFI_Py "_idris_get_global" (String -> PIO (Obj sig)) name
