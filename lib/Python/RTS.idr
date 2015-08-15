module Python.RTS

import Python.IO

%access abstract
%default total

private
getGlobalPrim : (name : String) -> PythonIO Ptr
getGlobalPrim name = foreign FFI_Py "_idris_get_global" (String -> PythonIO Ptr) name

abstract
getGlobal : (name : String) -> PIO Ptr
getGlobal = safeWrap . getGlobalPrim

private
fromPythonIO : PythonIO a -> Ptr
fromPythonIO action =
  unsafePerformIO $
    foreign FFI_Py "_idris_marshal_IO" (Raw (PythonIO a) -> PythonIO Ptr) (MkRaw action)

abstract
fromPIO : PIO a -> Ptr
fromPIO = fromPythonIO . runPIO
