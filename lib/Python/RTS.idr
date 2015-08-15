module Python.RTS

import Python.IO

%access abstract
%default total

abstract
getGlobal : (name : String) -> PIO Ptr
getGlobal name = foreign FFI_Py "_idris_get_global" (String -> PIO Ptr) name

abstract
fromPIO : PIO a -> Ptr
fromPIO action =
  unsafePerformIO $
    foreign FFI_Py "_idris_marshal_IO" (Raw (PIO a) -> PIO Ptr) (MkRaw action)

abstract
importModule : (modName : String) -> PIO Ptr
importModule modName =
  foreign FFI_Py "importlib.import_module" (String -> PIO Ptr) modName
