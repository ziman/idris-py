module Python.RTS

import Python.IO

%access abstract
%default total

abstract
getGlobal : (name : String) -> PIO Ptr
getGlobal name = foreign FFI_Py "_idris_get_global" (String -> PIO Ptr) name

abstract
ptrFromPIO : PIO a -> Ptr
ptrFromPIO action =
  unsafePerformIO $
    foreign FFI_Py "_idris_marshal_IO" (Raw (PIO a) -> PIO Ptr) (MkRaw action)

abstract
importModule : (modName : String) -> PIO a
importModule {a=a} modName =
  unRaw <$>
    foreign FFI_Py "importlib.import_module" (String -> PIO (Raw a)) modName
