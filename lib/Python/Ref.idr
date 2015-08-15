module Python.Ref

import Python
import Python.IO

%default total
%access public

record PyTy where
  constructor MkPyTy
  pyTy : Dyn

instance Show PyTy where
  show = toString . pyTy

private
mkPyTy : (name : String) -> PyTy
mkPyTy = MkPyTy . unsafePerformIO . getGlobal

abstract
PyType : PyTy
PyType = mkPyTy "type"

abstract
PyInt : PyTy
PyInt = mkPyTy "int"

abstract
PyFloat : PyTy
PyFloat = mkPyTy "float"

abstract
PyStr : PyTy
PyStr = mkPyTy "str"

abstract
PyBytes : PyTy
PyBytes = mkPyTy "bytes"

abstract
PyList : PyTy
PyList = mkPyTy "list"

abstract
PyDict : PyTy
PyDict = mkPyTy "dict"

abstract
PySet : PyTy
PySet = mkPyTy "set"

abstract
PyTuple : PyTy
PyTuple = mkPyTy "tuple"

abstract
getClass : Dyn -> Dyn
getClass x = unsafePerformIO $ foreign FFI_Py "getattr" (Dyn -> String -> PIO Dyn) x "__class__"

abstract
PyModule : PyTy
PyModule = unsafePerformIO $ do
  builtins <- getGlobal "__builtins__"
  return $ MkPyTy (getClass builtins)

class Native a (ty : PyTy) | a
instance Native Int     PyInt
instance Native Integer PyInt
instance Native Float   PyFloat
instance Native String  PyStr

record Ref (ty : PyTy) where
  constructor MkRef
  ref : Dyn

abstract
fromRef : Native a ty => Ref ty -> a
fromRef (MkRef ref) = believe_me ref

abstract
toRef : Native a ty => Dyn -> Ref ty
toRef x = MkRef $ believe_me x

abstract
toDyn : a -> Dyn
toDyn = believe_me

-- TODO: implement in pure python for speed?
abstract
fromDyn : Dyn -> Maybe (Ref a)
fromDyn {a = MkPyTy ty} x = unsafePerformIO $ do
    ok <- foreign FFI_Py "isinstance" (Dyn -> Dyn -> PIO Bool) x ty
    return $ if ok
      then Just $ believe_me x
      else Nothing

private
panic : String -> PIO a
panic {a=a} msg = unRaw <$> foreign FFI_Py "_idris_error" (String -> PIO (Raw a)) msg

private partial
verify : Dyn -> PIO (Ref a)
verify {a = expected} x =
  case fromDyn {a = expected} x of
    Just x  => return x
    Nothing => do
      actual <- x //. "__class__"
      panic $ "dynamic check failed: cannot cast " ++ toString actual ++ " to " ++ show expected

infixl 4 /.
abstract partial
(/.) : Ref a -> String -> PIO (Ref b)
(/.) (MkRef ref) f = (ref //. f) >>= verify

infixl 4 /:
abstract partial
(/:) : PIO (Ref a) -> String -> PIO (Ref b)
(/:) ref f = ref >>= (/. f)

infixl 4 $.
abstract partial
($.) : Ref a -> List Dyn -> PIO (Ref b)
($.) (MkRef ref) args = (ref $$. args) >>= verify

infixl 4 $:
abstract partial
($:) : PIO (Ref a) -> List Dyn -> PIO (Ref b)
($:) ref args = ref >>= ($. args)

abstract partial
importModule : (modName : String) -> PIO $ Ref PyModule
importModule modName =
  foreign FFI_Py "importlib.import_module" (String -> PIO Dyn) modName
    >>= verify

{-

record Class (typeName : String) where

* Dyn
* Class "int" / Class "numpy"

paper: comparison of approaches:
  - list-based signatures
  - datatype-based signatures (like edwin's ffi)
  - completely dynamic approach with Dyn
  - string-based typenames (Class "ndarray")
  - nominal, 1st-class classes (ndarray <- Numpy.import_ /. "ndarray"; Class ndarray)

-}
