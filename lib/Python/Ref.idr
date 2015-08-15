module Python.Ref

import Python
import Python.IO
import Python.Dyn
import Python.RTS

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
toRef : Native a ty => a -> Ref ty
toRef x = MkRef $ believe_me x

abstract
checkType : (ty : PyTy) -> Dyn -> Maybe (Ref ty)
checkType (MkPyTy ty) x = unsafePerformIO $ do
    ok <- foreign FFI_Py "isinstance" (Dyn -> Dyn -> PIO Bool) x ty
    return $ if ok
      then Just $ believe_me x
      else Nothing

private
panic : String -> PIO a
panic {a=a} msg = unRaw <$> foreign FFI_Py "_idris_error" (String -> PIO (Raw a)) msg

abstract
unsafeCheckType : Dyn -> PIO (Ref a)
unsafeCheckType {a = expected} x =
  case checkType expected x of
    Just y  => return y
    Nothing => do
      actual <- x /. "__class__"
      panic $ "dynamic check failed: cannot cast " ++ toString actual ++ " to " ++ show expected

abstract
convert : Dyn -> Ref ty
convert {ty = MkPyTy ctor} x =
  unsafePerformIO $
    (ctor $. [x]) >>= unsafeCheckType

infixl 4 //.
abstract partial
(//.) : Ref a -> String -> PIO (Ref b)
(//.) (MkRef ref) f = (ref /. f) >>= unsafeCheckType

infixl 4 //:
abstract partial
(//:) : PIO (Ref a) -> String -> PIO (Ref b)
(//:) ref f = ref >>= (//. f)

infixl 4 $$.
abstract partial
($$.) : Ref a -> List Dyn -> PIO (Ref b)
($$.) (MkRef ref) args = (ref $. args) >>= unsafeCheckType

infixl 4 $$:
abstract partial
($$:) : PIO (Ref a) -> List Dyn -> PIO (Ref b)
($$:) ref args = ref >>= ($$. args)

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
