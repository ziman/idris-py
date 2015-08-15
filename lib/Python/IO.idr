module Python.IO

import Python.Exceptions

%default total
%access public

unRaw : FFI_C.Raw a -> a
unRaw (MkRaw x) = x

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

  ||| Python objects, opaque to Idris.
  FFI_PyPtr : FFI_PyTypes Ptr

  ||| Arbitrary Idris objects, opaque to Python.
  FFI_PyAny : FFI_PyTypes (FFI_C.Raw a)

FFI_Py : FFI
FFI_Py = MkFFI FFI_PyTypes String String

||| Primitive low-level Python IO.
abstract
PythonIO : Type -> Type
PythonIO = IO' FFI_Py

||| High-level Python IO, including exception management.
abstract
PIO : Type -> Type
PIO = PythonIO . Result

instance Functor PIO where
  map f x = do
    OK x' <- x
      | Except etype e => return (Except etype e)
    return . OK $ f x'

instance Applicative PIO where
  pure x = pure $ OK x
  (<*>) f x = do
    OK f' <- f
      | Except etype e => return (Except etype e)
    OK x' <- x
      | Except etype e => return (Except etype e)
    return . OK $ f' x'

instance Monad PIO where
  (>>=) x g = do
    OK x' <- x
      | Except etype e => return (Except etype e)
    g x'

private partial
panic : (msg : String) -> PythonIO a
panic {a=a} msg = unRaw <$>
  foreign FFI_Py "_idris_error" (String -> PythonIO (Raw a)) msg

private
raise : Ptr -> PythonIO a
raise e = unRaw <$> foreign FFI_Py "_idris_raise" (Ptr -> PythonIO (Raw a)) e

private
runPIOPrim : PIO a -> PythonIO a
runPIOPrim action = do
  OK result <- action
    | Except etype e => raise e
  return result

abstract partial
runPIO : PIO a -> PythonIO a
runPIO = runPIOPrim

abstract
unsafeWrap : PythonIO a -> PIO a
unsafeWrap = map OK

abstract
putStr : String -> PIO ()
putStr = unsafeWrap . putStr

abstract
putStrLn : String -> PIO ()
putStrLn = unsafeWrap . putStrLn

abstract
print : Show a => a -> PIO ()
print = unsafeWrap . print

abstract
printLn : Show a => a -> PIO ()
printLn = unsafeWrap . printLn

abstract
safeWrap : PythonIO a -> PIO a
safeWrap {a = a} action =
  unRaw <$> foreign
      FFI_Py
      "_idris_try"
      (Raw (PythonIO a)
        -> (Raw a -> Raw (Result a))          -- succ
        -> (String -> Ptr -> Raw (Result a))  -- fail
        -> PythonIO (Raw $ Result a)
      )
      (MkRaw action)
      (MkRaw . OK . unRaw)
      (\et, e => MkRaw $ Except (fromString et) e)

abstract
unsafePerformPIO : PIO a -> a
unsafePerformPIO = unsafePerformIO . runPIOPrim
