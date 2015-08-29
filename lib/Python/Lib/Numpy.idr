module Python.Lib.Numpy

import Python
import Data.Erased

%default total
%access public

record DType (ty : Type) where
  constructor MkDType
  pythonName : String

DFloat : DType Float
DFloat = MkDType "float32"

Matrix : Nat -> Nat -> DType ty -> Signature
Matrix r c dtype f = case f of
  _ => Object f

NDArrayT : Signature
NDArrayT f = case f of
  {-

  -- You don't want to go all cranky with dependent types:

  "transpose" => with Erased fun (r : (Erased Nat) ** (c : (Erased Nat) ** (ty : (Erased Type) ** (dt : (Erased $ DType (unerase ty)) ** (m : (Obj $ Matrix (unerase r) (unerase c) (unerase dt)) ** Unit))))) $
    forall $ \r : Nat =>
      forall $ \c : Nat =>
        forall $ \ty : Type =>
          forall $ \dtype : DType ty =>
            pi $ \m : Obj (Matrix r c dtype) =>
              Return $ Obj (Matrix c r dtype)
  -}

  _ => PyType f

Numpy : Signature
Numpy f = case f of
  "ndarray" => Attr $ Obj NDArrayT
  _ => Module f

import_ : PIO $ Obj Numpy
import_ = importModule "numpy"
