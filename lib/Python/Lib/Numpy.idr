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

record Shape where
  constructor MkShape
  rows : Nat
  cols : Nat
  dtype : DType ty

Matrix : Nat -> Nat -> DType ty -> Signature
Matrix r c dtype f = case f of
  _ => Object f

NDArrayT : Signature
NDArrayT f = case f of
  "transpose" => fun (s : (Erased Shape) ** (m : (Obj $ Matrix (rows $ unerase s) (cols $ unerase s) (dtype $ unerase s)) ** ())) $
    forall $ \(MkShape r c dtype) =>
      pi $ \m : Obj (Matrix r c dtype) =>
        Return $ Obj (Matrix c r dtype)

  _ => PyType f

Numpy : Signature
Numpy f = case f of
  "ndarray" => Attr $ Obj NDArrayT
  _ => Module f

import_ : PIO $ Obj Numpy
import_ = importModule "numpy"
