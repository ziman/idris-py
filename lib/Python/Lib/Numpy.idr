module Python.Lib.Numpy

import Python
import Python.Prim
import Data.Erased

%default total
%access public export

NDArray : Signature
NDArray f = case f of
  "__str__" => [] ~~> String
  _ => Object f

Arr : Type
Arr = Obj NDArray

ArithT : Type -> Signature
ArithT a f = case f of
  "__add__" => [a, a] ~~> a
  "__mul__" => [a, a] ~~> a
  "__sub__" => [a, a] ~~> a
  "__div__" => [a, a] ~~> a
  "__str__" => [a] ~~> String
  _ => PyType f

Mat : Nat -> Nat -> Maybe a -> Signature
Mat _ _ _ = Object

NDArrayT : Signature
NDArrayT f = case f of
  "transpose" => [Arr] ~~> Arr
  _ => ArithT Arr f

  "transpose_fulldep" => fun $
    forall r : Nat .
      forall c : Nat .
        forall a : Type .
          forall dtype : (Maybe a) .
            pi m : (Obj $ Mat r c dtype) .
              Return (Obj $ Mat c r dtype)

Numpy : Signature
Numpy f = case f of

  "array" => PAttr _ $ \a : Type =>
      [Obj (PyList (Obj (PyList a))), String] ~> Arr

  "reshape" => [Arr, (Nat, Nat)] ~~> Arr
  "abs" => [Arr] ~~> Arr
  "dot" => [Arr, Arr] ~~> Arr
  "transpose" => [Arr] ~~> Arr
  "tile" => [Dyn, (Nat, Nat)] ~~> Arr
  "ndarray" => Attr $ Obj NDArrayT
  _ => Module f

import_ : PIO $ Obj Numpy
import_ = importModule "numpy"
