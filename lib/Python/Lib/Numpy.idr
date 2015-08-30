module Python.Lib.Numpy

import Python
import Python.Prim
import Data.Erased

%default total
%access public

NDArray : Signature
NDArray f = case f of
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

NDArrayT : Signature
NDArrayT f = case f of
  "transpose" => [Arr] ~~> Arr
  _ => ArithT Arr f

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

Numpy : Signature
Numpy f = case f of

  "array" => ParAttr _ $ \a : Type =>
      [Obj (PyList (Obj (PyList a))), String] ~> Obj NDArray

  "abs" => [Arr] ~~> Arr
  "transpose" => [Arr] ~~> Arr
  "tile" => [Dyn, Obj $ PyList Nat] ~~> Arr
  "ndarray" => Attr $ Obj NDArrayT
  _ => Module f

import_ : PIO $ Obj Numpy
import_ = importModule "numpy"
