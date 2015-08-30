module Python.Lib.Numpy.Matrix

import Python
import Python.Prim
import Python.Lib.Numpy

%access public
%default total

private
np : Obj Numpy
np = unsafePerformIO Numpy.import_

private
nda : Obj NDArrayT
nda = np /. "ndarray"

record DType (ty : Type) where
  constructor MkDType
  pythonName : String
  dtFromInteger : Integer -> ty
  dtFromFloat   : Float -> ty

DFloat : DType Float
DFloat = MkDType "float" fromInteger id

abstract
record Matrix (rows : Nat) (cols : Nat) (dtype : DType a) where
  constructor MkM
  arr : Obj NDArray

private
unsafeNp : PIO (Obj NDArray) -> Matrix r c dt
unsafeNp = MkM . unsafePerformIO

private
op : (f : String)
  -> {auto pf : NDArrayT f = [Obj NDArray, Obj NDArray] ~~> Obj NDArray}
  -> Matrix r c dt -> Matrix r c dt -> Matrix r c dt
op f (MkM x) (MkM y) = unsafeNp $ nda /. f $. [x, y]

abstract
fill : {dt : DType a} -> a -> Matrix r c dt
fill {r=r} {c=c} x = unsafeNp $ np /. "tile" $. [toDyn x, pyList [r,c]]

fromInteger : Integer -> Matrix r c dt
fromInteger {dt=dt} = fill . dtFromInteger dt

fromFloat : Float -> Matrix r c dt
fromFloat {dt=dt} = fill . dtFromFloat dt

abstract
(/) : Matrix r c dt -> Matrix r c dt -> Matrix r c dt
(/) = op "__div__"

instance Num (Matrix r c dt) where
  (+) = op "__add__"
  (-) = op "__sub__"
  (*) = op "__mul__"
  abs (MkM x) = unsafeNp $ np /. "abs" $. [x]
  fromInteger = Matrix.fromInteger
