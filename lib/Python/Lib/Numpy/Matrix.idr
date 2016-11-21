module Python.Lib.Numpy.Matrix

import Python
import Python.Prim
import Python.Lib.Numpy

import Data.Vect

%access public export
%default total

-- private
np : Obj Numpy
np = unsafePerformIO Numpy.import_

-- private
nda : Obj NDArrayT
nda = np /. "ndarray"

record DType (ty : Type) where
  constructor MkDType
  dtName : String
  dtFromInteger : Integer -> ty
  dtFromDouble   : Double -> ty

DDouble : DType Double
DDouble = MkDType "float" fromInteger id

-- abstract
record Matrix (rows : Nat) (cols : Nat) (dtype : DType a) where
  constructor MkM
  arr : Obj NDArray

-- private
unsafeNp : PIO (Obj NDArray) -> Matrix r c dt
unsafeNp = MkM . unsafePerformIO

-- private
op : (f : String)
  -> {auto pf : NDArrayT f = [Obj NDArray, Obj NDArray] ~~> Obj NDArray}
  -> Matrix r c dt -> Matrix r c dt -> Matrix r c dt
op f (MkM x) (MkM y) = unsafeNp $ nda /. f $. [x, y]

-- abstract
fill : {dt : DType a} -> a -> Matrix r c dt
fill {r=r} {c=c} x = unsafeNp $ np /. "tile" $. [toDyn x, (r,c)]

fromInteger : Integer -> Matrix r c dt
fromInteger {dt=dt} = fill . dtFromInteger dt

fromDouble : Double -> Matrix r c dt
fromDouble {dt=dt} = fill . dtFromDouble dt

-- abstract
singleton : {dt : DType a} -> a -> Matrix 1 1 dt
singleton {a=a} {dt=dt} x =
  unsafeNp $
    np //. FP "array" a $. [pyList [pyList [x]], dtName dt]

-- abstract
dot : Matrix r c dt -> Matrix c k dt -> Matrix r k dt
dot (MkM x) (MkM y) = unsafeNp $ np /. "dot" $. [x,y]

-- abstract
transpose : Matrix r c dt -> Matrix c r dt
transpose (MkM x) = unsafeNp $ np /. "transpose" $. [x]

-- abstract
array : (dt : DType a) -> Vect r (Vect c a) -> Matrix r c dt
array {a=a} dt xs = unsafeNp $ np //. FP "array" a $. [c (map c xs), dtName dt]
  where
    c : {a : Type} -> Vect n a -> Obj (PyList a)
    c xs = pyList $ toList xs

-- abstract
reshape : Matrix r c dt -> {auto pf : r*c = r'*c'} -> Matrix r' c' dt
reshape {r'=r'} {c'=c'} (MkM x) =
  unsafeNp $
    np /. "reshape" $. [x, (r', c')]

-- abstract
(/) : Matrix r c dt -> Matrix r c dt -> Matrix r c dt
(/) = op "__div__"

-- abstract
minus : Matrix r c dt -> Matrix r c dt -> Matrix r c dt
minus = op "__sub__"

-- abstract
abs : Matrix r c dt -> Matrix r c dt
abs (MkM x) = unsafeNp $ np /. "abs" $. [x]

implementation Num (Matrix r c dt) where
  (+) = op "__add__"
  (*) = op "__mul__"
  fromInteger = Matrix.fromInteger

implementation Show (Matrix r c dt) where
  show (MkM x) = unsafePerformIO $ x /. "__str__" $. []
